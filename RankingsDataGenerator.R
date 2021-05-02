# Rankings Generate
# Script to generate rankings and scores using TFRRS data and transforming into tangible power ranking

# Load libraries
library(tidymodels)
library(httr)
library(dplyr)
library(jsonlite)
library(RPostgreSQL)
library(DBI)
library(RSQLite)
library(reshape2)
library(yaml)
library(rvest)

# Read in functions
source("Scraping_Fxns.R")

# Read connection data from yaml
aws.yml <- read_yaml("aws.yaml")

# Connect to database
aws <- dbConnect(
  RPostgres::Postgres(),
  host = aws.yml$host,
  user = aws.yml$user,
  password = aws.yml$password,
  port = aws.yml$port
)

# Set URLs to be scraped
# D1
#indoorD121 <- "https://www.tfrrs.org/lists/3157/2020_2021_NCAA_Division_I_Indoor_Qualifying_List/2021/i"
outdoorD121 <- "https://www.tfrrs.org/lists/3191/2021_NCAA_Division_I_Outdoor_Qualifying/2021/o"

# D2
#indoorD221 <- "https://www.tfrrs.org/lists/3158/2020_2021_NCAA_Division_II_Indoor_Qualifying_List/2021/i"
outdoorD221 <- "https://www.tfrrs.org/lists/3194/2021_NCAA_Division_II_Outdoor_Qualifying"

# D3
#indoorD321 <- "https://www.tfrrs.org/lists/3161/2020_2021_NCAA_Division_III_Indoor_Qualifying_List/2021/i"
outdoorD321 <- "https://www.tfrrs.org/lists/3195/2021_NCAA_Division_III_Outdoor_Qualifying/2021/o"

# Pull in data from performance lines
perf_lines <- dbGetQuery(aws, 'SELECT * FROM performance_lines')

# Filter out data pre-2021
perf_lines <- perf_lines %>% filter(YEAR == '2021')

# Group results
perf_grp <- groupedResults(perf_lines)

# Functions for processing data
# Function to get time in seconds
getSeconds <- function(time) {
  splitTime <- strsplit(time, ":")
  min <- as.numeric(splitTime[[1]][1])
  sec <- as.numeric(splitTime[[1]][2])
  min2sec <- min*60
  totalTime <- min2sec + sec
  return(totalTime)
}

createName <- function(name) {
  splitName <- strsplit(name, ", ")
  splitName <- unlist(splitName)
  newName <- paste0(splitName[2], " ", splitName[1])
  newName <- toupper(newName)
  newName <- str_trim(newName)
  return(newName)
}

# Function to read in webpage and produce a dataframe of scored athletes
readPerfListIndoor <- function(url){
  # Read in webpage HTML
  # Get HTML
  webpage <- read_html(url)
  
  # Read in tables
  tables <- html_table(webpage)
  
  # Extract first df
  results <- as.data.frame(tables[1])
  
  # Check number of columns (sprints have wind)
  if(ncol(results) == 8)
  {
    names(results) <- c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE", "WIND")
  } else {
    names(results) <- c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE") 
  }
  
  # Create gender column
  results$Gender = "M"
  
  # Table for relays data
  relays <- data.frame()
  
  # Extract all the tables into dataframes
  for( i in 2:length(tables) )
  {
    # Temporary df
    temp = as.data.frame(tables[i])
    if ( ncol(temp) == 8 )
    {
      # Rename and bind
      names(temp) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE", "WIND")
      
      # Check to assign gender - womens tables should be "even" numbers
      if ( i %% 2 == 0 ) 
      { 
        temp$Gender = "F" 
      } else { 
        temp$Gender = "M" 
        }
      
      results = rbind(results, temp) 
    } else if ( ncol(temp) == 7 ) { 
      # Rename and bind
      names(temp) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE")
      
      # Check to assign gender - womens tables should be "even" numbers
      if ( i %% 2 == 0 ) 
      { 
        temp$Gender = "F" 
      } else { 
        temp$Gender = "M" 
      }
      
      results = rbind(results, temp) 
    } else if( ncol(temp) == 6) {
      relays = rbind(relays, temp)
    }
  }
  
  # Events are not tied in to data - need to add them based on times
  results <- results %>%
    mutate(EVENT = case_when(
      grepl('6\\.|7\\.', TIME) & (substr(TIME,1,1) %in% c('6','7')) & !grepl('7\\:', TIME) ~ '60m', # Have to add the substr clause to get only those that start with a 6
      grepl('21\\.|22\\.|23\\.|24\\.|25\\.|26\\.', TIME) & (substr(TIME,1,2) %in% c('21','22','23','24','25','26')) ~ '200m',
      grepl('44\\.|45\\.|46\\.|47\\.|48\\.|49\\.|50\\.|51\\.|52\\.|53\\.|54\\.|55\\.|56\\.|57\\.|58\\.|59\\.', TIME) & 
        (substr(TIME,1,2) %in% c('44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59')) ~ '400m',
      grepl('1:|2:', TIME) & (substr(TIME,1,2) %in% c('1:','2:')) ~ '800m',
      grepl('3:|4:|5:', TIME) & (substr(TIME,1,2) %in% c('3:','4:','5:')) ~ 'Mile',
      grepl('7:|8:|9:|10:|11:', TIME) & (substr(TIME,1,2) %in% c('7:', '8:', '9:', '10', '11')) ~ '3000m',
      grepl('13:|14:|15:|16:|17:|18:', TIME) & (substr(TIME,1,3) %in% c('13:','14:','15:','16:','17:','18:')) ~ '5000m',
      T ~ 'OTHER'
    ))
  
  # Filter to only distance events & drop # and @ from times
  dist <- results %>%
    filter(EVENT %in% c('800m', 'Mile', '3000m', '5000m')) %>%
    mutate(
      TIME = gsub("@", "", TIME)
    ) %>%
    mutate(
      TIME = gsub("#", "", TIME)
    )
  
  # Convert time to seconds
  dist <- dist %>%
    rowwise() %>%
    mutate(
      timeInSec = getSeconds(TIME)
    )
  
  # Get average times across events by gender for comparison
  avgTimes <- dist %>%
    group_by(EVENT, Gender) %>%
    summarise(
      avgTime = mean(timeInSec),
      minTime = min(timeInSec),
      maxTime = max(timeInSec)
    )
  
  # Join to times list and then create a relative to average field
  dist <- dist %>%
    left_join(avgTimes, by = c("EVENT", "Gender")) %>%
    mutate(
      timeRelAvg = round((timeInSec - minTime) / (maxTime - minTime), 3)
    )
  
  # Repeat process for 400m runners who run the 800m
  midD <- results %>%
    filter(EVENT == '400m') %>%
    mutate(
      TIME = gsub("@", "", TIME)
    ) %>%
    mutate(
      TIME = gsub("#", "", TIME)
    ) %>%
    filter(!grepl(":", TIME)) %>%
    mutate(
      timeInSec = as.numeric(TIME)
    )
  
  # Get average times across events by gender for comparison
  avgMidTimes <- midD %>%
    group_by(EVENT, Gender) %>%
    summarise(
      avgTime = mean(timeInSec),
      minTime = min(timeInSec),
      maxTime = max(timeInSec)
    )
  
  # Join to times list and then create a relative to average field
  midD <- midD %>%
    left_join(avgMidTimes, by = c("EVENT", "Gender")) %>%
    mutate(
      timeRelAvg = round((timeInSec - minTime) / (maxTime - minTime), 3)
    ) %>%
    filter(ATHLETE %in% dist$ATHLETE)
  
  # Row-bind back to distance
  dist <- rbind(dist, midD)
  
  # Create a ranking score & event time score
  dist <- dist %>%
    ungroup() %>%
    mutate(
      rankScore = 100 - PLACE + 1
    ) %>%
    mutate(
      timeScore = 125 * (1 - timeRelAvg)
    ) %>%
    mutate(
      bonusScore = case_when(
        rankScore == 100 ~ 40,
        rankScore < 100 & rankScore > 89 ~ 15,
        rankScore < 90 & rankScore > 79 ~ 5,
        T ~ 0
      )
    ) %>%
    mutate(
      totalScore = rankScore + timeScore + bonusScore
    )
  
  # Get athletes and count of events by athlete
  athletes <- dist %>%
    select(ATHLETE, EVENT) %>%
    group_by(ATHLETE) %>%
    summarise(
      num_events = n_distinct(EVENT)
    )
  
  # Rejoin events back to dist
  dist_ <- dist %>%
    left_join(athletes, by = c("ATHLETE")) %>%
    group_by(ATHLETE) %>%
    arrange(-totalScore, .by_group = TRUE) %>%
    mutate(
      eventNum = 1:n()
    ) %>%
    mutate(
      weightedScore = case_when(
        eventNum == 1 ~ totalScore,
        eventNum == 2 ~ 0.85 * totalScore,
        eventNum == 3 ~ 0.7 * totalScore,
        T ~ totalScore
      )
    )
  
  # Group to one line per runner
  ptsGrp <- dist_ %>%
    group_by(ATHLETE) %>%
    summarise(
      TEAM = max(TEAM), 
      YEAR = max(YEAR), 
      GENDER = max(Gender), 
      POINTS = sum(weightedScore, na.rm = T), 
      EVENTS = n_distinct(EVENT)
    ) %>%
    mutate(PTS_PER_EVENT = POINTS / EVENTS) %>%
    mutate(
      OVERALL_RANK = (0.5*POINTS) + (0.5*PTS_PER_EVENT)
    ) %>%
    arrange(-OVERALL_RANK)
  
  # Return dataframe
  return(ptsGrp)
}

readPerfListOutdoor <- function(url){
  # Read in webpage HTML
  # Get HTML
  webpage <- read_html(url)
  
  # Read in tables
  tables <- html_table(webpage)
  
  # Extract first df
  results <- as.data.frame(tables[1])
  
  # Check number of columns (sprints have wind)
  if (ncol(results) == 8) {
    names(results) <- c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE", "WIND")
  } else {
    names(results) <- c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE") 
  }
  
  # Create gender column
  results$Gender = "M"
  
  # Table for relays data
  relays <- data.frame()
  
  # Extract all the tables into dataframes
  for( i in 2:22 )
  {
    # Temporary df
    temp = as.data.frame(tables[i])
    if ( ncol(temp) == 8 )
    {
      # Rename and bind
      names(temp) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE", "WIND")
      
      # Check to assign gender - womens tables should be "even" numbers
      if ( i %% 2 == 0 ){ 
        temp$Gender = "F" 
      } else { 
        temp$Gender = "M" 
      }
      
      results = rbind(results, temp) 
    } else if ( ncol(temp) == 7 ) { 
      # Rename and bind
      names(temp) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE")
      
      # Assign column for WIND
      temp <- temp %>%
        mutate(
          WIND = ""
        )
      
      # Check to assign gender - womens tables should be "even" numbers
      if ( i %% 2 == 0 ){ 
        temp$Gender = "F" 
      } else { 
        temp$Gender = "M" 
      }
      
      results = rbind(results, temp) 
    } else if( ncol(temp) == 6) {
      relays = rbind(relays, temp)
    }
  }
  
  # Events are not tied in to data - need to add them based on times
  results <- results %>%
    mutate(EVENT = case_when(
      # Men
      grepl('6\\.|7\\.', TIME) & (substr(TIME,1,1) %in% c('6','7')) & !grepl('7\\:', TIME) ~ '60m', 
      grepl('21\\.|22\\.|23\\.|24\\.|25\\.|26\\.', TIME) & (substr(TIME,1,2) %in% c('21','22','23','24','25','26')) ~ '200m',
      grepl('44\\.|45\\.|46\\.|47\\.|48\\.|49\\.|50\\.|51\\.|52\\.|53\\.|54\\.|55\\.|56\\.|57\\.|58\\.|59\\.', TIME) & 
        (substr(TIME,1,2) %in% c('44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59')) ~ '400m',
      grepl('1:|2:', TIME) & (substr(TIME,1,2) %in% c('1:','2:')) & Gender == 'M' ~ '800m',
      grepl('3:|4:|5:', TIME) & (substr(TIME,1,2) %in% c('3:','4:','5:')) ~ '1500m',
      grepl('7:|8:|9:|10:|11:|12:', TIME) & (substr(TIME,1,2) %in% c('7:', '8:', '9:', '10', '11', '12')) ~ 'Steeplechase',
      grepl('13:|14:|15:|16:|17:|18:', TIME) & (substr(TIME,1,3) %in% c('13:','14:','15:','16:','17:','18:')) ~ '5000m',
      grepl('27:|28:|29:|30:|31:|32:|33:|34:|35:|36:', TIME) & (substr(TIME,1,3) %in% c('27:','28:','29:','30:','31:','32:','33:','34:','35:','36:')) ~ '10000m',
      
      # Women
      (grepl('48\\.|49\\.|50\\.|51\\.|52\\.|53\\.|54\\.|55\\.|56\\.|57\\.|58\\.|59\\.', TIME) & 
        (substr(TIME,1,2) %in% c('48','49','50','51','52','53','54','55','56','57','58','59'))) |
        (Gender == 'F' & grepl('1:0', TIME))~ '400m',
      grepl('1:5|2:', TIME) & (substr(TIME,1,2) == '2:' | substr(TIME,1,3) == '1:5') & Gender == 'F' ~ '800m',
      grepl('3:|4:|5:', TIME) & (substr(TIME,1,2) %in% c('3:','4:','5:')) ~ '1500m',
      grepl('7:|8:|9:|10:|11:|12:', TIME) & (substr(TIME,1,2) %in% c('7:', '8:', '9:', '10', '11', '12')) ~ 'Steeplechase',
      grepl('13:|14:|15:|16:|17:|18:', TIME) & (substr(TIME,1,3) %in% c('13:','14:','15:','16:','17:','18:')) ~ '5000m',
      grepl('27:|28:|29:|30:|31:|32:|33:|34:|35:|36:', TIME) & (substr(TIME,1,3) %in% c('27:','28:','29:','30:','31:','32:','33:','34:','35:','36:')) ~ '10000m',
      
      T ~ 'OTHER'
    ))
  
  # Filter to only distance events & drop # and @ from times
  dist <- results %>%
    filter(EVENT %in% c('800m', '1500m', 'Steeplechase', '5000m', '10000m')) %>%
    mutate(
      TIME = gsub("@", "", TIME)
    ) %>%
    mutate(
      TIME = gsub("#", "", TIME)
    ) %>%
    mutate(
      TIME = gsub("\\(.*", "", TIME)
    )
  
  # Convert time to seconds
  dist <- dist %>%
    rowwise() %>%
    mutate(
      timeInSec = getSeconds(TIME)
    )
  
  # Get average times across events by gender for comparison
  avgTimes <- dist %>%
    group_by(EVENT, Gender) %>%
    summarise(
      avgTime = mean(timeInSec),
      minTime = min(timeInSec),
      maxTime = max(timeInSec)
    )
  
  # Join to times list and then create a relative to average field
  dist <- dist %>%
    left_join(avgTimes, by = c("EVENT", "Gender")) %>%
    mutate(
      timeRelAvg = round((timeInSec - minTime) / (maxTime - minTime), 3)
    )
  
  # Repeat process for 400m runners who run the 800m
  midD <- results %>%
    filter(EVENT == '400m') %>%
    mutate(
      TIME = gsub("@", "", TIME)
    ) %>%
    mutate(
      TIME = gsub("#", "", TIME)
    ) %>%
    mutate(
      TIME = gsub("\\(.*", "", TIME)
    ) %>%
    filter(!grepl(":", TIME)) %>%
    mutate(
      timeInSec = as.numeric(TIME)
    )
  
  # Get average times across events by gender for comparison
  avgMidTimes <- midD %>%
    group_by(EVENT, Gender) %>%
    summarise(
      avgTime = mean(timeInSec),
      minTime = min(timeInSec),
      maxTime = max(timeInSec)
    )
  
  # Join to times list and then create a relative to average field
  midD <- midD %>%
    left_join(avgMidTimes, by = c("EVENT", "Gender")) %>%
    mutate(
      timeRelAvg = round((timeInSec - minTime) / (maxTime - minTime), 3)
    ) %>%
    filter(ATHLETE %in% dist$ATHLETE)
  
  # Row-bind back to distance
  dist <- rbind(dist, midD)
  
  # Create a ranking score & event time score
  dist <- dist %>%
    ungroup() %>%
    mutate(
      rankScore = 100 - PLACE + 1
    ) %>%
    mutate(
      timeScore = 150 * (1 - timeRelAvg)
    ) %>%
    mutate(
      bonusScore = case_when(
        rankScore == 100 ~ 40,
        rankScore < 100 & rankScore > 89 ~ 15,
        rankScore < 90 & rankScore > 79 ~ 5,
        T ~ 0
      )
    ) %>%
    mutate(
      totalScore = rankScore + timeScore + bonusScore
    )
  
  # Get athletes and count of events by athlete
  athletes <- dist %>%
    select(ATHLETE, EVENT) %>%
    group_by(ATHLETE) %>%
    summarise(
      num_events = n_distinct(EVENT)
    )
  
  # Rejoin events back to dist
  dist_ <- dist %>%
    left_join(athletes, by = c("ATHLETE")) %>%
    group_by(ATHLETE) %>%
    arrange(-totalScore, .by_group = TRUE) %>%
    mutate(
      eventNum = 1:n()
    ) %>%
    mutate(
      weightedScore = case_when(
        eventNum == 1 ~ totalScore,
        eventNum == 2 ~ 0.8 * totalScore,
        eventNum == 3 ~ 0.6 * totalScore,
        T ~ totalScore
      )
    )
  
  # Group to one line per runner
  ptsGrp <- dist_ %>%
    group_by(ATHLETE) %>%
    summarise(
      TEAM = max(TEAM), 
      YEAR = max(YEAR), 
      GENDER = max(Gender), 
      POINTS = sum(weightedScore, na.rm = T), 
      EVENTS = n_distinct(EVENT)
    ) %>%
    mutate(PTS_PER_EVENT = POINTS / EVENTS) %>%
    mutate(
      OVERALL_RANK = (0.5*POINTS) + (0.5*PTS_PER_EVENT)
    ) %>%
    arrange(-OVERALL_RANK)
  
  # Return dataframe
  return(ptsGrp)
}

# Function to get the rank
giveRank <- function(df){
  # Extract maximum total of any runner in each year
  maxPts = max(df$POINTS)
  # Extract maximum points per event for each year
  maxPtsPerEvent = max(df$PTS_PER_EVENT)
  
  # Create RANK value
  df <- df %>%
    mutate(
      RANK = (POINTS / maxPts) * 0.3 + (PTS_PER_EVENT / maxPtsPerEvent) * 0.7
    ) %>%
    arrange(-RANK) %>%
    mutate(
      RANK = round(RANK, 3)
    )
  
  # Return df
  return(df)
}

# Function to read data and create a ranking based on gender
createRankingsIndoor <- function(url) {
  # Generate rankings from list
  df <- readPerfListIndoor(url)
  #df <- giveRank(df)
  # Split into gender
  men <- df %>% filter(GENDER == 'M') # %>% arrange(-RANK)
  women <- df %>% filter(GENDER == 'F') # %>% arrange(-RANK)
  # Give rank
  men <- giveRank(men)
  women <- giveRank(women)
  # Arrange by RANK
  men <- men %>% arrange(-RANK)
  women <- women %>% arrange(-RANK)
  # Assign numeric values for overall ranking
  men$RANK_NUM <- c(1:nrow(men))
  women$RANK_NUM <- c(1:nrow(women))
  # Combine data
  df <- rbind(men, women)
  # Return data
  return(df)
}

createRankingsOutdoor <- function(url) {
  # Generate rankings from list
  df <- readPerfListOutdoor(url)
  #df <- giveRank(df)
  # Split into gender
  men <- df %>% filter(GENDER == 'M') # %>% arrange(-RANK)
  women <- df %>% filter(GENDER == 'F') # %>% arrange(-RANK)
  # Give rank
  men <- giveRank(men)
  women <- giveRank(women)
  # Arrange by RANK
  men <- men %>% arrange(-RANK)
  women <- women %>% arrange(-RANK)
  # Assign numeric values for overall ranking
  men$RANK_NUM <- c(1:nrow(men))
  women$RANK_NUM <- c(1:nrow(women))
  # Combine data
  df <- rbind(men, women)
  # Return data
  return(df)
}
################################################################
# Get Data From TFRRS
df1 <- createRankingsOutdoor(outdoorD121)
# Add LOAD_DATE, DIVISION, and YEAR columns
df1 <- df1 %>%
  mutate(
    EVENT_YEAR = '2021',
    DVISION = 'D1',
    LOAD_DATE = lubridate::today() + 2
  )

df2 <- createRankingsOutdoor(outdoorD221)
# Add LOAD_DATE, DIVISION, and YEAR columns
df2 <- df2 %>%
  mutate(
    EVENT_YEAR = '2021',
    DVISION = 'D2',
    LOAD_DATE = lubridate::today() + 2
  )

df3 <- createRankingsOutdoor(outdoorD321)
# Add LOAD_DATE, DIVISION, and YEAR columns
df3 <- df3 %>%
  mutate(
    EVENT_YEAR = '2021',
    DVISION = 'D3',
    LOAD_DATE = lubridate::today() + 2
  )

# Bind data
df <- rbind(df1, df2, df3)

# Write data to table
#dbRemoveTable(pg, "current_power_rankings")
#dbCreateTable(aws, "current_power_rankings", df)
dbWriteTable(aws, "current_power_rankings", df, append = TRUE)

################################################################
# Team Rankings
teamD1 <- df1 %>%
  group_by(TEAM, GENDER) %>%
  summarise(
    athletes = n_distinct(ATHLETE),
    events = sum(EVENTS),
    avg_rank = round(mean(RANK_NUM), 2),
    avg_rank_score = round(mean(RANK), 2),
    total_points = round(sum(POINTS), 2),
    avg_points_per_event = round(mean(PTS_PER_EVENT), 2)
  ) %>%
  mutate(
    division = 'D1'
  )

teamD2 <- df2 %>%
  group_by(TEAM, GENDER) %>%
  summarise(
    athletes = n_distinct(ATHLETE),
    events = sum(EVENTS),
    avg_rank = round(mean(RANK_NUM), 2),
    avg_rank_score = round(mean(RANK), 2),
    total_points = round(sum(POINTS), 2),
    avg_points_per_event = round(mean(PTS_PER_EVENT), 2)
  ) %>%
  mutate(
    division = 'D2'
  )

teamD3 <- df3 %>%
  group_by(TEAM, GENDER) %>%
  summarise(
    athletes = n_distinct(ATHLETE),
    events = sum(EVENTS),
    avg_rank = round(mean(RANK_NUM), 2),
    avg_rank_score = round(mean(RANK), 2),
    total_points = round(sum(POINTS), 2),
    avg_points_per_event = round(mean(PTS_PER_EVENT), 2)
  ) %>%
  mutate(
    division = 'D3'
  )

# Bind data
teams <- rbind(teamD1, teamD2, teamD3)

# Create a load date
teams <- teams %>%
  mutate(
    load_date = lubridate::today() + 2
  )

# Write data to table
#dbRemoveTable(aws, "team_rankings")
#dbCreateTable(aws, "team_rankings", teams)
dbWriteTable(aws, "team_rankings", teams, append = TRUE)



library(tidymodels)
library(httr)
library(dplyr)
library(jsonlite)
library(RPostgreSQL)
library(DBI)
library(RSQLite)
library(reshape2)
library(stringr)
library(yaml)
library(rvest)

# DYNAMICALLY SCRAPE TFRRS LINKS
## getURLs
# Function for results URLs
getResultsURLs = function(url) {
  # Read in the links on the webpage
  wp = read_html(url) %>%
    html_nodes(xpath = "//td/div/a") %>%
    html_attr("href")
  
  # Manipulate strings
  for ( i  in 1:length(wp) )
  {
    temp = wp[i]
    #temp = substring(temp, 3)
    temp = paste0("https:", temp)
    temp = substr(temp, 1, nchar(temp)-3)
    wp[i] = temp
  }
  
  # Grab only strings for athletes
  wp = wp[grepl("athletes", wp)]
  
  # Return webpage results
  return(wp)
  
}

# Function for Performance Lists
getPerfListURLs = function(url) {
  # Read in the links on the webpage
  wp = read_html(url) %>%
    html_nodes(xpath = "//td/a") %>%
    html_attr("href")
  
  # Manipulate strings
  for ( i  in 1:length(wp) )
  {
    temp = wp[i]
    #temp = substring(temp, 3)
    temp = paste0("https:", temp)
    temp = substr(temp, 1, nchar(temp)-3)
    wp[i] = temp
  }
  
  # Grab only strings for athletes
  wp = wp[grepl("athletes", wp)]
  
  # Return webpage results
  return(wp)
  
}

# Function to handle times
handleTimes <- function(mark) {
  # Split the tenths/hundreths off
  split_mark = unlist(strsplit(mark, "[.]"))
  # Split into minutes & seconds
  min_sec = unlist(strsplit(split_mark[1], ":"))
  # Calculate seconds from minutes and add seconds
  total_time = as.numeric(min_sec[1])*60 + as.numeric(min_sec[2])
  # Return time 
  return(total_time)
}

## runnerScrape Function
runnerScrape = function(url){
  # Get the name of the runner off of TFRRS HTML
  runner_name <- read_html(url) %>%
    html_nodes("h3") %>%
    html_text()
  
  runner_name = unlist(strsplit(runner_name[1], "[\n]"))[1]
  
  # Extract team name
  team_name <- read_html(url) %>%
    html_node(xpath = "/html/body/form/div/div/div/div[1]/a[3]/h3") %>%
    html_text()
  
  # Strip new line character
  team_name <- gsub('\\n', '', team_name)
  
  # Pull gender out via team link
  team_link <- read_html(url) %>%
    html_node(xpath = "/html/body/form/div/div/div/div[1]/a[3]") %>%
    html_attr("href")
  
  # Check if mens or womens team
  gender <- case_when(
    grepl("college_m", team_link) ~ 'M',
    T ~ 'F'
  )
  
  # Print statement
  print(paste0("Getting data for: ", runner_name))
  
  runner <- read_html(url) %>%
    html_nodes(xpath = '/html/body/form/div/div/div/div[3]/div/div/div[1]') %>%
    html_nodes("tr")
  
  keep_nodes <- c()
  
  for(i in 1:length(runner))
  {
    #if(grepl("<th", test[i]) == FALSE)
    {
      keep_nodes <- c(keep_nodes, html_text(runner[i]))
    }
  }
  
  for(i in 1:length(keep_nodes))
  {
    keep_nodes[i] = str_squish(gsub("[\t\n]", " ", keep_nodes[i]))
  }
  
  # Loop over to get stuff
  # Vectors
  years <- c()
  events <- c()
  times <- c()
  places <- c()
  
  for(i in 1:length(keep_nodes))
  {
    # Check for a year (date)
    if(grepl("2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021", keep_nodes[i]))
    {
      # Convert to string
      node_str <- as.character(keep_nodes[i])
      # Extract year
      year <- str_sub(node_str, nchar(node_str)-3, nchar(node_str))
      # Create a flag for XC
      xc_flag <- case_when(
        grepl("xc|cross country", tolower(node_str)) ~ TRUE,
        T ~ FALSE
      )
      # Now we extract results
      for (j in (i+1):length(keep_nodes))
      {
        if(grepl("2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021", keep_nodes[j]))
        {
          # If so, break out
          break
        }
        # Split the next string into event, time and place
        result_split <- unlist(str_split(keep_nodes[j], " "))
        # Indicate if result is XC or not
        event <- case_when(
          xc_flag ~ paste0(result_split[1], " XC"),
          T ~ result_split[1]
        )
        # Get place (need code to handle sprinters)
        place <- case_when(
          length(result_split) == 5 ~ result_split[4],
          T ~ result_split[3]
        )
        # Get the pieces
        years <- append(years, year)
        events <- append(events, event)
        times <- append(times, result_split[2])
        places <- append(places, place)
      }
    }
  }
  
  # Create a data frame
  athlete <- as.data.frame(cbind(years, events, times, places))
  names(athlete) = c("YEAR", "EVENT", "TIME", "PLACE")

  # Make sure athlete has rows (sprinters get removed)
  if (nrow(athlete) > 0)
  {
    # Clean up and group to one row per event
    athlete <- athlete %>%
      mutate(
        EVENT = case_when(
          grepl("5000|5K|5k", EVENT) & !(grepl("XC|xc|Xc", EVENT)) ~ "5000m",
          grepl("3000|3K|3k", EVENT) & !grepl("3000S|3000mS|3000SC", EVENT) ~ "3000m",
          grepl("mile|Mile|MILE", EVENT) ~ "Mile",
          grepl("6K|6k|6000", EVENT) ~ "6K XC",
          grepl("800|800m", EVENT) & !grepl("8000m|8000", EVENT) ~ "800m",
          grepl("8k|8K|8000m", EVENT) & !grepl("\\.", EVENT) ~ "8K XC",
          grepl("10,000|10K|10k|10000", EVENT) & !(grepl("XC|xc|Xc", EVENT)) ~ "10K",
          grepl("10,000|10K|10k|10000", EVENT) & grepl("XC|xc|Xc", EVENT) ~ "10K XC",
          grepl("1500|1500m", EVENT) & !grepl("4x", EVENT) ~ "1500m",
          grepl("3000S|3000s|3000SC|3000sc|3000mS", EVENT) ~ "3000S",
          T ~ "OTHER"
        )
      ) %>%
      filter(grepl("th|TH|st|ST|nd|ND|rd|RD", PLACE)) %>%
      mutate(
        PLACE = as.numeric(gsub("th|TH|st|ST|nd|ND|rd|RD", "", PLACE))
      ) %>%
      filter(!is.na(PLACE)) %>%
      mutate(
        TIME = as.character(TIME)
        )
    
    # Apply function to convert times to numbers
    athlete$TIME = sapply(athlete$TIME, handleTimes)
    athlete$NAME = runner_name
    athlete$GENDER = gender
    athlete$TEAM = team_name
  }
  # Handle when sprinters are included
  else
  {
    athlete = as.data.frame(cbind("NULL", 0, 0, 0, 0, 0, 0, "NULL"))
    names(athlete) = c("EVENT", "AVG_PLACE", "AVG_TIME", "PR", "WINS", "TIMES.RUN", "WIN_PCT", "NAME")
  }
  
  return(athlete)
}

groupedResults <- function(athleteDF){
  athlete <- athleteDF %>%
    group_by(NAME, GENDER, TEAM, EVENT) %>%
    summarise(
      AVG_PLACE = round(mean(PLACE, na.rm = T), 2),
      AVG_TIME = round(mean(TIME), 2),
      PR = min(TIME),
      WINS = n_distinct(TIME[PLACE == 1]),
      TIMES.RUN = n()
    ) %>%
    mutate(
      WIN_PCT = round(((WINS / TIMES.RUN) * 100), 2)
    )
  
  # Return data
  return(athlete)
}

## resultsQuery Function
# Create a function for results querying - takes in a list of webpage URLs from runnerScrape
resultsQuery = function(wp){
  # Create a temporary dataframe for runner line item performance
  runner_lines = as.data.frame(cbind("year", "event", 1.1, 1.1, "name", "gender", "team_name"))
  # Rename columns
  names(runner_lines) = c("YEAR", "EVENT", "TIME", "PLACE", "NAME", "GENDER", "TEAM")
  # Reformat var
  runner_lines <- runner_lines %>%
    mutate(
      YEAR = as.character(YEAR),
      EVENT = as.character(EVENT),
      TIME = as.numeric(TIME),
      PLACE = as.numeric(PLACE),
      NAME = as.character(NAME),
      GENDER = as.character(GENDER),
      TEAM = as.character(TEAM)
    )
  
  # Loop through every person from the results
  for ( i in 1:length(wp) )
  {
    # Set url to scrape
    url = wp[i]
    
    # Make a temporary df for the runner
    runner_temp <- runnerScrape(url)
    
    # Append lines to dataframe
    runner_lines <- rbind(runner_lines, runner_temp)
  }
  # Return data
  return(runner_lines)
}

## reformatRunners
# Create function to convert times and transpose
reformatRunners = function(df){
  df <- df %>%
    mutate( # Create times that are readable for QC purposes
      AVG_TIME_FORM = paste0(floor(as.numeric(AVG_TIME) / 60), ":", (str_pad(floor(as.numeric(AVG_TIME) %% 60), width = 2, side = "left", pad = "0")))
    ) %>%
    mutate(
      PR_FORM = paste0(floor(as.numeric(PR) / 60), ":", (str_pad(floor(as.numeric(PR) %% 60), width = 2, side = "left", pad = "0")))
    ) %>%
    filter(!(EVENT %in% c("event", "OTHER"))) %>%
    group_by(NAME, GENDER, TEAM) %>%
    summarise( # Transpose data frame
      # 800m
      AVG_PLACE_800 = min(AVG_PLACE[EVENT == '800m']),
      AVG_TIME_800 = min(AVG_TIME[EVENT == '800m']),
      PR_800 = min(PR[EVENT == '800m']),
      WINS_800 = min(WINS[EVENT == '800m']),
      TIMES_RUN_800 = min(TIMES.RUN[EVENT == '800m']),
      WIN_PCT_800 = min(WIN_PCT[EVENT == '800m']),
      AVG_TIME_FORM_800 = min(AVG_TIME_FORM[EVENT == '800m']),
      PR_FORM_800 = min(PR_FORM[EVENT == '800m']),
      # 1500m
      AVG_PLACE_1500 = min(AVG_PLACE[EVENT == '1500m']),
      AVG_TIME_1500 = min(AVG_TIME[EVENT == '1500m']),
      PR_1500 = min(PR[EVENT == '1500m']),
      WINS_1500 = min(WINS[EVENT == '1500m']),
      TIMES_RUN_1500 = min(TIMES.RUN[EVENT == '1500m']),
      WIN_PCT_1500 = min(WIN_PCT[EVENT == '1500m']),
      AVG_TIME_FORM_1500 = min(AVG_TIME_FORM[EVENT == '1500m']),
      PR_FORM_1500 = min(PR_FORM[EVENT == '1500m']),
      # Mile
      AVG_PLACE_MILE = min(AVG_PLACE[EVENT == 'Mile']),
      AVG_TIME_MILE = min(AVG_TIME[EVENT == 'Mile']),
      PR_MILE = min(PR[EVENT == 'Mile']),
      WINS_MILE = min(WINS[EVENT == 'Mile']),
      TIMES_RUN_MILE = min(TIMES.RUN[EVENT == 'Mile']),
      WIN_PCT_MILE = min(WIN_PCT[EVENT == 'Mile']),
      AVG_TIME_FORM_MILE = min(AVG_TIME_FORM[EVENT == 'Mile']),
      PR_FORM_MILE = min(PR_FORM[EVENT == 'Mile']),
      # 3000m
      AVG_PLACE_3000 = min(AVG_PLACE[EVENT == '3000m']),
      AVG_TIME_3000 = min(AVG_TIME[EVENT == '3000m']),
      PR_3000 = min(PR[EVENT == '3000m']),
      WINS_3000 = min(WINS[EVENT == '3000m']),
      TIMES_RUN_3000 = min(TIMES.RUN[EVENT == '3000m']),
      WIN_PCT_3000 = min(WIN_PCT[EVENT == '3000m']),
      AVG_TIME_FORM_3000 = min(AVG_TIME_FORM[EVENT == '3000m']),
      PR_FORM_3000 = min(PR_FORM[EVENT == '3000m']),
      # 3000mSC
      AVG_PLACE_3000S = min(AVG_PLACE[EVENT == '3000S']),
      AVG_TIME_3000S = min(AVG_TIME[EVENT == '3000S']),
      PR_3000S = min(PR[EVENT == '3000S']),
      WINS_3000S = min(WINS[EVENT == '3000S']),
      TIMES_RUN_3000S = min(TIMES.RUN[EVENT == '3000S']),
      WIN_PCT_3000S = min(WIN_PCT[EVENT == '3000S']),
      AVG_TIME_FORM_3000S = min(AVG_TIME_FORM[EVENT == '3000S']),
      PR_FORM_3000S = min(PR_FORM[EVENT == '3000S']),
      # 5000m
      AVG_PLACE_5000 = min(AVG_PLACE[EVENT == '5000m']),
      AVG_TIME_5000 = min(AVG_TIME[EVENT == '5000m']),
      PR_5000 = min(PR[EVENT == '5000m']),
      WINS_5000 = min(WINS[EVENT == '5000m']),
      TIMES_RUN_5000 = min(TIMES.RUN[EVENT == '5000m']),
      WIN_PCT_5000 = min(WIN_PCT[EVENT == '5000m']),
      AVG_TIME_FORM_5000 = min(AVG_TIME_FORM[EVENT == '5000m']),
      PR_FORM_5000 = min(PR_FORM[EVENT == '5000m']),
      # 6K XC
      AVG_PLACE_6KXC = min(AVG_PLACE[EVENT == '6K XC']),
      AVG_TIME_6KXC = min(AVG_TIME[EVENT == '6K XC']),
      PR_6KXC = min(PR[EVENT == '6K XC']),
      WINS_6KXC = min(WINS[EVENT == '6K XC']),
      TIMES_RUN_6KXC = min(TIMES.RUN[EVENT == '6K XC']),
      WIN_PCT_6KXC = min(WIN_PCT[EVENT == '6K XC']),
      AVG_TIME_FORM_6KXC = min(AVG_TIME_FORM[EVENT == '6K XC']),
      PR_FORM_6KXC = min(PR_FORM[EVENT == '6K XC']),
      # 8K XC
      AVG_PLACE_8KXC = min(AVG_PLACE[EVENT == '8K XC']),
      AVG_TIME_8KXC = min(AVG_TIME[EVENT == '8K XC']),
      PR_8KXC = min(PR[EVENT == '8K XC']),
      WINS_8KXC = min(WINS[EVENT == '8K XC']),
      TIMES_RUN_8KXC = min(TIMES.RUN[EVENT == '8K XC']),
      WIN_PCT_8KXC = min(WIN_PCT[EVENT == '8K XC']),
      AVG_TIME_FORM_8KXC = min(AVG_TIME_FORM[EVENT == '8K XC']),
      PR_FORM_8KXC = min(PR_FORM[EVENT == '8K XC']),
      # 10K
      AVG_PLACE_10K = min(AVG_PLACE[EVENT == '10K']),
      AVG_TIME_10K = min(AVG_TIME[EVENT == '10K']),
      PR_10K = min(PR[EVENT == '10K']),
      WINS_10K = min(WINS[EVENT == '10K']),
      TIMES_RUN_10K = min(TIMES.RUN[EVENT == '10K']),
      WIN_PCT_10K = min(WIN_PCT[EVENT == '10K']),
      AVG_TIME_FORM_10K = min(AVG_TIME_FORM[EVENT == '10K']),
      PR_FORM_10K = min(PR_FORM[EVENT == '10K']),
    ) %>%
    mutate_if(is.numeric, list(~na_if(., Inf))) # Replace Inf values created by runners not competing in an event
  
  # Return data frame
  return(df)
}


# Functions to get data for URL
# Meet results
getResultsData = function(url){
  # URL to scrape
  meet_url = url
  # Get webpage URLs for runners
  meet_wp = getResultsURLs(meet_url)
  # Query the runners results
  runners = resultsQuery(meet_wp)
  # Reformat the data
  runners = reformatRunners(runners)
  
  # Return final runners data frame
  return(runners)
}

# Performance lists
getPerfListData = function(url){
  # URL to scrape
  meet_url <- url
  # Get webpage URLs for runners
  meet_wp <- getPerfListURLs(meet_url)
  # Subset to distance events
  wp <- meet_wp[451:1050]
  # Remove duplicates
  wp <- unique(wp)
  # Query the runners results
  runners <- resultsQuery(wp)
  # Reformat the data
  runners <- reformatRunners(runners)
  
  # Return final runners data frame
  return(runners)
}

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

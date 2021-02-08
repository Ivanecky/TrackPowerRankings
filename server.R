# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(stats)
library(knitr)
library(DT)
library(dplyr)
library(rvest)

# Function to read in webpage and produce a dataframe of scored athletes
readPerfList = function(url){
    # Read in webpage HTML
    # Get HTML
    webpage = read_html(url)
    
    # Read in tables
    tables = html_table(webpage)
    
    # Extract first df
    results = as.data.frame(tables[1])
    names(results) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE")
    
    # Create gender column
    results$Gender = "M"
    
    # Table for relays data
    relays = data.frame()
    
    # Extract all the tables into dataframes
    for( i in 2:length(tables) )
    {
        # Temporary df
        temp = as.data.frame(tables[i])
        # Make sure table has 7 columns for binding - some tables using conversions for field events have 8 columns
        if ( ncol(temp) == 7 )
        { 
            # Rename and bind
            names(temp) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE")
            
            # Check to assign gender - womens tables should be "even" numbers
            if ( i%%2 == 0 ) 
            { 
                temp$Gender = "F" 
            }
            else { temp$Gender = "M" }
            
            results = rbind(results, temp) 
        }
        else if( ncol(temp) == 6)
        {
            relays = rbind(relays, temp)
        }
    }
    
    # Events are not tied in to data - need to add them based on times
    results = results %>%
        mutate(EVENT = case_when(
            grepl('6\\.|7\\.', TIME) & (substr(TIME,1,1) %in% c('6','7')) ~ '60m', # Have to add the substr clause to get only those that start with a 6
            grepl('21\\.|22\\.|23\\.|24\\.|25\\.|26\\.', TIME) & (substr(TIME,1,2) %in% c('21','22','23','24','25','26')) ~ '200m',
            grepl('44\\.|45\\.|46\\.|47\\.|48\\.|49\\.|50\\.|51\\.|52\\.|53\\.|54\\.|55\\.|56\\.|57\\.|58\\.|59\\.', TIME) & 
                (substr(TIME,1,2) %in% c('44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59')) ~ '400m',
            grepl('1:|2:', TIME) & (substr(TIME,1,2) %in% c('1:','2:')) ~ '800m',
            grepl('3:|4:|5:', TIME) & (substr(TIME,1,2) %in% c('3:','4:','5:')) ~ 'Mile',
            grepl('7:|8:|9:|10:|11:', TIME) & (substr(TIME,1,2) %in% c('7:', '8:', '9:', '10', '11')) ~ '3000m',
            grepl('13:|14:|15:|16:|17:|18:', TIME) & (substr(TIME,1,3) %in% c('13:','14:','15:','16:','17:','18:')) ~ '5000m',
            T ~ 'OTHER'
        ))
    
    # Subset to Top 50 per event
    top50 <- results %>%
        filter(PLACE <= 50) %>%
        filter(EVENT %in% c('800m', 'Mile', '3000m', '5000m')) %>%
        mutate(
            PTS = 41 - PLACE
        )
    
    # Group to one line per runner
    pts_grp <- top50 %>%
        group_by(ATHLETE) %>%
        summarise(
            TEAM = max(TEAM), 
            YEAR = max(YEAR), 
            GENDER = max(Gender), 
            POINTS = sum(PTS, na.rm = T), 
            EVENTS = n_distinct(EVENT)
        ) %>%
        mutate(PTS_PER_EVENT = POINTS / EVENTS) %>%
        arrange(-POINTS)
    
    # Return dataframe
    return(pts_grp)
}

# Function to get the rank
giveRank = function(df){
    # Extract maximum total of any runner in each year
    maxPtsCY = max(df$POINTS)
    maxPtsPY = max(df$POINTS_PY)
    # Extract maximum points per event for each year
    maxPtsPerEventCY = max(df$PTS_PER_EVENT)
    maxPtsPerEventPY = max(df$PTS_PER_EVENT_PY)
    
    # Create RANK value
    df <- df %>%
        mutate(
            RANK = case_when(
                    # Athlete has results from prior year
                    maxPtsPY != 0 ~ (((POINTS / maxPtsCY) * 34) + ((POINTS_PY / maxPtsPY) * 6) +  ((PTS_PER_EVENT / maxPtsPerEventCY) * 51) + ((PTS_PER_EVENT_PY / maxPtsPerEventPY) * 9)),
                    # Athlete has no results from prior year
                    T ~ ((POINTS / maxPtsCY) * 40) + ((PTS_PER_EVENT / maxPtsPerEventCY) * 60))
        ) %>%
        arrange(-RANK) %>%
        mutate(
            RANK = round(RANK, 2)
        )
    
    # Subset to top 100
    df = df[1:100, ]
    
    # Assign overall rank
    df$`OVERALL RANK` = 1:100
    
    # Return df
    return(df)
}

# Get information on athlete
searchAthlete = function(runner){
    
    # Split name into first and last
    runner = strsplit(runner, " ")[[1]]
    
    # Define first & last names
    first_name = tolower(runner[1])
    last_name = tolower(runner[2])
    
    # Search for the athlete
    searchedRunner = overall[which(grepl(first_name, tolower(overall$ATHLETE)) & grepl(last_name, tolower(overall$ATHLETE))), ]
    
    return(searchedRunner)
}

# Get rank for each athlete
getRank = function(runner){
    # Split name into first and last
    runner = strsplit(runner, " ")[[1]]
    
    # Define first & last names
    first_name = tolower(runner[1])
    last_name = tolower(runner[2])
    
    # Search for the athlete
    searchedRunner = overall[which(grepl(first_name, tolower(overall$ATHLETE)) & grepl(last_name, tolower(overall$ATHLETE))), ]
    
    # Return rank
    return(searchedRunner$`OVERALL RANK`)
}

# Read all of the data into one big table
d1_cy = readPerfList('https://www.tfrrs.org/lists/3157/2020_2021_NCAA_Division_I_Indoor_Qualifying_List/2021/i')
d1_py = readPerfList('https://www.tfrrs.org/lists/2770.html')
d2_cy = readPerfList('https://www.tfrrs.org/lists/3158/2020_2021_NCAA_Division_II_Indoor_Qualifying_List/2021/i')
d2_py = readPerfList('https://www.tfrrs.org/lists/2771.html')
d3_cy = readPerfList('https://www.tfrrs.org/lists/3161/2020_2021_NCAA_Division_III_Indoor_Qualifying_List/2021/i')
d3_py = readPerfList('https://www.tfrrs.org/lists/2772.html')

# Create division columns
d1_cy$DIVISION = "D1"
d1_py$DIVISION = "D1"
d2_cy$DIVISION = "D2"
d2_py$DIVISION = "D2"
d3_cy$DIVISION = "D3"
d3_py$DIVISION = "D3"

# Rename 2019 data
names(d1_py) = c("ATHLETE", "TEAM", "YEAR", "GENDER", "POINTS_PY", "EVENTS_PY", "PTS_PER_EVENT_PY", "DIVISION")
names(d2_py) = c("ATHLETE", "TEAM", "YEAR", "GENDER", "POINTS_PY", "EVENTS_PY", "PTS_PER_EVENT_PY", "DIVISION")
names(d3_py) = c("ATHLETE", "TEAM", "YEAR", "GENDER", "POINTS_PY", "EVENTS_PY", "PTS_PER_EVENT_PY", "DIVISION")

# Select only required columns from 2019 data
d1_py = d1_py %>% select(ATHLETE, POINTS_PY, EVENTS_PY, PTS_PER_EVENT_PY)
d2_py = d2_py %>% select(ATHLETE, POINTS_PY, EVENTS_PY, PTS_PER_EVENT_PY)
d3_py = d3_py %>% select(ATHLETE, POINTS_PY, EVENTS_PY, PTS_PER_EVENT_PY)

# Join divisional data together
d1 = d1_cy %>% left_join(d1_py, by = c("ATHLETE"))
d2 = d2_cy %>% left_join(d2_py, by = c("ATHLETE"))
d3 = d3_cy %>% left_join(d3_py, by = c("ATHLETE"))


# Function to handle NAs
handleNA = function(df)
{
    df = df %>%
        mutate(
            POINTS_PY = ifelse(is.na(POINTS_PY), 0, POINTS_PY),
            EVENTS_PY = ifelse(is.na(EVENTS_PY), 0, EVENTS_PY),
            PTS_PER_EVENT_PY = ifelse(is.na(PTS_PER_EVENT_PY), 0, PTS_PER_EVENT_PY)
        ) %>%
        mutate(
            PTS_PER_EVENT = round(PTS_PER_EVENT, 2),
            PTS_PER_EVENT_PY = round(PTS_PER_EVENT_PY, 2)
        )
    # Return data frame
    return(df)
}

# Handle all the NAs
d1 = handleNA(d1)
d2 = handleNA(d2)
d3 = handleNA(d3)

# Split by gender
menD1 = d1 %>% filter(GENDER == 'M')
womenD1 = d1 %>% filter(GENDER == 'F')

menD2 = d2 %>% filter(GENDER == 'M')
womenD2 = d2 %>% filter(GENDER == 'F')

menD3 = d3 %>% filter(GENDER == 'M')
womenD3 = d3 %>% filter(GENDER == 'F')

# Run ranking function against data
menD1 = giveRank(menD1)
womenD1 = giveRank(womenD1)

menD2 = giveRank(menD2)
womenD2 = giveRank(womenD2)

menD3 = giveRank(menD3)
womenD3 = giveRank(womenD3)

# Combine all the data from separate divisions
men = rbind(menD1, menD2, menD3)
women = rbind(womenD1, womenD2, womenD3)

# Create overall data set
overall = rbind(men, women)

# Rename
colnames(men) <- c("Athlete", "Team", "Year", "Gender", "Total Points", "Total Events", "Points Per Event", "DIVISION", "Points (Last Yr)", "Events (Last Yr.)", "Points Per Event (Last Yr.)", "Rank Score", "Overall Ranking")
colnames(women) <- c("Athlete", "Team", "Year", "Gender", "Total Points", "Total Events", "Points Per Event", "DIVISION", "Points (Last Yr)", "Events (Last Yr.)", "Points Per Event (Last Yr.)", "Rank Score", "Overall Ranking")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Function to select which division to query
    mensData = reactive({
        
        # Temporarily set data
        df = men %>% filter(DIVISION == "D1")
        
        if ( input$division == "D1" )
        {
            df = men %>% filter(DIVISION == "D1")
        }
        else if ( input$division == "D2" )
        {
            df = men %>% filter(DIVISION == "D2")
        }
        else if ( input$division == "D3" )
        {
            df = men %>% filter(DIVISION == "D3")
        }
        
        return(df)
    })
    
    womensData = reactive({
        
        # Temporarily set data
        df = women %>% filter(DIVISION == "D1")
        
        if ( input$division == "D1" )
        {
            df = women %>% filter(DIVISION == "D1")
        }
        else if ( input$division == "D2" )
        {
            df = women %>% filter(DIVISION == "D2")
        }
        else if ( input$division == "D3" )
        {
            df = women %>% filter(DIVISION == "D3")
        }
        
        return(df)
    })
    
    output$menRank = renderTable(mensData())
    
    output$womenRank = renderTable(womensData())
    
    # Functions to pull out runner info
    # Get name of first runner
    name1 = reactive({
        input$compareButton
        # Get the runner
        runner1 = searchAthlete(input$runnerName1)
        
        # Pull out the name
        runnerName1 = runner1$Athlete
    })
    
    # Get name of first runner
    name2 = reactive({
        input$compareButton
        # Get the runner
        runner2 = searchAthlete(input$runnerName2)
        
        # Pull out the name
        runnerName2 = runner2$Athlete
    })
    
    # Get the overall rank
    rank1 = reactive({
        input$compareButton
        # Pull out the name
        runnerRank1 = getRank(input$runnerName1)
    })
    
    # Get the overall rank
    rank2 = reactive({
        input$compareButton
        # Pull out the name
        runnerRank2 = getRank(input$runnerName2)
    })
    
    # Get the 2020 points
    pts1 = reactive({
        input$compareButton
        # Get the runner
        runner1 = searchAthlete(input$runnerName1)
        
        # Pull out the name
        runnerPts1 = runner1$`Total Points`
    })
    
    # Get the overall rank
    pts2 = reactive({
        input$compareButton
        # Get the runner
        runner2 = searchAthlete(input$runnerName2)
        
        # Pull out the name
        runnerPts2 = runner2$`Total Points`
    })
    
    # Get the points per event in 2020
    ptsPerEvent1 = reactive({
        input$compareButton
        # Get the runner
        runner1 = searchAthlete(input$runnerName1)
        
        # Pull out the name
        runnerPtsPerEvent1 = runner1$`Points Per Event`
    })
    
    # Get the overall rank
    ptsPerEvent2 = reactive({
        input$compareButton
        # Get the runner
        runner2 = searchAthlete(input$runnerName2)
        
        # Pull out the name
        runnerPtsPerEvent2 = runner2$`Points Per Event`
    })
    
    # Functions to output the value boxes
    # Runner 1 Name
    output$runnerName1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(name1())),
            subtitle = "Name"
        )
    })
    
    # Runner 2 Name
    output$runnerName2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(name2())),
            subtitle = "Name"
        )
    })
    
    # Runner 1 Rank
    output$runnerRank1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(rank1())),
            subtitle = "Rank"
        )
    })
    
    # Runner 2 Rank
    output$runnerRank2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(rank2())),
            subtitle = "Rank"
        )
    })
    
    # Runner 1 Points
    output$runnerPts1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(pts1())),
            subtitle = "Current Year Points"
        )
    })
    
    # Runner 2 Points
    output$runnerPts2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(pts2())),
            subtitle = "Current Year Points"
        )
    })
    
    # Runner 1 Pts Per Event
    output$runnerPtsPerEvent1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(ptsPerEvent1())),
            subtitle = "Current Year Points Per Event"
        )
    })
    
    # Runner 2 Points Per Event
    output$runnerPtsPerEvent2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(ptsPerEvent2())),
            subtitle = "Current Year Points Per Event"
        )
    })
    
})


# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(dplyr)
library(RPostgreSQL)
library(DBI)
library(RSQLite)
library(reshape2)
library(yaml)
library(config)
library(RJDBC)

# Read connection data from yaml
yml <- read_yaml("aws.yaml")

# Connect to AWS
# Connect to database
aws <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = yml$host,
    user = yml$user,
    password = yml$password,
    port = yml$port
)
 
# # Query the current data for individuals
df <- dbGetQuery(aws, 
                 'SELECT 
                    "ATHLETE" AS "Athlete",
                    "TEAM" AS "Team",
                    "YEAR" AS "Class",
                    "GENDER" AS "Gender",
                    "POINTS" AS "Total Points",
                    "EVENTS" AS "Events",
                    "PTS_PER_EVENT" AS "Points Per Event",
                    "RANK_NUM" AS "Power Ranking",
                    "EVENT_YEAR" AS "Event Year",
                    "DVISION" AS "Division",
                    "LOAD_DATE" AS "Load Date"
                 FROM current_power_rankings 
                 WHERE "LOAD_DATE" IN (SELECT MAX(DATE("LOAD_DATE")) FROM current_power_rankings)
                 AND "EVENT_YEAR"::int8 = 2021 ')

# Query historic data
df_hist <- dbGetQuery(aws, 'SELECT
                                "ATHLETE" AS "Athlete",
                                "TEAM" AS "Team",
                                "YEAR" AS "Class",
                                "GENDER" AS "Gender",
                                "POINTS" AS "Total Points",
                                "EVENTS" AS "Events",
                                "PTS_PER_EVENT" AS "Points Per Event",
                                "RANK_NUM" AS "Power Ranking",
                                "EVENT_YEAR" AS "Event Year",
                                "DVISION" AS "Division",
                                "LOAD_DATE" AS "Load Date"
                            FROM current_power_rankings
                            WHERE "EVENT_YEAR"::int8 <> 2021 ')

# Query team data
teams <- dbGetQuery(aws,
                    'SELECT
                        "TEAM" AS "Team",
                        "GENDER" AS "Gender",
                        "athletes" AS "Athletes",
                        "events" AS "Events",
                        "avg_rank" AS "Average Power Ranking",
                        "avg_rank_score" AS "Average Ranking Score",
                        "total_points" AS "Total Points",
                        "avg_points_per_event" AS "Average Points Per Event",
                        "division" AS "Division",
                        "load_date" AS "Load Date"
                    FROM team_rankings 
                    WHERE "load_date" IN (SELECT MAX(DATE("load_date")) FROM team_rankings) 
                    AND "athletes" > 1 ')

# # Disconnect
dbDisconnect(aws)

# Bind individual data
all_df <- rbind(df, df_hist)

# Reorder columns
all_df <- all_df %>%
    select(`Power Ranking`, Athlete, Team, Class, Gender, Division, `Event Year`, `Total Points`, Events, `Points Per Event`, `Load Date`) %>%
    mutate(
        `Points Per Event` = round(`Points Per Event`, 2)
    ) %>%
    arrange(
        `Power Ranking`
    )

# Split by gender
men <- all_df %>% filter(Gender == 'M')
women <- all_df %>% filter(Gender == 'F')

# Reorder teams columns & split by gender

menTeams <- teams %>% filter(Gender == 'M')
womenTeams <- teams %>% filter(Gender == 'F')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ### INPUTS
    
    # Set year
    setYear <- reactive({
        return(input$year)
    })
    
    # Set division
    setDiv <- reactive({
        return(input$division)
    })
    
    
    ### DATA HANDLING FUNCTIONS
    
    # Function to select which division to query
    # Individuals
    mensData <- eventReactive(input$loadData, {
        df <- men %>% 
            filter(Division == setDiv() & `Event Year` == setYear())
        
        # Drop unneeded cols
        df <- df %>% select(-c(`Event Year`, Gender, Division, `Load Date`))
        
        return(df)
    }, ignoreNULL = TRUE)
    
    womensData <- eventReactive(input$loadData, {
        df <- women %>% 
            filter(Division == setDiv() & `Event Year` == setYear())
        
        # Drop unneeded cols
        df <- df %>% select(-c(`Event Year`, Gender, Division, `Load Date`))
        
        return(df)
    }, ignoreNULL = TRUE)
    
    # Teams
    menTeamData <- eventReactive(input$loadData, {
        df <- menTeams %>%
            filter(Division == setDiv())

        # Drop unneeded cols
        df <- df %>% select(-c(Gender, Division))

        return(df)
    }, ignoreNULL = TRUE)

    womenTeamData <- eventReactive(input$loadData, {
        df <- womenTeams %>%
            filter(Division == setDiv())

        # Drop unneeded cols
        df <- df %>% select(-c(Gender, Division))

        return(df)
    }, ignoreNULL = TRUE)
    
    #### CONTENT TO BE DISPLAYED
    ### Info Boxes for Rankings Pages
    output$dataUpdated <- renderInfoBox({
        # Get data
        updateDate <- max(df$`Load Date`)
        valueBox(
            updateDate, "Data Updated On"
        )
    })
    
    output$mensAthletes <- renderInfoBox({
        # Get data and count athletes
        df <- mensData()
        maleRunners <- n_distinct(df$Athlete)
        valueBox(
           maleRunners, "Ranked Men's Runners"
        )
    })
    
    output$womensAthletes <- renderInfoBox({
        # Get data and count athletes
        df <- womensData()
        femaleRunners <- n_distinct(df$Athlete)
        valueBox(
            femaleRunners, "Ranked Women's Runners"
        )
    })
    
    ### Data Tables for Mens and Womens Individual Rankings
    
    output$menRank = DT::renderDataTable(mensData(), rownames = FALSE, filter = 'top')
    
    output$womenRank = DT::renderDataTable(womensData(), rownames = FALSE, filter = 'top')
    
    ### Data Tables for Mens and Womens Team Rankings
    
    # output$menTeamRank = DT::renderDataTable(menTeamData(), rownames = FALSE, filter = 'top')
    # 
    # output$womenTeamRank = DT::renderDataTable(womenTeamData(), rownames = FALSE, filter = 'top')
    
    ######################################################
    
    
})


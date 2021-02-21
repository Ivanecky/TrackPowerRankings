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
# 
# # Query the current data
df <- dbGetQuery(aws, 
                 'SELECT 
                    *
                 FROM current_power_rankings 
                 WHERE "LOAD_DATE" IN (SELECT MAX(DATE("LOAD_DATE")) FROM current_power_rankings)
                 AND "EVENT_YEAR"::int8 = 2021 ')

# Query historic data
df_hist <- dbGetQuery(aws, 'SELECT * FROM current_power_rankings
                            WHERE "EVENT_YEAR"::int8 <> 2021 ')

# # Disconnect
dbDisconnect(aws)

# Bind data
all_df <- rbind(df, df_hist)

# Drop load date
all_df <- all_df %>% select(-c("LOAD_DATE", "OVERALL_RANK", "RANK"))

# Rename columns
colnames(all_df) <- c("Athlete", "Team", "Class", "Gender", "Total Points", "Events", "Points Per Event", "Power Ranking", "Event Year", "Division")

# Reorder columns
all_df <- all_df %>%
    select(`Power Ranking`, Athlete, Team, Class, Gender, Division, `Event Year`, `Total Points`, Events, `Points Per Event`) %>%
    mutate(
        `Points Per Event` = round(`Points Per Event`, 2)
    ) %>%
    arrange(
        `Power Ranking`
    )
    

# Split by gender
men <- all_df %>% filter(Gender == 'M')
women <- all_df %>% filter(Gender == 'F')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Set year
    setYear <- reactive({
        return(input$year)
    })
    
    # Set division
    setDiv <- reactive({
        return(input$division)
    })
    
    # Function to select which division to query
    mensData <- eventReactive(input$loadData, {
        df <- men %>% 
            filter(Division == setDiv() & `Event Year` == setYear())
        
        # Drop unneeded cols
        df <- df %>% select(-c(`Event Year`, Gender, Division))
        
        return(df)
    }, ignoreNULL = TRUE)
    
    womensData <- eventReactive(input$loadData, {
        df <- women %>% 
            filter(Division == setDiv() & `Event Year` == setYear())
        
        # Drop unneeded cols
        df <- df %>% select(-c(`Event Year`, Gender, Division))
        
        return(df)
    }, ignoreNULL = TRUE)
    
    ### Info Boxes for Rankings Pages
    output$dataUpdated <- renderInfoBox({
        # Get data
        updateDate <- max(df$LOAD_DATE)
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
    
    ### Data Tables for Mens and Womens Rankings
    
    output$menRank = DT::renderDataTable(mensData(), rownames = FALSE, filter = 'top')
    
    output$womenRank = DT::renderDataTable(womensData(), rownames = FALSE, filter = 'top')
    
    ######################################################
    
    ### ATHLETE SEARCH FUNCTIONS
    
    # Get searched athlete
    getRunner <- eventReactive(input$searchRunner, {
        # Filter data for athlete
        runner <- all_df %>%
            filter(grepl(lower(input$runnerName), lower(Athlete)))
        
        # Return runner's data
        return(runner)
    })
    
})


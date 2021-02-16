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

options(shiny.trace = TRUE)

# Read connection data from yaml
#yml <- read_yaml("aws.yaml")
cred <- get('aws')

# Connect to AWS
# Connect to database
aws <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = cred$server,
    user = cred$user,
    password = cred$password,
    port = cred$port
)
# 
# # Query the data
df <- dbGetQuery(aws, 'SELECT * FROM current_power_rankings WHERE "LOAD_DATE" IN (SELECT MAX(DATE("LOAD_DATE")) FROM current_power_rankings)')
# 
# # Disconnect
dbDisconnect(aws)

# Drop load date
df <- df %>% select(-c("LOAD_DATE"))

# Rename columns
colnames(df) <- c("Athlete", "Team", "Class", "Gender", "Total Points", "Events", "Points Per Event", "Preliminary Rank Score", "Final Rank Score", "Overall Power Ranking", "Event Year", "Division")

# Split by gender
men <- df %>% filter(Gender == 'M')
women <- df %>% filter(Gender == 'F')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Function to select which division to query
    mensData = reactive({
        
        # Temporarily set data
        df = men %>% filter(Division == "D1")
        
        if ( input$division == "D1" )
        {
            df = men %>% filter(Division == "D1")
        }
        else if ( input$division == "D2" )
        {
            df = men %>% filter(Division == "D2")
        }
        else if ( input$division == "D3" )
        {
            df = men %>% filter(Division == "D3")
        }
        
        return(df)
    })
    
    womensData = reactive({
        
        # Temporarily set data
        df = women %>% filter(Division == "D1")
        
        if ( input$division == "D1" )
        {
            df = women %>% filter(Division == "D1")
        }
        else if ( input$division == "D2" )
        {
            df = women %>% filter(Division == "D2")
        }
        else if ( input$division == "D3" )
        {
            df = women %>% filter(Division == "D3")
        }
        
        return(df)
    })
    
    output$menRank = renderDataTable(mensData())
    
    output$womenRank = renderDataTable(womensData())
    
})


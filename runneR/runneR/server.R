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

# Set reactable theme
options(reactable.theme = reactableTheme(
    color = "hsl(233, 9%, 87%)",
    backgroundColor = "hsl(233, 9%, 19%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

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

res <- dbGetQuery(aws, 'select * from race_results')

resGrp <- dbGetQuery(aws, 'select * from results_grouped')

resGrpYr <- dbGetQuery(aws, 'select * from results_grouped_yr')

# Quickly clean data
res <- res %>%
    mutate(
        TIME = round(as.numeric(TIME), 2)
    ) %>%
    select(
        NAME, TEAM, GENDER, YEAR, EVENT, MEET_DATE, MEET_NAME, TIME, PLACE, PRELIM
    )

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$resTbl <- renderReactable({
        reactable(
            res,
            showSortable = TRUE,
            filterable = TRUE,
            rownames = FALSE,
            columns = list(
                NAME = colDef(name = "Name"),
                GENDER = colDef(name = 'Gender'),
                TEAM = colDef(name = "Team"),
                YEAR = colDef(name = "Year"),
                EVENT = colDef(name = "Event"),
                MEET_DATE = colDef(name = 'Meet Date', format = colFormat(date = TRUE)),
                MEET_NAME = colDef(name = 'Meet'),
                PRELIM = colDef(name = 'Prelim'),
                PLACE = colDef(name = 'Place'),
                TIME = colDef(name = 'Time')
            )
        )
    })
    
    output$resGrpTbl <- renderReactable({
        reactable(
            resGrp,
            showSortable = TRUE,
            filterable = TRUE,
            rownames = FALSE,
            columns = list(
                NAME = colDef(name = "Name"),
                GENDER = colDef(name = 'Gender'),
                TEAM = colDef(name = "Team"),
                EVENT = colDef(name = "Event"),
                AVG_PLACE = colDef(name = 'Avg. Place'),
                AVG_TIME = colDef(name = 'Avg. Time'),
                PR = colDef(name = 'PR'),
                WINS = colDef(name = 'Wins'),
                TIMES.RUN = colDef(name = 'Times Run'),
                WIN_PCT = colDef(name = 'Win %')
            )
        )
    })
    
    output$resGrpYrTbl <- renderReactable({
        reactable(
            resGrpYr,
            showSortable = TRUE,
            filterable = TRUE,
            rownames = FALSE,
            columns = list(
                NAME = colDef(name = "Name"),
                GENDER = colDef(name = 'Gender'),
                TEAM = colDef(name = "Team"),
                YEAR = colDef(name = "Year"),
                EVENT = colDef(name = "Event"),
                AVG_PLACE = colDef(name = 'Avg. Place'),
                AVG_TIME = colDef(name = 'Avg. Time'),
                PR = colDef(name = 'PR'),
                WINS = colDef(name = 'Wins'),
                TIMES.RUN = colDef(name = 'Times Run'),
                WIN_PCT = colDef(name = 'Win %')
            )
        )
    })

})

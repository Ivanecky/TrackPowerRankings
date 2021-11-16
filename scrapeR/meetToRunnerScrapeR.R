# Code to generate line item performances from TFRRS.
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
library(kit)

# Set system variables
# Sys.setenv("http_proxy"="")
# Sys.setenv("no_proxy"=TRUE)
# Sys.setenv("no_proxy"=1)

# Load functions for scraping
source("/Users/samivanecky/git/TrackPowerRankings/Scraping_Fxns.R")
source("/Users/samivanecky/git/TrackPowerRankings/ResultsQuery.R")
source("/Users/samivanecky/git/TrackPowerRankings/meetScrapingFxns.R")

# Connect to AWS
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

# Test URL
url <- "https://www.tfrrs.org/results_search.html"

# Read meet name links
links <- getMeetLinks(url)

# Create a links DF to upload links to AWS table, storing links for meets that have been scraped
linksDf <- as.data.frame(links)

# Query links from link table
linkTbl <- dbGetQuery(aws, "select * from meet_links")

# Get new links (not in table)
joinLinks <- linksDf %>%
  left_join(linkTbl, by = c("links"))

# Write data to table for URLs
# dbRemoveTable(aws, "runners_grouped")
# dbCreateTable(aws, "meet_links", linksDf)
dbWriteTable(aws, "meet_links", linksDf, append = TRUE)

# Create a temporary dataframe for runner line item performance
runner_lines = as.data.frame(cbind("year", "event", 1.1, 1.1, "meet", "meet date", TRUE, "name", "gender", "team_name"))
# Rename columns
names(runner_lines) = c("YEAR", "EVENT", "TIME", "PLACE", "MEET_NAME", "MEET_DATE", "PRELIM", "NAME", "GENDER", "TEAM")
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

# Iterate over meets and get data
for (i in 901:1001) {
  # Check url
  tempURL <- gsub("[[:space:]]", "", links[i])
  
  # Check URL validity
  if(class(try(read_html(tempURL))) == 'try-error') {
    print(paste0("Failed to get data for : ", tempURL))
    next
  }
  
  # Call query function
  meetResults <- meetResQuery(tempURL)

  # Bind to existing data
  runner_lines <- rbind(runner_lines, meetResults)
}

# Add load date to all records being uploaded
runRecs <- runner_lines %>%
  mutate(
    load_d = lubridate::today()
  ) %>%
  filter(MEET_NAME != "meet") %>%
  funique() %>%
  mutate(
    TIME = as.numeric(TIME),
    PLACE = as.numeric(PLACE),
    MEET_DATE = lubridate::ymd(MEET_DATE),
    NAME = gsub("[^\x01-\x7F]", "", NAME)
  )

# Upload runner data to table
# Write data to table for URLs
# dbRemoveTable(aws, "runners_grouped")
# dbCreateTable(aws, "race_results", runRecs)
dbWriteTable(aws, "race_results", runRecs, append = TRUE)



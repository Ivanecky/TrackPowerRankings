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

# Load functions for scraping
source("/Users/samivanecky/git/TrackPowerRankings/scrapeR//Scraping_Fxns.R")
source("/Users/samivanecky/git/TrackPowerRankings/scrapeR//ResultsQuery.R")

# Define function
getRunnerURLs <- function(url) {
  # Get runner links
  runners <- url %>%
    GET(., timeout(30)) %>%
    read_html() %>%
    html_nodes(xpath = "//tbody/tr/td/div/a") %>%
    html_attr("href")
  
  # Manipulate strings
  for ( i  in 1:length(runners) )
  {
    temp = runners[i]
    #temp = substring(temp, 3)
    temp = paste0("https:", temp)
    temp = substr(temp, 1, nchar(temp)-3)
    runners[i] = temp
  }
  
  # Grab only strings for athletes
  runners <- runners[grepl("athletes", runners)]
  
  # Return
  return(runners)
}

getMeetLinks <- function(url = "https://www.tfrrs.org/results_search.html") {
  # Get runner links
  meets <- url %>%
    GET(., timeout(30)) %>%
    read_html() %>%
    html_nodes(xpath = "//tr/td/a") %>%
    html_attr("href")
  
  # Manipulate strings
  for ( i  in 1:length(meets) )
  {
    temp <- meets[i]
    temp <- paste0("https:", temp)
    temp <- gsub("[[:space:]]", "", temp)
    # temp <- paste0(substr(temp, 1, nchar(temp)-3), "tml")
    meets[i] <- temp
  }
  
  # Return
  return(meets)
}

# Wrap runnerscrape function
getRunner <- function(url) {
  runner <- tryCatch(
    {
      runnerScrape(url)
    },
    # Error and warning handling
    error = function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )
  # Return result
  return(runner)
}

# Function to get indoor meet runner URLs
getIndoorRunnerURLs <- function(url) {
  
  # Query HTML
  eventLinks <- url %>%
    GET(., timeout(30)) %>%
    read_html() %>%
    html_nodes(xpath = "//tbody/tr/td/a") %>%
    html_attr("href")
  
  # Filter out select URLs
  eventLinks <- eventLinks[grepl("results", eventLinks) & grepl("1_Mile|800m|400m|3000m|5000m|1000m|600m", eventLinks)]
  
  # Create empty vector for runner URLs
  runnerURLs <- vector()
  
  # Iterate over links to fix their formats
  for (i in 1:length(eventLinks)) {
    temp = eventLinks[i]
    #temp = substring(temp, 3)
    temp = paste0("https:", temp)
    temp = paste0(substr(temp, 1, nchar(temp)-3), "tml")
    eventLinks[i] = temp
  }
  
  # Iterate over event links to get runners
  for (i in 1:length(eventLinks)) {
    # Try and get HTML
    runnerURLs <- append(runnerURLs, getMeetLinks(url = eventLinks[i]))
  }
  
  # Filter out teams and dups
  runnerURLs <- runnerURLs[grepl("athletes", runnerURLs)]
  runnerURLs <- unique(runnerURLs)
  
  return(runnerURLs)
}

# Meet results query (XC)
xcMeetResQuery <- function(meetURL){
  # Get runner URLs
  runnerLinks <- getRunnerURLs(meetURL)
  
  # Create a temporary dataframe for runner line item performance
  runner_lines = as.data.frame(cbind("year", "event", 1.1, 1.1, "meet", "meet date", TRUE, "name", "gender", "team_name", "team_division"))
  # Rename columns
  names(runner_lines) = c("YEAR", "EVENT", "TIME", "PLACE", "MEET_NAME", "MEET_DATE", "PRELIM", "NAME", "GENDER", "TEAM", "DIVISION")
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
  
  # Detect cores
  cores <- detectCores()
  cl <- makeCluster(cores[1] - 1, outfile = '/Users/samivanecky/git/TrackPowerRankings/scraperErrors.txt')
  registerDoParallel(cl)
  
  runner_lines <- foreach(i=1:length(runnerLinks), .combine = rbind) %dopar% {
    
    source("/Users/samivanecky/git/TrackPowerRankings/scrapeR/meetScrapingFxns.R")
    
    # Make function call
    runner_temp <- getRunner(runnerLinks[i])
    
    runner_temp
  }
  
  stopCluster(cl)
  
  # Return data
  return(runner_lines)
}

# Indoor meet results query
indoorMeetResQuery <- function(meetURL){
  # Get runner URLs
  runnerLinks <- getIndoorRunnerURLs(meetURL)
  
  # Create a temporary dataframe for runner line item performance
  runner_lines = as.data.frame(cbind("year", "event", 1.1, 1.1, "meet", "meet date", TRUE, "name", "gender", "team_name", "team_division"))
  # Rename columns
  names(runner_lines) = c("YEAR", "EVENT", "TIME", "PLACE", "MEET_NAME", "MEET_DATE", "PRELIM", "NAME", "GENDER", "TEAM", "DIVISION")
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
  
  # Detect cores
  cores <- detectCores()
  cl <- makeCluster(cores[1] - 1, outfile = '/Users/samivanecky/git/TrackPowerRankings/scraperErrors.txt')
  registerDoParallel(cl)
  
  runner_lines <- foreach(i=1:length(runnerLinks), .combine = rbind) %dopar% {
    
    source("/Users/samivanecky/git/TrackPowerRankings/scrapeR/meetScrapingFxns.R")
    
    # Make function call
    runner_temp <- getRunner(runnerLinks[i])
    
    runner_temp
  }
  
  stopCluster(cl)
  
  # Return data
  return(runner_lines)
}
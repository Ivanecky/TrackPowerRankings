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
source("/Users/samivanecky/git/TrackPowerRankings/Scraping_Fxns.R")
source("/Users/samivanecky/git/TrackPowerRankings/ResultsQuery.R")

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

getMeetLinks <- function(url) {
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
    temp <- substr(temp, 1, nchar(temp)-3)
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

# Meet results query
meetResQuery <- function(meetURL){
  # Get runner URLs
  runnerLinks <- getRunnerURLs(meetURL)
  
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
  
  # Detect cores
  cores <- detectCores()
  cl <- makeCluster(cores[1] - 1, outfile = '/Users/samivanecky/git/TrackPowerRankings/scraperErrors.txt')
  registerDoParallel(cl)
  
  runner_lines <- foreach(i=1:length(runnerLinks), .combine = rbind) %dopar% {
    
    source("meetScrapingFxns.R")
    
    # Make function call
    runner_temp <- getRunner(runnerLinks[i])
    
    runner_temp
  }
  
  stopCluster(cl)
  
  # Return data
  return(runner_lines)
}
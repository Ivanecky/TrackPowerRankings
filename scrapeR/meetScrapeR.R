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
source("Scraping_Fxns.R")
source("ResultsQuery.R")

# Test URL
url <- "https://www.tfrrs.org/results_search.html"

# Read meet name links
links <- read_html(url) %>%
  html_nodes(xpath = "//tr/td/a") %>%
  html_attr("href")

# Iterate over links

# Get meet html
meetHtml <- read_html("https://www.tfrrs.org/results/xc/18919/NESCAC_Cross_Country_Championships")

# Read the meet tables
meetTbls <- meetHtml %>%
  html_table()

# Get meet name & details
meetDets <- meetHtml %>%
  html_nodes("h3") %>%
  html_text()

# Extract meet name
meetName <- str_trim(unlist(strsplit(meetDets[1], "[\n\t]"))[5])

# Extract meet date
meetDate <- meetHtml %>%
  html_nodes("//div[3]//div//div//div[2]//div[1]//div//div//div//div[1]") %>%
  html_text()

# Extract order of tables
# Empty vector to hold results
tblOrder <- vector()

# Iterate over remaining values in dets
for( i in 2:length(meetDets)) {
  # Check line
  tempLine <- tolower(str_trim(unlist(strsplit(meetDets[i], "[\n\t]"))[1]))
  # Check cases 
  if(grepl("women", tempLine)) {
    if(grepl("team", tempLine)) {
      tblOrder <- append(tblOrder, "Women - Team")
    } else {
      tblOrder <- append(tblOrder, "Women - Ind")
    }
  } else {
    if(grepl("team", tempLine)) {
      tblOrder <- append(tblOrder, "Men - Team")
    } else {
      tblOrder <- append(tblOrder, "Men - Ind")
    }
  }
}

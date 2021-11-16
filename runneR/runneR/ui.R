# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(stats)
library(knitr)
library(dplyr)
library(png)
library(DT)
library(reactable)
library(reactR)

# Define UI for application that draws a histogram
ui <- navbarPage("runneR", 
    # All results
    tabPanel("All Results",
        mainPanel(
            reactableOutput("resTbl"),
            width = '100%'
        )
    ),
    
    # Grouped results
    tabPanel("Aggregated Results",
         mainPanel(
             reactableOutput("resGrpTbl"),
             width = '100%'
         )
    ),
    
    # Grouped yearly results
    tabPanel("Aggregated Yearly Results",
         mainPanel(
             reactableOutput("resGrpYrTbl"),
             width = '100%'
         )
    )
)

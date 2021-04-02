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

# Define UI for application
ui = dashboardPage(
    
    dashboardHeader(title = "DPI"),
    dashboardSidebar(
        # Select Division for data
        selectInput("division", 
                    "Select Division",
                    choices = c("D1", "D2", "D3")),
        # Select Division for data
        selectInput("year", 
                    "Select Year",
                    choices = c("2021", "2020", "2019", "2018", "2017")),
        # Button to change data
        actionButton("loadData", label = "Load Rankings"),
        # Sidebar tab menu
        sidebarMenu(
            menuItem("Individual Power Rankings", tabName = "powerRankings"),
            #menuItem("Team Power Rankings", tabName = "teamRankings"),
            menuItem("About", tabName = "about")
        )
    ),
    # Define body of application
    dashboardBody(
        # Styling
        # Create tabs
        tabItems(
            ### Individual Power Rankings
            tabItem("powerRankings", 
                fluidRow(
                  tags$h1("Distance Power Index"),
                  p("Please read the 'About' page to answer questions. Select a year and division and press 'Load Rankings' to generate rankings")
                ),
                # Info box row
                fluidRow(
                    valueBoxOutput("dataUpdated"),
                    valueBoxOutput("mensAthletes"),
                    valueBoxOutput("womensAthletes")
                ),
                # Mens rankings
                fluidRow(
                    box(
                        title = "Mens Power Rankings", solidHeader = TRUE,
                        collapsible = TRUE, background = "light-blue", width = '100%',
                        DT::dataTableOutput('menRank'), style = "overflow-x: scroll;"
                    )
                ),
                # Womens rankings
                fluidRow(
                    box(
                        title = "Womens Power Rankings", solidHeader = TRUE,
                        collapsible = TRUE, background = "light-blue", width = '100%',
                        DT::dataTableOutput('womenRank'), style = "overflow-x: scroll;"
                    )
                )
            ),
            
            ### Team Power Rankings
            # tabItem("teamRankings",
            #         fluidRow(
            #             tags$h1("NCAA Indoor Team Power Rankings"),
            #             p("Please read the 'About' page to answer questions. Select a year and division and press 'Load Rankings' to generate rankings")
            #         ),
            #         # Info box row
            #         fluidRow(
            #             valueBoxOutput("dataUpdated"),
            #             valueBoxOutput("mensTeams"),
            #             valueBoxOutput("womensTeams")
            #         ),
            #         # Mens rankings
            #         fluidRow(
            #             box(
            #                 title = "Mens Team Power Rankings", solidHeader = TRUE,
            #                 collapsible = TRUE, background = "light-blue", width = '100%',
            #                 DT::dataTableOutput('menTeamRank'), style = "overflow-x: scroll;"
            #             )
            #         ),
            #         # Womens rankings
            #         fluidRow(
            #             box(
            #                 title = "Womens Team Power Rankings", solidHeader = TRUE,
            #                 collapsible = TRUE, background = "light-blue", width = '100%',
            #                 DT::dataTableOutput('womenTeamRank'), style = "overflow-x: scroll;"
            #             )
            #         )
            # ),
            # About page
            tabItem("about",
                h1("Welcome to the NCAA Indoor Track Power Rankings"),
                    h2("How does it work?"),
                        p(
                            "In short, we calculate scores for athletes based on a number of factors. Performances are evaluated based on how an athlete sits on the indoor performance list, how strong their time is relative to others in that event, and their strength across multiple events.
                            An athlete accumulates points on all of these metrics, with a maximum of 200 point per event. After weighting a number of developed metrics, we end up with a `RANK` value which is their final score, and determines their overall power ranking."
                            ),
                    h2("Can you explain the different columns? There are a lot of numbers..."),
                        p("Sure. It can be a little confusing so here is an explanation of our developed metrics:"),
                        p("Total Points: The total number of points the ranking system gave an athlete based on their standing in the NCAA and their specific time."),
                        p("Events: Number of events an athlete is in the NCAA Top 100 for at the moment."),
                        p("Points Per Event: The average number of points (out of 230) an athlete has for all their events."),
                        p("Power Ranking: Where the athlete sits in their respective Division and Gender after ordering by our Final Rank Score."),
                    h2("Why aren't all the athletes in the NCAA in the lists?"),
                        p("In short - data limitations. Read more below."),
                    h2("Some of your rankings seem...bad."),
                        p("Well, that's not a question but it's a fair point. We are in the early stages of developing these (automated) rankings which means 1. There's a lot of improvements to be made and 2. We don't make manual 'corrections', what the algorithms develop is what you get.
                          No matter how good we tune them, there will ALWAYS be errors. The goal is that with more time and work, we can really dial these in. We also don't count relays right now. It's coming but not available just yet.
                          Of course, if you are upset about a ranking, just remember, blame the computer, not us."),
                    h2("What comes next?"),
                        p("We've mentioned things about our next steps already but there are a bunch of future adaptations that we'd love to build. Creating better algorithms, getting more (all) athletes into our system, adding more features like comparing runners, schools, etc. Maybe even doing 
                          cross country and/or outdoor track. There are plenty of opportunities to make this much better and this is essentially a 'beta' version. We wanted to get it public so you can check it out, maybe use it, and provide feedback on the good, bad, and otherwise. If you have 
                          comments, suggestions, or find bugs, please reach out and let us know. You can find us on Instagram at @run.the.numbers or @ivanxcky.")
            )
        )
    )
)



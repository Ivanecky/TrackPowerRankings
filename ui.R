# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(stats)
library(knitr)
library(dplyr)
library(png)

# Define UI for application
ui = dashboardPage(
    dashboardHeader(title = "NCAA Indoor Track Power Rankings"),
    dashboardSidebar(
        # Select Division for data
        selectInput("division", 
                    "Select Division",
                    choices = c("D1", "D2", "D3")),
        # Sidebar tab menu
        sidebarMenu(
            menuItem("Power Rankings", tabName = "powerRankings"),
            menuItem("About", tabName = "about")
        )
    ),
    # Define body of application
    dashboardBody(
        # Create tabs
        tabItems(
            # Main page with rankings
            tabItem("powerRankings", 
                fluidRow(
                  h1("NCAA Indoor Track Power Rankings"),
                  p("Please read the 'About' page to answer questions.")
                ),
                fluidRow(
                    column(12,
                           h1("Mens Rankings"),
                           dataTableOutput('menRank')
                    )
                ),
                fluidRow(
                    column(12,
                           h1("Womens Rankings"),
                           dataTableOutput ('womenRank')
                    )
                )
            ),
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
                        p("Points Per Event: The average number of points (out of 200) an athlete has for all their events."),
                        p("Preliminary Rank Score: Our first scoring iteration done. Won't give away the full details for this but it helps us for our next metric."),
                        p("Final Rank Score: The final ranking metric we use to order the athletes. Developed using the Preliminary Rank Score."),
                        p("Overall Power Ranking: Where the athlete sits in their respective Division and Gender after ordering by our Final Rank Score."),
                    h2("Why aren't all the athletes in the NCAA in the lists?"),
                        p("In short - data limitations. Read more below."),
                    h2("Some of your rankings seem...bad."),
                        p("Well, that's not a question but it's a fair point. We are in the early stages of developing these (automated) rankings which means 1. There's a lot of improvements to be made and 2. We don't make manual 'corrections', what the algorithms develop is what you get.
                          No matter how good we tune them, there will ALWAYS be errors. The goal is that with more time and work, we can really dial these in. Of course, if you are upset about a ranking, just remember, blame the computer, not us."),
                    h2("What comes next?"),
                        p("We've mentioned things about our next steps already but there are a bunch of future adaptations that we'd love to build. Creating better algorithms, getting more (all) athletes into our system, adding more features like comparing runners, schools, etc. Maybe even doing 
                          cross country and/or outdoor track. There are plenty of opportunities to make this much better and this is essentially a 'beta' version. We wanted to get it public so you can check it out, maybe use it, and provide feedback on the good, bad, and otherwise. If you have 
                          comments, suggestions, or find bugs, please reach out and let us know. You can find us on Instagram at @run.the.numbers or @ivanxcky.")
            )
        )
    )
)



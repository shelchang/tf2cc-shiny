library(shiny)
library(data.table)
library(DT)
library(tidyverse)
library(ggplot2)
library(bit64)

playerTable <- as.data.frame(fread('https://raw.githubusercontent.com/shelchang/tf2cc-data/main/persons.csv')) %>% select(steamID, steamName) %>% arrange(steamName)

shinyUI(
  fluidPage(
    titlePanel(title = "TF2 Coaching Classic PUG Stats"),
    p(textOutput("lastUpdate")),
    p(a("Download raw data", href="https://github.com/shelchang/tf2cc-data/blob/main/raw_data.zip?raw=true", download=NA, target="_blank")),
    tabsetPanel(
      tabPanel(title = "Match stats",
               sidebarLayout(
                 sidebarPanel(
                   p(strong("Stats")),
                   p(actionLink("sessionsPerMonth", "Sessions per month")),
                   p(actionLink("matchTypes", "Match types per month")),
                   p(actionLink("sessionPlayers", "Players per session")),
                   p(actionLink("playerSessions", "Sessions per player")),
                   p(actionLink("mapTime", "Time played by map")),
                   p(actionLink("uniquePlayers", "Unique players per month")),
                   width = 3
                 ),
                 mainPanel(plotOutput("mainPlot"), dataTableOutput("mainTable"))
               )
      ),
      tabPanel(title = "Player rankings",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "stats", "Stats to show",
                                      choices = c("KA/D","DPM","Win/loss record","Healing stats"), selected = "DPM"),
                   
                   p(textOutput("medicExp")),
                   
                   checkboxGroupInput(inputId = "classFilter", "Filter by class",
                                      choices = c("scout","soldier","pyro","demoman","heavyweapons","engineer","medic","sniper","spy"), selected = c("scout","soldier","demoman")),
                   
                   p(actionButton("applyRankings", "Apply changes")),
                   hr(),
                   p(strong("More filters")),
                   
                   dateRangeInput("dateFilter", "Date range",
                                  start = "2020-07-01", end = NULL,
                                  min = "2020-07-01", max = NULL
                   ),
                   
                   sliderInput(inputId = "matchFormat", "Number of match players",
                               min = 4, max = 18,
                               value = c(8,13)
                   ),
                   p("Format assumptions: 8-13 players will be 4s/5s/6s, anything below that is ultiduo, 14 and up is prolander, 18 is highlander"),
                   
                   checkboxGroupInput(inputId = "mapFilter", "Map type",
                                      choices = c("koth","cp","pl","ultiduo"), selected = c("koth","cp","pl","ultiduo"), inline = TRUE),
                   
                   
                   width = 3
                 ),
                 mainPanel(
                   dataTableOutput("rankingTable")
                 )
               )
      ),
      tabPanel(title = "Player comparison",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(inputId = "players", "Player select",
                                  choices = playerTable$steamName, multiple = TRUE),
                   
                   selectInput(inputId = "compare", "Stats to compare",
                                      choices = c("KA/D","DPM"), selected = "DPM"),
                   
                   checkboxGroupInput(inputId = "classFilter2", "Filter by class",
                                      choices = c("scout","soldier","pyro","demoman","heavyweapons","engineer","medic","sniper","spy"), selected = c("scout","soldier","demoman")),
                   
                   p(actionButton("applyCompare", "Apply changes")),
                   hr(),
                   p(strong("More filters")),
                   
                   dateRangeInput("dateFilter2", "Date range",
                                  start = "2020-07-01", end = NULL,
                                  min = "2020-07-01", max = NULL
                   ),
                   
                   sliderInput(inputId = "matchFormat2", "Number of match players",
                               min = 4, max = 18,
                               value = c(8,13)
                   ),
                   p("Format assumptions: 8-13 players will be 4s/5s/6s, anything below that is ultiduo, 14 and up is prolander, 18 is highlander"),
                   
                   checkboxGroupInput(inputId = "mapFilter2", "Map type",
                                      choices = c("koth","cp","pl","ultiduo"), selected = c("koth","cp","pl","ultiduo"), inline = TRUE),
                   width = 3
                 ),
                 mainPanel(
                   plotOutput("comparisonPlot")
                 )
               )
      )
    )
  )
)
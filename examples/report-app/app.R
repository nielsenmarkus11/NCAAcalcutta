library(shiny)
library(NCAAcalcutta)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(xml2)
library(rvest)

# Run the report
teams <- read.csv("data/teams-out-home.csv")
teams <- teams %>% 
  select(-X, -group, -opponent) %>% 
  filter(!is.na(bid))

results_app(teams, 1175)


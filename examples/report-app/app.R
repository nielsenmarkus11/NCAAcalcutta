library(shiny)
library(NCAAcalcutta)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(xml2)
library(rvest)

# Run the report
teams <- read.csv("data/teams-out.csv")
teams <- teams %>% 
  select(-X, -opponent) %>% 
  filter(!is.na(bid))

results_app(teams, 650, 2023)


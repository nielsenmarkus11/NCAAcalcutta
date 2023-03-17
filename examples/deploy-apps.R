library(NCAAcalcutta)
library(dplyr)
# Input the 2019 teams and run the auction app
teams <- import_teams(system.file("extdata", "ncaa-teams-2023.csv", package = "NCAAcalcutta"))

rteams <- randomize_teams(teams, random_seed = 15)

start_auction(rteams, randomize = F)


library(NCAAcalcutta)
# Run the report
teams <- read.csv("~/../Desktop/teams-out-home.csv")
teams <- teams %>% 
  select(-X, -opponent) %>% 
  filter(!is.na(bid))


results_app(teams, 1175, 2023)


library(rsconnect)
rsconnect::setAccountInfo(name='<USERNAME>', token='<TOKEN>', secret='<SECRET>')
deployApp('examples/report-app')

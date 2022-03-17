library(NCAAcalcutta)
library(dplyr)

games_2018=scrape.game.results(2018,'mens')
games_2019=scrape.game.results(2019,'mens')
games_2020=scrape.game.results(2020,'mens')
games_2021=scrape.game.results(2021,'mens')
games_2022=scrape.game.results(2022,'mens')

tournament_2018 = get_tournament_scores('mens', 2018)
tournament_2019 = get_tournament_scores('mens', 2019)
tournament_2021 = get_tournament_scores('mens', 2021)
tournament_2022 = get_tournament_scores('mens', 2022)

tournament_all <- tournament_2018 %>% mutate(year='2018') %>% 
  bind_rows(tournament_2019 %>% mutate(year='2019')) %>%  
  bind_rows(tournament_2021 %>% mutate(year='2021'))

all_games=rbind(games_2018,games_2019, games_2020, games_2021, games_2022)
all_teams=scrape_teams('mens')

library(RPostgreSQL)
con <- dbConnect(dbDriver("PostgreSQL"),
                 host='localhost',
                 port=5432,
                 user='postgres')

dbWriteTable(con, "ncaa_games", all_games)
dbWriteTable(con, "ncaa_teams", all_teams)
dbWriteTable(con, "ncaa_tournament_games", tournament_all)


dbDisconnect(con)
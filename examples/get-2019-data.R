library(NCAAcalcutta)
bracket <- get_tournament_scores_api(year=2023)
# team_names <- scrape_teams('mens')

library(dplyr)
teams1 <- bracket %>%
  dplyr::filter(round == 1) %>% 
  dplyr::select(game_id, rank = team1_seed, region, logo=team1_logo, team=team1_displayName) %>% 
  dplyr::mutate(rank = as.numeric(as.character(rank)),
                region = stringi::stri_trans_totitle(region))

teams2 <- bracket %>%
  dplyr::filter(round == 1) %>% 
  dplyr::select(game_id, rank = team2_seed, region, logo=team2_logo, team=team2_displayName) %>% 
  dplyr::mutate(rank = as.numeric(as.character(rank)),
                region = stringi::stri_trans_totitle(region))

# Find Opponent
t1opp <- teams2 %>% 
  dplyr::select(game_id, opponent = team, opponent_logo = logo)
teams1 <- teams1 %>% 
  left_join(t1opp, by='game_id')

t2opp <- teams1 %>% 
  dplyr::select(game_id, opponent = team, opponent_logo = logo)
teams2 <- teams2 %>% 
  left_join(t2opp, by='game_id')



teams <- bind_rows(teams1, teams2) %>% 
  arrange(region, rank)

teams$game_id <- NULL

# teams <- edit(teams)
teams <- teams %>% arrange(region, rank)

start_auction(teams, c("Mark", "Marko", "Marky", "Markus"), 1800)

write.csv(teams,file="inst/extdata/ncaa-teams-2023.csv", row.names = FALSE)

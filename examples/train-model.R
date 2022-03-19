library(dplyr)
library(tidymodels)
library(RPostgreSQL)
con <- dbConnect(dbDriver("PostgreSQL"),
                 host='localhost',
                 port=5432,
                 user='postgres')


tournament_games = dbGetQuery(con, "select * from ncaa_tournament_games")
teams = dbGetQuery(con, "select * from ncaa_teams")

dbDisconnect(con)


# Fit a simple model
team1 = tournament_games %>%
  mutate(match_id = row_number(),
         win = factor(ifelse(!is.na(team1_win),"YES","NO"),levels = c("NO","YES"))) %>% 
  select(match_id, round, region,
         team_seed=team1_seed,
         team2_seed=team2_seed,
         team_id=team1_id,
         team2_id=team2_id,
         team_score=team1_score,
         year,
         win)
team2 = tournament_games %>% 
  mutate(match_id = row_number(),
         win = factor(ifelse(!is.na(team2_win),"YES","NO"),levels = c("NO","YES"))) %>% 
  select(match_id, round, region,
         team_seed=team2_seed,
         team2_seed=team1_seed,
         team_id=team2_id,
         team2_id=team1_id,
         team_score=team2_score,
         year,
         win)
combo_team <- bind_rows(team1, team2)


#fit points model
set.seed(345)
folds <- vfold_cv(combo_team, v = 10)

lm_mod <- linear_reg() %>% 
  set_engine("lm")

lm_wf <- workflow() %>%
  add_model(lm_mod) %>%
  add_formula(team_score~round+region+team_seed+team2_seed)

set.seed(456)
lm_fit_rs <- 
  lm_wf %>% 
  fit_resamples(folds)

collect_metrics(lm_fit_rs)

rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_formula(team_score~round+region+team_seed+team2_seed)

set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

collect_metrics(rf_fit_rs)

# Save object
lm_fit <- lm_mod %>% 
  fit(team_score~round+region+team_seed+team2_seed, data=combo_team)

new_dat <- expand.grid(region=c("East","West","Midwest","South","Final Four"),
                       team_seed=1:16, round=1:6, team2_seed=1:16)
new_dat <- new_dat %>% 
  bind_cols(predict(lm_fit, new_data = new_dat) %>% select(pred_score=.pred)) %>% 
  mutate(pred_score = floor(pred_score))

# Fit win model
set.seed(543)
folds <- vfold_cv(combo_team, v = 10)

lm_mod <- logistic_reg() %>% 
  set_engine("glm")

lm_wf <- workflow() %>%
  add_model(lm_mod) %>%
  add_formula(win~round+region+team_seed+team2_seed)

set.seed(654)
lm_fit_rs <- 
  lm_wf %>% 
  fit_resamples(folds)

collect_metrics(lm_fit_rs)

rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_formula(win~round+region+team_seed+team2_seed)

set.seed(765)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

collect_metrics(rf_fit_rs)

# Save object
lm_fit <- lm_mod %>% 
  fit(win~round+region+team_seed+team2_seed, data=combo_team)

new_dat <- new_dat %>% 
  bind_cols(predict(lm_fit, new_data = new_dat, type = 'prob') %>% select(pred_win=.pred_YES))

write.csv(new_dat,file="inst/extdata/pred-scores.csv", row.names = FALSE)


#simulate games
library(NCAAcalcutta) 
tournament_2022 = get_tournament_scores(year=2022)

game_ids <- data.frame(game1_id = rep(1:32, each=1),
                       game2_id = rep(1:16, each=2),
                       game3_id = rep(1:8, each=4),
                       game4_id = rep(1:4, each=8),
                       game5_id = rep(1:2, each=16),
                       game6_id = rep(1, each=32))

round1 <- tournament_2022 %>% 
  filter(round==1) %>% 
  bind_cols(game_ids) %>% 
  left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
            by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
  left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
            by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
  mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
         team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))

winners_round1 <- round1 %>% 
  filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
  select(game1_id, game2_id, game3_id, game4_id, game5_id, game6_id,
         seed=team1_seed,
         id=team1_id,
         round,
         region) %>% 
  bind_rows(round1 %>% 
              filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
              select(game1_id, game2_id, game3_id, game4_id, game5_id, game6_id,
                     seed=team2_seed,
                     id=team2_id,
                     round,
                     region) ) %>% 
  arrange(game1_id, game2_id) %>% 
  select(-game1_id) %>% 
  mutate(name = paste0('team', rep(1:2,16))) %>% 
  pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
  left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
            by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
  left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
            by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
  mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
         team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))

round2 <- winners_round1 %>% 
  left_join(tournament_2022 %>% 
              filter(round==2)) %>% 
  mutate(round=2)

winners_round2 <- round2 %>% 
  filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
  select(game2_id, game3_id, game4_id, game5_id, game6_id,
         seed=team1_seed,
         id=team1_id,
         round,
         region) %>% 
  bind_rows(round2 %>% 
              filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
              select(game2_id, game3_id, game4_id, game5_id, game6_id,
                     seed=team2_seed,
                     id=team2_id,
                     round,
                     region)) %>% 
  arrange(game2_id, game3_id) %>% 
  select(-game2_id) %>% 
  mutate(name = paste0('team', rep(1:2,8))) %>% 
  pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
  left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
            by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
  left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
            by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
  mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
         team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))

round3 <- winners_round2 %>% 
  left_join(tournament_2022 %>% 
              filter(round==3)) %>% 
  mutate(round=3)

winners_round3 <- round3 %>% 
  filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
  select(game3_id, game4_id, game5_id, game6_id,
         seed=team1_seed,
         id=team1_id,
         round,
         region) %>% 
  bind_rows(round3 %>% 
              filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
              select(game3_id, game4_id, game5_id, game6_id,
                     seed=team2_seed,
                     id=team2_id,
                     round,
                     region)) %>% 
  arrange(game3_id, game4_id) %>% 
  select(-game3_id) %>% 
  mutate(name = paste0('team', rep(1:2,4))) %>% 
  pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
  left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
            by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
  left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
            by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
  mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
         team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))

round4 <- winners_round3 %>% 
  left_join(tournament_2022 %>% 
              filter(round==4)) %>% 
  mutate(round=4)

winners_round4 <- round4 %>% 
  filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
  select(game4_id, game5_id, game6_id,
         seed=team1_seed,
         id=team1_id,
         round) %>% 
  mutate(region="Final Four") %>% 
  bind_rows(round4 %>% 
              filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
              select(game4_id, game5_id, game6_id,
                     seed=team2_seed,
                     id=team2_id,
                     round) %>% 
              mutate(region="Final Four")) %>% 
  arrange(game4_id, game5_id) %>% 
  select(-game4_id) %>% 
  mutate(name = paste0('team', rep(1:2,2))) %>% 
  pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
  left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
            by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
  left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
            by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
  mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
         team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA),
         team1_pred_score = 1.75*team1_pred_score,
         team2_pred_score = 1.75*team2_pred_score)

round5 <- winners_round4 %>% 
  left_join(tournament_2022 %>% 
              filter(round==5)) %>% 
  mutate(round=5)

pred_tournament_scores <- bind_rows(round1 %>% select(-starts_with('game')),
                                    round2 %>% select(-starts_with('game')),
                                    round3 %>% select(-starts_with('game')),
                                    round4 %>% select(-starts_with('game')),
                                    round5 %>% select(-starts_with('game'))) %>% 
  mutate(team1_score = coalesce(team1_score, team1_pred_score),
         team2_score = coalesce(team2_score, team2_pred_score),
         team1_win = coalesce(team1_win, team1_pred_win),
         team2_win = coalesce(team2_win, team2_pred_win))


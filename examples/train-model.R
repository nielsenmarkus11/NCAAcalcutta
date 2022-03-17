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
  mutate(match_id = row_number()) %>% 
  select(match_id, round, region,
         team_seed=team1_seed,
         team2_seed=team2_seed,
         team_id=team1_id,
         team2_id=team2_id,
         team_score=team1_score,
         year)
team2 = tournament_games %>% 
  mutate(match_id = row_number()) %>% 
  select(match_id, round, region,
         team_seed=team2_seed,
         team2_seed=team1_seed,
         team_id=team2_id,
         team2_id=team1_id,
         team_score=team2_score,
         year)
combo_team <- bind_rows(team1, team2)


#fit models
set.seed(345)
folds <- vfold_cv(combo_team, v = 10)

lm_mod <- linear_reg() %>% 
  set_engine("lm")

lm_wf <- workflow() %>%
  add_model(lm_mod) %>%
  add_formula(team_score~round+region+team_seed)

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
  add_formula(team_score~round+region+team_seed)

set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

collect_metrics(rf_fit_rs)

# Save object
rf_fit <- rf_mod %>% 
  fit(team_score~round+region+team_seed, data=combo_team)

new_dat <- expand.grid(region=c("East","West","Midwest","South"),
                       team_seed=1:16, round=1:6)
new_dat <- new_dat %>% 
  bind_cols(predict(rf_fit, new_data = new_dat) %>% select(pred_score=.pred)) %>% 
  mutate(pred_score = floor(pred_score))

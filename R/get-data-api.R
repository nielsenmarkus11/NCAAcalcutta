firstDayOfMonth <- function(date, day="Mon", abbreviate=TRUE) {
  # first 7 days of month
  s <- seq(as.Date(format(date, format="%Y-%m-01")), by="day", length.out=7)
  # first day of month
  d <- s[weekdays(s,abbreviate)==day]
  d
}

#' Get live scores from NCAA tournament
#' 
#' Get live scores from NCAA tournament for auction and report app scoring.
#' 
#' @param league "mens" or "womens"
#' @param year NCAA tournament year
#' 
#' @returns data.frame with 63 games and scores.
#' 
#' @importFrom httr GET content
#' @importFrom dplyr '%>%' mutate select case_when
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_wider
#' 
#' @export

get_tournament_scores_api <- function(league = 'mens', year = NULL) {
  # Get dates of tournament
  date = paste0(year,'-04-01')
  final_game_date = firstDayOfMonth(paste0(year,'-04-01'), day = "Sun") + 1
  first_game_date = final_game_date - 18
  game_dates = paste0(format(first_game_date, format='%Y%m%d'), '-', format(final_game_date+1, format='%Y%m%d'))
  
  # Call ESPN API
  ncaa_json = content(GET(paste0("http://site.api.espn.com/apis/site/v2/sports/basketball/", league, "-college-basketball/scoreboard?dates=", game_dates, "&groups=50&limit=500")))
  
  # Extract team data
  length_df <- length(ncaa_json[["events"]])
  
  ncaa_out <- data.frame()
  
  game_vars <- c("id", "name", "shortName")
  team_vars <- c("id", "abbreviation", "displayName", "shortDisplayName", "logo", "conferenceId")
  
  for (i in 1:length_df){
    for (j in 1:2){
      
      game_vec <- ncaa_json[["events"]][[i]][game_vars]
      names(game_vec)[1] <- "game_id"
      game2_vec <- ncaa_json[["events"]][[i]][["competitions"]][[1]][["notes"]][[1]][["headline"]]
      game2_vec_raw <- str_split(game2_vec, " - ", simplify = TRUE)
      if (length(game2_vec_raw)==3){
        region_round = c(gsub(" Region","", game2_vec_raw[2]), game2_vec_raw[3])
        tournament = game2_vec_raw[1]
      } else {
        region_round = c("Final Four", game2_vec_raw[2])
        tournament = "Other"
      }
      names(region_round) <- c('region', 'round_text')
      
      team_vec <- ncaa_json[["events"]][[i]][["competitions"]][[1]][['competitors']][[j]][['team']][team_vars]
      names(team_vec) <- team_vars
      if (is.null(team_vec[['logo']])) team_vec[['logo']] <- ""
      if (is.null(team_vec[['conferenceId']])) team_vec[['conferenceId']] <- "" 
      seed <- ncaa_json[["events"]][[i]][["competitions"]][[1]][['competitors']][[j]][['curatedRank']][['current']]
      score <- ncaa_json[["events"]][[i]][["competitions"]][[1]][['competitors']][[j]][['score']]
      completed <- ncaa_json[['events']][[i]][["status"]][["type"]][["completed"]]
      
      combine_vec <- c(game_vec, region_round, team_vec, team=j, seed=seed, score=as.numeric(score), completed=completed, tournament=tournament)
      
      # convert the list into a dataframe 
      df_loop <- cbind.data.frame(combine_vec)
      
      # combine the march madness stats into the empty data.frame
      ncaa_out <- rbind(ncaa_out, df_loop)
    }
  }
  
  bracket <- ncaa_out %>% 
    filter(tournament == "Men's Basketball Championship") %>% 
    mutate(round = case_when(round_text == "1st Round"~1,
                             round_text == "2nd Round"~2,
                             round_text == "Sweet 16"~3,
                             round_text == "Elite 8"~4,
                             round_text == "Final Four"~5,
                             round_text == "National Championship"~6)) %>% 
    pivot_wider(names_from = team,
                names_glue = "team{team}_{.value}",
                values_from = c(id, seed, score, abbreviation, displayName, shortDisplayName, logo, conferenceId)) %>% 
    mutate(team2_seed = ifelse(round ==1 & team2_seed ==99, 17-team1_seed, team2_seed),
           team1_win = ifelse(completed & team1_score>team2_score, 'W', as.character(NA)),
           team2_win = ifelse(completed & team1_score<team2_score, 'W', as.character(NA))) %>% 
    select(game_id, round, region, team1_seed, team2_seed, team1_id, team2_id, team1_score, team2_score, team1_logo, team2_logo,
           team1_displayName, team2_displayName, team1_win, team2_win, team1_conferenceId, team2_conferenceId) %>% 
    mutate(region = factor(region, levels = c("South","East","Midwest","West","Final Four")),
           sort_seed = factor(team1_seed, levels=c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))) %>% 
    arrange(round, region, sort_seed)
 
  return(bracket) 
}


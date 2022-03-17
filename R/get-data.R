#' Scrape the game-by-game results of the NCAA BB season
#'
#' @param year a numeric value of the year, between 2002 and 2017 inclusive
#' @param league either 'mens' or 'womens'
#' @return data.frame with game-by-game results
#' @export
#' 
#' @importFrom dplyr '%>%' mutate transmute filter

scrape.game.results = function(year, league = c('mens', 'womens')) {
  # year = 2019; league = 'mens'
  league = match.arg(league)
  
  if (missing(year))
    stop('scrape.game.results: A year must be provided')
  if (!(class(year) %in% c('integer', 'numeric')))
    stop('scrape.game.results: The year must be numeric')
  if (year < 2002)
    stop('2002 is the earliest available season')
  if (year > 2018)
    warning('2018 is the latest season on which the scraper was tested')
  
  teams = scrape_teams(league)
  
  results = data.frame(game.id = character(0),
                       primary.id = character(0),
                       primary.score = character(0),
                       other.id = character(0),
                       other.score = character(0),
                       home = character(0),
                       location = character(0),
                       won = character(0),
                       ot = character(0),
                       year = character(0),
                       tournament = character(0))
  
  for (team.id in teams$id) {
    message(paste0("Pulling data for team ", team.id))
    results = rbind(results, scrape.team.game.results(year, team.id, league))
  }
  
  results = results %>%
    mutate(home = ifelse(location %in% c('H', 'A'),
                         ifelse(location == 'H', TRUE, FALSE),
                         ifelse(is.na(other.id) |
                                  primary.id < other.id,
                                TRUE, FALSE)))
  
  results = results %>%
    transmute(game.id = game.id,
              home.id = ifelse(home, primary.id, other.id),
              away.id = ifelse(home, other.id, primary.id),
              home.score = ifelse(home, primary.score, other.score),
              away.score = ifelse(home, other.score, primary.score),
              location = location,
              neutral = ifelse(location == 'N', 1, 0),
              won = won,
              ot = ot,
              year = year,
              tournament = tournament)
  
  results$home.id = ifelse(is.na(results$home.id), 'NA', results$home.id)
  results$away.id = ifelse(is.na(results$away.id), 'NA', results$away.id)
  
  results = results %>%
    filter(home.id %in% results$away.id & away.id %in% results$home.id)
  
  unique(results)
}
#' Scrape the team names and ids from the ESPN NCAA MBB index
#'
#' @param league either 'mens' or 'womens'
#' @return data.frame of team names and ids
#' @export
#' 
#' @importFrom dplyr '%>%' mutate transmute filter
#' @importFrom rvest html_text html_attr html_nodes
#' @importFrom xml2 read_html
scrape_teams = function(league) {
  
  url = paste0('https://www.espn.com/', league, '-college-basketball/teams')
  
  conferences = read_html(url) %>%
    html_nodes('div.mt7 > div.pb4') %>% html_text()
  
  iter_cells = read_html(url) %>%
    html_nodes('div.mt7 > div.mt4')
  
  output_df <- NULL
  
  for(i in 1:length(conferences)){
    cells = iter_cells[[i]] %>%
      html_nodes('.TeamLinks > div.pl3 > a')
    
    cname = cells %>%
      html_text(trim = TRUE)
    
    id = cells %>%
      html_attr('href') %>%
      strsplit('/') %>%
      sapply(identity) %>%
      `[`(6,)
    
    tmp <- data.frame(name = cname, id = id, conference=conferences[i], stringsAsFactors = FALSE)
    output_df <- rbind(output_df, tmp)
  }
  return(output_df)
}

#' Scrape game results for a single team-year combination
#' @param year a character value representing a year
#' @param team.id an ESPN team id
#' @param league either 'mens' or 'womens'
#' @return data.frame of game data for the team-year
#' 
#' @importFrom dplyr '%>%' mutate transmute filter
#' @importFrom rvest html_text html_attr html_nodes html_node
#' @importFrom xml2 read_html

scrape.team.game.results = function(year, team.id, league) {
  # year = 2021; team.id = 2670; league = 'mens'
  year = as.character(year)
  team.id = as.character(team.id)
  
  url = paste0('https://www.espn.com/', league, '-college-basketball/',
               'team/schedule/_/id/', team.id, '/season/', year)
  
  rows = tryCatch(
    read_html(url) %>%
      html_nodes('.Table__TBODY > tr'),
    error = function(e){
      message(paste0('-Trying one more time to get records for ', team.id, ' in year ', year,'.'))
      Sys.sleep(3)
      read_html(url) %>%
        html_nodes('.Table__TBODY > tr')
    })
  
  if(length(rows)==0) {
    message(paste0('-No records for ', team.id, ' in year ', year,'.'))
    return(NULL)
  }
  
  # remove tournament games
  tourney = rows %>%
    html_text(trim = TRUE) %>%
    toupper() %>% 
    startsWith(c("MEN'S BASKETBALL CHAMPIONSHIP",
                 "NCAA WOMEN'S CHAMPIONSHIP")) %>%
    which
  
  if (length(tourney) > 0) {
    rows_tourney = rows[1:(max(tourney) + 1)]
    rows = rows[(max(tourney) + 1):length(rows)]
    
    # Get tournament scores
    opponent.cells_tourney = rows_tourney %>%
      html_nodes('td:nth-child(2)')
    
    result.cells_tourney = rows_tourney %>%
      html_nodes('td:nth-child(3)')
    
    skip_tourney = result.cells_tourney %>%
      html_node('span.fw-bold') %>%
      html_text(trim = TRUE) %>%
      is.na %>%
      which 
    
    skip2_tourney = result.cells_tourney %>%
      html_node('span.ml4') %>%
      html_text(trim = TRUE) %>%
      strsplit(' ') %>%
      sapply(function(row) row[1]) %>%
      strsplit('-') %>%
      is.na %>%
      which 
    
    skip_tourney = unique(c(skip_tourney, skip2_tourney))
    
    if (length(skip_tourney) > 0) {
      opponent.cells_tourney = opponent.cells_tourney[-skip_tourney]
      result.cells_tourney = result.cells_tourney[-skip_tourney]
    }
    
    if(length(opponent.cells_tourney)>0){
      won_tourney = result.cells_tourney %>%
        html_node('span.fw-bold') %>%
        html_text(trim = TRUE) == 'W'
      score_tourney = result.cells_tourney %>%
        html_node('span.ml4') %>%
        html_text(trim = TRUE) %>%
        strsplit(' ') %>%
        sapply(function(row) row[1]) %>%
        strsplit('-') %>%
        sapply(identity) %>%
        t
      other_tourney = opponent.cells_tourney %>%
        html_node('span:nth-child(3) > a') %>%
        html_attr('href') %>%
        strsplit('/') %>%
        sapply(function(row) row[6])
      neutral_tourney = opponent.cells_tourney %>%
        html_node('span:nth-child(3)') %>%
        html_text(trim = TRUE) %>%
        endsWith('*')
      at.or.vs_tourney = opponent.cells_tourney %>%
        html_node('span.pr2') %>%
        html_text(trim = TRUE)
      location_tourney = ifelse(neutral_tourney, 'N', ifelse(at.or.vs_tourney == 'vs', 'H', 'A'))
      ot_tourney = result.cells_tourney %>%
        html_node('span.ml4') %>%
        html_text(trim = TRUE) %>%
        strsplit(' ') %>%
        sapply(function(row) row[2]) %>%
        ifelse(is.na(.), '', .)
      game.id_tourney = result.cells_tourney %>%
        html_node('span.ml4 a') %>%
        html_attr('href') %>%
        strsplit('/gameId/') %>%
        sapply(function(row) row[2])
      
      tourney_games <- data.frame(game.id = game.id_tourney,
                                  primary.id = team.id,
                                  primary.score = score_tourney[matrix(c(1:nrow(score_tourney), ifelse(won_tourney, 1, 2)),
                                                                       ncol = 2, byrow = FALSE)],
                                  other.id = other_tourney,
                                  other.score = score_tourney[matrix(c(1:nrow(score_tourney), ifelse(won_tourney, 2, 1)),
                                                                     ncol = 2, byrow = FALSE)],
                                  location = location_tourney,
                                  won = won_tourney,
                                  ot = ot_tourney,
                                  year = year,
                                  tournament = 'YES',
                                  stringsAsFactors = FALSE)
    } else {
      
      tourney_games <- data.frame(game.id = character(0),
                                  primary.id = character(0),
                                  primary.score = character(0),
                                  other.id = character(0),
                                  other.score = character(0),
                                  location = character(0),
                                  won = logical(0),
                                  ot = character(0),
                                  year = character(0),
                                  tournament = character(0),
                                  stringsAsFactors = FALSE)
    }
  }
  
  # Now get regular season scores
  opponent.cells = rows %>%
    html_nodes('td:nth-child(2)')
  
  result.cells = rows %>%
    html_nodes('td:nth-child(3)')
  
  # skip = result.cells %>%
  #   html_text(trim = TRUE) %in%
  #   c('Canceled', 'Postponed', 'Suspended') %>%
  #   which
  # skip = result.cells %>%
  #   html_node('a') %>%
  #   html_attr('href') %>%
  #   strsplit('/') %>%
  #   sapply(function(row) row[5] %in% c('preview', 'onair')) %>%
  #   which %>%
  #   c(skip)
  skip = result.cells %>%
    html_node('span.fw-bold') %>%
    html_text(trim = TRUE) %>%
    is.na %>%
    which 
  
  skip2 = result.cells %>%
    html_node('span.ml4') %>%
    html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[1]) %>%
    strsplit('-') %>%
    is.na %>%
    which 
  
  skip = unique(c(skip, skip2))
  
  if (length(skip) > 0) {
    opponent.cells = opponent.cells[-skip]
    result.cells = result.cells[-skip]
  }
  
  won = result.cells %>%
    html_node('span.fw-bold') %>%
    html_text(trim = TRUE) == 'W'
  score = result.cells %>%
    html_node('span.ml4') %>%
    html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[1]) %>%
    strsplit('-') %>%
    sapply(identity) %>%
    t
  other = opponent.cells %>%
    html_node('span:nth-child(3) > a') %>%
    html_attr('href') %>%
    strsplit('/') %>%
    sapply(function(row) row[6])
  neutral = opponent.cells %>%
    html_node('span:nth-child(3)') %>%
    html_text(trim = TRUE) %>%
    endsWith('*')
  at.or.vs = opponent.cells %>%
    html_node('span.pr2') %>%
    html_text(trim = TRUE)
  location = ifelse(neutral, 'N', ifelse(at.or.vs == 'vs', 'H', 'A'))
  ot = result.cells %>%
    html_node('span.ml4') %>%
    html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[2]) %>%
    ifelse(is.na(.), '', .)
  game.id = result.cells %>%
    html_node('span.ml4 a') %>%
    html_attr('href') %>%
    strsplit('/gameId/') %>%
    sapply(function(row) row[2])
  
  regular_games = data.frame(game.id = game.id,
                             primary.id = team.id,
                             primary.score = score[matrix(c(1:nrow(score), ifelse(won, 1, 2)),
                                                          ncol = 2, byrow = FALSE)],
                             other.id = other,
                             other.score = score[matrix(c(1:nrow(score), ifelse(won, 2, 1)),
                                                        ncol = 2, byrow = FALSE)],
                             location = location,
                             won = won,
                             ot = ot,
                             year = year,
                             tournament = "NO",
                             stringsAsFactors = FALSE)
  
  if (length(tourney) > 0) {
    return(rbind(tourney_games, regular_games))
  } else {
    return(regular_games)
  }
  
  
}



# Function to clean up scores
#' @export
#' 
#' @importFrom stringr str_remove str_match
fin_score <- function(x){
  x %>% 
    str_remove('<.*?>') %>% 
    str_remove('\\d\\/\\d{2}') %>% 
    str_remove('\\d+:\\d{2}') %>% 
    str_match("\\d+") %>% 
    as.numeric()
}

# Function to find winner
#' @export
#' 
#' @importFrom stringr str_locate
fin_win <- function(x){
  ifelse(!is.na(str_locate(x, '<b>')[,1]), 'W', as.character(NA))
}


# Function to clean up seeds
#' @export
#' 
#' @importFrom stringr str_extract
fin_seeds <- function(x){
  x %>% 
    str_extract('\\d+') %>% 
    as.numeric()
}

# Function to clean up teams
#' @export
#' 
#' @importFrom stringr str_match
fin_teams <- function(x){
  x %>% 
    str_match('\\".*?\\"') %>% 
    strsplit('/') %>%
    sapply(function(row) row[8])
}


#' @export
#' 
#' @importFrom dplyr '%>%' mutate transmute filter
#' @importFrom rvest html_text html_attr html_nodes
#' @importFrom xml2 read_html
#' @importFrom stringr str_split_fixed
#' @importFrom tidyr pivot_wider
get_tournament_scores <- function(league = 'mens', year = NULL, source='espn'){
  # league = 'mens'
  if (source=='espn'){
    if(is.null(year)){
      url = paste0('http://www.espn.com/', league, '-college-basketball/tournament/bracket')
    } else {
      url = paste0('http://www.espn.com/', league, '-college-basketball/tournament/bracket/_/id/', year, '22/', year, '-ncaa-tournament')
    }
    round1 = read_html(url) %>%
      html_nodes('.round1')
    
    teams = as.character(round1 %>% 
                           html_nodes('dt:nth-child(1)'))
    scores = as.character(round1 %>% 
                            html_nodes('.pointer')) 
    
    split_teams <- str_split_fixed(teams, '<br>', 2)
    split_scores <- str_split_fixed(scores, '<br>', 2)
    
    if(year==2021){
      # Fix for VCU/Oregon forfeit
      scores_final <- apply(split_scores, 2, fin_score)
      insert_score_pos = grep('2483',split_teams)
      insert_score = rep(floor(mean(scores_final)),2)
      insert_win = c('W',as.character(NA))
      
      fixed_scores <- rbind(scores_final[1:(insert_score_pos-1),],
                            insert_score,
                            scores_final[insert_score_pos:nrow(scores_final),]
      )
      
      win_final <- apply(split_scores, 2, fin_win)
      fixed_win <- rbind(win_final[1:(insert_score_pos-1),],
                         insert_win,
                         win_final[insert_score_pos:nrow(win_final),]
      )
      
      
      bracket_round1 <- data.frame(round = 1, region=rep(c("West","East","South","Midwest"), each=8),
                                   apply(split_teams, 2, fin_seeds),
                                   apply(split_teams, 2, fin_teams),
                                   # apply(split_scores, 2, fin_score),
                                   # apply(split_scores, 2, fin_win))
                                   fixed_scores,
                                   fixed_win)
    } else {

    bracket_round1 <- data.frame(round = 1, region=rep(c("West","East","South","Midwest"), each=8),
                                 apply(split_teams, 2, fin_seeds),
                                 apply(split_teams, 2, fin_teams),
                                 apply(split_scores, 2, fin_score),
                                 apply(split_scores, 2, fin_win))
    }

    
    ## ROUND 2
    
    round2 = read_html(url) %>%
      html_nodes('.round2')
    
    teams2 = as.character(round2 %>% 
                            html_nodes('dt:nth-child(1)'))
    scores2 = as.character(round2 %>% 
                             html_nodes('.pointer')) 
    
    split_teams2 <- str_split_fixed(teams2, '<br>', 2)
    split_scores2 <- str_split_fixed(scores2, '<br>', 2)
    
    
    check_scores <- apply(split_scores2, 2, fin_score)
    
    if(is.null(dim(check_scores)) || dim(check_scores)[1] < 16) check_scores <- matrix(NA, nrow = 16, ncol = 2)
    
    check_win <- apply(split_scores2, 2, fin_win)
    
    if(is.null(dim(check_win)) || dim(check_win)[1] < 16) check_win <- matrix(NA, nrow = 16, ncol = 2)
    
    bracket_round2 <- data.frame(round = 2, region=rep(c("West","East","South","Midwest"), each=4),
                                 apply(split_teams2, 2, fin_seeds),
                                 apply(split_teams2, 2, fin_teams),
                                 check_scores,
                                 check_win)
    
    
    ## ROUND 3
    
    round3 = read_html(url) %>%
      html_nodes('.round3')
    
    teams3 = as.character(round3 %>% 
                            html_nodes('dt:nth-child(1)'))
    scores3 = as.character(round3 %>% 
                             html_nodes('.pointer')) 
    
    split_teams3 <- str_split_fixed(teams3, '<br>', 2)
    split_scores3 <- str_split_fixed(scores3, '<br>', 2)
    
    check_scores <- apply(split_scores3, 2, fin_score)
    
    if(is.null(dim(check_scores)) || dim(check_scores)[1] < 8) check_scores <- matrix(NA, nrow = 8, ncol = 2)
    
    check_win <- apply(split_scores3, 2, fin_win)
    
    if(is.null(dim(check_win)) || dim(check_win)[1] < 8) check_win <- matrix(NA, nrow = 8, ncol = 2)
    
    bracket_round3 <- data.frame(round = 3, region=rep(c("West","East","South","Midwest"), each=2),
                                 apply(split_teams3, 2, fin_seeds),
                                 apply(split_teams3, 2, fin_teams),
                                 check_scores,
                                 check_win)
    
    
    ## ROUND 4
    
    round4 = read_html(url) %>%
      html_nodes('.round4')
    
    teams4 = as.character(round4 %>% 
                            html_nodes('dt:nth-child(1)'))
    scores4 = as.character(round4 %>% 
                             html_nodes('.pointer')) 
    
    split_teams4 <- str_split_fixed(teams4, '<br>', 2)
    split_scores4 <- str_split_fixed(scores4, '<br>', 2)
    
    check_scores <- apply(split_scores4, 2, fin_score)
    
    if(is.null(dim(check_scores)) || dim(check_scores)[1] < 4) check_scores <- matrix(NA, nrow = 4, ncol = 2)
    
    check_win <- apply(split_scores4, 2, fin_win)
    
    if(is.null(dim(check_win)) || dim(check_win)[1] < 4) check_win <- matrix(NA, nrow = 4, ncol = 2)
    
    bracket_round4 <- data.frame(round = 4, region=rep(c("West","East","South","Midwest"), each=1),
                                 apply(split_teams4, 2, fin_seeds),
                                 apply(split_teams4, 2, fin_teams),
                                 check_scores,
                                 check_win)
    
    
    ## ROUND 5
    
    round5 = read_html(url) %>%
      html_nodes('.round5')
    
    teams5 = as.character(round5 %>% 
                            html_nodes('dt:nth-child(1)'))
    scores5 = as.character(round5 %>% 
                             html_nodes('.pointer')) 
    
    split_teams5 <- str_split_fixed(teams5, '<br>', 2)
    split_scores5 <- str_split_fixed(scores5, '<br>', 2)
    
    check_scores <- apply(split_scores5, 2, fin_score)
    
    if(is.null(dim(check_scores)) || dim(check_scores)[1] < 2) check_scores <- matrix(NA, nrow = 2, ncol = 2)
    
    check_win <- apply(split_scores5, 2, fin_win)
    
    if(is.null(dim(check_win)) || dim(check_win)[1] < 2) check_win <- matrix(NA, nrow = 2, ncol = 2)
    
    bracket_round5 <- data.frame(round = 5, region="Final Four",
                                 apply(split_teams5, 2, fin_seeds),
                                 apply(split_teams5, 2, fin_teams),
                                 check_scores,
                                 check_win)
    
    
    
    ## ROUND 6
    
    round6 = read_html(url) %>%
      html_nodes('.round6')
    
    teams6 = as.character(round6 %>% 
                            html_nodes('dt:nth-child(1)'))
    scores6 = as.character(round6 %>% 
                             html_nodes('.pointer')) 
    
    split_teams6 <- str_split_fixed(teams6, '<br>', 2)
    split_scores6 <- str_split_fixed(scores6, '<br>', 2)
    
    check_scores <- apply(split_scores6, 2, fin_score) %>% t
    
    if(is.null(dim(check_scores)) || dim(check_scores)[1] < 1 || dim(check_scores)[2] < 1) check_scores <- matrix(NA, nrow = 1, ncol = 2)
    
    # They didn't bold the last score, switch to teams
    check_win <- apply(split_teams6, 2, fin_win) %>% t
    
    if(is.null(dim(check_win)) || dim(check_win)[1] < 1) check_win <- matrix(NA, nrow = 1, ncol = 2)
    
    bracket_round6 <- data.frame(round = 6, region="Final Four",
                                 matrix(apply(split_teams6, 2, fin_seeds), 1, 2),
                                 matrix(apply(split_teams6, 2, fin_teams), 1, 2),
                                 check_scores,
                                 check_win)
    
    
    
    ## Append all rounds
    bracket <- rbind(bracket_round1,
                     bracket_round2,
                     bracket_round3,
                     bracket_round4,
                     bracket_round5,
                     bracket_round6
    )
    names(bracket) <- c("round", "region", "team1_seed", "team2_seed", "team1_id", "team2_id",
                        "team1_score", "team2_score", "team1_win", "team2_win")
    
    return(bracket)
  } else {
    
    url = paste0('https://www.ncaa.com/brackets/basketball-men/d1/', year)
    
    round1 = read_html(url) %>%
      html_nodes('.round-1')
    
    teams = round1 %>% 
      html_nodes('div.team>span.name') %>% 
      html_text()
    scores = round1 %>% 
      html_nodes('div.team>span.score') %>% 
      html_text() %>% 
      as.numeric()
    seeds = round1 %>% 
      html_nodes('div.team>span.seed') %>% 
      html_text() %>% 
      as.numeric()
    
    bracket_round1 <- data.frame(round = 1,
                                 region=rep(c("East","South","West","Midwest"), each=16),
                                 game = rep(1:32, each=2),
                                 team_nbr = rep(1:2, 32),
                                 seed=seeds,
                                 team=teams,
                                 score=scores)
    
    bracket_round1 <- bracket_round1 %>% 
      pivot_wider(c(round, region, game), 
                  names_from=team_nbr, 
                  values_from = c(seed, team, score)) %>% 
      mutate(win_1 = ifelse(score_1 > score_2, 'W', as.character(NA)),
             win_2 = ifelse(score_1 < score_2, 'W', as.character(NA)))
    
    
    
    ## ROUND 2
    
    round2 = read_html(url) %>%
      html_nodes('.round-2')
    
    teams = round2 %>% 
      html_nodes('div.team>span.name') %>% 
      html_text()
    scores = round2 %>% 
      html_nodes('div.team>span.score') %>% 
      html_text() %>% 
      as.numeric()
    seeds = round2 %>% 
      html_nodes('div.team>span.seed') %>% 
      html_text() %>% 
      as.numeric()
    
    bracket_round2 <- data.frame(round = 2,
                                 region=rep(c("East","South","West","Midwest"), each=8),
                                 game = rep(1:16, each=2),
                                 team_nbr = rep(1:2, 16),
                                 seed=seeds,
                                 team=teams,
                                 score=scores)
    
    bracket_round2 <- bracket_round2 %>% 
      pivot_wider(c(round, region, game), 
                  names_from=team_nbr, 
                  values_from = c(seed, team, score)) %>% 
      mutate(win_1 = ifelse(score_1 > score_2, 'W', as.character(NA)),
             win_2 = ifelse(score_1 < score_2, 'W', as.character(NA)))
    
    ## ROUND 3
    
    round3 = read_html(url) %>%
      html_nodes('.round-3')
    
    teams = round3 %>% 
      html_nodes('div.team>span.name') %>% 
      html_text()
    scores = round3 %>% 
      html_nodes('div.team>span.score') %>% 
      html_text() %>% 
      as.numeric()
    seeds = round3 %>% 
      html_nodes('div.team>span.seed') %>% 
      html_text() %>% 
      as.numeric()
    
    bracket_round3 <- data.frame(round = 3,
                                 region=rep(c("East","South","West","Midwest"), each=4),
                                 game = rep(1:8, each=2),
                                 team_nbr = rep(1:2, 8),
                                 seed=seeds,
                                 team=teams,
                                 score=scores)
    
    bracket_round3 <- bracket_round3 %>% 
      pivot_wider(c(round, region, game), 
                  names_from=team_nbr, 
                  values_from = c(seed, team, score)) %>% 
      mutate(win_1 = ifelse(score_1 > score_2, 'W', as.character(NA)),
             win_2 = ifelse(score_1 < score_2, 'W', as.character(NA)))
    
    ## ROUND 4
    
    round4 = read_html(url) %>%
      html_nodes('.round-4')
    
    teams = round4 %>% 
      html_nodes('div.team>span.name') %>% 
      html_text()
    scores = round4 %>% 
      html_nodes('div.team>span.score') %>% 
      html_text() %>% 
      as.numeric()
    seeds = round4 %>% 
      html_nodes('div.team>span.seed') %>% 
      html_text() %>% 
      as.numeric()
    
    bracket_round4 <- data.frame(round = 4,
                                 region=rep(c("East","South","West","Midwest"), each=2),
                                 game = rep(1:4, each=2),
                                 team_nbr = rep(1:2, 4),
                                 seed=seeds,
                                 team=teams,
                                 score=scores)
    
    bracket_round4 <- bracket_round4 %>% 
      pivot_wider(c(round, region, game), 
                  names_from=team_nbr, 
                  values_from = c(seed, team, score)) %>% 
      mutate(win_1 = ifelse(score_1 > score_2, 'W', as.character(NA)),
             win_2 = ifelse(score_1 < score_2, 'W', as.character(NA)))
    
    ## ROUND 5 + 6
    
    round5 = read_html(url) %>%
      html_nodes('.center-final-games')
    
    teams = round5 %>% 
      html_nodes('div.team>span.name') %>% 
      html_text()
    scores = round5 %>% 
      html_nodes('div.team>span.score') %>% 
      html_text() %>% 
      as.numeric()
    seeds = round5 %>% 
      html_nodes('div.team>span.seed') %>% 
      html_text()  %>% 
      as.numeric()
    round = round5 %>% 
      html_nodes('div.game-pod') %>% 
      html_attr('id') %>% 
      substring(1,1) %>% 
      as.numeric()
    
    bracket_round5 <- data.frame(round = rep(round-1, each=2),
                                 region="Final Four",
                                 game = rep(1:3, each=2),
                                 team_nbr = rep(1:2, 3),
                                 seed=seeds,
                                 team=teams,
                                 score=scores)
    
    bracket_round5 <- bracket_round5 %>% 
      pivot_wider(c(round, region, game), 
                  names_from=team_nbr, 
                  values_from = c(seed, team, score)) %>% 
      mutate(win_1 = ifelse(score_1 > score_2, 'W', as.character(NA)),
             win_2 = ifelse(score_1 < score_2, 'W', as.character(NA)))
    
    
    
    ## Append all rounds
    bracket <- rbind(bracket_round1,
                     bracket_round2,
                     bracket_round3,
                     bracket_round4,
                     bracket_round5
    )
    bracket$game <- NULL
    names(bracket) <- c("round", "region", "team1_seed", "team2_seed", "team1_id", "team2_id",
                        "team1_score", "team2_score", "team1_win", "team2_win")
    
    return(bracket)
  }
}

# teams = scrape.teams('mens')
# teams
# 
# games = scrape.team.game.results(2019, 328, 'mens')
# 
# dat <- scrape.game.results(2019, "mens")
#
# get_tournament_scores('mens')
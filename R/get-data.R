#' Scrape the game-by-game results of the NCAA MBB seaon
#'
#' @param year a numeric value of the year, between 2002 and 2017 inclusive
#' @param league either 'mens' or 'womens'
#' @return data.frame with game-by-game results
#' @export
#' @author eshayer
scrape.game.results = function(year, league = c('mens', 'womens')) {
  # year = 2019; league = 'mens'
  league = match.arg(league)
  `%>%` = dplyr::`%>%`
  
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
                       ot = character(0))
  
  for (team.id in teams$id) {
    results = rbind(results, scrape.team.game.results(year, team.id, league))
  }
  
  results = results %>%
    dplyr::mutate(home = ifelse(location %in% c('H', 'A'),
                                ifelse(location == 'H', TRUE, FALSE),
                                ifelse(is.na(other.id) |
                                         primary.id < other.id,
                                       TRUE, FALSE)))
  
  results = results %>%
    dplyr::transmute(game.id = game.id,
                     home.id = ifelse(home, primary.id, other.id),
                     away.id = ifelse(home, other.id, primary.id),
                     home.score = ifelse(home, primary.score, other.score),
                     away.score = ifelse(home, other.score, primary.score),
                     neutral = ifelse(location == 'N', 1, 0),
                     ot = ot)
  
  results$home.id = ifelse(is.na(results$home.id), 'NA', results$home.id)
  results$away.id = ifelse(is.na(results$away.id), 'NA', results$away.id)
  
  results = results %>%
    dplyr::filter(home.id %in% results$away.id & away.id %in% results$home.id)
  
  unique(results)
}
#' Scrape the team names and ids from the ESPN NCAA MBB index
#'
#' @param league either 'mens' or 'womens'
#' @return data.frame of team names and ids
#' @author eshayer
#' @export
scrape_teams = function(league) {
  `%>%` = dplyr::`%>%`
  
  url = paste0('http://www.espn.com/', league, '-college-basketball/teams')
  
  cells = xml2::read_html(url) %>%
    rvest::html_nodes('.TeamLinks > div.pl3 > a')
  
  cname = cells %>%
    rvest::html_text(trim = TRUE)
  
  id = cells %>%
    rvest::html_attr('href') %>%
    strsplit('/') %>%
    sapply(identity) %>%
    `[`(6,)
  
  data.frame(name = cname, id = id, stringsAsFactors = FALSE)
}

#' Scrape game results for a single team-year combination
#' @param year a character value representing a year
#' @param team.id an ESPN team id
#' @param league either 'mens' or 'womens'
#' @return data.frame of game data for the team-year
#' @author eshayer
scrape.team.game.results = function(year, team.id, league) {
  # year = 2019; team.id = 328; league = 'mens'
  `%>%` = dplyr::`%>%`
  year = as.character(year)
  team.id = as.character(team.id)
  
  url = paste0('http://www.espn.com/', league, '-college-basketball/',
               'team/schedule/_/id/', team.id, '/year/', year)
  
  rows = xml2::read_html(url) %>%
    rvest::html_nodes('.Table2__tbody > tr')
  
  # remove tournament games
  tourney = rows %>%
    rvest::html_text(trim = TRUE) %>%
    startsWith(c("MEN'S BASKETBALL CHAMPIONSHIP",
                 "NCAA WOMEN'S CHAMPIONSHIP")) %>%
    which
  
  if (length(tourney) > 0) {
    rows = rows[(max(tourney) + 1):length(rows)]
  }
  
  opponent.cells = rows %>%
    rvest::html_nodes('td:nth-child(2)')
  
  result.cells = rows %>%
    rvest::html_nodes('td:nth-child(3)')
  
  # skip = result.cells %>%
  #   rvest::html_text(trim = TRUE) %in%
  #   c('Canceled', 'Postponed', 'Suspended') %>%
  #   which
  # skip = result.cells %>%
  #   rvest::html_node('a') %>%
  #   rvest::html_attr('href') %>%
  #   strsplit('/') %>%
  #   sapply(function(row) row[5] %in% c('preview', 'onair')) %>%
  #   which %>%
  #   c(skip)
  skip = result.cells %>%
    rvest::html_node('span.fw-bold') %>%
    rvest::html_text(trim = TRUE) %>%
    is.na %>%
    which 
  
  if (length(skip) > 0) {
    opponent.cells = opponent.cells[-skip]
    result.cells = result.cells[-skip]
  }
  
  won = result.cells %>%
    rvest::html_node('span.fw-bold') %>%
    rvest::html_text(trim = TRUE) == 'W'
  score = result.cells %>%
    rvest::html_node('span.ml4') %>%
    rvest::html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[1]) %>%
    strsplit('-') %>%
    sapply(identity) %>%
    t
  other = opponent.cells %>%
    rvest::html_node('span:nth-child(3) > a') %>%
    rvest::html_attr('href') %>%
    strsplit('/') %>%
    sapply(function(row) row[6])
  neutral = opponent.cells %>%
    rvest::html_node('span:nth-child(3)') %>%
    rvest::html_text(trim = TRUE) %>%
    endsWith('*')
  at.or.vs = opponent.cells %>%
    rvest::html_node('span.pr2') %>%
    rvest::html_text(trim = TRUE)
  location = ifelse(neutral, 'N', ifelse(at.or.vs == 'vs', 'H', 'A'))
  ot = result.cells %>%
    rvest::html_node('span.ml4') %>%
    rvest::html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[2]) %>%
    ifelse(is.na(.), '', .)
  game.id = result.cells %>%
    rvest::html_node('span.ml4 a') %>%
    rvest::html_attr('href') %>%
    strsplit('=') %>%
    sapply(function(row) row[2])
  
  data.frame(game.id = game.id,
             primary.id = team.id,
             primary.score = score[matrix(c(1:nrow(score), ifelse(won, 1, 2)),
                                          ncol = 2, byrow = FALSE)],
             other.id = other,
             other.score = score[matrix(c(1:nrow(score), ifelse(won, 2, 1)),
                                        ncol = 2, byrow = FALSE)],
             location = location,
             ot = ot,
             stringsAsFactors = FALSE)
}



# Function to clean up scores
#' @export
fin_score <- function(x){
  x %>% 
    stringr::str_remove('<.*?>') %>% 
    stringr::str_remove('\\d\\/\\d{2}') %>% 
    stringr::str_remove('\\d+:\\d{2}') %>% 
    stringr::str_match("\\d+") %>% 
    as.numeric()
}

# Function to find winner
#' @export
fin_win <- function(x){
  ifelse(!is.na(stringr::str_locate(x, '<b>')[,1]), 'W', as.character(NA))
}


# Function to clean up seeds
#' @export
fin_seeds <- function(x){
  x %>% 
    stringr::str_extract('\\d+') %>% 
    as.numeric()
}

# Function to clean up teams
#' @export
fin_teams <- function(x){
  x %>% 
    stringr::str_match('\\".*?\\"') %>% 
    strsplit('/') %>%
    sapply(function(row) row[8])
}


#' @export
get_tournament_scores <- function(league = 'mens'){
  `%>%` = dplyr::`%>%`
  # league = 'mens'
  
  url = paste0('http://www.espn.com/', league, '-college-basketball/tournament/bracket')
  
  round1 = xml2::read_html(url) %>%
    rvest::html_nodes('.round1')
  
  teams = as.character(round1 %>% 
                         rvest::html_nodes('dt:nth-child(1)'))
  scores = as.character(round1 %>% 
                          rvest::html_nodes('.pointer')) 
  
  split_teams <- stringr::str_split_fixed(teams, '<br>', 2)
  split_scores <- stringr::str_split_fixed(scores, '<br>', 2)
  
  
  bracket_round1 <- data.frame(round = 1, region=rep(c("East","West","South","Midwest"), each=8),
                               apply(split_teams, 2, fin_seeds),
                               apply(split_teams, 2, fin_teams),
                               apply(split_scores, 2, fin_score),
                               apply(split_scores, 2, fin_win))
  

  ## ROUND 2
  
  round2 = xml2::read_html(url) %>%
    rvest::html_nodes('.round2')
  
  teams2 = as.character(round2 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  scores2 = as.character(round2 %>% 
                           rvest::html_nodes('.pointer')) 
  
  split_teams2 <- stringr::str_split_fixed(teams2, '<br>', 2)
  split_scores2 <- stringr::str_split_fixed(scores2, '<br>', 2)
  
  
  check_scores <- apply(split_scores2, 2, fin_score)
  
  if(is.null(dim(check_scores)) || dim(check_scores)[1] < 16) check_scores <- matrix(NA, nrow = 16, ncol = 2)
  
  check_win <- apply(split_scores2, 2, fin_win)
  
  if(is.null(dim(check_win)) || dim(check_win)[1] < 16) check_win <- matrix(NA, nrow = 16, ncol = 2)
  
  bracket_round2 <- data.frame(round = 2, region=rep(c("East","West","South","Midwest"), each=4),
                               apply(split_teams2, 2, fin_seeds),
                               apply(split_teams2, 2, fin_teams),
                               check_scores,
                               check_win)
  
  
  ## ROUND 3
  
  round3 = xml2::read_html(url) %>%
    rvest::html_nodes('.round3')
  
  teams3 = as.character(round3 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  scores3 = as.character(round3 %>% 
                           rvest::html_nodes('.pointer')) 
  
  split_teams3 <- stringr::str_split_fixed(teams3, '<br>', 2)
  split_scores3 <- stringr::str_split_fixed(scores3, '<br>', 2)
  
  check_scores <- apply(split_scores3, 2, fin_score)
  
  if(is.null(dim(check_scores)) || dim(check_scores)[1] < 8) check_scores <- matrix(NA, nrow = 8, ncol = 2)
  
  check_win <- apply(split_scores3, 2, fin_win)
  
  if(is.null(dim(check_win)) || dim(check_win)[1] < 8) check_win <- matrix(NA, nrow = 8, ncol = 2)
  
  bracket_round3 <- data.frame(round = 3, region=rep(c("East","West","South","Midwest"), each=2),
                               apply(split_teams3, 2, fin_seeds),
                               apply(split_teams3, 2, fin_teams),
                               check_scores,
                               check_win)
  
  
  ## ROUND 4
  
  round4 = xml2::read_html(url) %>%
    rvest::html_nodes('.round4')
  
  teams4 = as.character(round4 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  scores4 = as.character(round4 %>% 
                           rvest::html_nodes('.pointer')) 
  
  split_teams4 <- stringr::str_split_fixed(teams4, '<br>', 2)
  split_scores4 <- stringr::str_split_fixed(scores4, '<br>', 2)
  
  check_scores <- apply(split_scores4, 2, fin_score)
  
  if(is.null(dim(check_scores)) || dim(check_scores)[1] < 4) check_scores <- matrix(NA, nrow = 4, ncol = 2)
  
  check_win <- apply(split_scores4, 2, fin_win)
  
  if(is.null(dim(check_win)) || dim(check_win)[1] < 4) check_win <- matrix(NA, nrow = 4, ncol = 2)
  
  bracket_round4 <- data.frame(round = 4, region=rep(c("East","West","South","Midwest"), each=1),
                               apply(split_teams4, 2, fin_seeds),
                               apply(split_teams4, 2, fin_teams),
                               check_scores,
                               check_win)
  
  
  ## ROUND 5
  
  round5 = xml2::read_html(url) %>%
    rvest::html_nodes('.round5')
  
  teams5 = as.character(round5 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  scores5 = as.character(round5 %>% 
                           rvest::html_nodes('.pointer')) 
  
  split_teams5 <- stringr::str_split_fixed(teams5, '<br>', 2)
  split_scores5 <- stringr::str_split_fixed(scores5, '<br>', 2)
  
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
  
  round6 = xml2::read_html(url) %>%
    rvest::html_nodes('.round6')
  
  teams6 = as.character(round6 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  scores6 = as.character(round6 %>% 
                           rvest::html_nodes('.pointer')) 
  
  split_teams6 <- stringr::str_split_fixed(teams6, '<br>', 2)
  split_scores6 <- stringr::str_split_fixed(scores6, '<br>', 2)
  
  check_scores <- apply(split_scores6, 2, fin_score)
  
  if(is.null(dim(check_scores)) || dim(check_scores)[1] < 1) check_scores <- matrix(NA, nrow = 1, ncol = 2)
  
  check_win <- apply(split_scores6, 2, fin_win)
  
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
}

# teams = scrape.teams('mens')
# teams
# 
# games = scrape.team.game.results(2019, 328, 'mens')
# 
# dat <- scrape.game.results(2019, "mens")
#
# get_tournament_scores('mens')
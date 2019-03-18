#' Scrape the game-by-game results of the NCAA MBB seaon
#'
#' @param year a numeric value of the year, between 2002 and 2017 inclusive
#' @param league either 'mens' or 'womens'
#' @return data.frame with game-by-game results
#' @export
#' @author eshayer
scrape.game.results = function(year, league = c('mens', 'womens')) {
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
  
  teams = scrape.teams(league)
  
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
scrape.teams = function(league) {
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
  
  data.frame(name = name, id = id, stringsAsFactors = FALSE)
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
    rows = rows[1:(min(tourney) - 1)]
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





#' @export
get_tournament_scores <- function(league = 'mens'){
  # league = 'mens'
  url = paste0('http://www.espn.com/', league, '-college-basketball/tournament/bracket')
  
  round1 = xml2::read_html(url) %>%
    rvest::html_nodes('.round1')
  
  teams = as.character(round1 %>% 
                         rvest::html_nodes('dt:nth-child(1)'))
  team1_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*?(/.*?href="(.*?)".*)*','\\1', teams)
  team1_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*?(/.*?href="(.*?)".*)*','\\2', teams) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team2_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*?(/.*?href="(.*?)".*)*','\\3', teams)
  team2_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*?(/.*?href="(.*?)".*)*','\\4', teams) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team3_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*?(/.*?href="(.*?)".*)*','\\3', teams)
  team3_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*?/.*?href="(.*?)".*','\\5', teams) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team3_seed <- ifelse(team3_id==team1_id, NA, team3_seed)
  team3_id <- ifelse(team3_id==team1_id, NA, team3_id)
  
  bracket_round1 <- data.frame(round = 1, region=rep(c("EAST","WEST","SOUTH","MIDWEST"), each=8),
                               team1_id, team1_seed, team2_id, team2_seed, team3_id, team3_seed,
                               stringsAsFactors = FALSE)
  
  ## Spaceholder for scores
  
  round2 = xml2::read_html(url) %>%
    rvest::html_nodes('.round2')
  
  teams2 = as.character(round2 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  team1_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\1', teams2)
  team1_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\2', teams2) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team2_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\3', teams2)
  team2_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\4', teams2) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  
  bracket_round2 <- data.frame(round = 2, region=rep(c("EAST","WEST","SOUTH","MIDWEST"), each=4),
                               team1_id, team1_seed, team2_id, team2_seed, team3_id=NA, team3_seed=NA,
                               stringsAsFactors = FALSE)
  
  
  round3 = xml2::read_html(url) %>%
    rvest::html_nodes('.round3')
  
  teams3 = as.character(round3 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  team1_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\1', teams3)
  team1_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\2', teams3) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team2_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\3', teams3)
  team2_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\4', teams3) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  
  bracket_round3 <- data.frame(round = 3, region=rep(c("EAST","WEST","SOUTH","MIDWEST"), each=2),
                               team1_id, team1_seed, team2_id, team2_seed, team3_id=NA, team3_seed=NA,
                               stringsAsFactors = FALSE)
  
  
  round4 = xml2::read_html(url) %>%
    rvest::html_nodes('.round4')
  
  teams4 = as.character(round4 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  team1_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\1', teams4)
  team1_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\2', teams4) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team2_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\3', teams4)
  team2_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\4', teams4) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  
  bracket_round4 <- data.frame(round = 4, region=rep(c("EAST","WEST","SOUTH","MIDWEST"), each=1),
                               team1_id, team1_seed, team2_id, team2_seed, team3_id=NA, team3_seed=NA,
                               stringsAsFactors = FALSE)
  
  
  round5 = xml2::read_html(url) %>%
    rvest::html_nodes('.round5')
  
  teams5 = as.character(round5 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  team1_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\1', teams5)
  team1_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\2', teams5) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team2_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\3', teams5)
  team2_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\4', teams5) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  
  bracket_round5 <- data.frame(round = 5, region='FINAL FOUR', team1_id, team1_seed, team2_id, team2_seed, team3_id=NA, team3_seed=NA,
                               stringsAsFactors = FALSE)
  
  
  round6 = xml2::read_html(url) %>%
    rvest::html_nodes('.round6')
  
  teams6 = as.character(round6 %>% 
                          rvest::html_nodes('dt:nth-child(1)'))
  team1_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\1', teams6)
  team1_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\2', teams6) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  team2_seed = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\3', teams6)
  team2_id = gsub('<dt>(\\d+?).*?href="(.*?)".*?<br>(\\d+).*?href="(.*?)".*','\\4', teams6) %>% strsplit('/') %>%
    sapply(function(row) row[8])
  
  bracket_round6 <- data.frame(round = 6, region='FINAL FOUR', team1_id, team1_seed, team2_id, team2_seed, team3_id=NA, team3_seed=NA,
                               stringsAsFactors = FALSE)
  
  bracket <- rbind(bracket_round1,
                   bracket_round2,
                   bracket_round3,
                   bracket_round4,
                   bracket_round5,
                   bracket_round6
  )
  
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
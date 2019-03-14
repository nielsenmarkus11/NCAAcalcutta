team_skeleton <- function(){
  # Create skeleton
  preteams <- data.frame(rank=rep(1:16,4),
                         region=rep(c("South","East","West","Midwest"),each=16),
                         team=NA)
  
  write.csv(preteams, "preteams.csv",row.names = FALSE, na = "")
  return(preteams)
}



# Upload Teams
input_teams <- function(file_path=system.file("extdata", "ncaa-teams.csv", package = "NCAAcalcutta", mustWork = TRUE)){
  df <- read.csv(file_path, header = TRUE)
  return(df)
}


# Randomize Teams
randomize_teams <- function(df,random_seed = 156){
  set.seed(random_seed)
  teams <- df
  rand1 <- runif(64)
  
  # Randomize which group each seed rank is in...
  teams <- teams[order(teams$rank,rand1),]
  teams$group <- rep(1:4,16)
  
  # Randomize which team is picked first for each group
  rand2 <- runif(64)
  teams <- teams[order(teams$group,rand2),]
  teams$owner <- NA
  teams$bid <- NA
  
  # Output for shiny app
  write.csv(teams, "ncaa-random-teams.csv",row.names = FALSE, na = "")
  return(teams)
}
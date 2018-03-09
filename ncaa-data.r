# Create skeleton
preteams <- data.frame(rank=rep(1:16,4),
                       region=rep(c("South","East","West","Midwest"),each=16),
                       team=NA)

write.csv(preteams, "data/preteams.csv",row.names = FALSE, na = "")

# Upload Teams
set.seed(156)
teams <- read.csv("data/ncaa-teams.csv")
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
write.csv(teams, "data/ncaa-random-teams.csv",row.names = FALSE, na = "")

# NCAAcalcutta

### Download instructions
Open R and type the following commands in your console:
```
install.packages('devtools')
devtools::install_github('nielsenmarkus11/NCAAcalcutta')
```
You'll need to get the latest data, but here is how you can run the Calcutta auction app:
```
library(NCAAcalcutta)
# Input the 2019 teams and run the auction app
teams <- import_teams(system.file("extdata", "ncaa-teams-2019.csv", package = "NCAAcalcutta"))
start_auction(teams)
```

Some functions were adapted from the [elishayer/mRchmadness](https://github.com/elishayer/mRchmadness) repo.
# NCAAcalcutta

### Download instructions
Open R and type the following commands in your console:
```
install.packages('devtools')
devtools::install_github('nielsenmarkus11/NCAAcalcutta')
```
You'll need to get the latest data, but here is how you can run the Calcutta auction app:
```
# Input the 2018 teams
teams <- import_teams(system.file("extdata", "ncaa-teams.csv", package = "NCAAcalcutta"))
```

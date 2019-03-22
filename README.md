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

### Here’s how it works:
Get everyone together over lunch for 1 1⁄2 to 2 hours on before the Round of 64 to bid on the teams that they want to represent them for the tournament. Each person will start with 500-800 or so points (for example) to “purchase” teams at the auction. Everyone will earn 1 point back for each point their team scores in the NCAA tournament, and the owner of the NCAA tournament championship team will earn 100 bonus points. The person with the most points after the final will be the winner and gets all the glory. And possibly lunch… :)

> ## Rules:
> #### Bidding:
> 1. Sum up last years total points (excluding the First Four games), multiply by 0.8, then divide by the number of players to get the starting points for each player. Round up to the nearest 25 points. For example, last year about 8,800 points were scored. If you have 9 players you take: (8800 x 0.8) / 9 = 782.2, so, everyone gets 800 points to spend.
> 2. When the timer runs out, the bidding is over. The auctioneer may choose to drop the hammer on a price before the timer runs out.
> 3. Only allow bids in 5 or 10 point increments.
> 4. In the app, each team has a minimum bid, this rule can be loosened by the auctioneer at the end if nobody has points left to spend.
> 
> #### Scoring:
> 5. If a player bids more than they have, 10 points per overspent point will be deducted from their final score.
> 6. Any remaining points after the auction are added to each respective player's final score.
> 7. The total number of points each player's teams earns are added back into each player's final score.
> 8. The player with the championship team gets a 100 point bonus added to their final score.

Feel free to create some reports to track how players are doing throughout the tournament.  Our office uses an Excel spreadsheet to track the tournament scores and competition results.  If I have some time I'll put together some of the basic reports we typically use and include them in this package in the near future.

### References
1. Some functions were adapted from the [elishayer/mRchmadness](https://github.com/elishayer/mRchmadness) repo.

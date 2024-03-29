% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-data.R
\name{scrape.game.results}
\alias{scrape.game.results}
\title{Scrape the game-by-game results of the NCAA BB season}
\usage{
scrape.game.results(year, league = c("mens", "womens"))
}
\arguments{
\item{year}{a numeric value of the year, between 2002 and 2017 inclusive}

\item{league}{either 'mens' or 'womens'}
}
\value{
data.frame with game-by-game results
}
\description{
Scrape the game-by-game results of the NCAA BB season
}

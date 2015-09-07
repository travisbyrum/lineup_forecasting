Lineup Forecasting
===
This project is an attempt to determine optimal lineup combinations in the NBA using predictive modeling. 

#The Problem
NBA lineups are usually easy to understand from a production standpoint if there is on-court data available for a specific player combination.  It becomes much harder to predict future performance when there is no historical precedent for the lineup in question.  This is a problem given the how common new player combinations are created through the draft and player transactions.


#The Solution
This tool creates model which takes in a lineup and returns an associated performance prediction in the form of a winning percentage.  This can be used to forecast new and unique player combinations and can even be extended to team performance over a season.  The model is fit using player data scraped from [Basketball Reference](http://www.basketball-reference.com).


In this repository is the following:
 - `data`, directory of the scraped player data
 - `reports`, directory which includes a sweave report detailing the model and its implications 
 - `boosting.R`, file fitting the final model
 - `bref_scraping.R`, file for scraping the necessary data 

The rest of the repository includes files dealing with various data transformations.

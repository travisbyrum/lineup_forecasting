#! /usr/bin/Rscript

library(dplyr)

#http://stats.nba.com/league/lineups/#!/opponent/
base_url <- 'http://stats.nba.com/league/lineups/#!/'
extra_url <- c('advanced', 'opponent', 'fourfactors', 'misc', 'scoring')

urls <- c(base_url, paste0(base_url, extra_url, '/'))

temp_in <- RCurl::getURIAsynchronous(urls)

library(rvest)

read_html( 'http://stats.nba.com/league/lineups/#!/') %>%
  html_table()

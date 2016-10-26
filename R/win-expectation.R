#!/usr/bin/Rscript

# Rscript ~/lineuper/R/lineuper.R ~/lineuper/data
args <- commandArgs(TRUE)

if (!length(args))
  stop('Directory path not provided')

library(dplyr)
DATA_DIR <- as.character(args[1])
LINEUP_PATH <- file.path(DATA_DIR, 'lineups.csv')
FINAL_PATH <- file.path(DATA_DIR, 'win_expectation.csv') 

if (!file.exists(LINEUP_PATH))
  stop(LINEUP_PATH, ' file does not exist')

readr::read_csv(LINEUP_PATH) %>% 
  mutate(
    net_pts = PTS - PTS_opp,
    win_expectation = (PTS ** 13.91) / ((PTS ** 13.91)+(PTS_opp ** 13.91))
  ) %>% 
  select(win_expectation) %>% 
  readr::write_csv(FINAL_PATH)
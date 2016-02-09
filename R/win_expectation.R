library(dplyr)
library(readr)

lineups <- read_csv("~/lineup_forecasting/data/lineups.csv") %>% 
  select(-X) %>%
  mutate(
    net_pts = PTS - PTS_opp
  )

# The win expectation is calculated according to the pythagorean win expectation formula.  The coefficient is empirically verified
lineups <- lineups %>% 
  mutate(win_expectation = (PTS ** 13.91)/((PTS ** 13.91)+(PTS_opp ** 13.91)))

write_csv(
  bind_cols(win_expectation = lineups$win_expectation),
  path = "/Users/travisbyrum/lineup_forecasting/data/win_expectation2015.csv"
)


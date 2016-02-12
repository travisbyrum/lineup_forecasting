lineups <- read_csv("~/lineuper/data/lineups.csv") %>% 
  mutate(
    net_pts = PTS - PTS_opp,
    win_expectation = (PTS ** 13.91)/((PTS ** 13.91)+(PTS_opp ** 13.91))
  ) %>% 
  select(win_expectation) %>% 
  write_csv("~/lineuper/data/win_expectation.csv")
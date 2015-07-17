library(dplyr)

lineups <- read.csv("~/lineup_forecasting/data/lineups.csv", stringsAsFactors=FALSE) %>% select(-X) %>% mutate(net_pts = PTS - PTS_opp)

# The win expectation is calculated according to the pythagorean win expectation formula.  The coefficient is empirically verified
lineups <- lineups %>% mutate(win_expectation = (PTS ** 13.91)/((PTS ** 13.91)+(PTS_opp ** 13.91)))

write.csv(as.data.frame(cbind(win_expectation = lineups$win_expectation)), file = "/Users/travisbyrum/lineup_forecasting/data/win_expectation2015.csv", row.names = F)


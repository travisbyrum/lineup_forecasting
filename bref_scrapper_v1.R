library(XML)
library(dplyr)

#########################################
# Basketball Reference URL: http://www.basketball-reference.com/play-index/plus/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=total&year_id=2015&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=opp_pts&c1comp=ge&c1val=&c2stat=pts&c2comp=ge&c2val=&c3stat=&c3comp=ge&c3val=&c4stat=&c4comp=ge&c4val=&order_by=mp&order_by_asc=&offset=100
offset <- 0
full_table <- NULL

repeat{
  url <- paste0("http://www.basketball-reference.com/play-index/plus/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=total&year_id=2015&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=opp_pts&c1comp=ge&c1val=&c2stat=pts&c2comp=ge&c2val=&c3stat=&c3comp=ge&c3val=&c4stat=&c4comp=ge&c4val=&order_by=mp&order_by_asc=&offset=", offset)
  row <- readHTMLTable(url, stringsAsFactors = F)[[2]] # qe want the second table from the HTML
  colnames(row) <- gsub("%", "per", colnames(row))
  colnames(row) <- make.names(colnames(row), unique = T) # removing duplicates from column names
  colnames(row)[which(colnames(row) %in% c("Tm.1", "Opp"))] <- c("Tm_poss", "Opp_poss")
  colnames(row) <- gsub(".1", "_opp", colnames(row)) # renaming opponent stats
  row <- row %>% filter(!(Lineup %in% c("Poss", "Lineup"))) # removing duplicate column names
  print(paste("Scraping Offset:", offset)) # print progress
  offset <- offset + 100 # going to the next lineup page  
  full_table <- rbind(full_table, row) # combining results
  
  if(sum(as.numeric(row$MP) < 5) > 0){ # quit scraping once lineups combine for less than 5 min
    break
  }
}

write.csv(full_table, "lineups.csv", row.names = F)










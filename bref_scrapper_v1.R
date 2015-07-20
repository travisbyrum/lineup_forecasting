library(XML)
library(dplyr)

#########################################
# Basketball Reference URL: http://www.basketball-reference.com/play-index/plus/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=total&year_id=2015&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=opp_pts&c1comp=ge&c1val=&c2stat=pts&c2comp=ge&c2val=&c3stat=&c3comp=ge&c3val=&c4stat=&c4comp=ge&c4val=&order_by=mp&order_by_asc=&offset=100
offset <- 0
full_table <- NULL

repeat{
  url <- paste0("http://www.basketball-reference.com/play-index/plus/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=total&year_id=2015&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=opp_pts&c1comp=ge&c1val=&c2stat=pts&c2comp=ge&c2val=&c3stat=&c3comp=ge&c3val=&c4stat=&c4comp=ge&c4val=&order_by=mp&order_by_asc=&offset=", offset)
  row <- readHTMLTable(url, stringsAsFactors = F)[[2]] # we want the second table from the HTML
  colnames(row) <- gsub("%", "per", colnames(row))
  colnames(row) <- make.names(colnames(row), unique = T) # removing duplicates from column names
  colnames(row)[which(colnames(row) %in% c("Tm.1", "Opp"))] <- c("Tm_poss", "Opp_poss")
  colnames(row) <- gsub(".1", "_opp", colnames(row)) # renaming opponent stats
  row <- row %>% filter(!(Lineup %in% c("Poss", "Lineup"))) # removing duplicate column names
  print(paste("Scraping Offset:", offset)) # print progress
  offset <- offset + 100 # going to the next lineup page  
  full_table <- rbind(full_table, row) # combining results
  
  if(sum(as.numeric(row$MP) < 5) > 0){ # finish scraping once lineups combine for less than 5 min
    break
  }
}

write.csv(full_table, "/Users/travisbyrum/lineup_forecasting/data/lineups.csv", row.names = F)

###############################################################################################
# Basketball Reference URL for player stats: http://www.basketball-reference.com/leagues/NBA_2015_per_game.html?lid=header_seasons

read_function <- function(index){
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_2015_", index, ".html?lid=header_seasons")
  row <- readHTMLTable(url, stringsAsFactors = F)[[1]] # we want the first table from the stats page
  colnames(row) <- make.names(colnames(row), unique = T) # removing duplicates from column names
  colnames(row) <- gsub("[.]", "per", colnames(row))
  colnames(row) <- gsub("1", "", colnames(row))
  row <- row %>% filter(Player != "Player") # removing duplicated headers
  if (index  == "per_minute") {index <- "per_36"}
  colnames(row) <- paste0(colnames(row), "_", index)
  return(row)
}

url_index <- c("totals", "per_minute", "advanced")
player_table <- do.call(cbind, sapply(url_index, function(x) read_function(x)))
colnames(player_table) <- gsub("^[^.]*.","", colnames(player_table)) #normalizing column names

rep_categories <- c("Player", "Pos", "Age", "Tm", "GS", "MP", "X2Pper", "FTper", "X3Pper")
comp <- colnames(player_table)[do.call(c, sapply(1:length(rep_categories), function(x) grep(rep_categories[x], colnames(player_table))[-1]))]
player_table <- player_table[,!(colnames(player_table) %in% comp)]
  
player_table <- player_table %>% select(-Rk_totals, -Rk_per_36, -Rk_advanced, -X_advanced, -Xper_advanced) %>% rename(Pos = Pos_totals, Player = Player_totals, Age = Age_totals,
                                 Tm = Tm_totals, G = G_totals, GS = GS_totals, MP = MP_totals, X2Pper = X2Pper_totals, X3Pper = X3Pper_totals, 
                                 FTper = FTper_totals) 

for(i in colnames(player_table)[!(colnames(player_table) %in% c("Player", "Tm", "Pos"))]){  # Forcing data to numeric type when appropriate
  player_table[,i] <- as.numeric(player_table[,i])
}

player_table <- player_table %>% mutate(possessions = .96*(FGA_totals-ORB_totals+TOV_totals+(.44*FTA_totals)))

############################################################################################### RAPM data from espn.com

rapm_reader <- function(page){ # function to read data off espn
  url_rapm <- paste0("http://espn.go.com/nba/statistics/rpm/_/page/", page, "/sort/RPM")
  row <- readHTMLTable(url_rapm, stringsAsFactors = F)[[1]] %>% select(-RK, -TEAM, -GP, -MPG) %>% rename(Player = NAME) # we want the second table from the HTML
  row$Player <- sub(",.*$","", row$Player) 
  return(row)
}

df_rapm <- do.call(rbind, lapply(1:12, function(x) rapm_reader(x)))

for(i in colnames(df_rapm)[-1]){ # forcing type to numeric
  df_rapm[,i] <- as.numeric(df_rapm[,i])
}

player_table <- left_join(player_table, df_rapm, by = NULL)

write.csv(player_table, "/Users/travisbyrum/lineup_forecasting/data/player_stats.csv", row.names = F)











library(dplyr)
library(stringr)
library(stringi)

##################################################################################

lineups <- read.csv("~/lineup_forecasting/data/lineups.csv", stringsAsFactors = F)
player_stats <- read.csv("~/lineup_forecasting/data/player_stats.csv", stringsAsFactors = F)

# standardizing positions 
player_stats$Pos[player_stats$Pos == "SG-PG"] <- "SG"
player_stats$Pos[player_stats$Pos == "PG-SG"] <- "PG"
player_stats$Pos[player_stats$Pos == "SF-PF"] <- "SF"
player_stats$Pos[player_stats$Pos == "SF-SG"] <- "SF"
player_stats$Pos[player_stats$Pos == "SG-SF"] <- "SG"
player_stats$Pos[player_stats$Pos == "PF-SF"] <- "PF"

pos_num <- function(){ # work around to apply switch statement to vector
  row <- sapply(1:length(player_stats$Pos), function(x) switch(player_stats$Pos[x], 
    "PG" = 1, "SG" = 2, "SF" = 3, "PF" = 4, "C" = 5))
  return(row)
}

##################################################################################

player_stats <- player_stats %>% mutate(first_abbrev = paste0(substring(Player, 1, 1)),
        last = str_split_fixed(player_stats$Player, " ", n = 2)[, 2]) %>% mutate(combined_name = paste0(first_abbrev, last),
                                                                                 pos_num = pos_num()) # creating new column with abbreviated names for easier comparisons

row_builder <- function(lineup_row) {
  team_stats <- player_stats %>% filter(Tm == lineup_row$Tm) #subset player stats of lineup by team to allow easier lookup 
  split <- strsplit(stri_replace_all_charclass(lineup_row$Lineup, "\\p{WHITE_SPACE}", ""), split="|", fixed = T)[[1]] # splitting lineup string into character vector
  index_pos <- gsub("[^[:alnum:]]", "", team_stats$combined_name) %in% gsub("[^[:alnum:]]", "", split)
  df <- team_stats %>% filter(index_pos) %>% arrange(pos_num) # put the player stats in order by position  
  if (sum(index_pos) > 5){ # error if there is a duplicated abbreviated name
    duplicate_rows <- df %>% filter(combined_name == df$combined_name[duplicated(df$combined_name)]) # finding duplicate name
    replacement_row <- duplicate_rows[which.min(abs(duplicate_rows$MP - mean(df$MP))),] # replacing player with closest playing time to the lineup average
    df <- df[-which(df$Player %in% duplicate_rows$Player),]
    df <- rbind(replacement_row, df)
    return(do.call(cbind, lapply(1:nrow(df), function(x) df[x,] %>% select(-first_abbrev, -last, -combined_name, -pos_num)))) # combining player stats in one row 
  } else if (sum(index_pos) < 5) { # if player stats are absent return NULL
    return(NULL)
  } else {
    return(do.call(cbind, lapply(1:nrow(df), function(x) df[x,] %>% select(-first_abbrev, -last, -combined_name, -pos_num)))) # combining player stats in one row
  }
}
  
final_df <- do.call(rbind, lapply(1:nrow(lineups), function(x) row_builder(lineups[x,]))) # creating the final dataset

write.csv(final_df, "/Users/travisbyrum/lineup_forecasting/data/final_data.csv", row.names = F)

# removing rows from win_expectation where the lineup has incomplete player data
df_vec <- rep(FALSE, nrow(lineups))
for(i in 1:nrow(lineups)){
  if (is.null(row_builder(lineups[i,])) == TRUE){
    df_vec[i] <- TRUE
  }
}

winexpectation <- read.csv("~/lineup_forecasting/data/win_expectation2015.csv", stringsAsFactors = F)# reading in win_expectation
winexpectation <- winexpectation[!df_vec,]
write.csv(winexpectation, file = "/Users/travisbyrum/lineup_forecasting/data/win_expectation2015.csv", row.names = F)






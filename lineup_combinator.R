library(dplyr)
library(stringr)
library(stringi)
library(readr)

##################################################################################

lineups <- read_csv("~/lineup_forecasting/data/lineups.csv")
player_stats <- read_csv("~/lineup_forecasting/data/player_stats.csv") %>% 
  mutate(
    Pos = switch(
      Pos,
      "SG-PG" = "PG",
      "PG-SG" = "SG", 
      "SF-PF" = "SF", 
      "SF-SG" = "SF",
      "SG-SF" = "SG",
      "PF-SF" = "PF",
      Pos
    ),
    pos_num = switch(
      Pos, 
      "PG" = 1, 
      "SG" = 2, 
      "SF" = 3, 
      "PF" = 4, 
      "C" = 5
    ),
    first_abbrev = paste0(substring(Player, 1, 1)),
    last = str_split_fixed(player_stats$Player, " ", n = 2)[, 2],
    combined_name = paste0(first_abbrev, last)
  )

row_builder <- function(lineup_row) {
  team_stats <- player_stats %>% 
    filter(Tm == lineup_row$Tm) #subset player stats of lineup by team to allow easier lookup 
  split <- strsplit(stri_replace_all_charclass(lineup_row$Lineup, "\\p{WHITE_SPACE}", ""), split="|", fixed = T)[[1]] # splitting lineup string into character vector
  index_pos <- gsub("[^[:alnum:]]", "", team_stats$combined_name) %in% gsub("[^[:alnum:]]", "", split)
  df <- team_stats %>% 
    filter(index_pos) %>% arrange(pos_num) # put the player stats in order by position  
  if (sum(index_pos) > 5){ # error if there is a duplicated abbreviated name
    duplicate_rows <- df %>% 
      filter(combined_name == df$combined_name[duplicated(df$combined_name)]) # finding duplicate name
    replacement_row <- duplicate_rows[which.min(abs(duplicate_rows$MP - mean(df$MP))),] # replacing player with closest playing time to the lineup average
    df <- df[-which(df$Player %in% duplicate_rows$Player),] %>% 
      bind_rows(replacement_row, .)
    
    return(
      bind_cols(
        lapply(
          seq_along(1:NROW(df)),
          function(x) {
            df %>% 
              slice(x) %>% 
              select(
                -first_abbrev,
                -last, 
                -combined_name, 
                -pos_num
              )
          }
        )
      )
    )
  } else if (sum(index_pos) < 5) { # if player stats are absent return NULL
    return(NULL)
  } else {
    return(
      bind_cols(
        lapply(
          seq_along(1:NROW(df)),
          function(x){
            df %>% 
              slice(x) %>% 
              select(
                -first_abbrev,
                -last, 
                -combined_name, 
                -pos_num
              )
            
          }
        )
      )
    )
  }
}
  
final_df <- bind_rows(
  lapply(1:NROW(lineups), function(x) row_builder(lineups[x,]))
) # creating the final dataset

write_csv(final_df, "/Users/travisbyrum/lineup_forecasting/data/final_data.csv", row.names = F)

# removing rows from win_expectation where the lineup has incomplete player data
df_vec <- rep(FALSE, NROW(lineups))
for (i in 1:nrow(lineups)){
  if (is.null(row_builder(lineups[i,])) == TRUE){
    df_vec[i] <- TRUE
  }
}

winexpectation <- read_csv("~/lineup_forecasting/data/win_expectation2015.csv") %>% 
  .[!df_vec, ]# reading in win_expectation

write_csv(winexpectation, file = "/Users/travisbyrum/lineup_forecasting/data/win_expectation2015.csv")






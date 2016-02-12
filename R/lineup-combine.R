player_stats <- read_csv("~/lineuper/data/player_stats.csv")
lineups <- read_csv("~/lineuper/data/lineups.csv") %>% 
  mutate(win_expectation = read_csv("~/lineuper/data/win_expectation.csv")$win_expectation)

final_data <- lineups %>% 
  slice(1:1000) %>% 
  purrr::by_row(
    function(lineup_row) {
      team_stats <- player_stats %>% 
        filter(Tm == lineup_row$Tm)
      
      index_pos <- str_split(lineup_row$Lineup, pattern = '\\s+\\|\\s+')[[1]] %>% 
        gsub("[^[:alnum:]]", "", .) %>% 
        {gsub("[^[:alnum:]]", "", team_stats$combined_name) %in% .}
      
      df <- team_stats %>% 
        filter(index_pos) %>% 
        arrange(pos_num) # put the player stats in order by position  
      
      if (sum(index_pos) > 5) {
        duplicate_rows <- df %>% 
          filter(combined_name == df$combined_name[duplicated(df$combined_name)]) # finding duplicate name
        replacement_row <- duplicate_rows[which.min(abs(duplicate_rows$MP - mean(df$MP))),]
        
        df <- df[-which(df$Player %in% duplicate_rows$Player),] %>% 
          bind_rows(replacement_row, .)
      }
      
      if (sum(index_pos) < 5) {
        return(NULL)
      } else {
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
      }
    },
    .collate = "rows"
  )

colnames(final_data) <- make.names(colnames(final_data), unique = TRUE)

write_csv(final_data, "~/lineuper/data/final_data.csv")





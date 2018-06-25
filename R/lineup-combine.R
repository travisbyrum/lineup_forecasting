#!/usr/bin/Rscript

# Rscript ~/lineuper/R/lineup-combine.R ~/lineuper/data
args <- commandArgs(TRUE)

if (!length(args))
  stop('lineup.csv path not provided')

library(dplyr)

DATA_DIR <- as.character(args[1])
FINAL_PATH <- file.path(DATA_DIR, "final_data.csv")

player_stats <- readr::read_csv(file.path(DATA_DIR, "player_stats.csv"))
win_expectation <- readr::read_csv(file.path(DATA_DIR, "win_expectation.csv")) %>%
  .$win_expectation

lineups <- readr::read_csv(file.path(DATA_DIR, "lineups.csv")) %>%
  mutate(
    win_expectation = win_expectation
  )

final_data <- lineups %>%
  purrr::by_row(
    function(lineup_row) {
      team_stats <- player_stats %>%
        filter(Tm == lineup_row$Tm)

      index_pos <- stringr::str_split(lineup_row$Lineup, pattern = '\\s+\\|\\s+')[[1]] %>%
        gsub("[^[:alnum:]]", "", .) %>%
        {gsub("[^[:alnum:]]", "", team_stats$combined_name) %in% .}

      df <- team_stats %>%
        filter(index_pos) %>%
        arrange(pos_num) # put the player stats in order by position

      if (sum(index_pos) > 5) {
        duplicate_rows <- df %>%
          filter(combined_name == df$combined_name[duplicated(df$combined_name)])
        replacement_row <- duplicate_rows[which.min(abs(duplicate_rows$MP - mean(df$MP))),]

        df <- df[-which(df$Player %in% duplicate_rows$Player),] %>%
          bind_rows(replacement_row, .)
      }

      if (sum(index_pos) < 5) {
        return(NULL)
      } else {
        return(
          lapply(
            seq_len(NROW(df)),
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
          ) %>%
            bind_cols()
        )
      }
    },
    .collate = "rows"
  )

colnames(final_data) <-colnames(final_data) %>%
  make.names(unique = TRUE)

readr::write_csv(final_data, FINAL_PATH)

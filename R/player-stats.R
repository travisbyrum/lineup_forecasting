player_stats <- read_csv("~/lineuper/data/player_stats.csv") %>% 
  mutate(
    Pos = vapply(
      Pos,
      function(x) switch(
        x,
        "SG-PG" = "PG",
        "PG-SG" = "SG", 
        "SF-PF" = "SF", 
        "SF-SG" = "SF",
        "SG-SF" = "SG",
        "PF-SF" = "PF",
        x
      ),
      character(1)
    ),
    pos_num = vapply(
      Pos,
      function(x) switch(
        x,
        "PG" = 1, 
        "SG" = 2, 
        "SF" = 3, 
        "PF" = 4, 
        "C" = 5
      ),
      numeric(1)
    ),
    
     first_abbrev = paste0(substring(Player, 1, 1)),
     last = str_split_fixed(player_stats$Player, "\\s", n = 2)[, 2],
     combined_name = paste0(first_abbrev, last)
  ) %>% 
  write_csv('~/lineuper/data/player_stats.csv')













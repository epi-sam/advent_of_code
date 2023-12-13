# Common header
.DAY <- "day_02"
.DAY <- paste0(.DAY, "_ex1")
source("/mnt/share/code/ssbyrne/advent_of_code/2023/paths.R")
sam <- readLines(PATHS$inputs)

library(purrr)
library(data.table)

# Part 1 ----

game_colors <- c("red", "green", "blue")
allowed_dt <- data.table(colors = game_colors,
                         allowed_value = c(12,13,14))
game_list <- list()
for(i in seq_along(sam)){
  
  game <- sam[[i]]
  pulls = sub("Game \\d+: ", "", game)
  pulls = unlist(strsplit(pulls, "; *"))
  
  pull_list <- list()
  
  for(j in seq_along(pulls)){
    
    pull <- pulls[[j]]
    
      sub_list <- list(
        red = 0
        , blue = 0
        , green = 0
      )
      
    pull_split = unlist(strsplit(pull, ", | "))
    i_odd <- seq(1, length(pull_split), 2)
    i_even <- seq(2, length(pull_split), 2)
    keys <- pull_split[i_even]
    vals <- pull_split[i_odd]
    pull_list[[j]] <- data.table(game = i, pull = j, colors = keys, value = as.integer(vals))
    
    
  }
  
  game_list[[i]] <- pull_list

}

game_dt <- rbindlist(lapply(game_list, rbindlist))
game_dt <- merge(game_dt, allowed_dt)
game_dt[, chk := value > allowed_value]
sum(unique(game_dt[!game %in% unique(game_dt[chk == TRUE]$game)]$game)) # 2716

# Part 2 ----

game_dt[, chk := NULL]
setkey(game_dt, game, pull)

by_cols <- c("colors", "game")
game_dt[, game_color_max := max(value), by = by_cols]

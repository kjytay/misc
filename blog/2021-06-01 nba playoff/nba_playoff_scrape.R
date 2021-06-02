library(tidyverse)
library(rvest)

# given a year Y (numeric), return a dataframe containing NBA playoff data
# for the season Y-Y+1. Each line in the dataframe represents one series.
# Note that in the returned dataframe, the year column is Y+1, the calendar
# year that the playoffs actually took place in.
getPlayoffResults <- function(year) {
  url <- paste0("https://www.landofbasketball.com/yearbyyear/",
                as.character(year),
                "_",
                as.character(year + 1),
                "_playoffs_brackets.htm")
  webpage <- read_html(url)
  
  # get raw playoff bracket data
  bracket <- webpage %>% html_node("table") %>%
    html_table(fill = TRUE)
  bracket <- bracket[c(3, 5), 1:3] %>%  # get relevant rows (NBA finals not needed)
    lapply(function(x) gsub("(\t|\r)", "", x)) %>% # remove all \t and \r
    lapply(function(x) gsub("76ers", "Seventysixers", x)) %>%  # 76ers starting with digit causes trouble later
    data.frame()
  
  # extract seeding
  seeding_string <- paste(bracket[, 1], collapse = "\n") %>%  # 1st round in one string
    str_split("\n") %>%
    sapply(function(x) gsub(" ", "", x))  # remove all spaces
  seeding_df <- data.frame(
    data = seeding_string[seeding_string != ""] %>%        # remove null rows
      sapply(function(x) substr(x, 1, nchar(x) - 1))) %>%  # remove last character (which is wins)
    remove_rownames() %>%
    separate(data, into = c("seed", "team"), sep = "\\)") %>%
    mutate(seed = as.numeric(seed))
  
  
  # extract game results
  results_string <- paste(do.call(paste, bracket), collapse = "\n") %>%
    gsub("\n", "", .) %>%       # remove newlines
    gsub("\\d\\)", "", .) %>%   # remove seeding info
    gsub(" ", "", .)            # remove spaces
  
  teams <- str_split(results_string, "\\d")[[1]]
  teams <- teams[teams != ""]
  wins <- str_split(results_string, "[a-zA-Z]")[[1]]
  wins <- wins[wins != ""] %>% as.numeric()
  if (length(teams) != length(wins))
    stop("length(teams) != length(wins), something went wrong with parsing")
  
  # put results and seeding info together in one df, each line represents
  # one series
  idx <- 1:length(teams)
  odd_idx  <- idx[idx %% 2 == 1]
  even_idx <- idx[idx %% 2 == 0]
  results_df <- data.frame(year = year + 1,  # the year the playoffs actually took place
                           team1  = teams[odd_idx], 
                           wins1 = wins[odd_idx],
                           team2  = teams[even_idx], 
                           wins2 = wins[even_idx]) %>%
    left_join(seeding_df, by = c("team1" = "team")) %>%
    rename(seed1 = seed) %>%
    left_join(seeding_df, by = c("team2" = "team")) %>%
    rename(seed2 = seed)
  
  return(results_df)
}

years <- 1983:2019
results_list <- lapply(years, getPlayoffResults)
results_df <- do.call(rbind, results_list)

#saveRDS(results_df, "nba_playoffs_data_1984-2020.rds")

# This script scrapes the box scores for all NBA games in the 2017-18 season.
# Results are saved into a list called `master`. The key for a game is its
# game_id (which we can get from the output of 2018-12-11_nba_game_data.R).
# master$game_id is itself a list of 5 items:
#   1. visitor basic box score
#   2. visitor advanced box score
#   3. home basic box score
#   4. home advanced box score
#   5. score profile against time (like data frame in nba_play_by_play_single_game.R)
#
# To use this script for other seasons, run 2018-12-11_nba_game_data.R for a 
# different year, then use that output file as the input for this script.

library(rvest)
library(lubridate)
library(tidyverse)

inputfile <- "../data/NBA-2018_game_data.rds"
outputfile <- "../data/NBA-2018_box_score.rds"

# remove the +'s from play-by-play score
parseScore <- function(x) {
    if (startsWith(x, "+")) {
        return(str_sub(x, 3, str_length(x)))
    } else if (endsWith(x, "+")) {
        return(str_sub(x, 1, str_length(x) - 1))
    } else {
        return(x)
    }
}

# helper function to get raw HTML box scores in better shape
parseBoxScore <- function(xx) {
    names(xx) <- c("Player", xx[1,][-1])  # get correct col names
    xx[xx == "Did Not Play"] <- NA
    
    # new col to say who started and who was reserve
    xx$Role <- "Reserve"
    xx$Role[1:6] <- "Starter"
    xx$Role[nrow(xx)] <- NA
    
    # remove old column headings, coerce statistics to numeric type
    xx <- xx[c(-1, -7), ]
    for (j in 3:(ncol(xx)-1)) {
        xx[, j] <- as.numeric(xx[, j])
    }
    xx
}

##############################
# SCRIPT STARTS HERE
##############################
game_df <- as.tibble(readRDS(inputfile))

master <- list()

for (current_id in game_df$game_id) {
    print(current_id)
    
    ##########
    # get box scores
    ##########
    url <- paste0("https://www.basketball-reference.com/boxscores/", current_id,
                  ".html")
    webpage <- read_html(url)
    
    tables <- webpage %>% html_nodes("table") %>%
        html_table()
    names(tables) <- c("visitor_basic_boxscore", "visitor_adv_boxscore",
                       "home_basic_boxscore", "home_adv_boxscore")
    tables <- lapply(tables, parseBoxScore)
    
    ##########
    # get play-by-play score profile
    ##########
    url <- paste0("https://www.basketball-reference.com/boxscores/pbp/", current_id,
                  ".html")
    webpage <- read_html(url)
    
    # pull out the events from the play-by-play table
    events <- webpage %>% 
        html_nodes("#pbp") %>%
        html_nodes("tr") %>% 
        html_text()
    
    # get event times & scores
    times  <- str_extract(events, "^\\d+:\\d+.\\d+")
    scores <- str_extract(events, "[\\+]*\\d+-\\d+[\\+]*")
    scores <- ifelse(str_detect(scores, "\\+"), scores, NA)
    
    pdp_df <- data.frame(time = times, score = scores, stringsAsFactors = FALSE) %>%
        na.omit()
    pdp_df$score <- sapply(pdp_df$score, parseScore)
    
    # split score into visitor and home score, get home advantage
    pdp_df <- pdp_df %>% 
        separate(score, into = c("visitor", "home"), sep = "-") %>%
        mutate(visitor = as.numeric(visitor), 
               home = as.numeric(home),
               time = ms(time))
    
    # get period of play (e.g. Q1, Q2, ...)
    pdp_df$period <- NA
    period <- 0
    prev_time <- ms("0:00")
    for (i in 1:nrow(pdp_df)) {
        curr_time <- pdp_df[i, "time"]
        if (prev_time < curr_time) {
            period <- period + 1
        }
        pdp_df[i, "period"] <- period
        prev_time <- curr_time
    }
    
    # convert time such that it runs upwards. regular quarters are 12M long, OT 
    # periods are 5M long
    pdp_df <- pdp_df %>% mutate(time = ifelse(period <= 4, 
                                              as.duration(12 * 60) - as.duration(time),
                                              as.duration(5  * 60) - as.duration(time))) %>%
        mutate(time = ifelse(period <= 4,
                             time + as.duration(12 * 60 * (period - 1)),
                             time + as.duration(12 * 60 * 4) + 
                                 as.duration(5 * 60 * (period - 5))
        ))
    
    tables$pdp_df <- pdp_df
    
    master[[current_id]] <- tables
}

#saveRDS(master, outputfile)

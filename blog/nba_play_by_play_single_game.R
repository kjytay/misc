# This script takes an NBA game ID (from basketball-reference.com) and produces 
# the lead tracker graphic for the game.

library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)

# PARAMETERS: GAME ID, plot colors
current_id <- "201810190NOP"
visitor_col <- "blue"
home_col <- "red"

# get webpage
url <- paste0("https://www.basketball-reference.com/boxscores/pbp/", current_id,
              ".html")
webpage <- read_html(url)

# pull out title
title <- webpage %>% 
    html_nodes("title") %>% 
    html_text() %>%
    str_extract(".+ Play")
title <- str_sub(title, 1, length(title) - 7)
plot_title <- paste0(title, ", ", 
                     str_sub(current_id, 1, 4), "-", str_sub(current_id, 5, 6),
                     "-", str_sub(current_id, 7, 8))

# pull out the events from the play-by-play table
events <- webpage %>% 
    html_nodes("#pbp") %>%
    html_nodes("tr") %>% 
    html_text()

# get event times & scores
times  <- str_extract(events, "^\\d+:\\d+.\\d+")
scores <- str_extract(events, "[\\+]*\\d+-\\d+[\\+]*")
scores <- ifelse(str_detect(scores, "\\+"), scores, NA)

df <- data.frame(time = times, score = scores, stringsAsFactors = FALSE) %>%
    na.omit()

# remove the +'s
parseScore <- function(x) {
    if (startsWith(x, "+")) {
        return(str_sub(x, 3, str_length(x)))
    } else if (endsWith(x, "+")) {
        return(str_sub(x, 1, str_length(x) - 1))
    } else {
        return(x)
    }
}
df$score <- sapply(df$score, parseScore)

# split score into visitor and home score, get home advantage
df <- df %>% 
    separate(score, into = c("visitor", "home"), sep = "-") %>%
    mutate(visitor = as.numeric(visitor), 
           home = as.numeric(home),
           time = ms(time)) %>%
    mutate(visitor_adv = visitor - home)

# get period of play (e.g. Q1, Q2, ...)
df$period <- NA
period <- 0
prev_time <- ms("0:00")
for (i in 1:nrow(df)) {
    curr_time <- df[i, "time"]
    if (prev_time < curr_time) {
        period <- period + 1
    }
    df[i, "period"] <- period
    prev_time <- curr_time
}

# convert time such that it runs upwards. regular quarters are 12M long, OT 
# periods are 5M long
df <- df %>% mutate(time = ifelse(period <= 4, 
                                  as.duration(12 * 60) - as.duration(time),
                                  as.duration(5  * 60) - as.duration(time))) %>%
    mutate(time = ifelse(period <= 4,
                         time + as.duration(12 * 60 * (period - 1)),
                         time + as.duration(12 * 60 * 4) + 
                             as.duration(5 * 60 * (period - 5))
    ))

# some work to get data in shape for the two-color ribbon plot
# see https://gist.github.com/Teebusch/db0ab76d31fd31a13ccf93afa7d77df5
df$visitor_lead <- pmax(df$visitor_adv, 0)
df$home_lead    <- pmin(df$visitor_adv, 0)

df_extraSteps <- df %>% mutate(visitor_adv = lag(visitor_adv),
                               visitor_lead = lag(visitor_lead),
                               home_lead = lag(home_lead))
df2 <- bind_rows(df_extraSteps, df) %>%
    arrange(time)

# get score differential range (pad with some space, round to nearest 5)
ymax <- round(max(df$visitor_adv + 2) * 2, digits = -1) / 2
ymin <- round(min(df$visitor_adv - 2) * 2, digits = -1) / 2

# get period positions and labels
periods <- unique(df$period)
x_value <- ifelse(periods <= 4, 12 * 60 * periods, 
                  12 * 60 * 4 + 5 * 60 * (periods - 4))
x_label <- ifelse(periods <= 4, paste0("Q", periods), 
                  paste0("OT", periods - 4))

ggplot() +
    geom_ribbon(data = df2, aes(x = time, ymin = 0, ymax = visitor_lead), 
                fill = visitor_col) +
    geom_ribbon(data = df2, aes(x = time, ymin = home_lead, ymax = 0), 
                fill = home_col) +
    geom_vline(aes(xintercept = x_value), linetype = 2, col = "grey") +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = plot_title) +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

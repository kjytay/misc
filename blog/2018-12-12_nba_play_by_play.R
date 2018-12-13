# This is code for the blog post on 2018-12-12 titled 
# “Recreating the NBA lead tracker graphic". 

library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)

# PARAMETER: GAME ID
current_id <- "201812100PHO"

# get webpage
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

# crude line plot
ggplot() +
    geom_line(data = df, aes(x = time, y = visitor_adv)) +
    labs(title = "LAC @ PHX, 2018-12-10") +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5))

# crude step plot
ggplot() +
    geom_step(data = df, aes(x = time, y = visitor_adv)) +
    labs(title = "LAC @ PHX, 2018-12-10") +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5))

# basic geom_ribbon plot
df$visitor_lead <- pmax(df$visitor_adv, 0)
df$home_lead    <- pmin(df$visitor_adv, 0)

df_extraSteps <- df %>% mutate(visitor_adv = lag(visitor_adv),
                               visitor_lead = lag(visitor_lead),
                               home_lead = lag(home_lead))
df2 <- bind_rows(df_extraSteps, df) %>%
    arrange(time)

ggplot() +
    geom_ribbon(data = df2, aes(x = time, ymin = 0, ymax = visitor_lead), fill = "#F7174E") +
    geom_ribbon(data = df2, aes(x = time, ymin = home_lead, ymax = 0), fill = "#F16031") +
    labs(title = "LAC @ PHX, 2018-12-10") +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5))

# better geom_ribbon plot
# get score differential range (round to nearest 5)
ymax <- round(max(df$visitor_adv) * 2, digits = -1) / 2
ymin <- round(min(df$visitor_adv) * 2, digits = -1) / 2

# get period positions and labels
periods <- unique(df$period)
x_value <- ifelse(periods <= 4, 12 * 60 * periods, 
                  12 * 60 * 4 + 5 * 60 * (periods - 4))
x_label <- ifelse(periods <= 4, paste0("Q", periods), 
                  paste0("OT", periods - 4))

ggplot() +
    geom_ribbon(data = df2, aes(x = time, ymin = 0, ymax = visitor_lead), fill = "#F7174E") +
    geom_ribbon(data = df2, aes(x = time, ymin = home_lead, ymax = 0), fill = "#F16031") +
    geom_vline(aes(xintercept = x_value), linetype = 2, col = "grey") +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = "LAC @ PHX, 2018-12-10") +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

# cumulative step plot
ggplot() +
    geom_step(data = df, aes(x = time, y = visitor), col = "#F7174E") +
    geom_step(data = df, aes(x = time, y = home), col = "#F16031") +
    geom_vline(aes(xintercept = x_value), linetype = 2, col = "grey") +
    labs(title = "LAC @ PHX, 2018-12-10") +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

# cumulative geom_ribbon plot
ggplot() +
    geom_ribbon(data = df2, aes(x = time, ymin = pmin(visitor, home), ymax = visitor), fill = "#F7174E") +
    geom_ribbon(data = df2, aes(x = time, ymin = pmin(visitor, home), ymax = home), fill = "#F16031") +
    geom_vline(aes(xintercept = x_value), linetype = 2, col = "grey") +
    labs(title = "LAC @ PHX, 2018-12-10") +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

# add a row for time 0
df3 <- rbind(c(0, 0, 0, 0, 1, 0, 0), df)

# plot with points annotation
df3$pts <- factor(with(df3, abs(visitor_adv - lag(visitor_adv))))
ggplot() +
    geom_line(data = df3, aes(x = time, y = visitor_adv)) +
    geom_point(data = df3[-1,], aes(x = time, y = visitor_adv, col = pts)) +
    geom_vline(aes(xintercept = x_value), linetype = 2, col = "grey") +
    labs(title = "LAC @ PHX, 2018-12-10") +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank())

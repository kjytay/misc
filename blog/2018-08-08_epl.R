library(tidyverse)

# load data
load("../data/england.rda")
standings <- read_csv("../data/epl_standings.csv")

# filter for just data in premier league and in 2008-2016
england <- england %>% filter(Season >= 2008 & division == 1)
standings <- standings %>% filter(Season <= 2016) %>% 
    select(Season, Rank, Club)

# add ranking data to england df
england <- england %>% 
    left_join(standings, by = c("Season" = "Season", "home" = "Club")) %>%
    rename(home_rank = Rank) %>%
    left_join(standings, by = c("Season" = "Season", "visitor" = "Club")) %>%
    rename(away_rank = Rank)

# 1 if better team wins / win-or-draw, 0 otherwise
england$better_W <- with(england,
                         ifelse((home_rank < away_rank & result == "H") |
                                (home_rank > away_rank & result == "A"),
                                1, 0))
england$better_WD <- with(england,
                          ifelse(better_W == 1 | result == "D",
                                 1, 0))

# better_W and better_WD overall
england %>%
    summarize(better_W = mean(better_W),
              better_WD = mean(better_WD))


# table of better_W and better_WD by season
england %>% group_by(Season) %>%
    summarize(better_W = mean(better_W),
              better_WD = mean(better_WD))

# plot of the above
england %>% group_by(Season) %>%
    summarize(better_W = mean(better_W),
              better_WD = mean(better_WD)) %>%
    gather(key = "stat", value = "pct", better_W, better_WD) %>%
    ggplot(aes(x = Season, y = pct, col = stat)) +
    geom_line() + geom_point() +
    geom_abline(slope = 0, intercept = 0.5, linetype = "dashed") +
    geom_abline(slope = 0, intercept = 1, linetype = "dashed") +
    ylim(c(0, 1)) +
    labs(title = "Win & win/draw % for the better team", 
         y = "proportion") +
    theme(legend.position = "bottom")

# plot as above, but ignoring games which ended in a draw
england %>% 
    filter(result != "D") %>%
    group_by(Season) %>%
    summarize(better_W = mean(better_W)) %>%
    ggplot(aes(x = Season, y = better_W)) +
    geom_line() + geom_point() +
    geom_abline(slope = 0, intercept = 0.5, linetype = "dashed") +
    geom_abline(slope = 0, intercept = 1, linetype = "dashed") +
    ylim(c(0, 1)) +
    labs(title = "Win % for the better team (for games ending in W/L)", 
         y = "proportion")

# table of better_W and better_WD with D baseline
df <- england %>% group_by(Season) %>%
    summarize(better_W = mean(better_W),
              better_WD = mean(better_WD),
              D = mean(result == "D"))
df$W_baseline <- with(df, (1-D)/2)
df$W_topline  <- with(df, 1-D)
df$WD_baseline <- with(df, D + W_baseline)
df$WD_topline  <- with(df, 1)
df$D <- NULL

# plot for better_W with adjusted baseline
ggplot(df, aes(x = Season, y = better_W)) +
    geom_line(col = "#F8766D") + geom_point(col = "#F8766D") +
    geom_line(aes(y = W_baseline), linetype = "dashed") +
    geom_line(aes(y = W_topline), linetype = "dashed") +
    ylim(c(0, 1)) +
    labs(title = "Win % for the better team", 
         y = "proportion")

# plot for better_WD with adjusted baseline
ggplot(df, aes(x = Season, y = better_WD)) +
    geom_line(col = "#00BFC4") + geom_point(col = "#00BFC4") +
    geom_line(aes(y = WD_baseline), linetype = "dashed") +
    geom_line(aes(y = WD_topline), linetype = "dashed") +
    ylim(c(0, 1)) +
    labs(title = "Win/draw % for the better team", 
         y = "proportion")

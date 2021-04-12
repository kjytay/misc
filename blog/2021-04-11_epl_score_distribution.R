library(tidyverse)
df <- read_csv("../data/epl_standings.csv")

theme_set(theme_bw())

# joy plot
library(ggridges)
ggplot(df, aes(x = Points, y = factor(Season))) +
    geom_density_ridges(scale = 3) +
    labs(y = "Season")

# boxplot
ggplot(df, aes(x = Season, y = Points, group = Season)) +
    geom_boxplot() +
    scale_x_continuous(breaks = seq(min(df$Season), max(df$Season), by = 2)) +
    labs(title = "Distribution of points by season")

# plot of maximum and minimum
df %>% group_by(Season) %>%
    summarize(max = max(Points), min = min(Points)) %>%
    mutate(range = max - min) %>%
    pivot_longer(max:range) %>%
    ggplot(aes(x = Season, y = value, col = name)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_continuous(breaks = seq(min(df$Season), max(df$Season), by = 2)) +
    labs(title = "Max/min points by season", y = "Points") +
    theme(legend.title = element_blank(), legend.position = "bottom")

# plot of variance
max_var_dist <- seq(0, 38 * 3, by = 6)
max_var <- var(max_var_dist)
df %>% group_by(Season) %>%
    summarize(var = var(Points)) %>%
    ggplot(aes(x = Season, y = var)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = c(0, max_var), col = "red", linetype = "dashed") +
    scale_x_continuous(breaks = seq(min(df$Season), max(df$Season), by = 2)) +
    labs(title = "Variance of point distribution by season", y = "Variance")

# plot of Gini
library(DescTools)
df %>% group_by(Season) %>%
    summarize(gini = DescTools::Gini(Points)) %>%
    ggplot(aes(x = Season, y = gini)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = c(0, 1), col = "red", linetype = "dashed") +
    scale_x_continuous(breaks = seq(min(df$Season), max(df$Season), by = 2)) +
    labs(title = "Gini coefficient of point dist by season", 
         y = "Gini coefficient")

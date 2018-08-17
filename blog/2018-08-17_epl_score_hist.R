library(tidyverse)
df <- read_csv("../data/epl_standings.csv")

# histogram of points by Season
ggplot(df, aes(x = Points)) +
    geom_histogram() +
    facet_wrap(~ Season, nrow = 2)

# density plot
ggplot(df, aes(x = Points, group = Season)) +
    geom_density()

# density plot with smoothed estimate aggregating all years
ggplot(df, aes(x = Points)) +
    geom_density(aes(group = Season), size = 0.2) +
    geom_density(col = "blue", size = 1.5)

# joy plot
library(ggridges)
ggplot(df, aes(x = Points, y = factor(Season))) +
    geom_density_ridges(scale = 3) +
    labs(y = "Season")

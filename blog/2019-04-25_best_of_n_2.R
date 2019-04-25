# This script is the code for my blog post on 2019-xx-xx. We simulate best-of-7
# contests to see how the probability of winning any one game translates to the 
# win probability for a best-of-n contest. We allow for different win 
# probabilities for home and away, as well as whether the player starts at home
# or away.

library(tidyverse)
library(plotly)
library(metR)

# player has prob home_p and away_p of winning 1 game in a two-player contest 
# at home and away respectively.
# Out of simN runs of best-of-n contests, in how many runs does the player
# win (as a percentage of total no. of rounds)? A simulation of simN runs, 
# NOT an exact answer!
# n must be odd.
# `start` must be "home" or "away", indicating where the player starts. It is
# assumed that in the series, |home games - away games| = 1, with `start` games 
# having one more.
sim_fn <- function(simN, n, home_p, away_p, start) {
    if (n %% 2 == 0) {
        stop("n should be an odd number")
    }
    
    if (start == "home") {
        more_p <- home_p
        less_p <- away_p
    } else {
        more_p <- away_p
        less_p <- home_p
    }
    
    mean(rbinom(simN, (n+1)/2, more_p) + rbinom(simN, (n-1)/2, less_p) > n / 2)
}

# construct grid of home and away probabilities and starting location
home_p <- seq(0, 1, length.out = 51)
away_p <- seq(0, 1, length.out = 51)
df <- expand.grid(home_p = home_p, away_p = away_p, start = c("home", "away"),
                  stringsAsFactors = FALSE)

set.seed(111)
simN <- 50000
n <- 7
df$win_prob <- apply(df, 1, function(x) sim_fn(simN, n, as.numeric(x[1]), 
                                               as.numeric(x[2]), x[3]))

# plot: facet by away win probability (many facets: not advisable!!)
ggplot(df, aes(x = home_p, y = win_prob, col = start)) +
    geom_line() +
    labs(title = paste("Win probability for best of", n, "series"),
         x = "Win prob for single game", y = "Win prob for series") +
    facet_wrap(~ away_p)

# ggplot 2D raster plot
ggplot(df, aes(x = home_p, y = away_p)) +
    geom_raster(aes(fill = win_prob)) +
    labs(title = paste("Win probability for best of", n, "series"),
         x = "Home win prob", y = "Away win prob") +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    facet_wrap(~ start, labeller = label_both) +
    coord_fixed() +
    theme(legend.position = "bottom")

# ggplot 2D raster plot with labeled contours
ggplot(df, aes(x = home_p, y = away_p, z = win_prob)) +
    geom_raster(aes(fill = win_prob)) +
    geom_contour(col = "white", breaks = 1:9 / 10) +
    geom_text_contour(col = "#595959") +
    labs(title = paste("Win probability for best of", n, "series"),
         x = "Home win prob", y = "Away win prob") +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    facet_wrap(~ start, labeller = label_both) +
    coord_fixed() +
    theme(legend.position = "bottom")

# plotly 3D surface plot
df_home <- df %>% filter(start == "home")
win_prob_matrix <- matrix(df_home$win_prob, nrow = length(away_p), byrow = TRUE)

plot_ly(x = home_p, y = away_p, z = win_prob_matrix) %>% add_surface() %>%
    layout(
        title = paste("Win probability for best of", n, "series (start at home)"),
        scene = list(
            xaxis = list(title = "Home win prob"),
            yaxis = list(title = "Away win prob"),
            zaxis = list(title = "Series win prob")
        )
    )

# plotly 3D surface plot with red contour projection
plot_ly(x = home_p, y = away_p, z = win_prob_matrix) %>% 
    add_surface(contours = list(
        z = list(
            show = TRUE,
            usecolormap = TRUE,
            highlightcolor = "#ff0000",
            project = list(z = TRUE)
            )
        )
    ) %>%
    layout(
        title = paste("Win probability for best of", n, "series (start at home)"),
        scene = list(
            xaxis = list(title = "Home win prob"),
            yaxis = list(title = "Away win prob"),
            zaxis = list(title = "Series win prob")
        )
    )

# difference in starting at home vs. away
df2 <- df %>% spread(start, value = win_prob) %>%
    mutate(home_adv = home - away)

# raster plot
ggplot(df2, aes(x = home_p, y = away_p)) +
    geom_raster(aes(fill = home_adv)) +
    labs(title = "Difference in series win prob",
         subtitle = "Starting at home vs. away",
         x = "Home win prob", y = "Away win prob") +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    coord_fixed() +
    theme(legend.position = "bottom")

# raster plot with contours
breaks <- c(-0.5, -0.2, -0.1, -0.05, -0.02, 0, 0.02, 0.05, 0.1, 0.2, 0.5)
ggplot(df2, aes(x = home_p, y = away_p, z = home_adv)) +
    geom_raster(aes(fill = home_adv)) +
    geom_contour(col = "white", breaks = breaks) +
    geom_text_contour(breaks = breaks, 
                      col = "#595959", check_overlap = TRUE) +
    labs(title = "Difference in series win prob",
         subtitle = "Starting at home vs. away",
         x = "Home win prob", y = "Away win prob") +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    coord_fixed() +
    theme(legend.position = "bottom")

# when does being at home tip you over (or back under) the threshold?
threshold <- 0.5
df2$home_impt <- ifelse(df2$home > threshold & df2$away < threshold, 1,
                        ifelse(df2$home < threshold & df2$away > threshold, -1, NA))
df2$home_impt <- factor(df2$home_impt)
df2$home_impt <- fct_recode(df2$home_impt, 
                            "Better" = "1", "Worse" = "-1")
p <- df2 %>% filter(!is.na(home_impt)) %>%
    ggplot(aes(x = home_p, y = away_p)) +
        geom_raster(aes(fill = home_impt)) +
        labs(title = paste("Does starting at home push you \nover the series win threshold", threshold, "?"),
             x = "Home win prob", y = "Away win prob") +
        scale_fill_manual(values = c("#ff0000", "#0000ff")) + 
        coord_fixed() +
        theme(legend.position = "bottom")
p

# NBA 2019 regular season data
nba_df <- data.frame(
    home_p = c(33, 32, 31, 28, 29, 23, 25, 26, 25, 19, 22, 17, 9, 13, 9,
               30, 34, 32, 31, 29, 27, 32, 26, 24, 22, 25, 21, 19, 24, 12) / 41,
    away_p = c(27, 26, 20, 21, 19, 19, 17, 15, 14, 20, 10, 12, 13, 6, 8,
               27, 20, 21, 22, 21, 22, 16, 22, 15, 15, 11, 12, 14, 9, 7) / 41
)

# above plot overlaid with NBA data
p + geom_point(data = nba_df, aes(x = home_p, y = away_p), col = "orange")


# contour plot for multiple n
# construct grid of home and away probabilities and starting location
master_df <- data.frame(home_p = numeric(0), away_p = numeric(0), 
                        start = character(0), win_prob = numeric(0), n = numeric(0))

set.seed(111)
simN <- 10000
for (n in 1:5 * 2 - 1) {
    df <- expand.grid(home_p = home_p, away_p = away_p, start = c("home", "away"),
                      stringsAsFactors = FALSE)
    df$n <- n
    df$win_prob <- apply(df, 1, function(x) sim_fn(simN, n, as.numeric(x[1]), 
                                                   as.numeric(x[2]), x[3]))
    master_df <- rbind(master_df, df)
}

# ggplot 2D raster plot with labeled contours
ggplot(master_df, aes(x = home_p, y = away_p, z = win_prob)) +
    geom_raster(aes(fill = win_prob)) +
    geom_contour(col = "white", breaks = 1:9 / 10) +
    geom_text_contour(col = "#595959") +
    labs(title = "Win probability for best of n series",
         x = "Home win prob", y = "Away win prob") +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    facet_grid(start ~ n, labeller = label_both) +
    coord_fixed() +
    theme(legend.position = "bottom")

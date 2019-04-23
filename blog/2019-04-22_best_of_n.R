# This script is the code for my blog post on 2019-04-22. We simulate best-of-n
# contests to see how the probability of winning any one game translates to the 
# win probability for a best-of-n contest.

library(ggplot2)

# player has prob p of winning 1 game in a two-player contest
# Out of simN runs of best-of-n contests, in how many runs does the player
# win (as a percentage of total no. of rounds)? A simulation of simN runs, 
# NOT an exact answer!
sim_fn <- function(simN, n, p) {
    if (n %% 2 == 0) {
        stop("n should be an odd number")
    }
    mean(rbinom(simN, n, p) > n / 2)
}

# simulation for single n value
n <- 7
df <- data.frame(p = seq(0.5, 1, length.out = 26), n = n)

set.seed(1043)
simN <- 100000
df$win_prob <- apply(df, 1, function(x) sim_fn(simN, x[2], x[1]))

# compute std err & p_hat \pm 1 std err
df$std_err <- with(df, sqrt(win_prob * (1 - win_prob) / simN))
df$lower <- with(df, win_prob - std_err)
df$upper <- with(df, win_prob + std_err)

# plot
ggplot(df, aes(x = p, y = win_prob)) +
    geom_line() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    labs(title = paste("Win probability for best of", n, "series"),
         x = "Win prob for single game", y = "Win prob for series")

# simulation for multiple n values
p <- seq(0.5, 1, length.out = 26)
n <- seq(1, 15, length.out = 8)
df <- expand.grid(p, n)
names(df) <- c("p", "n")

set.seed(422)
simN <- 100000
df$win_prob <- apply(df, 1, function(x) sim_fn(simN, x[2], x[1]))

# compute std err & p_hat \pm 1 std err
df$std_err <- with(df, sqrt(win_prob * (1 - win_prob) / simN))
df$lower <- with(df, win_prob - std_err)
df$upper <- with(df, win_prob + std_err)

ggplot(df, aes(x = p, y = win_prob, col = factor(n))) +
    geom_line() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    labs(title = paste("Win probability for best of n series"),
         x = "Win prob for single game", y = "Win prob for series") +
    theme(legend.position = "bottom")

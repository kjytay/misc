# Player 1 has win probability p. Simulates a series of games which ends when
# player 1 reaches `n_wins1` wins or when player 2 reaches `n_wins2` wins.
SampleSeries <- function(p, n_wins1, n_wins2 = n_wins1) {
  game_results <- rbinom(n_wins1 + n_wins2 - 1, size = 1, prob = p)
  if (sum(game_results) >= n_wins1) {
    n_games <- which(game_results == 1)[n_wins1]
  } else {
    n_games <- which(game_results == 0)[n_wins2]
  }
  player1_wins <- sum(game_results[1:n_games])
  return(c(player1_wins, n_games - player1_wins))
}

# Run SampleSeries `n_sim` times and for each run, estimate p by player 1's
# win percentage in that series. Returns the estimated probabilities.
EstimateWinProb <- function(p, n_sim, n_wins1, n_wins2 = n_wins1) {
  win_data <- replicate(n_sim, SampleSeries(p, n_wins1, n_wins2))
  prob_estimates <- apply(win_data, 2, function(x) x[1] / (x[1] + x[2]))
  return(mean(prob_estimates))
}

# Monte-Carlo plot for first to 4 wins
set.seed(1)
p <- 20:40 / 40
n_wins1 <- 4
n_sim <- 20000
mean_phat <- sapply(p, function(p) EstimateWinProb(p, n_sim, n_wins1))
plot(p, mean_phat, asp = 1, pch = 16, ylab = "Win prob estimator mean",
     main = "First to 4 wins")
abline(0, 1)
# lines(p, sapply(p, function(x) GetEstimatorMean(x, n_wins1)), 
#       col = "red", lty = 2, lwd = 1.5)

# Return expected value of the "series win percentage" estimator
GetEstimatorMean <- function(p, n_wins1, n_wins2 = n_wins1) {
  # first line is for when player 1 wins, second line is for when player 2 wins
  summands1 <- sapply(0:(n_wins2 - 1),
                      function(x) n_wins1 / (n_wins1 + x) * p^n_wins1 * (1 - p)^x *
                        choose(x + n_wins1 - 1, x)
  )
  summands2 <- sapply(0:(n_wins1 - 1), 
                      function(x) x / (x + n_wins2) * p^x * (1 - p)^n_wins2 * 
                        choose(x + n_wins2 - 1, x)
  )
  return(sum(c(summands1, summands2)))
}

# Way to get a better estimate for win probability (method of moments)
p <- 20:40 / 40
original_estimate <- c(4 / 7:4)
mm_estimate <- c(0.56, 0.64, 0.77, 1)  # by trial and error
plot(p, sapply(p, function(x) GetEstimatorMean(x, n_wins1 = 4)), type = "l",
     asp = 1, ylab = "Win prob estimator mean")
abline(0, 1, lty = 2)
segments(x0 = 0, x1 = mm_estimate, y0 = original_estimate, col = "red", lty = 2)
segments(y0 = 0, y1 = original_estimate, x0 = mm_estimate, col = "red", lty = 2)

# varying n_wins1 from 1 to 8 (inclusive)
library(tidyverse)
theme_set(theme_bw())
df <- expand.grid(p = 20:40 / 40, n_wins1 = 1:8)
df$mean_phat <- apply(df, 1, function(x) GetEstimatorMean(x[1], x[2]))
ggplot(df, aes(group = factor(n_wins1))) +
  geom_line(aes(x = p, y = mean_phat, col = factor(n_wins1))) +
  coord_fixed(ratio = 1) +
  scale_color_discrete(name = "n_wins") +
  labs(y = "Win prob estimator mean", title = "First to n_wins")

# zoom in
ggplot(df, aes(group = factor(n_wins1))) +
  geom_line(aes(x = p, y = mean_phat, col = factor(n_wins1))) +
  coord_fixed(ratio = 1, xlim = c(0.7, 0.8), ylim = c(0.7, 0.8)) +
  scale_color_discrete(name = "n_wins") +
  labs(y = "Win prob estimator mean", title = "First to n_wins (zoomed in)")

# Plot when varying n_wins1 and n_wins2
df <- expand.grid(p = 20:40 / 40, n_wins1 = 1:8, n_wins2 = 1:8)
df$mean_phat <- apply(df, 1, function(x) GetEstimatorMean(x[1], x[2], x[3]))
ggplot(df, aes(group = factor(n_wins1))) +
  geom_line(aes(x = p, y = mean_phat, col = factor(n_wins1))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_fixed(ratio = 1) +
  scale_color_discrete(name = "n_wins1") +
  facet_wrap(~ n_wins2, ncol = 4, labeller = label_both) +
  labs(y = "Win prob estimator mean", title = "Player 1 to n_wins1 or Player 2 to n_wins2")

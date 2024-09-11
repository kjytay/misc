library(tidyverse)
library(gridExtra)

x <- seq(0, pi, length.out = 101)

quad_function <- function(x) {
  (x - 0)*(x - pi) / ((pi/2 - 0)*(pi/2 - pi)) * 1
}

cubic_function <- function(x) {
  a <- 0; b <- pi/3; c <- 2*pi/3; d <- pi
  (x - b)*(x - c)*(x - d) / ((a - b)*(a - c)*(a - d)) * 0 +
    (x - a)*(x - c)*(x - d) / ((b - a)*(b - c)*(b - d)) * sqrt(3)/2 +
    (x - a)*(x - b)*(x - d) / ((c - a)*(c - b)*(c - d)) * sqrt(3)/2 +
    (x - a)*(x - b)*(x - c) / ((d - a)*(d - b)*(d - c)) * 0
}

sin_df <- data.frame(
  x = x,
  approx = "sin x",
  y = sin(x)
)

quad_df <- data.frame(
  x = x,
  approx = "quad approx",
  y = quad_function(x)
)

cubic_df <- data.frame(
  x = x,
  approx = "cubic approx",
  y = cubic_function(x)
)

quad_diff_df <- data.frame(
  x = x,
  approx = "quad approx",
  diff = sin(x) - quad_function(x)
)

cubic_diff_df <- data.frame(
  x = x,
  approx = "cubic approx",
  diff = sin(x) - cubic_function(x)
)


theme_set(theme_bw())
actual_plot <- rbind(sin_df, quad_df, cubic_df) %>%
  ggplot(aes(x = x, y = y, col = approx)) +
  geom_line() +
  labs(title = "True f and approximation", col = NULL) +
  scale_color_manual(values = c("blue", "red", "black")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") 

diff_plot <- rbind(quad_diff_df, cubic_diff_df) %>%
  ggplot(aes(x = x, y = diff, col = approx)) +
  geom_line() +
  labs(title = "Actual - Approximation", y = "actual - approximation", col = NULL) +
  scale_color_manual(values = c("blue", "red")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

combined_plot <- arrangeGrob(actual_plot, diff_plot, ncol = 2)
#ggsave("both_plot.png", combined_plot, width = 10, height = 5)

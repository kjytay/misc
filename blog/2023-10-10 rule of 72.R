library(ggplot2)
theme_set(theme_bw())

getNumPeriods <- function(x) { log(2) / log(1 + x/100) }
getApproxNumPeriods1 <- function(x) { 72 / x }
getApproxNumPeriods2 <- function(x) { log(2) * 100 / x }
getApproxNumPeriods3 <- function(x) { log(2) / (x/100 - (x/100)^2 / 2) }

# Overall plot for wider range of interest rates: the lines are basically the same
x <- 1:40 / 2
df <- data.frame(
  x = rep(x, times = 4),
  method = rep(
    rep(c("Exact", "Rule of 72", "Rule of '69'", "Quadratic approx"), each = length(x)),
    times = 4),
  y = c(getNumPeriods(x), getApproxNumPeriods1(x), getApproxNumPeriods2(x), getApproxNumPeriods3(x))
)

ggplot(df, aes(x, y)) +
  geom_line(aes(col = method)) +
  labs(title = "Years to double vs. annual interest rate",
       x = "Annual interest rate (%)",
       y = "Years to double") +
  theme(legend.position = "bottom")
#ggsave("periods1.png", width = 7, height = 5)

# Overall plot for narrower range of interest rates
x <- seq(from = 0.5, 5, by = 0.1)
df <- data.frame(
  x = rep(x, times = 4),
  method = rep(
    rep(c("Exact", "Rule of 72", "Rule of '69'", "Quadratic approx"), each = length(x)),
    times = 4),
  y = c(getNumPeriods(x), getApproxNumPeriods1(x), getApproxNumPeriods2(x), getApproxNumPeriods3(x))
)

ggplot(df, aes(x, y)) +
  geom_line(aes(col = method)) +
  labs(title = "Years to double vs. annual interest rate",
       x = "Annual interest rate (%)",
       y = "Years to double") +
  scale_y_log10() +
  theme(legend.position = "bottom")
#ggsave("periods2.png", width = 7, height = 5)

# Relative error plot
df$relError <- abs(df$y - getNumPeriods(x)) / getNumPeriods(x) * 100
ggplot(df, aes(x, relError)) +
  geom_line(aes(col = method)) +
  labs(title = "Relative error in estimate vs. annual interest rate",
       x = "Annual interest rate (%)",
       y = "Relative error (%)") +
  theme(legend.position = "bottom")
#ggsave("rel_error.png", width = 7, height = 5)
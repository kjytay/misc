#####
# 1. Comparing tail probabilities directly, with no regard for scale
#####
dfVal <- c(Inf, 100, 50, 30, 10, 5, 3, 2.1)
x <- 1:10

tbl <- lapply(dfVal, function(df) {
  data.frame(df = df,
             x = x,
             log10Prob = log10(pt(x, df = df, lower.tail = FALSE)))
})

tbl <- do.call(rbind, tbl)
tbl$df <- factor(tbl$df)

ggplot(tbl, aes(x = x, y = log10Prob, col = df)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Spectral", direction = 1) +
  labs(x = "x", y = "log10(P{ T_df > x})",
       title = "log10 Tail probabilities for t distribution",
       col = "Deg. of freedom") +
  theme_bw()

#####
# 2. Comparing tail probabilities directly for normal distributions with
# different scale
#####
scaleVal <- 1:10
x <- 1:10

tbl <- lapply(scaleVal, function(sc) {
  data.frame(scaleFactor = sc,
             x = x,
             log10Prob = log10(pnorm(x, sd = sc, lower.tail = FALSE)))
})
tbl <- do.call(rbind, tbl)
tbl$scaleFactor <- factor(tbl$scaleFactor)

ggplot(tbl, aes(x = x, y = log10Prob, col = scaleFactor)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Spectral", direction = -1) +
  labs(x = "x", y = "log10(P{ T_df > x})",
       title = "log10 Tail probabilities for normal distribution",
       col = "Standard deviation") +
  theme_bw()

# Given a t distribution's degree of freedom, find the scale factor
# `scaleFactor` such that 
# P(scaleFactor * T >= threshold) = P(N(0, 1) >= threshold).
# `threshold` assumed to be positive.
getScaleFactor <- function(df, threshold) {
  tailProb <- pnorm(threshold, lower.tail = FALSE)
  tQuantile <- qt(tailProb, df = df, lower.tail = FALSE)
  return(threshold / tQuantile)
}

#####
# 3. plot scale factors
#####
dfVal <- c(Inf, 100, 50, 30, 10, 5, 3, 2.1)
thresholdVal <- 1:20 / 5
tbl <- lapply(dfVal, function(df) data.frame(
  df = df,
  threshold = thresholdVal,
  scaleFactor = sapply(thresholdVal, 
                       function(thresh) getScaleFactor(df, thresh)))
)
tbl <- do.call(rbind, tbl)
tbl$df <- factor(tbl$df)

ggplot(tbl, aes(x = threshold, y = scaleFactor, col = df)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Spectral", direction = 1) +
  labs(x = "threshold", y = "Implied scale factor",
       title = "k such that that P(kT > threshold) = P(N(0,1) > threshold)",
       col = "Deg. of freedom") +
  theme_bw()

#####
# 4. Tail probabilities for t, but where P(random variable > threshold)
# is the same for all the random variables. This takes scale into account.
#####
dfVal <- c(Inf, 100, 50, 30, 10, 5, 3, 2.1)
x <- 1:10
threshold <- 3

tbl <- lapply(dfVal, function(df) {
  scaleFactor <- getScaleFactor(df, threshold)
  data.frame(df = df,
             x = x,
             log10Prob = log10(pt(x / scaleFactor, df = df, lower.tail = FALSE)))
})

tbl <- do.call(rbind, tbl)
tbl$df <- factor(tbl$df)

ggplot(tbl, aes(x = x, y = log10Prob, col = df)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Spectral", direction = 1) +
  geom_vline(xintercept = threshold, linetype = "dashed") +
  labs(x = "x", y = "log10(P{ T_df > x})",
       title = "log10 Tail probabilities for t distribution (scaled)",
       col = "Deg. of freedom") +
  theme_bw()

#####
# 5. Same as 4, but for smaller deg of freedom.
#####
dfVal <- c(10, 5, 3, 2, 1, 0.5)
x <- 1:10
threshold <- 3

tbl <- lapply(dfVal, function(df) {
  scaleFactor <- getScaleFactor(df, threshold)
  data.frame(df = df,
             x = x,
             log10Prob = log10(pt(x / scaleFactor, df = df, lower.tail = FALSE)))
})

tbl <- do.call(rbind, tbl)
tbl$df <- factor(tbl$df)

ggplot(tbl, aes(x = x, y = log10Prob, col = df)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Spectral", direction = 1) +
  geom_vline(xintercept = threshold, linetype = "dashed") +
  labs(x = "x", y = "log10(P{ T_df > x})",
       title = "log10 Tail probabilities for t distribution (scaled)",
       col = "Deg. of freedom") +
  theme_bw()
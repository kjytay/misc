# This script is a timing simulation of glmnet with option type.gaussian="naive"
# vs. type.gaussian="covariance".

library(glmnet)
library(tidyverse)

# parameters
nlist <- c(100, 300, 1000, 3000, 10000)
plist <- c(100, 300, 1000, 3000)
nsim <- 5  # no. of simulations for each (n, p) combination
s <- 0.1  # level of sparsity: I assume that p * s is always an integer

# timing simulation
set.seed(1)
res <- matrix(NA, nrow = length(nlist) * length(plist) * nsim, ncol = 6)
rownum <- 0
for (n in nlist) {
    for (p in plist) {
        for (i in 1:nsim) {
            rownum <- rownum + 1
            cat("row", rownum, "n", n, "p", p, "simulation no.", i, fill = T)
            
            # create fake data
            x <- matrix(rnorm(n * p), nrow = n)
            beta <- matrix(c(2 + rnorm(p * s), rep(0, times = p * (1-s))), ncol = 1)
            y <- scale(x %*% beta + rnorm(n))
            row <- c(n, p, i)
            
            # run models
            naive_time <- system.time(glmnet(x, y, type.gaussian = "naive"))[1]
            cov_time <- system.time(glmnet(x, y, type.gaussian = "covariance"))[1]
            
            row <- c(row, naive_time, cov_time, naive_time / cov_time)
            res[rownum, ] <- row
        }
    }
}

# make results into a dataframe
df <- data.frame(res)
names(df) <- c("n", "p", "run", "naive", "covariance", "ratio")

# custom theme for plots
custom_theme <- theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
          axis.title = element_text(size = rel(1.2)),
          strip.text = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1))
    )

# boxplots of naive timing relative to covariance
df %>%
    ggplot(aes(x = factor(n), y = ratio)) +
    geom_boxplot() +
    labs(title = paste0("naive timing relative to covariance"),
         x = "n", y = "Ratio") +
    geom_hline(yintercept = 1, lty = 2) +
    facet_wrap(~ p, nrow = 1, labeller = label_both) +
    custom_theme

# points showing actual function run times
df_tidy <- df %>% select(-ratio) %>%
    gather(naive:covariance, key = "method", value = "time")
df_tidy %>%
    ggplot(aes(x = n, y = time, col = method)) +
    geom_point() +
    labs(title = paste0("Absolute fitting times"),
         x = "n", y = "Fitting time (s)") +
    facet_wrap(~ p, nrow = 1, labeller = label_both) +
    scale_y_log10() + scale_x_log10() +
    custom_theme

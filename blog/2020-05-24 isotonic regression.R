set.seed(1)
n <- 30
x <- 1:n
y <- rnorm(n) + 0.2 * x

# fit isotonic regression with isotone::gpava
library(isotone)
isofit <- gpava(x, y)
plot(x, y, pch = 4)
points(x, isofit$x, col = "blue", pch = 16)

# my version of isofit
# not robust! use isotone::gpava if you have to
my_isofit <- function(y) {
    n <- length(y)
    beta <- y
    
    # initialize
    blocks <- c()       # index of start of each block
    b_length <- c()     # no. of points in each block
    b_value <- c()      # value for each block
    b <- 0              # block no.
    beta_hist <- c()    # container for beta history
    
    for (i in 1:n) {
        # add beta[i] as a new block
        blocks <- c(blocks, i)
        b_length <- c(b_length, 1)
        b_value <- c(b_value, beta[i])
        b <- b + 1
        
        # add to history
        curr_beta <- rep(NA, length = n)
        curr_beta[1:i] <- beta[1:i]
        beta_hist <- c(beta_hist, curr_beta)
        
        # check for violations
        curr <- b
        while (curr > 1) {
            if (b_value[curr] < b_value[curr-1]) {
                # if violations: merge and average with previous block
                blocks <- blocks[1:(curr-1)]
                b_value[curr-1] <- (b_value[curr] * b_length[curr] + 
                                        b_value[curr-1] * b_length[curr-1]) / 
                    (b_length[curr] + b_length[curr-1])
                b_value <- b_value[1:(curr-1)]
                b_length[curr-1] <- b_length[curr-1] + b_length[curr]
                b_length <- b_length[1:(curr-1)]
                beta[blocks[curr-1]:i] <- b_value[curr-1]
                b <- b - 1
                
                # add to history
                curr_beta <- rep(NA, length = n)
                curr_beta[1:i] <- beta[1:i]
                beta_hist <- c(beta_hist, curr_beta)
                
                # decrement curr to check if we caused an earlier violation
                curr <- curr - 1
            } else {
                break
            }
        }
    }
    list(beta = beta, beta_hist = beta_hist)
}

z <- my_isofit(y)

# check agreement with gpava result
plot(isofit$x, z$beta)
abline(0, 1)

# make PAVA animation 
library(ggplot2)
library(gganimate)
df <- data.frame(iter = rep(1:(length(z$beta_hist) / n), each = n),
                 x = rep(1:n, length.out = length(z$beta_hist)),
                 beta = z$beta_hist)
truth_df <- data.frame(x = 1:n, y = y)

p <- ggplot(df, aes(x = x, y = beta)) +
    geom_point(col = "blue") +
    geom_point(data = truth_df, aes(x = x, y = y), shape = 4) +
    labs(title = "PAVA algorithm for isotonic regression") +
    transition_time(iter) +
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), face = "bold"))
animate(p, fps = 5)
#anim_save("pava.gif")

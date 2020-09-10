# returns the random walk path values as a vector 
# (random walk always starts at 0)
# p: probability of increasing by 1
# stop if path value hits either `lower` or `upper`
run <- function(p, lower, upper) {
    values <- c(0)
    current <- 0
    while (current > lower & current < upper) {
        current <- current + ifelse(runif(1) < p, 1, -1)
        values <- c(values, current)
    }
    values
}

#####
# paths without any color
#####
N <- 100  # no. of paths to simulate
p <- 0.48
lower <- -50
upper <- 50

# simulate paths
set.seed(1055)
vlist <- replicate(N, run(p, lower, upper))

# get length of longest path
max_length <- max(sapply(vlist, length))

# make plot
par(mar = rep(0, 4))  # no margins
plot(c(1, max_length), c(lower, upper), type = "n")
for (i in 1:N) {
    lines(1:length(vlist[[i]]), vlist[[i]])
}
abline(h = 0, lty = "dashed")
abline(h = lower, lwd = 2)
abline(h = upper, lwd = 2)

#####
# paths with color
#####
# choose color for each path based on (i) length of path, and 
# (ii) whether lower or upper limit was hit
colorPicker <- function(values, max_length,
                        ls_color = c(178, 34, 34), ll_color = c(255, 204, 0),
                        us_color = c(0, 0, 102), ul_color = c(102, 204, 225)) {
    l <- length(values)
    if (values[l] < 0) {
        rgb_values <- (ls_color + (ll_color - ls_color) * l / max_length) / 255
    } else {
        rgb_values <- (us_color + (ul_color - us_color) * l / max_length) / 255
    }
    rgb(rgb_values[1], rgb_values[2], rgb_values[3])
}

#####
# paths with color
#####
# make plot with color
plot(c(1, max_length), c(lower, upper), type = "n")
for (i in 1:N) {
    lines(1:length(vlist[[i]]), vlist[[i]], 
          col = colorPicker(vlist[[i]], max_length), lwd = 0.5)
}
abline(h = 0, lty = "dashed")
abline(h = lower, lwd = 2)
abline(h = upper, lwd = 2)

#####
# paths with color, different color scale
#####
plot(c(1, max_length), c(lower, upper), type = "n")
for (i in 1:N) {
    lines(1:length(vlist[[i]]), vlist[[i]], 
          col = colorPicker(vlist[[i]], max_length,
                            ls_color = c(230, 230, 230), 
                            ll_color = c(166, 166, 166),
                            us_color = c(255, 0, 0),
                            ul_color = c(0, 0, 255)),
          lwd = 0.5)
}
abline(h = 0, lty = "dashed")
abline(h = lower, lwd = 2)
abline(h = upper, lwd = 2)

#####
# p = 0.5, 0.52 (change line 91)
#####
N <- 100
p <- 0.5
lower <- -50
upper <- 50
set.seed(1055)
vlist <- replicate(N, run(p, lower, upper))
max_length <- max(sapply(vlist, length))

# make plot
par(mar = rep(0, 4))  # no margins
plot(c(1, max_length), c(lower, upper), type = "n")
for (i in 1:N) {
    lines(1:length(vlist[[i]]), vlist[[i]], 
          col = colorPicker(vlist[[i]], max_length), lwd = 0.5)
}
abline(h = 0, lty = "dashed")
abline(h = lower, lwd = 2)
abline(h = upper, lwd = 2)
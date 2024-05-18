# threshold: number of elements buffer can hold
# trajectory: if TRUE, return the estimate after every item in stream (default FALSE)
estimate_distinct_elements <- function(data_stream, threshold, trajectory = FALSE) {
  p <- 1
  m <- length(data_stream)
  buffer <- c()
  hit_fail_event <- FALSE
  estimates <- rep(NA, m)
  for (i in 1:m) {
    buffer <- setdiff(buffer, data_stream[i])
    if (runif(1) < p) buffer <- c(buffer, data_stream[i])
    if (length(buffer) == threshold) {
      buffer <- buffer[runif(threshold) < 0.5]
      p <- p / 2
      if (length(buffer) == threshold) {  # fail event
        hit_fail_event <- TRUE
      }
    }
    estimates[i] <- ifelse(hit_fail_event, -1, length(buffer) / p)
  }
  
  if (trajectory) {
    return(estimates)
  } else {
    return(estimates[m])
  }
}

# naive solution for getting exact element count after every item in stream
get_distinct_element_count <- function(data_stream) {
  m <- length(data_stream)
  buffer <- c()
  estimates <- rep(NA, m)
  for (i in 1:m) {
    buffer <- setdiff(buffer, data_stream[i])
    buffer <- c(buffer, data_stream[i])
    estimates[i] <- length(buffer)
  }
  estimates
}

# parameters
max_int <- 4000       # each element is randomly picked from [1, max_int]
stream_size <- 10000  # size of data stream
threshold <- 500      # size of buffer
n_simulations <- 10   # number of times to run the estimation algo
n_trajectories <- 10  # number of trajectories to plot

# run estimation algorithm several times for a given set of data
set.seed(1)
data_stream <- sample(max_int, size = stream_size, replace = TRUE)
true_final_distinct_count <- length(unique(data_stream))
true_distinct_count <- get_distinct_element_count(data_stream)
estimates <- t(replicate(n_simulations,
                         estimate_distinct_elements(data_stream, threshold, TRUE)))
final_estimates <- estimates[, stream_size]

# print some results
data_stream
true_distinct_count
true_final_distinct_count
summary(final_estimates)
sum(final_estimates < 0)  # number of fails

# Plot histogram of final estimates
#png("hist_10k_4k_50.png", width = 800, height = 800, res = 120)
hist(final_estimates,
     main = paste0("Histogram of estimates of # distinct elements\n",
                   "Stream length: ", stream_size, ", Buffer length: ", threshold),
     xlab = "Estimate of # distinct elements",
     breaks = 30)
abline(v = true_final_distinct_count, col = "black", lwd = 3)
abline(v = mean(final_estimates), col = "red", lwd = 2, lty = 2)    # mean
abline(v = median(final_estimates), col = "blue", lwd = 2, lty = 2) # median
abline(v = quantile(final_estimates, 0.25), col = "blue", lwd = 2, lty = 3) # 25th quantile
abline(v = quantile(final_estimates, 0.75), col = "blue", lwd = 2, lty = 3) # 75th quantile
legend("topright", legend = c("Truth", "Mean", "Median", "25th/75th quantile"),
       col = c("black", "red", "blue", "blue"), lty = c(1, 2, 2, 3), lwd = 2)
#dev.off()

# Plot trajectories
trajectory_estimates <- estimates[1:n_trajectories, , drop = FALSE]
max_estimate <- max(trajectory_estimates)
#png("lines_10k_4k_50.png", width = 800, height = 600, res = 120)
plot(1:stream_size, true_distinct_count, type = "l", col = "blue", lwd = 2,
     ylim = c(0, max_estimate),
     main = paste0("Trajectory of estimates\n",
                   "Stream length: ", stream_size, ", Buffer length: ", threshold),
     xlab = "Stream size", ylab = "Distinct count estimate"
)
for (i in 1:n_trajectories) {
  lines(1:stream_size, trajectory_estimates[i, ], lwd = 0.2)
}
#dev.off()

# Plot relative error trajectories
rel_error_estimates <- t(apply(trajectory_estimates, 1,
                             function(x) (x - true_distinct_count) / true_distinct_count) * 100)


max_rel_error_estimate <- max(rel_error_estimates)
min_rel_error_estimate <- min(rel_error_estimates)
#png("rel_error_10k_4k_50.png", width = 800, height = 600, res = 120)
plot(1:stream_size, rep(0, stream_size), type = "l", col = "blue", lwd = 2,
     ylim = c(min_rel_error_estimate, max_rel_error_estimate),
     main = paste0("Trajectory of estimate's relative error\n",
                   "Stream length: ", stream_size, ", Buffer length: ", threshold),
     xlab = "Stream size", ylab = "Relative error (%)"
)
for (i in 1:n_trajectories) {
  lines(1:stream_size, rel_error_estimates[i, ], lwd = 0.2)
}
#dev.off()
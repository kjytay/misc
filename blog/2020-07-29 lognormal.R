# Plot for varying SD
x_max <- 3
sd_vec <- c(0.25, 0.5, 1, 2)
col_vec <- c("red", "darkgreen", "blue", "black")

x <- seq(0, x_max, length.out = x_max * 100 + 1)
density_mat <- sapply(sd_vec, function(sd_i) dlnorm(x, meanlog = 0, sdlog = sd_i))

plot(c(0, x_max), c(0, max(density_mat)), type = "n",
     main = "Probability density function:\nmean = 0, varying SD",
     xlab = "x", ylab = "PDF")
for (i in 1:length(sd_vec)) {
  lines(x, density_mat[, i], col = col_vec[i], lwd = 2, lty = i)
}
legend("topright", legend = paste("SD =", sd_vec), col = col_vec, 
       lty = 1:length(sd_vec), cex = 0.7)

# Plot for varying mean
x_max <- 3
mean_vec <- c(-0.5, 0, 0.5, 1)
col_vec <- c("red", "darkgreen", "blue", "black")

x <- seq(0, x_max, length.out = x_max * 100 + 1)
density_mat <- sapply(mean_vec, 
                      function(mean_i) dlnorm(x, meanlog = mean_i, sdlog = 0))

plot(c(0, x_max), c(0, max(density_mat)), type = "n",
     main = "Probability density function:\nvarying mean, SD = 0",
     xlab = "x", ylab = "PDF")
for (i in 1:length(sd_vec)) {
  lines(x, density_mat[, i], col = col_vec[i], lwd = 2, lty = i)
}
legend("topright", legend = paste("mu =", mean_vec), col = col_vec, 
       lty = 1:length(mean_vec), cex = 0.7)


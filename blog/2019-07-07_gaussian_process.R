library(MASS)

# generate covariance matrix for points in `x` using given kernel function
cov_matrix <- function(x, kernel_fn, ...) {
    outer(x, x, function(a, b) kernel_fn(a, b, ...))
}

# given x coordinates, take N draws from kernel function at those points
draw_samples <- function(x, N, seed = 1, kernel_fn, ...) {
    Y <- matrix(NA, nrow = length(x), ncol = N)
    set.seed(seed)
    for (n in 1:N) {
        K <- cov_matrix(x, kernel_fn, ...)
        Y[, n] <- mvrnorm(1, mu = rep(0, times = length(x)), Sigma = K)
    }
    Y
}

x <- seq(0, 2, length.out = 201)  # x-coordinates
N <- 3  # no. of draws
col_list <- c("red", "blue", "black")  # for line colors

#####
# SQUARED EXPONENTIAL KERNEL
#####
se_kernel <- function(x, y, sigma = 1, length = 1) {
    sigma^2 * exp(- (x - y)^2 / (2 * length^2))
}

Y <- draw_samples(x, N, kernel_fn = se_kernel, length = 0.2)

#pdf("se_kernel0.pdf", width = 5, height = 4)
plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
     main = "SE kernel, length = 0.2")
for (n in 1:N) {
    lines(x, Y[, n], col = col_list[n], lwd = 1.5)
}
#dev.off()

#pdf("se_kernel.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
for (l in c(0.2, 0.7, 1.5)) {
    Y <- draw_samples(x, N, kernel_fn = se_kernel, length = l)
    
    plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
         main = paste("SE kernel, length =", l))
    for (n in 1:N) {
        lines(x, Y[, n], col = col_list[n], lwd = 1.5)
    }
}
#dev.off()

#####
# RATIONAL QUADRATIC KERNEL
#####
rq_kernel <- function(x, y, alpha = 1, sigma = 1, length = 1) {
    sigma^2 * (1 + (x - y)^2 / (2 * alpha * length^2))^(-alpha)
}

#pdf("rq_kernel.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
for (a in c(0.01, 0.5, 50)) {
    Y <- draw_samples(x, N, kernel_fn = rq_kernel, alpha = a)
    
    plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
         main = paste("RQ kernel, alpha =", a))
    for (n in 1:N) {
        lines(x, Y[, n], col = col_list[n], lwd = 1.5)
    }
}
#dev.off()

#####
# MATERN COVARIANCE
#####
matern_kernel <- function(x, y, nu = 1.5, sigma = 1, l = 1) {
    if (!(nu %in% c(0.5, 1.5, 2.5))) {
        stop("p must be equal to 0.5, 1.5 or 2.5")
    }
    p <- nu - 0.5
    d <- abs(x - y)
    if (p == 0) {
        sigma^2 * exp(- d / l)
    } else if (p == 1) {
        sigma^2 * (1 + sqrt(3)*d/l) * exp(- sqrt(3)*d/l)
    } else {
        sigma^2 * (1 + sqrt(5)*d/l + 5*d^2 / (3*l^2)) * exp(-sqrt(5)*d/l)
    }
}

#pdf("mat_kernel.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
for (nu in c(0.5, 1.5, 2.5)) {
    Y <- draw_samples(x, N, kernel_fn = matern_kernel, nu = nu)
    
    plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
         main = paste("Matern kernel, nu =", nu * 2, "/ 2"))
    for (n in 1:N) {
        lines(x, Y[, n], col = col_list[n], lwd = 1.5)
    }
}
#dev.off()

#####
# PERIODIC KERNEL
#####
period_kernel <- function(x, y, p = 1, sigma = 1, length = 1) {
    sigma^2 * exp(-2 * sin(pi * abs(x - y) / p)^2 / length^2)
}

#pdf("period_kernel.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
for (p in c(0.5, 1, 2)) {
    Y <- draw_samples(x, N, kernel_fn = period_kernel, p = p)
    
    plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
         main = paste("Periodic kernel, p =", p))
    for (n in 1:N) {
        lines(x, Y[, n], col = col_list[n], lwd = 1.5)
    }
}
#dev.off()

#####
# POLYNOMIAL KERNEL
#####
poly_kernel <- function(x, y, sigma = 1, d = 1) {
    (sigma^2 + x * y)^d
}

# linear kernel w different sigma
#pdf("linear_kernel.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
for (s in c(0.5, 1, 5)) {
    Y <- draw_samples(x, N, kernel_fn = poly_kernel, sigma = s)
    
    plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
         main = paste("Linear kernel, sigma =", s))
    for (n in 1:N) {
        lines(x, Y[, n], col = col_list[n], lwd = 1.5)
    }
}
#dev.off()

# poly kernel of different dimensions
#pdf("poly_kernel.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
for (d in c(1, 2, 3)) {
    Y <- draw_samples(x, N, kernel_fn = poly_kernel, d = d)
    
    plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
         main = paste("Polynomial kernel, d =", d))
    for (n in 1:N) {
        lines(x, Y[, n], col = col_list[n], lwd = 1.5)
    }
}
#dev.off()

#####
# BROWNIAN MOTION KERNEL
#####
bm_kernel <- function(x, y) {
    pmin(x, y)
}

Y <- draw_samples(x, N, kernel_fn = bm_kernel)

#pdf("bm_kernel.pdf", width = 5, height = 4)
par(mfrow = c(1, 1))
plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
     main = "Brownian motion kernel")
for (n in 1:N) {
    lines(x, Y[, n], col = col_list[n], lwd = 1.5)
}
#dev.off()

library(MASS)

cov_matrix <- function(x, kernel_fn) {
    outer(x, x, function(a, b) kernel_fn(a, b))
}

bm_kernel <- function(x, y) {
    pmin(x, y)
}



x <- seq(0, 1, length.out = 201)
K <- cov_matrix(x, bm_kernel)

set.seed(1)
y <- mvrnorm(1, mu = rep(0, times = length(x)), Sigma = K)
plot(x, y, type = "l")

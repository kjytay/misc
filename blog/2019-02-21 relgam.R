# Script to make animation showing how the fit changes as lambda changes.
# NOTE: This code WILL write out files to your system. Read through the code
# carefully and comment out the relevant lines if you do not want this to 
# happen!

library(magick)
library(relgam)

#####
# DATA PARAMETERS
#####
n = 100     # no. of training observations
p = 12      # no. of features
seed = 1    # random seed for simulation
snr = 4     # signal-to-noise ratio in response

# generate data: features and response
set.seed(seed)
x = matrix(rnorm((n) * p), ncol = p)
f4 = 2 * x[,4]^2 + 4 * x[,4] - 2
f5 = -2 * x[, 5]^2 + 2
f6 = 0.5 * x[, 6]^3
lin = rowSums(x[, 1:3])
mu = lin + f4 + f5 + f6
sigma <- sqrt(var(mu) / snr)
y = mu + sigma * rnorm(n)

# create CV folds
foldid <- sample(rep(seq(5), length = length(y)))

# fit model
rgamfit <- rgam(x, y, foldid = foldid, init_nz = 1:p, gamma = 0.6)

# iterate over lambda index, save frames as PDF
n_idx <- 30  # last lambda index that we will use in the animation
for (idx in 1:n_idx) {
    pdf(paste0("frame_", idx, ".pdf"), width=8,height=6)
    par(mfcol = c(3, 4))
    for (j in 1:p) {
        xx <- sort(x[,j])
        
        # in the below, the plot() command draws the model fit, while the
        # lines() command draws the truth
        if (j <= 3) {
            plot(rgamfit, x, which = j, index = idx, ylim = c(-4, 4))
            lines(xx, xx)
        } else if (j == 4) {
            plot(rgamfit, x, which = j, index = idx, ylim = c(-4.5, 9))
            lines(xx, 2 * xx^2 + 4 * xx - 2)
        } else if (j == 5) {
            plot(rgamfit, x, which = j, index = idx, ylim = c(-15, 3))
            lines(xx, -2 * xx^2 + 2)
        } else if (j == 6) {
            plot(rgamfit, x, which = j, index = idx, ylim = c(-8, 6))
            lines(xx, 0.5 * xx^3)
        } else {
            plot(rgamfit, x, which = j, index = idx, ylim = c(-4, 4))
            lines(xx, rep(0, length(xx)))
        }
    }
    dev.off()
}

# annotate frames
for (idx in 1:n_idx) {
    img <- image_read(paste0("frame_", idx, ".pdf"), density = 300)
    img %>% image_annotate(paste("Lambda index =", idx), size = 60, location = "+40+40") %>%
        image_write(paste0("frame_", idx, ".pdf"))
}

# create the animation (code takes a while to run)
files <- sapply(1:n_idx, function(idx) paste0("frame_", idx, ".pdf"))
image_read(files, density = 300) %>% image_animate(fps = 4) %>%
    image_write(path = "animation.gif")

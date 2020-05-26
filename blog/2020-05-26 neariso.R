library(neariso)
library(tidyverse)
library(gganimate)

# generate data
set.seed(51)
n <- 30
x <- 1:n
y <- rnorm(n) + 0.2 * x
plot(x, y)

# perform nearly isotonic fit
fit <- neariso(y)
dim(fit$beta)

# make data frames for animation 
df <- data.frame(iter = rep(1:ncol(fit$beta), each = n),
                 x = rep(1:n, times = ncol(fit$beta)),
                 beta = c(fit$beta))
truth_df <- data.frame(x = x, y = y)
lambda <- round(fit$lambda, 2)

# animated plot: transitions don't seem great
p <- ggplot(df, aes(x = x, y = beta)) +
    geom_path(col = "blue") +
    geom_point(data = truth_df, aes(x = x, y = y), shape = 4) +
    labs(title = "Nearly isotonic regression fits",
         subtitle = paste("Lambda = ", "{lambda[as.integer(closest_state)]}")) +
    transition_states(iter, transition_length = 1, state_length = 2) +
    theme_bw() + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold"))
animate(p, fps = 5)
#anim_save("neariso_animation2.gif")

# alternative method for making plot: you will have to uncomment the 
# lines below for it to work
library(magick)
for (idx in 1:ncol(fit$beta)) {
    ggplot(filter(df, iter == idx), aes(x = x, y = beta)) +
        geom_path(col = "blue") +
        geom_point(data = truth_df, aes(x = x, y = y), shape = 4) +
        labs(title = "Nearly isotonic regression fits",
             subtitle = paste("Lambda = ", lambda[idx])) +
        theme_bw() + 
        theme(plot.title = element_text(size = rel(1.5), face = "bold"))
    #ggsave(paste0("frame_", idx, ".pdf"))
}

# create the animation
files <- sapply(1:ncol(fit$beta), function(idx) paste0("frame_", idx, ".pdf"))
# image_read(files, density = 300) %>% image_animate(fps = 2) %>%
#     image_write(path = "neariso_animation.gif")

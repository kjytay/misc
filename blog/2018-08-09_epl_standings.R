library(tidyverse)
df <- read_csv("../data/epl_standings.csv")

# line plot of club rank by season
ggplot(df, aes(x = Season, y = Rank, col = Club)) +
    geom_line() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = 2008:2017, labels = 2008:2017) +
    labs(title = "Club rank by season")

# make a matrix with clubs as rows, seasons as columns, rank as matrix entry
df$Points <- NULL
df2 <- df %>% spread(key = Season, value = Rank)
clubs <- df2$Club
X <- as.matrix(df2[2:ncol(df2)])
X[is.na(X)] <- 21 # replace NAs with rank 25

# Get principal components of X matrix for plotting purposes
U <- svd(X)$u

# 2 clusters
set.seed(7)
km.out <- kmeans(X, 2, nstart = 20)

cluster_df <- data.frame(Club = clubs, 
                         Cluster = as.character(km.out$cluster))

# line plot, color by cluster membership
df %>% left_join(cluster_df, by = "Club") %>%
    ggplot(aes(x = Season, y = Rank, group = Club, col = Cluster)) +
    geom_line() +
    scale_x_continuous(breaks = 2008:2017, labels = 2008:2017) +
    labs(title = "Club rank by season") +
    theme(legend.position = "none")

# plot points in principal component space
cluster_df$PC1 <- U[,1]
cluster_df$PC2 <- U[,2]
g <- ggplot(cluster_df, aes(x = PC1, y = PC2, col = Cluster)) +
    geom_point(size = 2) +
    labs(title = "Plot of teams in PC space") +
    theme(legend.position = "none")

# plot above with team names
g + geom_text(aes(label = Club), size = 3, nudge_y = 0.02)


# 3 clusters
set.seed(7)
km.out <- kmeans(X, 3, nstart = 20)
cluster_df$Cluster <- as.character(km.out$cluster)

# line plot, color by cluster membership 
df %>% left_join(cluster_df, by = "Club") %>%
    ggplot(aes(x = Season, y = Rank, group = Club, col = Cluster)) +
    geom_line() +
    #theme(legend.position = "none") +
    scale_x_continuous(breaks = 2008:2017, labels = 2008:2017) +
    labs(title = "Club rank by season")

# plot points in principal component space (w team names)
ggplot(cluster_df, aes(x = PC1, y = PC2, col = Cluster)) +
    geom_point(size = 2) +
    geom_text(aes(label = Club), size = 3, nudge_y = 0.02) +
    labs(title = "Plot of teams in PC space") +
    theme(legend.position = "none")

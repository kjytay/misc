library(tidyverse)

# read in data files
game_df <- readRDS("../data/NBA-2018_game_data.rds")
master_list <- readRDS("../data/NBA-2018_box_score.rds")

##########
# Plots of points scored by two teammates
##########
team_name <- "Golden State Warriors"
player1 <- "Stephen Curry"
player2 <- "Kevin Durant"

# get game_ids
game_rows <- which(game_df$visitor_team_name == team_name |
                       game_df$home_team_name == team_name)
game_ids <- game_df[game_rows, "game_id"]

# set up points data frame
points_df <- data.frame(matrix(NA, ncol = 4, nrow = length(game_ids)))
names(points_df) <- c("game_id", "game_type", player1, player2)

for (i in 1:nrow(points_df)) {
    id <- game_ids[i]
    points_df[i, 1] <- id
    points_df[i, 2] <- game_df[game_rows[i], "game_type"]
    
    # get the correct basic box score
    if (game_df[game_rows[i], "visitor_team_name"] == team_name) {
        boxscore <- master_list[[id]]$visitor_basic_boxscore
    } else {
        boxscore <- master_list[[id]]$home_basic_boxscore
    }
    
    # get player points
    if (player1 %in% boxscore$Player) {
        points_df[i, 3] <- subset(boxscore, Player == player1)$PTS
    }
    if (player2 %in% boxscore$Player) {
        points_df[i, 4] <- subset(boxscore, Player == player2)$PTS
    }
}

ggplot(data = points_df, aes(x = get(player1), y = get(player2))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, col = "red", lty = 2) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = player1, y = player2, title = "Points scored") +
    coord_fixed()

# include games where only one of them play in the plot
points_df2 <- points_df[!(is.na(points_df[[3]]) & is.na(points_df[[4]])), ]
points_df2[is.na(points_df2)] <- 0
ggplot(data = points_df2, aes(x = get(player1), y = get(player2))) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, col = "red", lty = 2) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = player1, y = player2, title = "Points scored") +
    coord_fixed()

# compare games where both played vs. games where only one played
points_df2 <- points_df[!(is.na(points_df[[3]]) & is.na(points_df[[4]])), ]
points_df2[[paste(player1, "played")]] <- !is.na(points_df2[[3]])
points_df2[[paste(player2, "played")]] <- !is.na(points_df2[[4]])

ggplot(data = points_df2, aes(x = get(paste(player1, "played")), y = get(player2))) +
    geom_boxplot() +
    geom_jitter(height = 0, alpha = 0.5, col = "blue") +
    labs(x = paste(player1, "played?"), y = "Points scored", 
         title = paste("Points", player2, "scored"))
ggplot(data = points_df2, aes(x = get(paste(player2, "played")), y = get(player1))) +
    geom_boxplot() +
    geom_jitter(height = 0, alpha = 0.5, col = "blue") +
    labs(x = paste(player2, "played?"), y = "Points scored", 
         title = paste("Points", player1, "scored"))


##########
# Plots of ORtg and DRtg for two teammates
##########
points_df <- data.frame(matrix(NA, ncol = 6, nrow = length(game_ids)))
names(points_df) <- c("game_id", "game_type", 
                      sapply(c(player1, player2), function(x) paste(x, "ORtg")),
                      sapply(c(player1, player2), function(x) paste(x, "DRtg")))

for (i in 1:nrow(points_df)) {
    id <- game_ids[i]
    points_df[i, 1] <- id
    points_df[i, 2] <- game_df[game_rows[i], "game_type"]
    
    # get the correct advanced box score
    if (game_df[game_rows[i], "visitor_team_name"] == team_name) {
        boxscore <- master_list[[id]]$visitor_adv_boxscore
    } else {
        boxscore <- master_list[[id]]$home_adv_boxscore
    }
    
    # get player ORtg & DRtg
    if (player1 %in% boxscore$Player) {
        points_df[i, 3] <- subset(boxscore, Player == player1)$ORtg
        points_df[i, 5] <- subset(boxscore, Player == player1)$DRtg
    }
    if (player2 %in% boxscore$Player) {
        points_df[i, 4] <- subset(boxscore, Player == player2)$ORtg
        points_df[i, 6] <- subset(boxscore, Player == player2)$DRtg
    }
}

ggplot(data = points_df, aes(x = get(paste(player1, "ORtg")), 
                             y = get(paste(player2, "ORtg")))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, col = "red", lty = 2) +
    labs(x = player1, y = player2, title = "ORtg comparison") +
    coord_fixed()

ggplot(data = points_df, aes(x = get(paste(player1, "DRtg")), 
                             y = get(paste(player2, "DRtg")))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, col = "red", lty = 2) +
    labs(x = player1, y = player2, title = "DRtg comparison") +
    coord_fixed()


##########
# Quarter differentials
##########
team_name <- "Golden State Warriors"

game_rows <- which(game_df$visitor_team_name == team_name |
                       game_df$home_team_name == team_name)
game_ids <- game_df[game_rows, "game_id"]

# set up game data frame and play-by-play list
df <- data.frame(matrix(NA, ncol = 3, nrow = length(game_ids)))
names(df) <- c("game_id", "home", "win")
df$game_id <- game_ids
df$home <- game_df[game_rows, "home_team_name"] == team_name
df$win <- (df$home & game_df[game_rows, "home_pts"] > game_df[game_rows, "visitor_pts"]) |
    ((!df$home) & game_df[game_rows, "home_pts"] < game_df[game_rows, "visitor_pts"])

pbp_list <- list()
for (i in 1:nrow(df)) {
    id <- game_ids[i]
    pdp <- master_list[[id]]$pdp_df
    if (df[i, "home"]) {
        names(pdp) <- c("time", "opp", "team", "period")
    } else {
        names(pdp) <- c("time", "team", "opp", "period")
    }
    pbp_list[[id]] <- pdp
}

# NOTE: mistake in 201710200NOP. original data on website is wrong
# https://www.basketball-reference.com/boxscores/pbp/201710200NOP.html
# so we remove it
df <- df[-which(df$game_id == "201710200NOP"), ]
pbp_list[["201710200NOP"]] <- NULL

parse_pbp <- function(pbp) {
    pbp <- rbind(0, pbp)
    new_pbp <- pbp
    
    # get points scored in the period
    last_opp <- 0; last_team <- 0; last_period <- 0
    for (i in 2:nrow(pbp)) {
        if (pbp[i, "period"] > last_period + 1) {
            last_period <- last_period + 1
            last_opp <- pbp[i-1, "opp"]
            last_team <- pbp[i-1, "team"]
        }
        new_pbp$opp[i] <- pbp$opp[i] - last_opp
        new_pbp$team[i] <- pbp$team[i] - last_team
    }
    
    # add extra rows to denote beginning and end of periods
    num_period <- max(new_pbp$period)
    for (i in 1:num_period) {
        end_row <- new_pbp[max(which(new_pbp$period == i)), ]
        end_row[1] <- 12 * 60 * min(i, 4) + 5 * 60 * max(i-4, 0) 
        beg_row <- c(0, 0, 0, i)
        beg_row[1] <- 12 * 60 * min(i-1, 4) + 5 * 60 * max(i-1-4, 0) 
        new_pbp <- rbind(new_pbp, beg_row)
        new_pbp <- rbind(new_pbp, end_row)
    }
    new_pbp <- new_pbp[order(new_pbp$time), ]
    new_pbp$adv <- with(new_pbp, team - opp)
    new_pbp[-1, ]
}

pbp_list <- lapply(pbp_list, parse_pbp)

# build up a master play-by-play data frame
pbp_df <- data.frame(matrix(ncol = 6, nrow = 0))
names(pbp_df) <- c("game_id", "home", "win", "time", "period", "adv")
for (i in seq_along(pbp_list)) {
    xx <- pbp_list[[i]]
    xx$game_id <- df$game_id[i]
    xx$home <- df$home[i]
    xx$win <- df$win[i]
    pbp_df <- rbind(pbp_df, xx)
}

periods <- unique(pbp_df$period)
x_value <- ifelse(periods <= 4, 12 * 60 * periods, 
                  12 * 60 * 4 + 5 * 60 * (periods - 4))
x_label <- ifelse(periods <= 4, paste0("Q", periods), 
                  paste0("OT", periods - 4))

ggplot(pbp_df, aes(x = time, y = adv)) +
    geom_line(aes(col = win, group = interaction(game_id, period)), 
              lwd = 0.1) +
    geom_smooth(aes(group = period), col = "black", se = FALSE) +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    scale_color_manual(values = c("#ff6600", "#3366ff")) +
    coord_cartesian(ylim = c(-10, 10)) +
    labs(title = paste("Point Advantage by Quarter,", team_name)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom")

ggplot(pbp_df, aes(x = time, y = adv)) +
    geom_line(aes(col = win, group = interaction(game_id, period)), 
              lwd = 0.1) +
    geom_smooth(aes(group = period), col = "black", se = FALSE) +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    scale_color_manual(values = c("#ff6600", "#3366ff")) +
    coord_cartesian(ylim = c(-10, 10)) +
    facet_grid(win ~ .) +
    labs(title = paste("Point Advantage by Quarter,", team_name)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom")
    

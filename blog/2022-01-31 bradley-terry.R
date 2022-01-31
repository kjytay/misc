library(DBI)
library(tidyverse)
library(ggrepel)
library(plotROC)

seasonYear <- 2018  # represents 2018-2019 season

# this filepath specific to my local drive
mainFile <- "../data/nba-kaggle-wyattowalsh/basketball.sqlite"

# get all regular season games (only relevant columns 
# selected)
mydb <- dbConnect(RSQLite::SQLite(), mainFile)
df <- dbGetQuery(mydb, "SELECT * FROM Game")
dbDisconnect(mydb)
season_df <- df %>% mutate(GAME_DATE = as.Date(GAME_DATE),
                              SEASON = as.numeric(SEASON)) %>% 
  filter(SEASON == seasonYear) %>%
  select(GAME_DATE, TEAM_NAME_HOME, TEAM_NAME_AWAY, WL_HOME, WL_AWAY) %>%
  arrange(GAME_DATE)

head(season_df)

# get team abbreviations and names
team_abbrev_df <- df %>% select(team = TEAM_NAME_HOME, 
                                team_abbr = TEAM_ABBREVIATION_HOME) %>%
  distinct()
teams <- sort(unique(season_df$TEAM_NAME_HOME))

# get dataframe for Bradley-Terry model
get_data_vec <- function(home_team, away_team, teams) {
  vec <- rep(0, length(teams))
  vec[teams == home_team] <- 1
  vec[teams == away_team] <- -1
  vec
}

X <- apply(season_df, 1, 
           function(row) get_data_vec(row["TEAM_NAME_HOME"], 
                                      row["TEAM_NAME_AWAY"], 
                                      teams))
X <- t(X)
colnames(X) <- teams
dim(X)
y <- as.numeric(season_df$WL_HOME == "W")
bt_df <- as.data.frame(cbind(X, y))

# Bradley-Terry model with home advantage
bt_mod <- glm(y ~ ., data = bt_df, family = binomial())
summary(bt_mod)

# Compare BT coefficients with overall win percentage
coef_df <- data.frame(
  team = teams,
  beta = c(summary(bt_mod)$coefficients[2:length(teams), "Estimate"], 0)
)

# get team win percentages
home_df <- season_df %>% group_by(TEAM_NAME_HOME) %>%
  summarize(home_win  = sum(WL_HOME == "W"),
            home_loss = sum(WL_HOME == "L"))
away_df <- season_df %>% group_by(TEAM_NAME_AWAY) %>%
  summarize(away_win  = sum(WL_AWAY == "W"),
            away_loss = sum(WL_AWAY == "L"))
win_pct_df <- inner_join(home_df, away_df, 
                         by = c("TEAM_NAME_HOME" = "TEAM_NAME_AWAY")) %>%
  transmute(team = TEAM_NAME_HOME,
            win = home_win + away_win,
            loss = home_loss + away_loss) %>%
  mutate(win_pct = win / (win + loss)) %>%
  arrange(desc(win_pct)) %>%
  left_join(team_abbrev_df)

win_pct_df %>% inner_join(coef_df) %>%
  ggplot(aes(x = win_pct, y = beta)) +
  geom_point() +
  geom_text_repel(aes(label = team_abbr)) +
  labs(x = "Win percentage", y = "Bradley-Terry beta",
       title = "Bradley-Terry beta vs. Win %")

# Bradley-Terry model without home advantage
bt_mod1 <- glm(y ~ . + 0, data = bt_df, family = binomial())
coef_df1 <- data.frame(
  team = teams,
  beta = c(summary(bt_mod1)$coefficients[, "Estimate"], 0)
)
plot(coef_df$beta, coef_df1$beta, pch = 16,
     xlab = "Beta with home adv.",
     ylab = "Beta w/o home adv.")
abline(0, 1, col = "red", lty = 2)

# Predicting second half of season
n_train <- nrow(X) / 2
train_df <- bt_df[1:n_train, ]
test_df <- bt_df[(n_train + 1):nrow(X), ]

train_mod_home <- glm(y ~ ., data = train_df, family = binomial())
pred_home <- predict(train_mod_home, newdata = test_df, type = "response")
train_mod_no_home <- glm(y ~ . + 0, data = train_df, family = binomial())
pred_no_home <- predict(train_mod_no_home, newdata = test_df, type = "response")

# comparing predictions
plot(pred_home, pred_no_home, pch = 16, cex = 0.5,
     xlab = "Prob. of home win (model w. home adv.)",
     ylab = "Prob. of home win (model w/o home adv.)")
abline(0, 1)

# plot ROC curve
pred_df <- data.frame(
  truth = test_df$y,
  pred_home = pred_home,
  pred_no_home = pred_no_home
) %>%
  pivot_longer(pred_home:pred_no_home, 
               names_to = "model", 
               values_to = "prediction")

roc_plot <- ggplot(pred_df) +
  geom_roc(aes(d = truth, m = prediction, color = model), labels = FALSE)

roc_plot +
  labs(x = "True positive fraction",
       y = "False positive fraction",
       title = "Test set ROC plots") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom")

# compute AUC
calc_auc(roc_plot)

# compute Brier score
mean((pred_home - test_df$y)^2)
mean((pred_no_home - test_df$y)^2)

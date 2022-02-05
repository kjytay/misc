library(DBI)
library(plotROC)
library(readr)
library(tidyverse)

###
# get game data
###
# Download data from https://www.kaggle.com/wyattowalsh/basketball
seasonYear <- 2018  # represents 2018-2019 season

# this filepath specific to my local drive
mainFile <- "basketball.sqlite"

# get all regular season games (only relevant columns 
# selected)
mydb <- dbConnect(RSQLite::SQLite(), mainFile)
df <- dbGetQuery(mydb, "SELECT * FROM Game")
dbDisconnect(mydb)
season_df <- df %>% mutate(GAME_DATE = as.Date(GAME_DATE),
                           SEASON = as.numeric(SEASON)) %>% 
  filter(SEASON == seasonYear) %>%
  select(GAME_DATE, TEAM_NAME_HOME, TEAM_NAME_AWAY, WL_HOME) %>%
  arrange(GAME_DATE)

head(season_df)

###
# get betting odds data
###
# Download data from https://www.kaggle.com/erichqiu/nba-odds-and-scores
# 2018-19/vegas.txt
vegas <- read_csv("vegas.txt")

bet_df <- vegas %>% filter(Location == "home") %>%
  select(Date, Team, OppTeam, line = Average_Line_ML) %>%
  mutate(winProb = ifelse(line < 0, -line / (100 - line), 100 / (100 + line)))

head(bet_df)

###
# join the two data sources
###
season_teams <- sort(unique(season_df$TEAM_NAME_HOME))
bet_teams <- sort(unique(bet_df$Team))
names(bet_teams) <- season_teams

df <- season_df %>%
  transmute(Date = GAME_DATE,
            Team = bet_teams[TEAM_NAME_HOME],
            OppTeam = bet_teams[TEAM_NAME_AWAY],
            HomeWinLoss = WL_HOME) %>%
  left_join(bet_df)

# 4 games don't have betting odds data, impute 0.5
df$winProb[is.na(df$winProb)] <- 0.5

###
# fit Bradley-Terry models
###
# Get data in a form suitable for the BT model
get_data_vec <- function(home_team, away_team, teams) {
  vec <- rep(0, length(teams))
  vec[teams == home_team] <- 1
  vec[teams == away_team] <- -1
  vec
}

X <- apply(df, 1, 
           function(row) get_data_vec(row["Team"], 
                                      row["OppTeam"], 
                                      bet_teams))
X <- t(X)
colnames(X) <- bet_teams
dim(X)
y <- as.numeric(df$HomeWinLoss == "W")
bt_df <- as.data.frame(cbind(X, y))

# split into train and test
n_train <- nrow(X) / 2
train_df <- bt_df[1:n_train, ]
test_df <- bt_df[(n_train + 1):nrow(X), ]

# train BT models
train_mod_home <- glm(y ~ ., data = train_df, family = binomial())
train_mod_no_home <- glm(y ~ . + 0, data = train_df, family = binomial())

# get predictions
pred_home <- predict(train_mod_home, newdata = test_df, type = "response")
pred_no_home <- predict(train_mod_no_home, newdata = test_df, type = "response")
pred_bet <- df$winProb[(n_train + 1):nrow(X)]

# comparing predictions
plot(pred_home, pred_bet, pch = 16, cex = 0.5,
     xlab = "Prob. of home win (BT model w. home adv.)",
     ylab = "Prob. of home win (betting odds)")
abline(0, 1)

# plot ROC curve
pred_df <- data.frame(
  truth = test_df$y,
  pred_home = pred_home,
  pred_no_home = pred_no_home,
  pred_bet = pred_bet
) %>%
  pivot_longer(pred_home:pred_bet, 
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
mean((pred_bet - test_df$y)^2)
mean((pred_home - test_df$y)^2)
mean((pred_no_home - test_df$y)^2)

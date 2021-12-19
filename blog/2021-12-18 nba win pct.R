library(DBI)
library(tidyverse)

theme_set(theme_bw())

firstYear <- 1980  # represents 1980-1981 season

# these filepaths are specific to my local drive
mainFile <- "../data/nba-kaggle-wyattowalsh/basketball.sqlite"
finalsFile <- "../data/datatouille-nba-finals-and-mvps/data/data-updated.csv"

# get all regular season games (only relevant columns 
# selected)
mydb <- dbConnect(RSQLite::SQLite(), mainFile)
df <- dbGetQuery(mydb, "SELECT * FROM Game")
dbDisconnect(mydb)
regular_df <- df %>% mutate(GAME_DATE = as.Date(GAME_DATE),
                              SEASON = as.numeric(SEASON)) %>% 
  filter(SEASON >= firstYear) %>%
  select(SEASON, GAME_DATE, TEAM_NAME_HOME, TEAM_NAME_AWAY, WL_HOME, WL_AWAY) %>%
  arrange(SEASON, GAME_DATE)
head(regular_df)

# get NBA finals info
# season = year - 1 to match regular_df
finals_df <- read_csv(finalsFile) %>%
  select(season = year, nba_champion, nba_vice_champion) %>%
  mutate(season = season - 1) %>%
  filter(season >= firstYear)
names(finals_df) <- toupper(names(finals_df))
head(finals_df)

# join the two DFs, keep just the games where one of the teams was an NBA
# finalist
joined_df <- regular_df %>%
  inner_join(finals_df, by = "SEASON") %>%
  filter(TEAM_NAME_HOME == NBA_CHAMPION |
           TEAM_NAME_HOME == NBA_VICE_CHAMPION |
           TEAM_NAME_AWAY == NBA_CHAMPION |
           TEAM_NAME_AWAY == NBA_VICE_CHAMPION) %>%
  mutate(FOR_CHAMPION = TEAM_NAME_HOME == NBA_CHAMPION |
                                 TEAM_NAME_AWAY == NBA_CHAMPION,
         FOR_VICE_CHAMPION = TEAM_NAME_HOME == NBA_VICE_CHAMPION |
           TEAM_NAME_AWAY == NBA_VICE_CHAMPION)

# Compute win percentages across games for champions
champion_df <- joined_df %>% filter(FOR_CHAMPION) %>%
  select(SEASON, GAME_DATE, TEAM_NAME_HOME, WL_HOME, NBA_CHAMPION) %>%
  group_by(SEASON) %>%
  mutate(GAME = 1,
         WIN = ifelse((TEAM_NAME_HOME == NBA_CHAMPION & WL_HOME == "W") |
                        (TEAM_NAME_HOME != NBA_CHAMPION & WL_HOME == "L"),
                      1, 0)) %>%
  transmute(TEAM_TYPE = "Champion",
            SEASON = factor(SEASON),
            GAMES_PLAYED = cumsum(GAME),
            WINS = cumsum(WIN),
            WIN_PCT = WINS / GAMES_PLAYED)

# Compute win percentages across games for vice champions
vice_champion_df <- joined_df %>% filter(FOR_VICE_CHAMPION) %>%
  select(SEASON, GAME_DATE, TEAM_NAME_HOME, WL_HOME, NBA_VICE_CHAMPION) %>%
  group_by(SEASON) %>%
  mutate(GAME = 1,
         WIN = ifelse((TEAM_NAME_HOME == NBA_VICE_CHAMPION & WL_HOME == "W") |
                        (TEAM_NAME_HOME != NBA_VICE_CHAMPION & WL_HOME == "L"),
                      1, 0)) %>%
  transmute(TEAM_TYPE = "Vice Champion", 
            SEASON = factor(SEASON),
            GAMES_PLAYED = cumsum(GAME),
            WINS = cumsum(WIN),
            WIN_PCT = WINS / GAMES_PLAYED)

# put champion & vice champion data together
# final_win_pct_df is used for the year labels in the plots
win_pct_df <- rbind(champion_df, vice_champion_df)
final_win_pct_df <- win_pct_df %>% group_by(TEAM_TYPE, SEASON) %>%
  slice_tail(n = 1)

# plot for champions
ggplot(champion_df, aes(x = GAMES_PLAYED, y = WIN_PCT, col = SEASON)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 20, linetype = "dashed") +
  geom_text(data = filter(final_win_pct_df, TEAM_TYPE == "Champion"), 
            aes(x = max(GAMES_PLAYED), y = WIN_PCT, label = SEASON),
            hjust = 0, size = 3) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "none") +
  labs(title = "Win percentage by games played (champions)",
       x = "Games played", y = "Win percentage")
#ggsave("champions_win_pct.png", device = "png", width = 8, height = 6)

# plot for vice champions
ggplot(vice_champion_df, aes(x = GAMES_PLAYED, y = WIN_PCT, col = SEASON)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 20, linetype = "dashed") +
  geom_text(data = filter(final_win_pct_df, TEAM_TYPE == "Vice Champion"), 
            aes(x = max(GAMES_PLAYED), y = WIN_PCT, label = SEASON),
            hjust = 0, size = 3) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "none") +
  labs(title = "Win percentage by games played (vice champions)",
       x = "Games played", y = "Win percentage")
#ggsave("vice_champions_win_pct.png", device = "png", width = 8, height = 6)

# joint plot
ggplot(win_pct_df, aes(x = GAMES_PLAYED, y = WIN_PCT, col = TEAM_TYPE,
                             group = interaction(SEASON, TEAM_TYPE))) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 16, linetype = "dashed") +
  theme(legend.position = "bottom") +
  labs(title = "Win percentage by games played",
       x = "Games played", y = "Win percentage")

# plot where we start everyone off with 2 wins and 2 losses. This helps to
# reduce variability at the beginning of the graph, and doesn't change whether
# a team is above 50% or not.
win_pct_df %>%
  mutate(GAMES_PLAYED = GAMES_PLAYED + 4,
         WINS = WINS + 2,
         WIN_PCT = WINS / GAMES_PLAYED) %>%
  ggplot(aes(x = GAMES_PLAYED, y = WIN_PCT, col = TEAM_TYPE,
                       group = interaction(SEASON, TEAM_TYPE))) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 20, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Win percentage by games played",
       x = "Games played", y = "Win percentage")

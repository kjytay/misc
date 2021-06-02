library(tidyverse)

readRDS("nba_playoffs_data_2016-2020.rds") %>% head()

#   year    team1 wins1        team2 wins2 seed1 seed2
# 1 2016 Warriors     4      Rockets     1     1     8
# 2 2016 Clippers     2 TrailBlazers     4     4     5
# 3 2016  Thunder     4    Mavericks     1     3     6
# 4 2016    Spurs     4    Grizzlies     0     2     7
# 5 2016 Warriors     4 TrailBlazers     1     1     5
# 6 2016  Thunder     4        Spurs     2     3     2

results_df <- readRDS("nba_playoffs_data_2016-2020.rds") %>%
  mutate(higher_seed = pmin(seed1, seed2),
         lower_seed = pmax(seed1, seed2),
         n_games = wins1 + wins2) %>%
  mutate(seed_diff = lower_seed - higher_seed,
         higher_seed_wins = ifelse(higher_seed == seed1, wins1, wins2),
         lower_seed_wins = ifelse(higher_seed == seed1, wins2, wins1)) %>%
  mutate(series_winner = ifelse(wins1 > wins2, higher_seed, lower_seed))

# No. of series played between seeds
theme_set(theme_bw())
results_df %>%
  group_by(higher_seed, lower_seed) %>%
  summarize(n_games = n()) %>%
  ggplot(aes(x = higher_seed, y = lower_seed)) +
  geom_tile(aes(fill = n_games)) +
  geom_text(aes(label = n_games), col = "white") +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = 1:8) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient(low = "red", high = "blue", name = "# games") +
  labs(x = "Higher seed", y = "Lower seed",
       title = "# series played between seeds") +
  theme(panel.grid.major = element_blank())

# No. of games played between seeds
results_df %>%
  group_by(higher_seed, lower_seed) %>%
  summarize(n_games = sum(n_games)) %>%
  ggplot(aes(x = higher_seed, y = lower_seed)) +
  geom_tile(aes(fill = n_games)) +
  geom_text(aes(label = n_games), col = "white") +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = 1:8) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient(low = "red", high = "blue", name = "# games") +
  labs(x = "Higher seed", y = "Lower seed",
       title = "# games played between seeds") +
  theme(panel.grid.major = element_blank())

# win-loss percentage for each matchup
win_pct_df <- results_df %>%
  group_by(higher_seed, lower_seed) %>%
  summarize(higher_seed_wins = sum(higher_seed_wins),
            lower_seed_wins = sum(lower_seed_wins)) %>%
  mutate(higher_seed_win_pct = higher_seed_wins / 
           (higher_seed_wins + lower_seed_wins)) %>%
  select(higher_seed, lower_seed, higher_seed_win_pct)

head(win_pct_df)
# # A tibble: 6 x 3
# # Groups:   higher_seed [2]
#   higher_seed lower_seed higher_seed_win_pct
#         <dbl>      <dbl>               <dbl>
# 1           1          2               0.5  
# 2           1          3               0.75 
# 3           1          4               0.645
# 4           1          5               0.684
# 5           1          8               0.8  
# 6           2          3               0.554

ggplot(win_pct_df, aes(x = higher_seed, y = lower_seed)) +
  geom_tile(aes(fill = higher_seed_win_pct)) +
  geom_text(aes(label = round(higher_seed_win_pct, 2))) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = 1:8) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0.5, 
                       name = "Higher seed win %") +
  labs(x = "Higher seed", y = "Lower seed",
       title = "Higher seed win percentage") +
  theme(panel.grid.major = element_blank())

# one would expect something more like this
expand.grid(higher_seed = 1:8, lower_seed = 1:8) %>%
  filter(higher_seed < lower_seed) %>%
  mutate(seed_diff = lower_seed - higher_seed,
         higher_seed_win_pct = 0.5 + seed_diff * 0.4 / 7) %>%
  ggplot(aes(x = higher_seed, y = lower_seed)) +
  geom_tile(aes(fill = higher_seed_win_pct)) +
  geom_text(aes(label = round(higher_seed_win_pct, 2))) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = 1:8) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0.5, 
                       name = "Higher seed win %") +
  labs(x = "Higher seed", y = "Lower seed",
       title = "Expected higher seed win percentage") +
  theme(panel.grid.major = element_blank())

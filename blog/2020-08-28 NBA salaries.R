library(tidyverse)
theme_set(theme_bw())

# read in data
players <- read_csv("players.csv") %>% select(id, name)
salaries <- read_csv("salaries_1985to2018.csv") %>%
  inner_join(players, by = c("player_id" = "id"))

# no. of records by season
salaries %>% group_by(season_start) %>%
  count() %>%
  ggplot(aes(season_start, n)) +
  geom_col() +
  labs(x = "Year", y = "# of observations", 
       title = "# of observations by year")

# we only look at salaries from 2000 onwards
# drop and rename some columns
salaries <- salaries %>% filter(season_start >= 2000) %>%
  select(player_id, name, salary, year = season_start, team)

# count no. of teams by year
# looks correct
# (https://en.wikipedia.org/wiki/Expansion_of_the_National_Basketball_Association)
salaries %>% group_by(year) %>%
  summarize(n_teams = n_distinct(team)) %>%
  ggplot(aes(year, n_teams)) +
  geom_line() + 
  geom_point() +
  ylim(c(0, 30)) +
  labs(x = "Year", y = "# of teams",
       title = "# of teams by year")

# count no. of players by year
# 2013 looks fishy, but we will ignore for now
salaries %>% group_by(year, team) %>%
  count() %>%
  ggplot(aes(year, n, fill = team)) +
  geom_col(col = "black", size = 0.2) +
  labs(x = "Year", y = "# of players",
       title = "# of players by year") +
  theme(legend.position = "none")

# total salary by year
salaries %>% group_by(year) %>%
  summarize(tot_salary = sum(salary)) %>%
  ggplot(aes(year, tot_salary)) +
  geom_point() + geom_line() +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Total salary",
       title = "Total salary of all players by year")

# compare with constant inflation
tot_2000 <- salaries %>% filter(year == 2000) %>%
  summarize(tot_salary = sum(salary)) %>% pull()
inflation_df <- data.frame(year = 2000:2017,
                           inflation_amt = tot_2000 * 1.04^(0:17))
salaries %>% group_by(year) %>%
  summarize(tot_salary = sum(salary)) %>%
  ggplot(aes(year, tot_salary)) +
  geom_point() + geom_line() +
  geom_line(aes(year, inflation_amt), data = inflation_df, 
            col = "red", linetype = 2) +
  annotate("text", x = 2008, y = 2.6e9, 
           label = c("4% increase/yr"), color="red") +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Total salary",
       title = "Total salary of all players by year")

# total salary by year by team
salaries %>% group_by(year, team) %>%
  summarize(tot_salary = sum(salary)) %>%
  ggplot(aes(year, tot_salary)) +
  geom_line(aes(group = team), size = 0.1) +
  geom_smooth(size = 2, se = FALSE) +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Total salary",
       title = "# of players by year",
       subtitle = "One line per team") +
  theme(legend.position = "none")

# team ranking comparison by total salary by year
salaries %>% group_by(year, team) %>%
  summarize(tot_salary = sum(salary)) %>%
  arrange(year, desc(tot_salary)) %>%
  mutate(rank = row_number()) %>%
  group_by(team) %>%
  mutate(overall_rank = mean(rank)) %>%
  ggplot(aes(year, fct_reorder(team, overall_rank, .desc = TRUE))) +
  geom_tile(aes(fill = rank)) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1) +
  labs(x = "Year", y = NULL, 
       title = "Teams ranked by total salary by year") +
  theme(legend.position = "bottom")

# top paid player in each year
salaries %>% group_by(year) %>%
  top_n(salary, n = 1) %>%
  arrange(year)

salaries %>% group_by(year) %>%
  top_n(salary, n = 1) %>%
  ggplot(aes(year, salary)) +
  geom_line() + geom_point() +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Top salary",
       title = "Top salary by year")

# histogram of player salaries for 4 years
salaries %>% filter(year %in% c(2000, 2005, 2010, 2015)) %>%
  ggplot(aes(log10(salary))) +
  geom_histogram() +
  facet_wrap(~ year) +
  labs(x = "log10(Salary)", y = "Count",
       title = "Histogram of player salaries for select years")

# Lorenz curve for 4 years
salaries %>% filter(year %in% c(2000, 2005, 2010, 2015)) %>%
  arrange(year, salary) %>%
  group_by(year) %>%
  mutate(cum_salary = cumsum(salary),
         tot_salary = sum(salary),
         cum_n = row_number(),
         tot_n = n()) %>%
  mutate(cum_salary_prop = cum_salary / tot_salary * 100,
         cum_n_prop = cum_n / tot_n * 100) %>%
  ggplot(aes(cum_n_prop, cum_salary_prop, col = factor(year))) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Bottom x%", y = "% of total salary",
       title = "% of total salary made by bottom x% of players") +
  coord_equal() +
  theme(legend.title = element_blank())

# Gini index for each year
GetGini <- function(df) {
  x <- c(0, df$cum_n_prop) / 100
  y <- c(0, df$cum_salary_prop) / 100
  n <- length(x)
  1 - 2 * sum(sapply(1:(n-1), 
                     function(i) 0.5 * (x[i+1] - x[i]) * (y[i+1] + y[i])))
}
temp <- salaries %>% arrange(year, salary) %>%
  group_by(year) %>%
  mutate(cum_salary = cumsum(salary),
         tot_salary = sum(salary),
         cum_n = row_number(),
         tot_n = n()) %>%
  mutate(cum_salary_prop = cum_salary / tot_salary * 100,
         cum_n_prop = cum_n / tot_n * 100)
gini_vec <- unlist(lapply(split(temp, temp$year), GetGini))
gini_df <- data.frame(year = as.numeric(names(gini_vec)), gini = gini_vec)
ggplot(gini_df, aes(year, gini)) +
  geom_line() + geom_point() +
  expand_limits(y = c(0, 1)) +
  labs(x = "Year", y = "Gini index", 
       title = "Gini index of player salaries by year")

# OLS of gini index on year
summary(lm(gini ~ year, data = gini_df))

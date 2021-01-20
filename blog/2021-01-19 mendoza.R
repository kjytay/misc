library(Lahman)
library(tidyverse)
data(Batting)
data(People)

# add AVG to Batting
Batting$AVG <- with(Batting, H / AB)

# find Mario Mendoza in People
People %>% filter(nameFirst == "Mario" & nameLast == "Mendoza")
# his ID is mendoma01

Batting %>% filter(playerID == "mendoma01") %>%
  summarize(career_avg = sum(H) / sum(AB))
#   career_avg
# 1  0.2146597

# Batting average for players with >= 1000 AB
avg_df <- Batting %>% group_by(playerID) %>%
  summarize(tot_AB = sum(AB), career_avg = sum(H) / sum(AB)) %>%
  filter(tot_AB >= 1000) %>%
  left_join(People, by = "playerID") %>%
  select(playerID, tot_AB, career_avg, nameFirst, nameLast) %>%
  arrange(desc(career_avg))

# top 10
head(avg_df, n = 10)
# # A tibble: 10 x 5
#    playerID  tot_AB career_avg nameFirst    nameLast 
#    <chr>      <int>      <dbl> <chr>        <chr>    
#  1 cobbty01   11436      0.366 Ty           Cobb     
#  2 barnero01   2391      0.360 Ross         Barnes   
#  3 hornsro01   8173      0.358 Rogers       Hornsby  
#  4 jacksjo01   4981      0.356 Shoeless Joe Jackson  
#  5 meyerle01   1443      0.356 Levi         Meyerle  
#  6 odoulle01   3264      0.349 Lefty        O'Doul   
#  7 delahed01   7510      0.346 Ed           Delahanty
#  8 mcveyca01   2513      0.346 Cal          McVey    
#  9 speaktr01  10195      0.345 Tris         Speaker  
# 10 hamilbi01   6283      0.344 Billy        Hamilton 

# bottom 10
tail(avg_df, n = 10)
# # A tibble: 10 x 5
#    playerID  tot_AB career_avg nameFirst nameLast
#    <chr>      <int>      <dbl> <chr>     <chr>   
#  1 seaveto01   1315      0.154 Tom       Seaver  
#  2 donahre01   1150      0.152 Red       Donahue 
#  3 fellebo01   1282      0.151 Bob       Feller  
#  4 grovele01   1369      0.148 Lefty     Grove   
#  5 suttodo01   1354      0.144 Don       Sutton  
#  6 amesre01    1014      0.141 Red       Ames    
#  7 faberre01   1269      0.134 Red       Faber   
#  8 perryga01   1076      0.131 Gaylord   Perry   
#  9 pappami01   1073      0.123 Milt      Pappas  
# 10 frienbo01   1137      0.121 Bob       Friend  

# How far down was Mario Mendoza?
which(avg_df$playerID == "mendoma01") / nrow(avg_df)
# [1] 0.9630212

# how many ABs did Mendoza have
avg_df[which(avg_df$playerID == "mendoma01"), ]
# # A tibble: 1 x 5
#     playerID  tot_AB career_avg nameFirst nameLast
#     <chr>      <int>      <dbl> <chr>     <chr>   
#   1 mendoma01   1337      0.215 Mario     Mendoza 

# Look at player-seasons with at least 100 ABs
batting_df <- Batting %>% filter(AB >= 100)

#####
# HOW MANY PLAYERS FALL BELOW THE MENDOZA LINE?
#####
batting_df %>% group_by(yearID) %>%
  summarize(below200 = mean(AVG < 0.200)) %>%
  ggplot(aes(yearID, below200)) +
  geom_line() +
  labs(title = "Proportion of players below Mendoza line by year",
       x = "Year", y = "Prop. below .200") +
  theme_bw()

#####
# BATTING AVERAGE FOR BOTTOM 5%
#####
batting_df %>% group_by(yearID) %>%
  summarize(bottom5 = quantile(AVG, 0.05)) %>%
  ggplot(aes(yearID, bottom5)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = c(0.2), color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 0.24)) +
  labs(title = "Batting average of 5th quantile by year",
       x = "Year", y = "5th quantile batting average") +
  theme_bw()

#####
# BATTING AVERAGE FOR OTHER QUANTILES
#####
batting_df %>% group_by(yearID) %>%
  summarize(AVG = quantile(AVG, c(0.05, 1:4 / 5, 0.95)),
            quantile = c(0.05, 1:4 / 5, 0.95)) %>%
  mutate(quantile = factor(quantile, levels = c(0.95, 4:1 / 5, 0.05))) %>%
  ggplot(aes(x = yearID, y = AVG, col = quantile)) +
  geom_line() +
  geom_hline(yintercept = c(0.2), color = "red", linetype = "dashed") +
  labs(title = "Batting average for various quantiles by year",
       x = "Year", y = "Quantile batting average") +
  theme_bw()

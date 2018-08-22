library(tidyverse)
standings <- read_csv("../data/epl_standings.csv")

# Chelsea/Man City/Man U: Most championships in last 10 years
standings %>% filter(Rank == 1) %>%
    group_by(Club) %>%
    summarize(Champions = n()) %>%
    arrange(desc(Champions))

# Chelsea/Man City: Most top 3 finishes in last 10 years
standings %>% filter(Rank <= 3) %>%
    group_by(Club) %>%
    summarize(`Top 3` = n()) %>%
    arrange(desc(`Top 3`))

# Arsenal/Man City: Most top 5 finishes in last 10 years
standings %>% filter(Rank <= 5) %>%
    group_by(Club) %>%
    summarize(`Top 5` = n()) %>%
    arrange(desc(`Top 5`))

# Man U: Most points in last 10 years
standings %>% group_by(Club) %>%
    summarize(`Total Points` = sum(Points)) %>%
    arrange(desc(`Total Points`))

# Man U: Best median rank in last 10 years
standings %>% 
    group_by(Club) %>%
    summarize(`Median Rank` = median(Rank)) %>%
    arrange(`Median Rank`)

# Man City: Best median rank in last 5 years
standings %>% 
    group_by(Club) %>% filter(Season >= 2013) %>%
    summarize(`Median Rank` = median(Rank)) %>%
    arrange(`Median Rank`)

# Arsenal: Best worst ranking in last 10 years
standings %>% 
    group_by(Club) %>%
    summarize(`Worst Rank` = max(Rank)) %>%
    arrange(`Worst Rank`)

# Tottenham Hotspur: Best worst ranking in last 3 years
standings %>% filter(Season >= 2015) %>%
    group_by(Club) %>%
    summarize(`Worst Rank` = max(Rank)) %>%
    arrange(`Worst Rank`)

# Arsenal: Most consistent in ranking (SD)
standings %>% 
    filter(Club %in% c("Arsenal", "Chelsea", "Everton", 
                       "Liverpool", "Manchester United", 
                       "Manchester City", "Tottenham Hotspur")) %>%
    group_by(Club) %>%
    summarize(sd = sd(Rank)) %>%
    arrange(sd)

# Liverpool: Most consistent in ranking (linear regression coef)
standings %>% 
    filter(Club %in% c("Arsenal", "Chelsea", "Everton", 
                       "Liverpool", "Manchester United", 
                       "Manchester City", "Tottenham Hotspur")) %>%
    group_by(Club) %>%
    summarize(beta = lm(Rank ~ Season)$coefficients[2]) %>%
    arrange(abs(beta))

# Tottenham Hotspur: Most improved team in last 5 years (linear regression coef)
standings %>% filter(Season >= 2013) %>%
    filter(Club %in% c("Arsenal", "Chelsea", "Everton", 
                       "Liverpool", "Manchester United", 
                       "Manchester City", "Tottenham Hotspur")) %>%
    group_by(Club) %>%
    summarize(beta = lm(Rank ~ Season)$coefficients[2]) %>%
    arrange(beta)
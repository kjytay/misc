# This script scrapes top-level NBA schedule and results from basketball-reference.com.
# User can set year and list of months to determine the window of games to scrape.
# At the end of the script, I reconstruct the conference standings based on W-L
# percentage.

library(rvest)
library(lubridate)

########
# PARAMETERS
########
year <- "2018"
monthList <- c("october", "november", "december", "january", "february", 
               "march", "april", "may", "june")
playoff_startDate <- ymd("2018-04-14")
outputfile <- "NBA-2018_game_data.rds"

########
# SCRIPT FOR SCRAPING DATA STARTS HERE
########
df <- data.frame()
for (month in monthList) {
    # get webpage
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                  "_games-", month, ".html")
    webpage <- read_html(url)
    
    # get column names
    col_names <- webpage %>% 
        html_nodes("table#schedule > thead > tr > th") %>% 
        html_attr("data-stat")    
    col_names <- c("game_id", col_names)
    
    # extract dates column
    # note that in april, there is a break in the table which just says 
    # "Playoffs". this messes with the data merging later, so we get rid of it
    dates <- webpage %>% 
        html_nodes("table#schedule > tbody > tr > th") %>% 
        html_text()
    dates <- dates[dates != "Playoffs"]
    
    # extract game id
    # we need to remove the NA that is due to the "Playoffs" row in april
    game_id <- webpage %>% 
        html_nodes("table#schedule > tbody > tr > th") %>%
        html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    
    # extract all columns (except date)
    data <- webpage %>% 
        html_nodes("table#schedule > tbody > tr > td") %>% 
        html_text() %>%
        matrix(ncol = length(col_names) - 2, byrow = TRUE)
    
    # combine game IDs, dates and columns in dataframe for this month, add col names
    month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
    names(month_df) <- col_names
    
    # add to overall dataframe
    df <- rbind(df, month_df)
}

# change columns to the correct types
df$visitor_pts <- as.numeric(df$visitor_pts)
df$home_pts    <- as.numeric(df$home_pts)
df$attendance  <- as.numeric(gsub(",", "", df$attendance))
df$date_game   <- mdy(df$date_game)

# add column to indicate if regular season or playoff
df$game_type <- with(df, ifelse(date_game >= playoff_startDate, 
                                "Playoff", "Regular"))

# drop boxscore column
df$box_score_text <- NULL

# save to file
#saveRDS(df, outputfile)

########
# SCRIPT FOR RANKING TABLE STARTS HERE
########

# get winner and loser of each game
df$winner <- with(df, ifelse(visitor_pts > home_pts, 
                             visitor_team_name, home_team_name))
df$loser <- with(df, ifelse(visitor_pts < home_pts, 
                             visitor_team_name, home_team_name))

# build up standings table for regular season
regular_df <- subset(df, game_type == "Regular")
teams <- sort(unique(regular_df$visitor_team_name))
standings <- data.frame(team = teams, stringsAsFactors = FALSE)

# conference & division information: manually input
standings$conf <- c("East", "East", "East", "East", "East",
                    "East", "West", "West", "East", "West",
                    "West", "East", "West", "West", "West",
                    "East", "East", "West", "West", "East",
                    "West", "East", "East", "West", "West",
                    "West", "West", "East", "West", "East")
standings$div <- c("Southeast", "Atlantic", "Atlantic", "Southeast", "Central",
                   "Central", "Southwest", "Northwest", "Central", "Pacific",
                   "Southwest", "Central", "Pacific", "Pacific", "Southwest",
                   "Southeast", "Central", "Northwest", "Southwest", "Atlantic",
                   "Northwest", "Southeast", "Atlantic", "Pacific", "Northwest",
                   "Pacific", "Southwest", "Atlantic", "Northwest", "Southeast")

# populate W-L column, W pct
standings$win <- 0; standings$loss <- 0
for (i in 1:nrow(standings)) {
    standings$win[i]  <- sum(regular_df$winner == standings$team[i])
    standings$loss[i] <- sum(regular_df$loser  == standings$team[i])
}
standings$wl_pct <- with(standings, win / (win + loss))

# Eastern conference standings
east_standings <- subset(standings, conf == "East")
east_standings[with(east_standings, order(-wl_pct, team)), 
               c("team", "win", "loss")]

# Western conference standings
west_standings <- subset(standings, conf == "West")
west_standings[with(west_standings, order(-wl_pct, team)), 
               c("team", "win", "loss")]

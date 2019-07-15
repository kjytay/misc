library(tidyverse)
library(choroplethr)
library(choroplethrMaps)


df <- read_csv("openfema_claims20190331.csv", 
               col_types = cols(asofdate = col_date(format = "%Y-%m-%d"), 
                                dateofloss = col_date(format = "%Y-%m-%d"), 
                                originalconstructiondate = col_date(format = "%Y-%m-%d"), 
                                originalnbdate = col_date(format = "%Y-%m-%d")))

str(df)

##########
# data wrangling
##########

# select just the columns we want and remove rows with no state info or
# negative claim amounts
small_df <- df %>% 
    select(year = yearofloss, state, countycode, 
           claim_bldg = amountpaidonbuildingclaim, 
           claim_contents = amountpaidoncontentsclaim, 
           claim_ICC = amountpaidonincreasedcostofcomplianceclaim) %>% 
    filter(!is.na(state)) %>%
    replace_na(list(claim_bldg = 0, claim_contents = 0, claim_ICC = 0)) %>%
    filter(!(claim_bldg < 0 | claim_contents < 0 | claim_ICC < 0))

# remove original (large) df to save space
rm(df)

# histogram of year
with(small_df, hist(year, breaks = c(min(year):max(year)), 
                    main = "No. of claims / year", xlab = "Year"))

# filter for just years between 1978 and 2018 inclusive, 
# add total cost column
small_df <- small_df %>% filter(year >= 1978 & year <= 2018) %>%
    mutate(total_cost = claim_bldg + claim_contents + claim_ICC)

##########
# make plot of total no. of claims by state
##########
state_no_claims_df <- small_df %>% group_by(state) %>%
    summarize(claim_cnt = n())

# map to state name
data(state.regions)
state_no_claims_df$region <- plyr::mapvalues(state_no_claims_df$state, 
                                             from = state.regions$abb, 
                                             to = state.regions$region)

# rename columns for choroplethr
names(state_no_claims_df) <- c("state", "value", "region")

# basic map
state_choropleth(state_no_claims_df)

# above and below median
state_choropleth(state_no_claims_df, num_colors = 2)

# continuous scale
state_choropleth(state_no_claims_df, num_colors = 1)

# one with title
state_choropleth(state_no_claims_df, num_colors = 1, 
                 title = "No. of claims by state", legend = "No. of claims")

# choroplethr output is a ggplot object
gg1 <- state_choropleth(state_no_claims_df, num_colors = 1, 
                        title = "No. of claims by state", legend = "No. of claims")
class(gg1)

gg1 + scale_fill_distiller(palette = "Spectral") +
    theme(plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5))

# zoom in on gulf states: have a shoreline on gulf of mexico
gulf_states <- c("texas", "louisiana", "mississippi", "alabama", "florida")
state_choropleth(state_no_claims_df, num_colors = 1, 
                 title = "No. of claims by state (Gulf states)", legend = "No. of claims",
                 zoom = gulf_states)

# bar plot of no. of claims
ggplot(data = state_no_claims_df %>% arrange(desc(value)) %>% head(n = 10)) +
    geom_bar(aes(x = reorder(state, -value), y = value), stat = "identity",
             fill = "gray", col = "black") +
    geom_abline(intercept = 100000, slope = 0, col = "red", lty = 2) +
    labs(x = "State", y = "No. of claims", title = "Top 10 states by no. of claims")

##########
# make plot of total claim amounts by state
##########
state_claims_amt_df <- small_df %>% group_by(state) %>%
    summarize(value = log10(sum(total_cost)))

# map to state name
state_claims_amt_df$region <- plyr::mapvalues(state_claims_amt_df$state, 
                                              from = state.regions$abb, 
                                              to = state.regions$region)

state_choropleth(state_claims_amt_df, num_colors = 1,
                 title = "log10(Claim amount) by state", legend = "") +
    scale_fill_distiller(palette = "Spectral") +
    theme(plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5))

# bar plot of claim amt
ggplot(data = state_claims_amt_df %>% arrange(desc(value)) %>% head(n = 10)) +
    geom_bar(aes(x = reorder(state, -value), y = 10^value), stat = "identity", 
             fill = "gray", col = "black") +
    geom_abline(intercept = 10^9, slope = 0, col = "red", lty = 2) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(x = "State", y = "Total claims", title = "Top 10 states by total claims")

##########
# county maps
##########
county_summary_df <- small_df %>% group_by(countycode) %>%
    summarize(claim_cnt = n(), claim_amt = sum(total_cost)) %>%
    mutate(countycode = as.numeric(countycode))

names(county_summary_df) <- c("region", "value", "claim_amt")

county_choropleth(county_summary_df)

# change fill color for NAs to be white
county_choropleth(county_summary_df, title = "No. of claims by county") +
    scale_fill_brewer(na.value = "white")

# zoom into florida only
county_choropleth(county_summary_df, title = "No. of claims by county (Florida)",
                  state_zoom = "florida") +
    scale_fill_brewer(na.value = "white")

# zoom into gulf
county_choropleth(county_summary_df, title = "No. of claims by county (Gulf states)",
                  state_zoom = gulf_states) +
    scale_fill_brewer(na.value = "white")

##########
# from here, we work with just the top 5 states (which happen to be the same)
##########
top_states <- (state_no_claims_df %>% arrange(desc(value)) %>% head(n = 5))[[1]]
top_df <- small_df %>% filter(state %in% top_states)
top_regions <- plyr::mapvalues(top_states, from = state.regions$abb, 
                               to = state.regions$region)

state_yr_summary_df <- top_df %>% group_by(state, year) %>%
    summarize(claim_cnt = n(), claim_amt = sum(total_cost))

ggplot(data = state_yr_summary_df, aes(x = year, y = claim_cnt, col = state)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(x = "Year", y = "No. of claims", title = "No. of claims by state") +
    theme_bw()

ggplot(data = state_yr_summary_df, aes(x = year, y = claim_amt, col = state)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(x = "Year", y = "Claim amount", title = "Claim amount by state") +
    theme_bw()

# This script plots fire stations from Santa Clara county on a map as fire
# station emojis. Demonstrates the use of the emoGG package.
# Fire station data taken from 
# https://hifld-geoplatform.opendata.arcgis.com/datasets/fire-stations

library(tidyverse)
library(readr)
library(emoGG)
library(maps)

# get coordinates for Santa Clara County outline from maps package
map_county <- map_data("county") %>% 
    filter(region == "california" & subregion == "santa clara")

map_theme <- theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white")
)

# outline of santa clara county
ggplot(data = map_county, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", col = "black") +
    coord_quickmap() +
    map_theme

# read in fire station data
df <- read_csv("Fire_Stations.csv")
small_df <- df %>% select(STATE, COUNTY, X, Y) %>% 
    filter(STATE == "CA" & COUNTY == "SANTA CLARA")

# fire stations as points
ggplot() +
    geom_polygon(data = map_county, aes(x = long, y = lat),
                 fill = "white", col = "black") +
    geom_point(data = small_df, mapping = aes(X, Y)) + 
    coord_quickmap() +
    map_theme

# fire stations as emojis
ggplot() +
    geom_polygon(data = map_county, aes(x = long, y = lat),
                 fill = "white", col = "black") +
    geom_emoji(data = small_df, mapping = aes(X, Y), emoji = "1f692", size = 0.025) + 
    coord_quickmap() +
    map_theme

# with stamen map background
library(ggmap)
height <- max(map_county$lat) - min(map_county$lat)
width <- max(map_county$long) - min(map_county$long)
sc_borders <- c(bottom  = min(map_county$lat)  - 0.05 * height, 
                 top     = max(map_county$lat)  + 0.05 * height,
                 left    = min(map_county$long) - 0.05 * width,
                 right   = max(map_county$long) + 0.05 * width)
map <- get_stamenmap(sc_borders, maptype = "terrain")

ggmap(map) +
    geom_polygon(data = map_county, aes(x = long, y = lat),
                 fill = NA, col = "red", linetype = 2, size = 1) +
    geom_emoji(data = small_df, mapping = aes(X, Y), emoji = "1f46e", size = 0.025) + 
    map_theme
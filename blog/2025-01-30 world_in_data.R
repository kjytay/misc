library(tidyverse)
library(httr)
library(jsonlite)

###
# GET METADATA
###
# define URL and query parameters
url <- "https://ourworldindata.org/grapher/average-monthly-surface-temperature.metadata.json"
query_params <- list(
  v = "1",
  csvType = "full",
  useColumnShortNames = "true"
)

# get metadata
headers <- add_headers(`User-Agent` = "Our World In Data data fetch/1.0")
response <- GET(url, query = query_params, headers)
metadata <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

# view the metadata keys
names(metadata)

#> [1] "chart"          "columns"        "dateDownloaded" "activeFilters" 

names(metadata$columns$temperature_2m)
#>  [1] "titleShort"            "titleLong"             "descriptionShort"      "descriptionProcessing" "shortUnit"             "unit"                 
#>  [7] "timespan"              "type"                  "owidVariableId"        "shortName"             "lastUpdated"           "citationShort"        
#> [13] "citationLong"          "fullMetadata"   


glimpse(metadata$chart)
#> List of 5
#>  $ title           : chr "Average monthly surface temperature"
#>  $ subtitle        : chr "The temperature of the air measured 2 meters above the ground, encompassing land, sea, and in-land water surfaces."
#>  $ citation        : chr "Contains modified Copernicus Climate Change Service information (2025)"
#>  $ originalChartUrl: chr "https://ourworldindata.org/grapher/average-monthly-surface-temperature?v=1&csvType=full&useColumnShortNames=true"
#>  $ selection       : chr "World"


###
# GET DATASET EXAMPLE 1
###
# define the URL and query parameters
url <- "https://ourworldindata.org/grapher/average-monthly-surface-temperature.csv"
query_params <- list(
  v = "1",
  csvType = "filtered",
  useColumnShortNames = "true",
  tab = "chart",
  country = "USA"
)

# get data
headers <- add_headers(`User-Agent` = "Our World In Data data fetch/1.0")
response <- GET(url, query = query_params, headers)
df <- read_csv(content(response, as = "text", encoding = "UTF-8"))

head(df)
#> # A tibble: 6 × 6
#>   Entity        Code   year Day        temperature_2m...5 temperature_2m...6
#>   <chr>         <chr> <dbl> <date>                  <dbl>              <dbl>
#> 1 United States USA    1941 1941-12-15            -1.88                 8.02
#> 2 United States USA    1942 1942-01-15            -4.78                 7.85
#> 3 United States USA    1942 1942-02-15            -3.87                 7.85
#> 4 United States USA    1942 1942-03-15             0.0978               7.85
#> 5 United States USA    1942 1942-04-15             7.54                 7.85
#> 6 United States USA    1942 1942-05-15            12.1                  7.85

plot(df$Day, df$temperature_2m...5, type = "l",
     main = "USA ave monthly temperature",
     xlab = "Date", ylab = "Temperature in C")


###
# GET DATASET EXAMPLE 2
###
# define the URL and query parameters
url <- "https://ourworldindata.org/grapher/average-monthly-surface-temperature.csv"
query_params <- list(
  v = "1",
  csvType = "filtered",
  useColumnShortNames = "true",
  tab = "chart",
  time = "2001-05-15..2024-05-15",
  country = "OWID_WRL~THA"
)

# get data
headers <- add_headers(`User-Agent` = "Our World In Data data fetch/1.0")
response <- GET(url, query = query_params, headers)
df <- read_csv(content(response, as = "text", encoding = "UTF-8"))
ggplot(df) +
  geom_line(aes(x = Day, y = temperature_2m...5, col = Entity)) +
  labs(title = "Ave monthly temperature for World & Thailand",
       x = "Date", y = "Temperature in C") +
  theme_bw() +
  theme(legend.position = "bottom")

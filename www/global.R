library(readr)
library(here)
# Main data sets
summary_all <- read_csv(here("www", "data", "data-for-app.csv"))
map.data <- read_csv(here("www","data", "mapping","site-data_with_counties.csv"))

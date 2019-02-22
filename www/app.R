############    #
# RUN THE APP!  #
#################

#### LOAD PACKAGES
library(tidyverse) # For reading and manipulating data
library(here) # to deal with tehfact we are using a sub directory (www)

#### SET DIRECTORY
#setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www")
#setwd("www")

#### READ DATA
summary_all <- read_csv(here("www", "data", "data-for-app.csv"))
map.data <- read_csv(here("www","data", "mapping/site-data_with_counties.csv"))

#### RUN APP
runApp(here("www",'appScripts'))

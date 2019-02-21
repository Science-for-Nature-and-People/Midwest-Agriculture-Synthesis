############    #
# RUN THE APP!  #
#################

#### LOAD PACKAGES
library(tidyverse) # For reading and manipulating data

#### SET DIRECTORY
setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www")

#### READ DATA
summary_all <- read_csv("data/data-for-app.csv")
map.data <- read_csv("data/mapping/site-data_with_counties.csv")

#### RUN APP
runApp('appScripts/')

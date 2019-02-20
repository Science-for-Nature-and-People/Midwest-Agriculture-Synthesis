###################################################
# Script to pull county information from Lat/Long #
###################################################

library(tigris)
library(sp)

setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data")

counties <- counties(cb=TRUE)

data <- read.csv("site-data-no_counties.csv")
coordinates(data) <- ~Longitude+Latitude

pointsSP <- SpatialPoints(data, 
                          proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

countynames <- over(pointsSP, counties)
final <- data.frame(data, countynames)[,c(1:7,9:11,13:14)]

write.csv(final,"site-data_with_counties.csv")


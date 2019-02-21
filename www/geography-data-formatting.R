###################################################
# Script to pull county information from Lat/Long #
###################################################

library(tigris)
library(sp)

setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data/mapping")

counties <- counties(cb=TRUE)

covercrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", 
                        sheet = "ExpD_Location")
nutrient <- read_excel("Nutrient_Synthesis_Database_Atwood.xlsx", 
                        sheet = "ExpD_Location")
pest <- read_excel("PestMgmt_Review_Atwood.xlsx", 
                       sheet = "ExpD_Location")

all.data <- rbind(covercrop, nutrient, pest)

coordinates(all.data) <- ~Longitude+Latitude

points <- SpatialPoints(all.data, 
                          proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

countynames <- over(points, counties)
final <- data.frame(all.data, countynames)[,c(1,2,6:11,15,19,33)]

write.csv(final,"site-data_with_counties.csv")


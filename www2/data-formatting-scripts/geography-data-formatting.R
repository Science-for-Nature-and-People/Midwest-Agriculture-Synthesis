###################################################
# Script to pull county information from Lat/Long #
###################################################

library(readxl)
library(tigris)
library(sp)

setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www2/data/mapping")
#setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www2/data/mapping")

counties <- counties(cb=TRUE)

tillage <- read_excel("Tillage_Synthesis_Database_Atwood.xlsx", 
                        sheet = "ExpD_Location")
tillage$Review <- paste("Tillage")

#nutrient <- read_excel("Nutrient_Synthesis_Database_Atwood.xlsx", 
 #                       sheet = "ExpD_Location")
#nutrient$Review <- paste("Fertility Management")
#pest <- read_excel("PestMgmt_Review_Atwood.xlsx", 
 #                      sheet = "ExpD_Location")
#pest$Review <- paste("Pest Management")

#all.data <- rbind(covercrop, nutrient, pest)
all.data <- tillage

coordinates(all.data) <- ~Longitude+Latitude

points <- SpatialPoints(all.data, 
                          proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

countynames <- over(points, counties)

final <- data.frame(all.data, countynames)[,c(1,2,6:11,15,27,34)]

write.csv(final,"site-data_with_counties.csv")


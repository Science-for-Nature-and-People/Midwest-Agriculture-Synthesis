###################################################
# Script to pull county information from Lat/Long #
###################################################

library(readxl)
library(readr)
library(tigris)
library(sp)
library(dplyr)

setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data/mapping")
#setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/mapping")

counties <- counties(cb=TRUE)

covercrop <- read_csv("CC_ExpD.csv",
                      col_types = cols(Trtmt_main_levels = col_character(), 
                                       Reps = col_character()))
covercrop$Review <- paste("Cover Crop")

nutrient <- read_csv("Nutrient_ExpD.csv",
                     col_types = cols(Trtmt_main_levels = col_character(), 
                                      Reps = col_character()))
nutrient$Review <- paste("Nutrient Management")

pest <- read_csv("PestMgmt_ExpD.csv",
                 col_types = cols(Trtmt_main_levels = col_character(),
                                  Trtmt_splitC = col_character(), 
                                  Reps = col_character())) 
                      
pest$Review <- paste("Early Season Pest Management")

tillage <- read_csv("Tillage_ExpD.csv", 
                    col_types = cols(Trtmt_main_levels = col_character(), 
                                     Reps = col_character()))
tillage$Review <- paste("Tillage")


                                            
all.data <- full_join(covercrop, nutrient)
all.data <- full_join(all.data, tillage)
all.data <- full_join(all.data, pest)

coordinates(all.data) <- ~Longitude+Latitude

points <- SpatialPoints(all.data, 
                          proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

countynames <- over(points, counties)

final <- data.frame(all.data, countynames)[,c(1,2,6:11,15,27,34)]

write.csv(final,"site-data_with_counties.csv")


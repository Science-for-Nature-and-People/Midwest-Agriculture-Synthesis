###Geography Data###
##Midwest Agriculture Synthesis####

library(dplyr)
library("readxl", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/")

df <- read.csv("Cover Crop Review/CoverCrop_ExpDLoc.csv", header=TRUE)

df2 <- df %>%
      select(Paper_id, Soil_type, City, State, Latitude, Longitude) %>%
      distinct(Paper_id, Soil_type, City, State, Latitude, Longitude)


#Export cover crop review geography data table
write.csv(df2, file = "www/testing/test-scripts/CC_geography_data.csv")
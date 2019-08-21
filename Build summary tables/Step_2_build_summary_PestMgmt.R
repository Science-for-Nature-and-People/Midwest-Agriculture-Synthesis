###Build Summary: Early Season Pest Management Review
#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############


library(dplyr)
library(tidyverse)
library(stringr) #string detection/ working with strings
library(plotrix) # for standard error calculations
library(splitstackshape) #expanding results by # of years the data recorded represent


#This file:
#Summarizes all quantitative data to be used for the Shiny App and data display#####

#######################################################################################################################


#import data
setwd(".")
datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 
df <- read.csv(file.path(datapath, "PestMgmt Review/PestMgmt_ResultsGrouped.csv"), row.names = NULL)

####### Filter###################################################################
#inspect 'Group_finelevel' column to determine which comparisons to exclude
#list of 'Group_finelevel' to exclude 
levels(df$Group_finelevel)

remove_these <- c("seedI_seedI",
                  "seedIF_seedF",
                  "seedIF_seedIF",
                  "untreated_seedunknown",
                  "foliarI_seedI",
                  "seedF_foliarI",
                  "seedIF_foliarI",
                  "seedF_seedIFfoliarI",
                  "seedf_seedFfoliarI",
                  "seedF_seedFfoliarI",
                  "seedIF_seedFfoliarI",
                  "seedIF_seedIFfoliari",
                  "seedIF_seedIFfoliarI",
                  "seedF_seedIF",
                  "seedFfoliarI_seedIFfoliarI")


df <- filter(df, !(finelevel_group %in% remove_these), Stat_type == "mean", !is.na(Trt1_value), !is.na(Trt2_value) )

#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
  select(Review, group_level1, group_level2, group_level3, Response_var_units) %>%
  group_by(Review, group_level1, group_level2, group_level3) %>%
  mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(Response_var_units), collapse = "; "))) %>%
  select(Review, group_level1, group_level2, group_level3, unit_list) %>%
  distinct()


write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/PestMgmt Review/Pest_Group_units.csv", row.names = FALSE)



#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
df$Trt1_value <- as.numeric(as.character(df$Trt1_value))
df$Trt2_value <- as.numeric(as.character(df$Trt2_value))


df <- df %>%
  mutate(per_change = if_else(Trt1_value == 0,  ((Trt2_value - 0)/1)*100, ((Trt2_value - Trt1_value)/Trt1_value)*100)) %>%      
  mutate(actual_diff = (Trt2_value - Trt1_value))

unique(df$pm_group1)
unique(df$pm_group2)


##Expand dataset based on years####
df <- cSplit(df, splitCols = "RV_year", sep = ";", direction = "long") # all data were expanded based on list of years 


######Organize soil sampling depth and year variables########

unique(levels(df$RV_depth))

#####soil depth groupings#####
#These will display such that it is always displying the results from more shallow sampling depths + deepest depth
#Organized by means of sampling depth

##No soil depths included in this dataset thus far.####

depth_0_30 <- c()
depth_0_60 <- c()
depth_0_100 <- c()
depth_0_150 <- c()
  
#####Apply soil depth groupings####



#df <- df %>%
   #mutate(
    #sample_depth = case_when(
      
     # RV_depth %in% depth_0_30 ~ "0-30 cm",
    #  RV_depth %in% depth_0_60 ~ "0-60 cm",
     # RV_depth %in% depth_0_100 ~ "0-100 cm",
    #  RV_depth %in% depth_0_150 ~ "0-150 cm"
     ))


####Group_RV: Other Soil Properties####
df_othersoilprops <- df %>%
  filter (group_level1 == "Other Soil Properties")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_othersoilprops[df_othersoilprops$actual_diff<1000,],  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Crop Yields####
df_cropyields <- df %>%
  filter (group_level1 == "Crop Yields")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Climate Mitigation####
df_climatemitigation <- df %>%
  filter (group_level1 == "Climate Mitigation")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Soil Nutrients####
df_soilnutrients <- df %>%
  filter (group_level1 == "Soil Nutrients")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Pests####
df_pests <- df %>%
  filter (group_level1 == "Pests")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Water Quality####
df_waterquality <- df %>%
  filter (group_level1 == "Water Quality")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Join Raw  results back into one file ####
all_data <- rbind(df_othersoilprops, 
                  df_cropyields, 
                  df_climatemitigation,
                  df_pests,
                  df_soilnutrients,
                  df_waterquality)






write.csv(all_data, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/PestMgmt_ALL_raw.csv", row.names = FALSE)


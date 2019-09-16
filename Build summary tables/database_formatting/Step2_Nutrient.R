###Build Summary: Nutrient Management Review
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
df <-  read.csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Nutrient_ResultsGrouped.csv", row.names = NULL)

####### Filter###################################################################
#list of 'Group_finelevel' to exclude 

df$finelevel_group <- as.factor(df$finelevel_group)
levels(df$finelevel_group)

#some treatment comparisons are beyond the scope of this analysis
#these treatments comparisons will be removed
remove_these <- c("application_variable_variablereduced",
                  "placement_pointinjection_knifeinjection",
                  "varrate_varrate",
                  "injection_injection",
                  "unfertilized_plant",
                  "unfertilized_split",
                  #"timing_spring_fall",
                  "split_preplantplantV6_plantV6",
                  "split_preplantV6_V6",
                  "split_plantV6_V6",
                  "variable_variable",
                  "knife_knife")

df <- filter(df, !(finelevel_group %in% remove_these), stat_type == "mean", !is.na(trt1_value), !is.na(trt2_value) )


#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
  select(review, group_level1, group_level2, group_level3, rv_units) %>%
  group_by(review, group_level1, group_level2, group_level3) %>%
  mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(rv_units), collapse = "; "))) %>%
  select(review, group_level1, group_level2, group_level3, unit_list) %>%
  distinct()


write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_Group_units.csv", row.names = FALSE)



#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
df$trt1_value <- as.numeric(as.character(df$trt1_value))
df$trt2_value <- as.numeric(as.character(df$trt2_value))


df <- df %>%
  mutate(per_change = if_else(trt1_value == 0,  ((trt2_value - 0)/1)*100, ((trt2_value - trt1_value)/trt1_value)*100)) %>%      
  mutate(actual_diff = (trt2_value - trt1_value))



#Use number change for changes in Invertebrate Pest and Predator populations
levels(df$group_level1)
levels(df$group_level2)
levels(df$group_level3)


#####Groups to remove
###These do no align with the web tool format
          #"unfertilized_plant"                     
          #"unfertilized_split"
          #"variable_variable" 
          #"band_injection"
          #"injection_injection"
          #"knife_knife"
          #"placement_pointinjection_knifeinjection"
          #"surfaceband_belowsurface"
 df <- filter(df, !is.na(trt1_name))

####################Expand table to display results for all years####

df <- cSplit(df, splitCols = "rv_year", sep = ";", direction = "long") # all data were expanded based on list of years 



####Group_RV: Other Soil Properties####
df_othersoilprops <- df %>%
  filter (group_level1 == "Other Soil Properties")

#Explore data distribution
#look by Response_var

qplot(rv, per_change, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_othersoilprops[df_othersoilprops$actual_diff<1000,],  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Crop Yields####
df_cropyields <- df %>%
  filter (group_level1 == "Crop Yields")

#Explore data distribution
#look by Response_var

qplot(rv, per_change, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Climate Mitigation####
df_climatemitigation <- df %>%
  filter (group_level1 == "Climate Mitigation")

#Explore data distribution
#look by Response_var

qplot(rv, per_change, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Soil Nutrients####
df_soilnutrients <- df %>%
  filter (group_level1 == "Soil Nutrients")

#Explore data distribution
#look by Response_var

qplot(rv, per_change, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Pests####
df_pests <- df %>%
  filter (group_level1 == "Pests")

#Explore data distribution
#look by Response_var

qplot(rv, per_change, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Water Quality####
df_waterquality <- df %>%
  filter (group_level1 == "Water Quality")

#Explore data distribution
#look by Response_var

qplot(rv, per_change, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Join Raw  results back into one file ####
all_data <- rbind(df_othersoilprops, 
                  df_cropyields, 
                  df_climatemitigation,
                  df_pests,
                  df_soilnutrients,
                  df_waterquality)



write.csv(all_data, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Nutrient_raw.csv", row.names = FALSE)

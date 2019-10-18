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
####Filtering Data Files###############################################################


#import data
setwd(".")
datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 
df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_ResultsGrouped.csv", row.names = NULL)

df$Stat_type <- as.character(df$Stat_type)
#remove rows with no group metric, blank values, and something other than mean values

df <- df %>% filter(Stat_type == "mean", !is.na(Trt_id1value), !is.na(group_level1)) %>%
        select(Paper_id:Review_id, Response_var:group_level3)


#####Tillage Review ######################################################


#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
              select(Review_id, group_level1, group_level2, group_level3, Response_var_units) %>%
              group_by(Review_id, group_level1, group_level2, group_level3) %>%
              mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(Response_var_units), collapse = "; "))) %>%
              select(Review_id, group_level1, group_level2, group_level3, unit_list) %>%
              distinct()

write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Group_units.csv", row.names = FALSE)

######################################################################################

###Use this newly created file for the analysis####
df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_treatmentsorganized.csv", row.names = NULL)

        

#####Calculate Percent Change [(Trtmt-Control)/Control] & Actual Difference [Trtment-Control] for each row
df$Trt1_value <- as.numeric(as.character(df$Trt1_value))
df$Trt2_value <- as.numeric(as.character(df$Trt2_value))

#Calculates percent change and change in abundance based on the order of the tillage types listed in the dataframe
#If the lower ranking tillage type is in tilltype_2 then tilltype_2 is used as the control and tilltype_1 is used as the treatment

df <- df %>%
  mutate(per_change = if_else(Trt1_value == 0,  ((Trt2_value - 0)/1)*100, ((Trt2_value - Trt1_value)/Trt1_value)*100)) %>%      
  mutate(actual_diff = (Trt2_value - Trt1_value))
           #if_else((str_detect(main_group,"Invertebrates") & 
            #          (str_detect(Response_var_units, "# | number")) | (str_detect(Response_var_units, "%"))),
             #      (Trt2_value - Trt1_value), NULL))
         
           #if_else((str_detect(main_group,"Weeds") & (str_detect(Response_var_units, "# | number")) | (str_detect(Response_var_units, "%")))
                                    #(Trt2_value - Trt1_value), NULL)))
#Use number change for changes in Invertebrate Pest and Predator populations
levels(df$group_level1)
levels(df$group_level2)
levels(df$group_level3)

                              
####################Expand table to display results for all years####

df <- cSplit(df, splitCols = "RV_year", sep = ";", direction = "long") # all data were expanded based on list of years 
levels(as.factor(df$RV_year))

df <- df %>% filter(RV_year > 0) %>% droplevels() #removes rows where data were collected prior to the adoption of the practice


#Change types
df$Paper_id <- as.factor(df$Paper_id)


unique(df$group_level1)

####Group_RV: Other Soil Properties####
df_othersoilprops <- df %>%
  filter (group_level1 == "Other Soil Properties")

#Explore data distribution
#look by rv

qplot(rv, per_change, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_othersoilprops[df_othersoilprops$actual_diff<1000,],  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Crop Yields####
df_cropyields <- df %>%
  filter (group_level1 == "Crop Yields")

#Explore data distribution
#look by rv

qplot(rv, per_change, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Climate Mitigation####
df_climatemitigation <- df %>%
  filter (group_level1 == "Climate Mitigation") %>% droplevels()
levels(df_climatemitigation$group_level2)
levels(df_climatemitigation$group_level3)
#Explore data distribution
#look by rv

qplot(rv, per_change, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Soil Nutrients####
df_soilnutrients <- df %>%
  filter (group_level1 == "Soil Nutrients")

#Explore data distribution
#look by rv

qplot(rv, per_change, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Pests####
df_pests <- df %>%
  filter (group_level1 == "Pests")

#Explore data distribution
#look by rv

qplot(rv, per_change, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Water Quality####
df_waterquality <- df %>%
  filter (group_level1 == "Water Quality")

#Explore data distribution
#look by rv

qplot(rv, per_change, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(rv, actual_diff, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Join Raw  results back into one file ####
all_data <- rbind(df_othersoilprops, 
                  df_cropyields, 
                  df_climatemitigation,
                  df_pests,
                  df_soilnutrients,
                  df_waterquality)

all_data <- rename(all_data, Trt1_details = Trt1_description, Trt2_details = Trt2_description )

all_data <- all_data %>% mutate(trt_specifics = NA) %>% mutate(nutrient_groups = NA)
all_data$Trt1 <- as.factor(all_data$Trt1)
all_data$Trt2 <- as.factor(all_data$Trt2)
all_data$Trt2_int2 <- as.factor(all_data$Trt2_int2)
all_data$nutrient_groups <- as.factor(all_data$nutrient_groups)



write.csv(all_data, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/TillageMgmt_ALL_raw.csv", row.names = FALSE)

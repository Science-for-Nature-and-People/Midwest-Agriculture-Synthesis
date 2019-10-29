#Synthesis of Midwestern Agriculture#######################

library(dplyr)
library(tidyverse)
library(stringr) #string detection/ working with strings
library(plotrix) # for standard error calculations
library(splitstackshape) #expanding results by # of years the data recorded represent


#This file:
  #1. Filters data set to exclude data comparisons outside the AgEvidence framework
  #2. Creates a list of units for groups of response variables
  #3. Calculates percent change and actual difference for each row comparison - creates new columns for these results
  #4. Expands data based on number of years experiments were conducted
  #5. Groups results based on number of years tillage has been implemented at the site
  #6. Plots data distribution for each group_level1 grouping for inspection


#Summarizes all quantitative data to be used for the Shiny App and data display#####

#######################################################################################################################
####Filtering Data Files###############################################################


#import data
df <- read.csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Tillage_ResultsGrouped.csv", row.names = NULL)


df$stat_type <- as.character(df$stat_type)
#remove rows with no group metric, blank values, and something other than mean values

df <- df %>% filter(stat_type == "mean", !is.na(trt1_value), !is.na(group_level1))

#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
              select(review, group_level1, group_level2, group_level3, rv_units) %>%
              group_by(review, group_level1, group_level2, group_level3) %>%
              mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(rv_units), collapse = "; "))) %>%
              select(review, group_level1, group_level2, group_level3, unit_list) %>%
              distinct()

write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Group_units.csv", row.names = FALSE)

######################################################################################


#####Calculate Percent Change [(Trtmt-Control)/Control] & Actual Difference [Trtment-Control] for each row
df$trt1_value <- as.numeric(as.character(df$trt1_value))
df$trt2_value <- as.numeric(as.character(df$trt2_value))

#Calculates percent change and change in abundance based on the order of the tillage types listed in the dataframe
#If the lower ranking tillage type is in tilltype_2 then tilltype_2 is used as the control and tilltype_1 is used as the treatment

df <- df %>%
  mutate(per_change = if_else(trt1_value == 0,  ((trt2_value - 0)/1)*100, ((trt2_value - trt1_value)/trt1_value)*100)) %>%      
  mutate(actual_diff = (trt2_value - trt1_value))
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

df <- cSplit(df, splitCols = "rv_year", sep = ";", direction = "long") # all data were expanded based on list of years 
levels(as.factor(df$rv_year))

df <- df %>% filter(rv_year > 0) %>% droplevels() #removes rows where data were collected prior to the adoption of the practice

#####Sampling year#####
unique(df$rv_year)

year_1_5 <- c(1:5)
year_6_10 <- c(6:10)
year_11_20 <- c(11:20)
year_21_30 <- c(21:30)
year_31_40 <- c(31:40)
year_41_50 <- c(41:50)

#####Apply sampling year groupings####

#Years need to be displayed cumulatively...these groupings don't do that, but the web tool should
df <- df %>%
  mutate(
    sample_year = case_when(
      
      rv_year %in% year_1_5 ~ "Year 1-5",
      rv_year %in% year_6_10 ~ "Years 6-10",
      rv_year %in% year_11_20 ~ "Years 11-20",
      rv_year %in% year_21_30 ~ "Years 21-30",
      rv_year %in% year_31_40 ~ "Years 31-40",
      rv_year %in% year_41_50 ~ "Years 41-50"
    ))


#Change types
df$paper_id <- as.factor(df$paper_id)


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



write.csv(all_data, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Tillage_raw.csv", row.names = FALSE)

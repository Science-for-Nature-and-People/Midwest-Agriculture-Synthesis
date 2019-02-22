#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############

library(dplyr)
library(tidyverse)
library(stringr) #string detection/ working with strings
library(plotrix) # for standard error calculations


#This file:
      #Summarizes all quantitative data to be used for the Shiny App and data display#####

#######################################################################################################################
####Filtering Data Files###############################################################


#import data
setwd(".")
datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 




#data set should include all treatment comparisons with the control (absence of the treatment)
#and only treatment means (remove SEMs)


#####Nutrient Review Filter######################################################################



      
            

#################################################################################################################



#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
df$Trt_id1value <- as.numeric(as.character(df$Trt_id1value))
df$Trt_id2value <- as.numeric(as.character(df$Trt_id2value))


df <- df %>%
  mutate(per_change = if_else(Trt_id1value == 0,  ((Trt_id2value - 0)/1)*100, ((Trt_id2value - Trt_id1value)/Trt_id1value)*100)) %>%      
  mutate(abundance_change = if_else((str_detect(main_group,"Invertebrates") & ((str_detect(group_metric, "#") | str_detect(Response_var_units, "# | number"))) | (str_detect(Response_var_units, "%"))), 
                                     (Trt_id2value - Trt_id1value), NULL))
#Use number change for changes in Invertebrate Pest and Predator populations
 levels(df$group_metric)

#Change types
df$per_change <- as.numeric(as.character(df$per_change))
df$abundance_change <- as.numeric(as.character(df$abundance_change))
df$Paper_id <- as.factor(df$Paper_id)
df$main_group <- as.factor(df$main_group)

#df$Review_specific <- as.factor(df$Review_specific)

#Adjust data so values representing multiple years are replicated within the dataframe####
#Weight means so they reflect # of years those data represent

#Create column that shows total number of years project was conducted (duration.yr )
df <- df %>%
  mutate(duration.yr = sub('.*-', '', df$Duration)) 

df$duration.yr <- as.numeric(df$duration.yr)

#determine number of years each mean represents
df$Year_result <- as.numeric(df$Year_result)

df <- df %>%
  mutate(per_change_yr = if_else(Year_result < 1, duration.yr, 1))


#  replicate rows so that total number of rows for that datapoint is equal to the # years the datapoint represents
df <- df %>% 
  uncount(per_change_yr)


####Replace group_finelevel with a more descriptive description########################


####Replace group_finelevel with a more descriptive description######################


######Summaries for each Grouping: Soil, Crop Production, Water, Pest Regulation####

#Separate Nutrient Review by Review_specific column
levels(df$Review_specific)

df2 <- filter(df, Review_specific == "Application (Split)")
df2 <- filter(df, Review_specific == "Application (Variable Rate)")
df2 <- filter(df, Review_specific == "Placement (Banding)")
df2 <- filter(df, Review_specific == "Placement (Subsurface)" )
df2 <- filter(df, Review_specific == "Timing (Fall & Spring)")
df2 <- filter(df, Review_specific == "Timing (Pre/Post- Planting)")
      

###ADd specific review groupings back to the nutrient review...should we do something similar for other reviews?

#Make sure duplicate rows are included with other summary tables... First one is complete.

####Group_RV: Soil####
df_soil <- df %>%
  filter (Group_RV == "Soil")
colnames(df_soil)

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_soil,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_soil,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_soil, per_change > 90)



soil_summary3 <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>% #Legend_2, Legend_3
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1 ) %>% #Legend_2, Legend_3
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id)) 

soil_summary2 <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

  soil_summary1 <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id))%>%
    mutate(Group_RV = "Soil") %>%
    mutate(Review = "Early Season Pest Management") #%>%
    mutate(Review_specific = "Timing (Pre/Post- Planting)")
  
                      
  

  #Calculates Change in Mean Abundance...maybe useful for pests and % values

#soil_summary0 <- df_soil %>%
 # select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  #filter(!is.na(abundance_change)) %>%
  #group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  #summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  #mutate(Group_RV = "Soil") %>%
  #mutate(Review = "Cover Crop")

soil_summary <- left_join(soil_summary1, soil_summary2)
soil_summary <- left_join(soil_summary, soil_summary3)



####Group_RV: Pest Regulation####
df_pest <- df %>%
  filter (Group_RV == "Pest Regulation")      

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_pest[df_pest$per_change < 1000,],  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
#[df_pest$per_change < 1000,]
qplot(Response_var, abundance_change, data=df_pest[df_pest$abundance_change > -1000,],  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
#[df_pest$abundance_change > -1000,]
outliers <- filter(df_pest, per_change > 2000)


pest_summary3 <- df_pest[df_pest$per_change < 1000,] %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id))


pest_summary2 <- df_pest[df_pest$per_change < 1000,] %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

pest_summary1 <- df_pest[df_pest$per_change < 1000,] %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id)) %>%
            mutate(Group_RV = "Pest Regulation") %>%
            mutate(Review = "Early Season Pest Management") #%>%
            mutate(Review_specific = "Timing (Pre/Post- Planting)")
                 



#pest_summary0 <- df_pest[df_pest$abundance_change > -1000,] %>% #[df_pest$abundance_change > -1000,]
 # select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
#  filter(!is.na(abundance_change)) %>%
#  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
#  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
#  mutate(Group_RV = "Pest Regulation") %>%
#  mutate(Review = "Cover Crop")

pest_summary <- left_join(pest_summary1, pest_summary2)
pest_summary <- left_join(pest_summary, pest_summary3)

summary(pest_summary$mean_abundance_change)

#Explore data distribution
#look by Response_var
#cc_pest_summary2 <- cc_pest_summary %>%
#                filter(!is.na(per_change > 1000))
test <- df_pest[!is.na(df_pest$abundance_change),]


####Group_RV: Crop Production####
df_yield <- df %>%
  filter (Group_RV == "Crop Production")      

#Explore data distribution
#look by Response_var
qplot(Response_var, per_change, data=df_yield[df_yield$per_change < 1000,],  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_yield,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


outliers <- filter(df_yield, per_change > 500)



yield_summary3 <- df_yield[df_yield$per_change < 1000,] %>% #
  select(Paper_id, Review_id, main_group, group_metric, Legend_1,  Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id)) 

yield_summary2 <- df_yield[df_yield$per_change < 1000,] %>% #
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

yield_summary1 <- df_yield[df_yield$per_change < 1000,] %>% #[df_yield$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id))%>%
            mutate(Group_RV = "Crop Production") %>%
            mutate(Review = "Early Season Pest Management") #%>%
            mutate(Review_specific = "Timing (Pre/Post- Planting)")

           

#Difference in Abundance
#yield_summary2 <- df_yield %>%
 # select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  #filter(!is.na(abundance_change)) %>%
  #group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  #summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  #mutate(Group_RV = "Crop Production") %>%
  #mutate(Review = "Cover Crop")

yield_summary <- left_join(yield_summary1, yield_summary2)
yield_summary <- left_join(yield_summary, yield_summary3)



####Group_RV: Water####
df_water <- df %>%
  filter (Group_RV == "Water")      

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_water,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_water,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_water, per_change > 100)

water_summary3 <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id)) 

water_summary2 <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

water_summary1 <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id))%>%
            mutate(Group_RV = "Water") %>%
            mutate(Review = "Fertilizer") %>%
            mutate(Review_specific = "Timing (Pre/Post- Planting)")

            
#Calculates mean abundance
#water_summary0 <- df_water %>%
 # select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  #filter(!is.na(abundance_change)) %>%
  #group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  #summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  #mutate(Group_RV = "Water") %>%
  #mutate(Review = "Cover Corp")

water_summary <- left_join(water_summary1, water_summary2)
water_summary <- left_join(water_summary, water_summary3)



####Join Summary results back into one file ####
summary_all <- full_join(soil_summary, yield_summary)
summary_all <- full_join(summary_all, pest_summary)
summary_all <- full_join(summary_all, water_summary)    


#Nutrient Review
summary_all_appsplit <- full_join(soil_summary1, yield_summary1)    
summary_all_appvarrate <- full_join(soil_summary1, yield_summary1) 
summary_all_placement_banding <- full_join(soil_summary1, yield_summary1) 
summary_all_placement_subsurface <- full_join(soil_summary1, yield_summary1)
summary_all_timing_fallspring <- full_join(soil_summary1, yield_summary1)
    summary_all_timing_fallspring <- full_join(summary_all_timing_fallspring, water_summary1)
summary_all_timing_prepostplant <- full_join(soil_summary1, yield_summary1)
    summary_all_timing_prepostplant <- full_join(summary_all_timing_prepostplant, water_summary1)
    
    #merge all above
    summary_all <- full_join(summary_all_appsplit, summary_all_appvarrate)
    summary_all2 <- full_join(summary_all_placement_banding, summary_all_placement_subsurface)
    summary_all3 <- full_join(summary_all_timing_fallspring, summary_all_timing_prepostplant)
    summary_all <- full_join(summary_all, summary_all2)
    summary_all <- full_join(summary_all, summary_all3)
    
summary_all$Review2 <- as.factor(paste(summary_all$Review, summary_all$Review_specific, sep = " "))
summary_all$Review_specific <- NULL
    
    

write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/NutrientMgmt_FULL_Summary.csv", row.names = FALSE)

write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/PestMgmt_FULL_Summary.csv", row.names = FALSE)

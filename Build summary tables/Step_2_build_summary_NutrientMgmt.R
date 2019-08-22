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
setwd(".")
datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 
df <-  read.csv(file.path(datapath, "Nutrient Review/Nutrient_ResultsGrouped.csv"), row.names = NULL)

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

df <- filter(df, !(finelevel_group %in% remove_these), Stat_type == "mean", !is.na(Trt1_value), !is.na(Trt2_value) )


#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
  select(Review, group_level1, group_level2, group_level3, Response_var_units) %>%
  group_by(Review, group_level1, group_level2, group_level3) %>%
  mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(Response_var_units), collapse = "; "))) %>%
  select(Review, group_level1, group_level2, group_level3, unit_list) %>%
  distinct()


write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_Group_units.csv", row.names = FALSE)



#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
df$Trt1_value <- as.numeric(as.character(df$Trt1_value))
df$Trt2_value <- as.numeric(as.character(df$Trt2_value))


df <- df %>%
  mutate(per_change = if_else(Trt1_value == 0,  ((Trt2_value - 0)/1)*100, ((Trt2_value - Trt1_value)/Trt1_value)*100)) %>%      
  mutate(actual_diff = (Trt2_value - Trt1_value))



#Use number change for changes in Invertebrate Pest and Predator populations
levels(df$group_level1)
levels(df$group_level2)
levels(df$group_level3)

###Apply treatment name groups#####

df <- df %>%
  mutate(
    Trt_1name = case_when(
      #Fertilizer Application####
      
      #Broadcast - Variable rate
      finelevel_group %in% "uniform_variable"  ~ "Broadcast",
      
      #Broadcast - Banded
      finelevel_group %in% "broad_band"  ~ "Broadcast",
      finelevel_group %in% "broadcast_band_ridge"  ~ "Broadcast",
      finelevel_group %in% "broadcast_injected_interrow"  ~ "Broadcast",
      finelevel_group %in% "broadcast_sidedress"  ~ "Broadcast",
      finelevel_group %in% "broadcast_injected_ridge"  ~ "Broadcast",
      finelevel_group %in% "surface_interrow"  ~ "Broadcast",
      finelevel_group %in% "surface_knife"  ~ "Broadcast",
      
      
      #Application Timing ####
      #Preplant - Early season
      finelevel_group %in% "preplant_postplant"  ~ "Preplant",
      finelevel_group %in% "preplant_V6"  ~ "Preplant",
      finelevel_group %in% "split_plant_plantV6"  ~ "Preplant",
      finelevel_group %in% "split_preplant_plant"   ~ "Preplant",
      finelevel_group %in% "timing_preplant_plant"   ~ "Preplant",
      finelevel_group %in% "timing_spring_V3"   ~ "Preplant",
      finelevel_group %in% "timing_preplant_splitV4"   ~ "Preplant",
      finelevel_group %in% "split_plant_V6"   ~ "Preplant",
      finelevel_group %in% "timing_plant_V8"  ~ "Preplant",
      
      
      #Fall - Spring
      finelevel_group %in% "timing_fall_preplant"  ~ "Fall",
      finelevel_group %in% "timing_fall_V3"  ~ "Fall",
      finelevel_group %in% "timing_fall_spring"  ~ "Fall",
      
      
      #Split applications####
      finelevel_group %in% "timing_plant_plantV6"  ~ "Single Application",
      finelevel_group %in% "timing_preplant_plantV12"  ~ "Single Application",
      finelevel_group %in% "timing_preplant_splitpreplantV16"  ~ "Single Application",
      finelevel_group %in% "timing_preplant_splitpreplantV4"  ~ "Single Application",
      finelevel_group %in% "timing_preplant_splitpreplantV7"  ~ "Single Application",
      finelevel_group %in% "split_V6_plantV6"  ~ "Single Application",
      finelevel_group %in% "split_V6_preplantV6"  ~ "Single Application",
      finelevel_group %in% "timing_preplant_splitv4"  ~ "Single Application")) %>%
  
  mutate(
    Trt_2name = case_when(
      #Fertilizer Application####
      
      #Broadcast - Variable rate
      finelevel_group %in% "uniform_variable"  ~ "Variable Rate Application",
      
      #Broadcast - Banded
      finelevel_group %in% "broad_band"  ~ "Banded",
      finelevel_group %in% "broadcast_band_ridge"  ~ "Banded",
      finelevel_group %in% "broadcast_injected_interrow"  ~ "Banded",
      finelevel_group %in% "broadcast_sidedress"  ~ "Banded",
      finelevel_group %in% "broadcast_injected_ridge"  ~ "Banded",
      finelevel_group %in% "surface_interrow"  ~ "Banded",
      finelevel_group %in% "surface_knife"  ~ "Banded",
      
      
      #Application Timing ####
      #Preplant - Early season
      finelevel_group %in% "preplant_postplant"  ~ "Early Season",
      finelevel_group %in% "preplant_V6"  ~ "Early Season",
      finelevel_group %in% "split_plant_plantV6"  ~ "Early Season",
      finelevel_group %in% "split_preplant_plant"   ~ "Early Season",
      finelevel_group %in% "timing_preplant_plant"   ~ "Early Season",
      finelevel_group %in% "timing_spring_V3"   ~ "Early Season",
      finelevel_group %in% "timing_preplant_splitV4"   ~ "Early Season",
      finelevel_group %in% "split_plant_V6"   ~ "Early Season",
      finelevel_group %in% "timing_plant_V8"  ~ "Early Season",
      
      
      #Fall - Spring
      finelevel_group %in% "timing_fall_preplant"  ~ "Spring",
      finelevel_group %in% "timing_fall_V3"  ~ "Spring",
      finelevel_group %in% "timing_fall_spring"  ~ "Spring",
      
      
      #Split applications####
      finelevel_group %in% "timing_plant_plantV6"  ~ "Split Application",
      finelevel_group %in% "timing_preplant_plantV12"  ~ "Split Application",
      finelevel_group %in% "timing_preplant_splitpreplantV16"  ~ "Split Application",
      finelevel_group %in% "timing_preplant_splitpreplantV4"  ~ "Split Application",
      finelevel_group %in% "timing_preplant_splitpreplantV7"  ~ "Split Application",
      finelevel_group %in% "split_V6_plantV6"  ~ "Split Application",
      finelevel_group %in% "split_V6_preplantV6"  ~ "Split Application",
      finelevel_group %in% "timing_preplant_splitv4"  ~ "Split Application"))

#####Groups to remove
#"unfertilized_plant"                     
#"unfertilized_split"
#"variable_variable" 
#"band_injection"
#"injection_injection"
#"knife_knife"
#"placement_pointinjection_knifeinjection"
#"surfaceband_belowsurface"
 df <- filter(df, !is.na(trt_1name))

####################Expand table to display results for all years####

df <- cSplit(df, splitCols = "RV_year", sep = ";", direction = "long") # all data were expanded based on list of years 


######Organize soil sampling depth and year variables########

unique(levels(df$RV_depth))

#####soil depth groupings#####
#These will display such that it is always displying the results from more shallow sampling depths + deepest depth
#Organized by means of sampling depth

depth_0_30 <- c(
  "0-30 cm",
  "0-60 cm"
  
)

depth_0_60 <- c(
  "0-60 cm",
  "30-60 cm",
  "0-90 cm",
  "0-120 cm",
  "0-107 cm")


depth_0_100 <- c(
  )

depth_0_150 <- c(
  "0-240 cm")




#####Apply soil depth groupings####

df <- df %>%
  mutate(
    sample_depth = case_when(

      RV_depth %in% depth_0_30 ~ "0-30 cm",
      RV_depth %in% depth_0_60 ~ "0-60 cm",
      RV_depth %in% depth_0_100 ~ "0-100 cm",
      RV_depth %in% depth_0_150 ~ "0-150 cm"))


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



write.csv(all_data, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/NutrientMgmt_ALL_raw.csv", row.names = FALSE)



###################Mean Abundance code

#pest_summary0 <- df_pest[df_pest$abundance_change > -1000,] %>% #[df_pest$abundance_change > -1000,]
# select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
#  filter(!is.na(abundance_change)) %>%
#  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
#  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
#  mutate(Group_RV = "Pest Regulation") %>%
#  mutate(Review = "Cover Crop")


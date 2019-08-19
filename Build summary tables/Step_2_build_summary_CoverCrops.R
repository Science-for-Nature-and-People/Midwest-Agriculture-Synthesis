###Build Summary: Cover Crop Review
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
df <-  read.csv(file.path(datapath, "Cover Crop Review/CC_ResultsGrouped.csv"), row.names = NULL)

#####Filter######################################################################

#Rename review_id to Review
df <- rename(df, finelevel_group = Group_finelevel, Review = Review_id, Trt1_value = Trt_id1value, Trt2_value = Trt_id2value)

###Keep all comparisons - use Trt_id1name and Trt_id2name to make comparisons

#keep rows only with Stat_type == mean
df <- df %>% filter(Stat_type == "mean", !is.na(Trt1_value) & !is.na(Trt2_value),
#remove rows with comparisons not suited for this analysis, where finelevel_group = mgmt, termination, none
 !(finelevel_group == "mgmt"), !(finelevel_group == "termination"), !(finelevel_group == "none"),
!(Trt_id1name == c("oat (corn), clover (soybean)" ,"tillage", "winter rye (corn), hairy vetch (soybean)"))) %>% droplevels()

df <- df %>% filter(!(Trt_id2name == c("hairy vetch (corn) / winter rye (soybean)", 
                                          "hairy vetch + winter rye (corn) / winter rye (soybean)",
                                          "herbicide (no tillage)", "mowing", "oat (corn), clover (soybean)",
                                          "oat, Italian ryegrass, or forage radish",
                                          "slender wheatgrass (corn), winter lentils (soybean)",
                                          "winter rye (corn), hairy vetch (soybean)")),
              Trt_id1 == 0)  %>% droplevels()

#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
  select(Review, group_level1, group_level2, group_level3, Response_var_units) %>%
  group_by(Review, group_level1, group_level2, group_level3) %>%
  mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(Response_var_units), collapse = "; "))) %>%
  select(Review, group_level1, group_level2, group_level3, unit_list) %>%
  distinct()


write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Cover Crop Review/CC_Group_units.csv", row.names = FALSE)



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

###Apply treatment name groups to tillage_1 and tillage_2 columns#####

levels(df$finelevel_group)

##########################################Start HERRE##################################################################

df <- df %>%
  mutate(
    ##Cover crop diversity####  
    Tillage_1 = case_when(
      
      #single species####
      finelevel_group %in% "mono"  ~ "Single species",    
      
      #Two species####
      finelevel_group %in% "mix_2"  ~ "Two species",
      
      #Three or more species####
      finelevel_group %in% "mix_3"  ~ "Three or more species"
      
#      #Mixture comparisons####
      
#      finelevel_group %in% "mono_mono"  ~ "Mixture comparisons",
#      finelevel_group %in% "mono_mix_2"  ~ "Mixture comparisons",
#      finelevel_group %in% "mix_2_mix_2"  ~ "Mixture comparisons",
#      finelevel_group %in% "mono_mix_3"  ~ "Mixture comparisons",
#      finelevel_group %in% "mix_2_mix_3"  ~ "Mixture comparisons"
    )) %>%
  
  mutate(
    ##Functional diversity####  
    Tillage_2 = case_when(
      
      #Non-legume + Legume Mixture####
      Trt_id2name %in% "forage radish + hairy vetch + winter rye"  ~ "Non-legume + Legume Mixture",    
      Trt_id2name %in% "winter rye + hairy vetch"  ~ "Non-legume + Legume Mixture",    
      Trt_id2name %in% "winter rye + Austrian winter pea"  ~ "Non-legume + Legume Mixture",    
      Trt_id2name %in% "forage radish + hairy vetch"  ~ "Non-legume + Legume Mixture",    
      
            
      #Non-legume Mixture####
      Trt_id2name %in% "forage radish + buckwheat"  ~ "Non-legume Mixture",    
      Trt_id2name %in% "forage radish + winter triticale"  ~ "Non-legume Mixture",    
      Trt_id2name %in% "winter rye + forage radish"  ~ "Non-legume Mixture",                                
      Trt_id2name %in% "winter rye + oat"  ~ "Non-legume Mixture",    
       
      #Cover crop rotation####
      Trt_id2name %in% "hairy vetch (corn) / winter rye (soybean)"  ~ "Rotation of Cover Crops",    
      Trt_id2name %in% "hairy vetch + winter rye (corn) / winter rye (soybean)"  ~ "Rotation of Cover Crops",    
      Trt_id2name %in% "oat (corn), clover (soybean)"   ~ "Rotation of Cover Crops",    
      Trt_id2name %in% "slender wheatgrass (corn), winter lentils (soybean)"  ~ "Rotation of Cover Crops",    
      Trt_id2name %in% "winter rye (corn), hairy vetch (soybean)"  ~ "Rotation of Cover Crops",    
      
      #Non-legumes####
      Trt_id2name %in% "barley"  ~ "Non-legume",    
      Trt_id2name %in% "Canada bluegrass"  ~ "Non-legume",    
      Trt_id2name %in% "canola"  ~ "Non-legume",    
      Trt_id2name %in% "chickweed"  ~ "Non-legume",    
      Trt_id2name %in% "downy brome"  ~ "Non-legume",    
      Trt_id2name %in% "forage radish"  ~ "Non-legume",    
      Trt_id2name %in% "forage rape"  ~ "Non-legume",    
      Trt_id2name %in% "Italian ryegrass"  ~ "Non-legume",    
      Trt_id2name %in% "mustard"  ~ "Non-legume",    
      Trt_id2name %in% "oat"  ~ "Non-legume",    
      Trt_id2name %in% "oat, Italian ryegrass, or forage radish"  ~ "Non-legume",    
      Trt_id2name %in% "perennial ryegrass"  ~ "Non-legume",    
      Trt_id2name %in% "rapeseed"  ~ "Non-legume",    
      Trt_id2name %in% "wheatgrass"  ~ "Non-legume",    
      Trt_id2name %in% "winter rye"  ~ "Non-legume",    
      Trt_id2name %in% "winter triticale"  ~ "Non-legume",                                          
      Trt_id2name %in% "winter wheat"  ~ "Non-legume",       

      #Legumes####
      Trt_id2name %in% "alfalfa"  ~ "Legume",    
      Trt_id2name %in% "Austrian winter pea"  ~ "Legume",
      Trt_id2name %in% "clover"  ~ "Legume",
      Trt_id2name %in% "hairy vetch"  ~ "Legume"))
      
          
missing <- df %>% filter(is.na(Tillage_1))
missing <- df %>% filter(is.na(Tillage_2))
                           

      
df <- cSplit(df, splitCols = "RV_year", sep = ",", direction = "long") # all data were expanded based on list of years 


######Organize soil sampling depth and year variables########

unique(levels(df$RV_depth))

#####soil depth groupings#####
#These will display such that it is always displying the results from more shallow sampling depths + deepest depth
#Organized by means of sampling depth

depth_0_30 <- c(
  "0-5 cm" ,
  "0-7.6 cm",
  "0-10 cm",
  "0-15 cm",
  "0-20 cm",
  "0-30 cm",
  "10-20 cm",
  "10 cm",
  "15-30 cm",
  "15.2-30.5 cm",
  "20-40 cm",
  "20 cm",
  "20 cm ",
  "5-15 cm",
  "5-20 cm",
  "7.6-15.2 cm",
  "topsoil"
)

depth_0_60 <- c(
  "0-45 cm",
  "0-46 cm",
  "0-50 cm",
  "0-60 cm",
  "15-75 cm",
  "30-60 cm",
  "30.5-61 cm",
  "40-60 cm",
  "60-90 cm"
)
  

depth_0_100 <- c(
  "0-75 cm",
  "0-80 cm",
  "0-90 cm",
  "0-100 cm",
  "80 cm"
)

depth_0_150 <- c(
  "0-120 cm", 
  "0-150 cm"
  )



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

#Rename columns to match column names in other reviews####
all_data <- all_data %>% rename(Trt1 = Trt_id1, Trt1_int = Trt1_interaction, Trt1_int2 = Trt1_interaction2,
                           Trt2 = Trt_id2, Trt2_int = Trt2_interaction, Trt2_int2 = Trt2_interaction2,
                           significance = Sig_level, 
                           Trt_1name = Trt_id1name, Trt1_details = Trt_id1description,
                           Trt_2name = Trt_id2name, Trt2_details = Trt_id2description)


####Export CSV#####
write.csv(all_data, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/CC_FULL_Summary.csv", row.names = FALSE)


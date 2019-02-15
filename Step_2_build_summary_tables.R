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

df <- read.csv(file.path(datapath, "PestMgmt Review/PestMgmt_ResultsGrouped.csv"), row.names = NULL)
df <-  read.csv(file.path(datapath, "Cover Crop Review/CC_ResultsGrouped.csv"), row.names = NULL)
df <-  read.csv(file.path(datapath, "Nutrient Review/Nutrient_ResultsGrouped.csv"), row.names = NULL)

#data set should include all treatment comparisons with the control (absence of the treatment)
#and only treatment means (remove SEMs)


#####Nutrient Review Filter######################################################################
#list of 'Group_finelevel' to exclude 

remove_these <- c("application_variable_variablereduced",
                  "placement_pointinjection_knifeinjection",
                  "varrate_varrate",
                  "injection_injection",
                  "unfertilized_plant",
                  "unfertilized_split",
                  "timing_spring_fall",
                  "split_preplantplantV6_plantV6",
                  "split_preplantV6_V6",
                  "split_plantV6_V6",
                  "variable_variable",
                  "knife_knife")

df <- filter(df, !(Group_finelevel %in% remove_these), Stat_type == "mean", !is.na(Trt_id1value), !is.na(Trt_id2value) )




#######Pest Management Review Filter###################################################################
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
            
            
            df <- filter(df, !(Group_finelevel %in% remove_these), Stat_type == "mean", !is.na(Trt_id1value), !is.na(Trt_id2value) )
            
  
#####Cover Crop Review Filter######################################################################
            
  df <- filter(df,!(Trt_id1 > 0), Group_finelevel != "none", Stat_type == "mean", !is.na(Trt_id1value) & !is.na(Trt_id2value)) 

            
            
            
            
#################################################################################################            
df <- arrange(df, Paper_id)

#alphabetize groups (needed for figures) #not working properly!####################
levels(as.factor(df_results$group_metric))
group_order <- c("Aboveground growth of weed community",
                 "Ammonium (Preplant)",                          
                 "Carbon Dioxide Emissions",
                 "Cocklebur",
                 "Compaction",
                 "Corn Rootworm (#)",
                 "Corn Rootworm (Damage to Crop)",
                 "Deadnettle",
                 "Drainage",
                 "Erosion",
                 "Giant Foxtail",
                 "Grain (Maize)",
                 "Grain (Soybean)",
                 "Microbial Biomass",                              
                 "Nitrate",
                 "Nitrate (Maize)",
                 "Nitrate (Post Harvest)",
                 "Nitrate (Preplant)",                             
                 "Nitrate (Soybean)",
                 "Nitrous Oxide Emissions",                        
                 "Non-predators & Non-pests" ,
                 "Pathogens" ,                                     
                 "pH",
                 "Phosphorous" ,
                 "Pigweed" ,   
                 "Postassium" ,                                    
                 "Predator Activity",
                 "Predator community inhabiting foliage (#)" ,     
                 "Predator community inhabiting soils (#)",
                 "Predator community inhabiting soils (Diversity)",
                 "Predators (#)",
                 "Root biomass"   ,                                
                 "Seedcorn Maggot (#)" ,
                 "Seedcorn Maggot (Damage to Crop)",               
                 "Seedling Density",
                 "Soil Aggregates",                                
                 "Soil Bulk Density" ,
                 "Soil Carbon, 0-20 cm depth",                     
                 "Soil Carbon, 20-60 cm depth",
                 "Soil Carbon, 0-75 cm depth" ,
                 "Soil Organic Matter" ,
                 "Soil Pores" ,                                    
                 "Soil Temperature"  ,
                 "Soil Texture" ,                                  
                 "Soil Water Content",
                 "Soybean Aphid (#)",                              
                 "Soybean Cyst Nematode (#)",
                 "Stalk/Stover (Maize)",                           
                 "Stover Biomass",
                 "Total Nitrogen"  ,                               
                 "Waterhemp" ,
                 "Weed community (abundance of weeds)" )

#library(plyr)  ## or dplyr (transform -> mutate)
df_order <- df_results %>%
  mutate(group_metric2 = factor(group_metric,levels=group_order))

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
###########Nutrient Management Review########################

df <- df %>%
  mutate(
    Legend_1 = case_when(
      #Fertilizer Application####
      Group_finelevel %in% "uniform_variable"  ~ "Variable Rate Application",
      
      
      #Fertilizer Placement####
      Group_finelevel %in% "broad_band"  ~ "In-Row",
      Group_finelevel %in% "broadcast_band_ridge"  ~ "In-Row",
      Group_finelevel %in% "broadcast_sidedress"  ~ "In-Row",
      Group_finelevel %in% "broadcast_injected_interrow"  ~ "Interrow",
      Group_finelevel %in% "broadcast_injected_ridge"  ~ "In-Row",
      Group_finelevel %in% "surface_interrow"  ~ "Interrow",
      
      Group_finelevel %in% "band_knife"  ~ "Knife",
      Group_finelevel %in% "band_injection"  ~ "Injection",
      Group_finelevel %in% "surfaceband_belowsurface"  ~ "Injection",
      Group_finelevel %in% "surface_knife"  ~ "Knife",

      #Fertilizer Timing####
      
      Group_finelevel %in% "preplant_postplant"  ~ "Preplant / Postplant",
      Group_finelevel %in% "split_preplant_plant"  ~ "Preplant / Postplant",
      Group_finelevel %in% "timing_preplant_plant"  ~ "Preplant / Postplant",
      
      
      Group_finelevel %in% "timing_plant_V8"  ~ "Preplant / Early Season",
      Group_finelevel %in% "timing_preplant_V6"  ~ "Preplant / Early Season",
      Group_finelevel %in% "timing_spring_V3"  ~ "Preplant / Early Season",
      Group_finelevel %in% "preplant_V6"  ~ "Preplant / Early Season",
      
      Group_finelevel %in% "timing_fall_preplant"  ~ "Fall / Preplant",
      Group_finelevel %in% "timing_fall_spring"  ~ "Fall / Preplant",
      Group_finelevel %in% "timing_fall_V3"  ~ "Fall / Early Season",
      
      
      
      Group_finelevel %in%  "timing_plant_V8"  ~ "Application (Split)",
      Group_finelevel %in%  "preplant_V6"  ~ "Application (Split)", 
      Group_finelevel %in%  "timing_spring_V3"  ~ "Application (Split)", 
      
      
#Fertilizer Timing####

      Group_finelevel %in%  "split_plant_V6"  ~ "Planting / Planting & Early Season",
      Group_finelevel %in%  "timing_plant_plantV6"  ~ "Planting / Planting & Early Season",
      Group_finelevel %in%  "split_plant_plantV6"  ~ "Planting / Planting & Early Season",
      Group_finelevel %in%  "timing_preplant_plantV12"  ~ "Preplanting / Planting & Mid Season",
      Group_finelevel %in%  "timing_preplant_splitpreplantV16"  ~ "Preplanting / Planting & Mid Season",
      Group_finelevel %in%  "timing_preplant_splitpreplantV4"  ~ "Preplanting / Planting & Early Season",
      Group_finelevel %in%  "timing_preplant_splitpreplantV7"  ~ "Preplanting / Planting & Early Season",
      Group_finelevel %in%  "timing_preplant_splitV4"  ~ "Preplanting / Planting & Early Season"
      
    )) 


###########Pest Management Review########################

df <- df %>%
  mutate(
    Legend_1 = case_when(
      
      #untreated to soil
      Group_finelevel %in% "untreated_soilI" ~ "Soil",
      Group_finelevel %in% "untreated_furrowI" ~ "Soil",
      Group_finelevel %in% "untreated_bandI" ~ "Soil",
      Group_finelevel %in% "untreated_broadcastI" ~ "Soil",
      
      #untreated to foliar
      Group_finelevel %in% "untreated_foliarI" ~ "Foliage",
      Group_finelevel %in% "untreated_foliarIF" ~ "Foliage",
      
      #untreated to seed
      Group_finelevel %in% "untreated_seedF" ~ "Seed",
      Group_finelevel %in% "untreated_seedFbio" ~ "Seed",
      Group_finelevel %in% "untreated_seedfoliarI" ~ "Seed & Foliage",
      Group_finelevel %in% "untreated_seedI" ~ "Seed",
      Group_finelevel %in% "untreated_seedIF" ~ "Seed",
      Group_finelevel %in% "untreated_seedIFN" ~ "Seed",
      TRUE                      ~ "other"
      
    )) %>%

#test for missing groupings
#missing <- df[df$Legend_1 %in% "other",]
      
      
      
        mutate(
          Legend_2 = case_when(  
            
            #untreated to soil
            Group_finelevel %in% "untreated_soilI" ~ "Insecticide",
            Group_finelevel %in% "untreated_furrowI" ~ "Insecticide",
            Group_finelevel %in% "untreated_bandI" ~ "Insecticide",
            Group_finelevel %in% "untreated_broadcastI" ~ "Insecticide",
            
            #untreated to foliar
            Group_finelevel %in% "untreated_foliarI" ~ "Insecticide",
            Group_finelevel %in% "untreated_foliarIF" ~ "Insecticide-Fungicide",
            
            #untreated to seed
            Group_finelevel %in% "untreated_seedF" ~ "Fungicide",
            Group_finelevel %in% "untreated_seedFbio" ~ "Fungicide-Biologic",
            Group_finelevel %in% "untreated_seedfoliarI" ~ "Insecticide",
            Group_finelevel %in% "untreated_seedI" ~ "Insecticide",
            Group_finelevel %in% "untreated_seedIF" ~ "Insecticide-Fungicide",
            Group_finelevel %in% "untreated_seedIFN" ~ "Insecticide-Fungicide-Nematicide",
            TRUE                      ~ "other"
            )) %>%
      #test for missing groupings
      #missing <- df[df$Legend_2 %in% "other",]
      
      
      
        mutate(
          Legend_3 = case_when( 
            #untreated to soil
            Group_finelevel %in% "untreated_soilI" ~ "Broadcast",
            Group_finelevel %in% "untreated_furrowI" ~ "In-Furrow",
            Group_finelevel %in% "untreated_bandI" ~ "In-Row",
            Group_finelevel %in% "untreated_broadcastI" ~ "Broadcast"
            
          ))
      
      #test for missing groupings
      #missing <- df[df$Legend_3 == "",]
      




####Replace group_finelevel with a more descriptive description######################
###################Cover Crop Review##############
      
      
      nonlegume <- c("rye", "canola", "rapeseed", "radish", "triticale", "wheat", "oat", "mustard", "buckwheat", "barley")
      legume <- c("hairy vetch", "vetch", "red clover", "white clover", "crimson clover", "alfalfa", "Austrian winter peas")      
      
      
df <- df %>%
  mutate(
    Legend_1 = case_when(
      Group_finelevel %in% "mono" ~ "Single species", 
      Group_finelevel %in% "mix_2" ~ "Two species",
      Group_finelevel %in% "mix_3" ~ "Three or more species",
      Group_finelevel %in% "none" ~ "Exclude")
    ) %>%
      filter(Legend_1 != "Exclude")  %>%
  mutate(
    Legend_2 = case_when(
      #Monoculture: Rotation of Legume and Non-Legume
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "vetch") ~ "Non-legume/Legume Rotation",
      Group_finelevel %in% "mono" & str_detect(Trt_id2name, "rye") &  str_detect(Trt_id2name, "vetch") ~ "Non-legume/Legume Rotation",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rape")  ~ "Non-Legume",
      
      #Monocultures of Non-Legume
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rye") ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "canola") ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rapeseed")  ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "radish")  ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "triticale" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "tritical" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "wheat" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "oat" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "mustard" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "buckwheat" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "barley" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "bluegrass" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "chickweed" ) ~ "Non-Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "brome" ) ~ "Non-Legume",
      
      #Moncultures of Legumes
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "hairy vetch") ~ "Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "vetch") ~ "Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "clover") ~ "Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "alfalfa") ~ "Legume",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "peas") ~ "Legume",
      
      #Mixtures
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "vetch") ~ "Non-Legume & Legume",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "radish") &  str_detect(Trt_id2description, "vetch") ~ "Non-Legume & Legume",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "pea") ~ "Non-Legume & Legume",
      Group_finelevel %in% "mix_3" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "vetch") ~ "Non-Legume & Legume",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "radish") &  str_detect(Trt_id2description, "buckwheat") ~ "Multiple Non-Legumes",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "oat") ~ "Multiple Non-Legumes",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "radish") ~ "Multiple Non-Legumes",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "radish") &  str_detect(Trt_id2description, "triticale") ~ "Multiple Non-Legumes",
      TRUE ~ "remove"
    )) %>%
      filter(Legend_2 != "remove") %>%
      
    mutate( Legend_3 = case_when(
      #Monoculture: Rotation of Legume and Non-Legume
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "vetch") ~ "Cereal Rye/Vetch Rotation",
      #Group_finelevel %in% "mono" & str_detect(Trt_id2name, "rye") &  str_detect(Trt_id2name, "vetch") ~ "Non-legume/Legume Rotation",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rape")  ~ "Rapeseed",
      
      #Monocultures of Non-Legume
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rye") ~ "Cereal Rye",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "canola") ~ "Canola",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "rapeseed")  ~ "Rapeseed",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "radish")  ~ "Tillage Radish",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "triticale" ) ~ "Winter Triticale",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "tritical" ) ~ "Winter Triticale",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "wheat" ) ~ "Winter Wheat",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "oat" ) ~ "Oat",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "mustard" ) ~ "Mustard",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "buckwheat" ) ~ "Buckwheat",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "barley" ) ~ "Barley",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "bluegrass" ) ~ "Canada Bluegrass",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "chickweed" ) ~ "Chickweed",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "brome" ) ~ "Downy Brome",
      
      #Moncultures of Legumes
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "hairy vetch") ~ "Vetch",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "vetch") ~ "Vetch",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "clover") ~ "Clover",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "alfalfa") ~ "Alfalfa",
      Group_finelevel %in% "mono" & str_detect(Trt_id2description, "peas") ~ "Austrian Winter Peas",
      
      #Mixtures
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "vetch") ~ "Cereal Rye/Vetch",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "radish") &  str_detect(Trt_id2description, "vetch") ~ "Radish/Vetch",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "pea") ~ "Cereal Rye/Austrian Winter Pea",
      Group_finelevel %in% "mix_3" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "vetch") &  str_detect(Trt_id2description, "radish") ~ "Rye/Vetch/Tillage Radish",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "radish") &  str_detect(Trt_id2description, "buckwheat") ~ "Tillage Radish/Buckwheat",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "oat") ~ "Cereal Rye/Oat",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "rye") &  str_detect(Trt_id2description, "radish") ~ "Cereal Rye/TillageRadish",
      Group_finelevel %in% "mix_2" & str_detect(Trt_id2description, "radish") &  str_detect(Trt_id2description, "triticale") ~ "Tillage Radish/Winter Triticale"
      #TRUE ~ "inspect"
    ))

##Monoculture Groupings ####

####Add broad groupings of monocultured cover crops####
# Mismatches for descriptions of rotated monocultures of cover crops
#levels(df$Trt_id2name)
#str_view(df$Trt_id2name, "rye")



#   df2 <- df %>%
#             mutate(
#                     monocultures = case_when(
#                            Trt_id2name %in% str_match(Trt_id2name, nonlegume & legume) & 
#                                      Cover_crop_diversity %in% "Monoculture" ~ "Rotation of Non-Legume & Legume", 
#                            Trt_id2name %in% str_match(Trt_id2name, "rye") & Cover_crop_diversity %in% "Monoculture" ~ "Winter Rye", 
#                            Trt_id2name %in% str_match(Trt_id2name, legume) &
#                                      Cover_crop_diversity %in% "Monoculture" ~ "Legume", 
#                            Cover_crop_diversity %in% "Monoculture" ~ "Non-Legume", 
#                            TRUE ~ ""
#                        ))

#   rm(df2)


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
    
    
    
write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/CC_FULL_Summary.csv", row.names = FALSE)

write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/NutrientMgmt_FULL_Summary.csv", row.names = FALSE)

write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/PestMgmt_FULL_Summary.csv", row.names = FALSE)

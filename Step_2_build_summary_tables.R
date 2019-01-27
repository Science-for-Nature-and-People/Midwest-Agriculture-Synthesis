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

df <- read.csv(file.path(datapath, "PestMgmt Review/PestMgmt_ResultsGrouped.csv"))
df <-  read.csv(file.path(datapath, "Cover Crop Review/CC_ResultsGrouped.csv"))

#data set should include all treatment comparisons with the control (absence of the treatment)
#and only treatment means (remove SEMs)


#######Pest Management Review Filter###################################################################
          #inspect 'Group_finelevel' column to determine which comparisons to exclude
          #list of 'Group_finelevel' to exclude 
            levels(Results$Group_finelevel)
            
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
            
  #df <- filter(df,!(Trt_id1 > 0), Group_finelevel != "none", Stat_type == "mean", !is.na(Trt_id1value) & !is.na(Trt_id2value)) 

            
            
            
            
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
df$Trt_id1value <- as.integer(as.character(df$Trt_id1value))
df$Trt_id2value <- as.integer(as.character(df$Trt_id2value))

#df <- df %>%
 #      mutate(per_change = ((Trt_id2value - Trt_id1value)/Trt_id1value)*100)
#df$per_change <- as.numeric(df$per_change)

df <- df %>%
  mutate(per_change = if_else(Trt_id1value == 0,  ((Trt_id2value - 0)/1)*100, ((Trt_id2value - Trt_id1value)/Trt_id1value)*100)) %>%      
  #mutate(per_change = ((Trt_id2value - Trt_id1value_alt)/Trt_id1value_alt)*100) %>%
  mutate(abundance_change = if_else((str_detect(main_group,"Invertebrates") & ((str_detect(group_metric, "#") | str_detect(Response_var_units, "# | number"))) | (str_detect(Response_var_units, "%"))), 
                                     (Trt_id2value - Trt_id1value), NULL))
#Use number change for changes in Invertebrate Pest and Predator populations
 levels(df$group_metric)

#Change types
df$per_change <- as.numeric(as.character(df$per_change))
df$abundance_change <- as.numeric(as.character(df$abundance_change))
df$Paper_id <- as.factor(df$Paper_id)
df$main_group <- as.factor(df$main_group)



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
      
    ))

#test for missing groupings
#missing <- df[df$Legend_1 %in% "other",]
      
      
      df <- df %>%
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
            ))
      #test for missing groupings
      #missing <- df[df$Legend_2 %in% "other",]
      
      
      df <- df %>%
        mutate(
          Legend_3 = case_when( 
            #untreated to soil
            Group_finelevel %in% "untreated_soilI" ~ "Broadcast",
            Group_finelevel %in% "untreated_furrowI" ~ "In-Furrow",
            Group_finelevel %in% "untreated_bandI" ~ "In-Row",
            Group_finelevel %in% "untreated_broadcastI" ~ "Broadcast",
            TRUE                      ~ ""
            
          ))
      
      #test for missing groupings
      #missing <- df[df$Legend_3 == "",]
      




####Replace group_finelevel with a more descriptive description######################
###################Cover Crop Review##############
df <- df %>%
  mutate(
    Legend_1 = case_when(
      Group_finelevel %in% "mono" ~ "Monoculture", 
      Group_finelevel %in% "mix_2" ~ "Mixture (2 Spp.)",
      Group_finelevel %in% "mix_3" ~ "Mixture (3+ Spp.)",
      Group_finelevel %in% "none" ~ "Exclude")) %>%
      filter(Legend_1 != "Exclude")  %>%
    
  mutate(
    Legend_2 = "Legume, NonLegume"
  ) %>%
  mutate(
    Legend_3 = "Specific Cover crops used"
  )


##Monoculture Groupings ####

####Add broad groupings of monocultured cover crops####
# Mismatches for descriptions of rotated monocultures of cover crops
#levels(df$Trt_id2name)
#str_view(df$Trt_id2name, "rye")

#nonlegume <- c("rye", "canola", "rapeseed", "radish", "triticale", "wheat", "oat", "mustard", "buckwheat", "barley")
#legume <- c("hairy vetch", "vetch", "red clover", "white clover", "crimson clover", "alfalfa", "Austrian winter peas")

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

#Make sure duplicate rows are included with other summary tables... First one is complete.

####Group_RV: Soil####
df_soil <- df %>%
  filter (Group_RV == "Soil")
colnames(df_soil)

soil_summary <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE),
            sem_per_change = std.error(per_change, na.rm = TRUE),
            num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Soil") %>%
  mutate(Review = "Pest Management")


soil_summary2 <- df_soil %>%
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  filter(!is.na(abundance_change)) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  mutate(Group_RV = "Soil") %>%
  mutate(Review = "Pest Management")

soil_summary <- left_join(soil_summary, soil_summary2)


#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_soil,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_soil,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_soil, per_change > 100)


write.csv(soil_summary, file = "PestMgmt Review/PestMgmt_Soil_Summary.csv")



####Group_RV: Pest Regulation####
df_pest <- df %>%
  filter (Group_RV == "Pest Regulation")      


pest_summary <- df_pest %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE),
            sem_per_change = std.error(per_change, na.rm = TRUE),
            num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
            mutate(Group_RV = "Pest Regulation") %>%
            mutate(Review = "Pest Management")


pest_summary2 <- df_pest %>% #[df_pest$abundance_change > -1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  filter(!is.na(abundance_change)) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  mutate(Group_RV = "Pest Regulation") %>%
  mutate(Review = "Pest Management")

pest_summary <- left_join(pest_summary, pest_summary2)

summary(pest_summary$mean_abundance_change)

#Explore data distribution
#look by Response_var
#cc_pest_summary2 <- cc_pest_summary %>%
#                filter(!is.na(per_change > 1000))
test <- df_pest[!is.na(df_pest$abundance_change),]

qplot(Response_var, per_change, data=df_pest,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
#[df_pest$per_change < 1000,]
qplot(Response_var, abundance_change, data=df_pest,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
#[df_pest$abundance_change > -1000,]
outliers <- filter(df_pest, per_change > 2000)


write.csv(pest_summary, file = "PestMgmt Review/PestMgmt_Pest_Summary.csv")



####Group_RV: Crop Production####
df_yield <- df %>%
  filter (Group_RV == "Crop Production")      

yield_summary <- df_yield[df_yield$per_change < 1000,] %>% #[df_yield$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE),
            sem_per_change = std.error(per_change, na.rm = TRUE),
            num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Crop Production") %>%
  mutate(Review = "Pest Management")


yield_summary2 <- df_yield %>%
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  filter(!is.na(abundance_change)) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  mutate(Group_RV = "Crop Production") %>%
  mutate(Review = "Pest Management")

yield_summary <- left_join(yield_summary, yield_summary2)


qplot(Response_var, per_change, data=df_yield,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_yield,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


outliers <- filter(df_yield, per_change > 1000)


write.csv(yield_summary, file = "PestMgmt Review/PestMgmt_Yield_Summary.csv")


####Group_RV: Water####
df_water <- df %>%
  filter (Group_RV == "Water")      


water_summary <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE),
            sem_per_change = std.error(per_change, na.rm = TRUE),
            num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Water") %>%
  mutate(Review = "Pest Management")


water_summary2 <- df_water %>%
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
  filter(!is.na(abundance_change)) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
  mutate(Group_RV = "Water") %>%
  mutate(Review = "Pest Management")

water_summary <- left_join(water_summary, water_summary2)

qplot(Response_var, per_change, data=df_water,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_water,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


outliers <- filter(df_water, per_change > 100)

write.csv(water_summary, file = "PestMgmt Review/PestMgmt_Water_Summary.csv")


####Join Summary results back into one file ####
summary_all <- soil_summary %>%
    full_join(pest_summary) %>%
    full_join(yield_summary) %>%
    full_join(water_summary)

#write.csv(summary_all, file = "Cover Crop Review/CC_FULL_Summary.csv")
write.csv(summary_all, file = "PestMgmt Review/PestMgmt_FULL_Summary.csv")


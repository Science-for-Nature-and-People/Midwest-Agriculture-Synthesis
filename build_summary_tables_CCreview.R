#Summarizes Cover Crop review for Shiny App and data display#####

#######################################################################################################################
####Filtering Data Files###############################################################


#import data
covercrops <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
covercrops_refexp <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_RefExp.csv", header=TRUE, row.names = "X")
covercrops_cashcrop <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_CashCrop.csv", header=TRUE, row.names = "X")
covercrops_trtmt <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Trt.csv", header=TRUE, row.names = "X")
covercrops_results <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Results.csv", header=TRUE, row.names = "X")

#set dataframe to work with - only using comparisons to control (0), excluded groups, and means only (remove SEMs) 
df <- filter(covercrops_results,!(Trt_id1 > 0), Group_finelevel != "none", Stat_type == "mean", !is.na(Trt_id1value) & !is.na(Trt_id2value)) 

df <- arrange(df, Paper_id)
df_results <- arrange(covercrops_results, Paper_id)
df_refexp <- covercrops_refexp
df_trtmt <- covercrops_trtmt

#alphabetize groups (needed for figures) #not working properly!
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



#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
#df <- df %>%
#       mutate(per_change = ((Trt_id2value - Trt_id1value)/Trt_id1value)*100)
#df$per_change <- as.numeric(df$per_change)

df <- df_order %>%
  mutate( Trt_id1value_alt = if_else(Trt_id1value == 0, 0.000000001, Trt_id1value)) %>%      
  mutate(per_change = ((Trt_id2value - Trt_id1value_alt)/Trt_id1value_alt)*100)
df$per_change <- as.numeric(df$per_change)


#df$per_change2 <- as.numeric(if_else(df$per_change == 0, 0.000001, df$per_change ))
df$per_change <- as.numeric(if_else(df$per_change == Inf, 0.000001, df$per_change ))
df$Paper_id <- as.factor(df$Paper_id)
df$main_group <- as.factor(df$main_group)



#Adjust data so values representing multiple years are replicated within the dataframe####
#Weight means so they reflect # of years those data represent

#Create column that shows total number of years project was conducted (duration.yr )
df <- df %>%
  mutate(duration.yr = sub('.*-', '', df$Duration)) 

df$duration.yr <- as.numeric(df$duration.yr)

#determine number of years each mean represents
df <- df %>%
  mutate(per_change_yr = if_else(Year_result < 1, duration.yr, 1))


#  replicate rows so that total number of rows for that datapoint is equal to the # years the datapoint represents
df <- df %>% 
  uncount(per_change_yr)



####Replace group_finelevel with a more descriptive description
df <- df %>%
  mutate(
    Cover_crop_diversity = case_when(
      Group_finelevel %in% "mono" ~ "Monoculture", 
      Group_finelevel %in% "mix_2" ~ "Mixture (2 Spp.)",
      Group_finelevel %in% "mix_3" ~ "Mixture (3+ Spp.)",
      Group_finelevel %in% "none" ~ "Exclude"
    ))

df2 <- df[df$Cover_crop_diversity != "Exclude",]
df2$Cover_crop_diversity <- as.factor( df2$Cover_crop_diversity)
diversity_order <- c("Monoculture", "Mixture (2 Spp.)", "Mixture (3+ Spp.)")
df_order2 <- df2 %>%
  mutate(Cover_crop_diversity2 =factor( Cover_crop_diversity,levels=diversity_order))

levels(df_order2$Cover_crop_diversity2)
colnames(df_order2)

##Monoculture Groupings ####

####Add broad groupings of monocultured cover crops
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


######Review: Cover crop####

#Make sure duplicate rows are included with other summary tables... First one is complete.

####Group_RV: Soil####
df_soil <- df_order2 %>%
  filter (Group_RV == "Soil")
colnames(df_soil)


cc_soil_summary <- df_soil %>%
  select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity2, per_change) %>%
  group_by(main_group, group_metric, Cover_crop_diversity2) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Soil") %>%
  mutate(Review = "Cover Cropping")


levels(as.factor(cc_soil_summary$Cover_crop_diversity2))     



#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_soil,  colour=Cover_crop_diversity2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_soil, per_change > 100)


write.csv(cc_soil_summary, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv")



####Group_RV: Pest Regulation####
df_pest <- df_order2 %>%
  filter (Group_RV == "Pest Regulation")      


cc_pest_summary <- df_pest[df_pest$per_change < 2000,] %>%
  select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity2, per_change) %>%
  group_by(main_group, group_metric, Cover_crop_diversity2) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Pest Regulation") %>%
  mutate(Review = "Cover Cropping")

levels(df_pest$Cover_crop_diversity2)

#Explore data distribution
#look by Response_var
#cc_pest_summary2 <- cc_pest_summary %>%
#                filter(!is.na(per_change > 1000))

qplot(Response_var, per_change, data=df_pest[df_pest$per_change < 500,],  colour=Cover_crop_diversity2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

qplot(Response_var, per_change, data=df_pest[df_pest$per_change < 2000,],  colour=Cover_crop_diversity2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
outliers <- filter(df_pest, per_change > 2000)

write.csv(cc_pest_summary, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv")



####Group_RV: Crop Production####
df_yield <- df_order2 %>%
  filter (Group_RV == "Crop Production")      


cc_yield_summary <- df_yield %>%
  select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity2, per_change) %>%
  group_by(main_group, group_metric, Cover_crop_diversity2) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Crop Production") %>%
  mutate(Review = "Cover Cropping")

qplot(Response_var, per_change, data=df_yield,  colour=Cover_crop_diversity2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_yield, per_change > 100)


write.csv(cc_yield_summary, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv")


####Group_RV: Water####
df_water <- df_order2 %>%
  filter (Group_RV == "Water")      


cc_water_summary <- df_water %>%
  select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity2, per_change) %>%
  group_by(main_group, group_metric, Cover_crop_diversity2) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id)) %>%
  mutate(Group_RV = "Water") %>%
  mutate(Review = "Cover Cropping")


qplot(Response_var, per_change, data=df_water,  colour=Cover_crop_diversity2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_water, per_change > 100)

write.csv(cc_water_summary, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv")


####Join Cover Crop Summary results back into one file ####
CC_summary_all <- cc_soil_summary %>%
  full_join(cc_pest_summary) %>%
  full_join(cc_yield_summary) %>%
  full_join(cc_water_summary)

write.csv(CC_summary_all, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/CoverCrop_Summary.csv")


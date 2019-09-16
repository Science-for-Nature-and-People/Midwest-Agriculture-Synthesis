#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############

#libraries#####

library("dplyr", lib.loc = "~/R/win-library/3.5")
library("readxl", lib.loc = "~/R/win-library/3.5")
library("tidyverse", lib.loc = "~/R/win-library/3.5")
library("tibble", lib.loc = "~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/")

############Import dataframes######################################################################

df <- read.csv("Nutrient Review/Nutrient_Results.csv", row.names = NULL)


#Remove unneeded columns
df$Year_result <- NULL
df$Effect <- NULL
df$Authors_comments <- NULL
df$Reviewers_results_short <- NULL
df$Reviewers_results_long <- NULL
df$Group_RV <- NULL


#Rename column rows to be consistent###
df <- rename(df, paper_id = Paper_id, duration = Duration, loc_multi_results = Loc_multi_results, 
             rv = Response_var, rv_trtspecifics = RV_trtspecifics, rv_depth = RV_depth, rv_year = RV_year,
             rv_units = Response_var_units, stat_test = Stat_test, stat_type = Stat_type,
             finelevel_group = Group_finelevel, review = Review_id, 
             trt1_name = Trt_id1name, trt1_value = Trt_id1value, trt1_details = Trt_id1description,
             trt1 = Trt_id1, trt1_int = Trt1_interaction, trt1_int2 = Trt1_interaction2,
             trt2_name = Trt_id2name, trt2_value = Trt_id2value,
             trt2_details = Trt_id2description, 
             trt2 = Trt_id2, trt2_int = Trt2_interaction, trt2_int2 = Trt2_interaction2,
             significance = Sig_level, rv_depth = RV_depth, effect_norm = Effect_norm 
)




#add surrogate key to Results
df$review_key = rownames(df)

########################Nutrient Management Review####################################################

#Bulk Groups####
####Soils####
##Soil Chemical Properties####

chem_nitrate_early <- c("soil nitrate at maize planting (0-30 cm depth)",
                        "soil nitrate at maize V6 stage (0-30 cm depth)",
                        "total soil mineral nitrate at maize V6 stage (0-30 cm depth)",
                        "soil nitrate (0-240 cm depth) prior to planting cash crop",
                        "total soil mineral nitrate at maize planting (0-30 cm depth)")


chem_nitrate_mid <- c("soil nitrate at maize R1 stage (0-30 cm depth)",
                      "total soil mineral nitrate at maize R1 stage (0-30 cm depth)")

chem_nitrate_harvest <- c("soil nitrate residual (0-0.3m)",
                          "residual soil nitrate (0-60 cm depth)",
                          "soil nitrate content (0.0 to 0.6 m depth) after harvest",
                          "soil nitrate (0-240 cm depth) post harvest of cash crop",
                          "soil residual nitrate (0-90 cm depth, after harvest or early spring)",
                          "soil residual nitrate (0-90 cm depth)",
                          "soil nitrate (0-1.2 m depth)",
                          "residual soil nitrate from enriched UAN fertilizer (0-107 cm depth)",
                          "residual soil nitrate (0-0.9 m depth)",
                          "postharvest soil nitrate content",
                          "residual soil nitrate (0-90 cm depth)")

chem_totalN <- c("soil nitrate + ammonium content (0.0 to 0.6 m depth) after harvest",
                 "residual total soil nitrogen (0-60 cm depth)",
                 "residual total soil nitrogen from enriched UAN fertilizer (0-107 cm depth)",
                 "fertilizer-derived nitrogen recovered in soil (0-90 cm depth)",
                 "fertilizer-derived nitrogen recovered in soil (0-130 cm depth)")



chem_ammonium_early <- c("soil ammonium at maize planting (0-30 cm depth)",
                         "soil ammonium at maize V6 stage (0-30 cm depth)")

chem_ammonium_mid <- c("soil ammonium at maize R1 stage (0-30 cm depth)")

chem_ammonium_harvest <- c("soil ammonium residual (0-0.3m)",
                           "soil ammonium at post harvest of maize (0-30 cm depth)",
                           "residual soil ammonium (0-60 cm depth)",
                           "soil ammonium content (0.0 to 0.6 m depth) after harvest",
                           "soil ammonium at post harvest of maize (60-90 cm depth)",
                           "soil ammonium at post harvest of maize (30-60 cm depth)",
                           "residual soil ammonium from enriched UAN fertilizer (0-107 cm depth)")

##Soil Environmental Properties####

enviro_N2Oemissions <- c("area-scaled nitrous oxide",
                         "yield-scaled nitrous oxide",
                         "nitrous oxide (N20)",
                         "area scaled nitrous oxide emissions (N2O) in maize",
                         "yield scaled nitrous oxide emissions (N2O) in maize",
                         "ferilizer-induced cumulative nitrous oxide emission factor (EF)",
                         "fertilizer-induced nitrous oxide emissions factor",
                         "nitrous oxide (N20) + nitric oxide (NO)")

enviro_NOemissions <- ("nitric oxide (NO)")


##########Crop Production#######
##Crop Nitrogen Uptake#### 
cropNuptake_grainsoy <- c("soybean grain nitrogen concentration",
                          "grain nitrogen (soybean)")

cropNuptake_grainmaize <- c("maize grain nitrogen concentration",
                            "maize grain nitrogen yield",
                            "maize grain nitrogen removal",
                            "maize grain total nitrogen concentration",
                            "grain nitrogen uptake (maize)",
                            "grain nitrogen content (maize)",
                            "maize grain nitrogen surplus",
                            "maize grain nitrogen uptake")

cropNuptake_abvgdbiomass_soy <- c("plant nitrogen concentration (soybean)",
                                  "plant nitrogen (soybean)")

cropNuptake_abvgdbiomass_maize <- c("stalk nitrate",
                                    "maize whole plant nitrogen surplus",
                                    "maize stover nitrogen concentration",
                                    "maize total N",
                                    "maize total nitrogen uptake",
                                    "maize nitrogen recovery",
                                    "aboveground N uptake (maize)",
                                    "plant nitrogen uptake (maize)",
                                    "maize whole plant (grain & stover) nitrogen uptake",
                                    "maize total nitrogen uptake (3 year average)",
                                    "maize leaf nitrogen content at silking (3 year average)",
                                    "maize total aboveground nitrogen yield")

cropNuptake_NUE <- c("agronomic efficiency",
                     "Nitrogen Use Efficiency",
                     "Nitrogen Recovery Efficiency",
                     "Nitrogen Internal Efficiency",
                     "maize nitrogen use efficiency",
                     "Harvest Index",
                     "nitrogen fertilizer recovery efficiency")

##Stand Count#### 

standcount_maize <- c("stand count (maize)",
                      "maize stand count (3 year average)")

standcount_damagedmaize <- "broken stalks (maize)"

##Yields####

yield_maizegrain <- c("maize grain yield (early planting)",
                      "maize grain yield (late planting)",
                      "maize grain yield (early planting, hybrid 1)",
                      "maize grain yield (late planting, hybrid 1)",
                      "maize grain yield (early planting, hybrid 2)",
                      "maize grain yield (late planting, hybrid 2)",
                      "grain yield (maize)",
                      "maize grain yield",
                      "crop yield (maize grain)",
                      "maize grain yield",
                      "maize yield",
                      "maize yields",
                      "maize grain yield (3 year average)")


yield_soygrain <- c("soybean grain yield",
                    "soybean yield")

grainquality_soy <- c("soybean protein",
                      "soybean oil",
                      "soybean fiber")


abovegroundbiomass_maize <- c("maize whole plant dry matter",
                              "dry matter stover yield",
                              "V6 dry wt.",
                              "silage yield",
                              "stover yield")

abovegroundbiomass_soy <- "plant dry matter (soybean)"






##########Water#######
##Runoff####

runoff_nitrate <- c("flow-weighted nitrate concentration in drainage tiles",
                    "annual nitrate load in drainage tiles",
                    "yield-scaled nitrate load",
                    "yield-scaled nitrate concentration")

##Drainage####

drainage <- "annual tile drainage discharge volume"

#Nutrient Groups####
#Fertilizer Placement####
uniform_variable <- "uniform_variable"

broadcast_zone <- c("broad_band",
                    "broadcast_band_ridge",
                    "broadcast_sidedress",
                    "broadcast_injected_interrow",
                    "broadcast_injected_ridge", 
                    "surface_interrow"
)

surface_subsurface <- c("band_knife",
                        "band_injection",
                        "surfaceband_belowsurface",
                        "surface_knife")

#Application Timing####

preplant_plant <- c("preplant_postplant",
                    "split_preplant_plant",
                    "timing_preplant_plant",
                    "timing_plant_V8",
                    "timing_spring_V3",
                    "preplant_V6"
)

fall_spring <- c("timing_fall_preplant",
                 "timing_fall_spring",
                 "timing_fall_V3",
                 "timing_fall_spring"
)

single_split <- c("timing_plant_plantV6",
                  "split_plant_V6",
                  "split_plant_plantV6",
                  "timing_preplant_plantV12",
                  "timing_preplant_splitpreplantV16",
                  "timing_preplant_splitpreplantV4",
                  "timing_preplant_splitpreplantV7",
                  "timing_preplant_splitV4"
)


#Create Main Groupings #####
  
groups_added <-   df %>%
                  select(paper_id, Response_var_org, finelevel_group, review_key) %>%
  ##Nutrient groups####
mutate(
  nutrient_groups = case_when( 
    
    #Application Timing####
    finelevel_group %in% preplant_plant  ~ "Application Timing",
    finelevel_group %in% fall_spring  ~ "Application Timing",
    finelevel_group %in% single_split  ~ "Application Timing",
    
    
    #Fertilizer Placement####
    finelevel_group %in% uniform_variable  ~ "Fertilizer Placement",
    finelevel_group %in% broadcast_zone  ~ "Fertilizer Placement",
    finelevel_group %in% surface_subsurface  ~ "Fertilizer Placement"
  )) %>%
  
  
  #Group_level1####    
mutate(
  group_level1 = case_when( 
    
    #Soil Nutrients####
    #Nitrogen####
    Response_var_org %in%  chem_nitrate_early  ~ "Soil Nutrients",
    Response_var_org %in%  chem_nitrate_mid  ~ "Soil Nutrients",
    Response_var_org %in%  chem_nitrate_harvest  ~ "Soil Nutrients",
    Response_var_org %in%  chem_totalN  ~ "Soil Nutrients",
    Response_var_org %in%  chem_ammonium_early  ~ "Soil Nutrients",
    Response_var_org %in%  chem_ammonium_mid  ~ "Soil Nutrients",
    Response_var_org %in%  chem_ammonium_harvest  ~ "Soil Nutrients",
    
    
    
    ##Climate Mitigation####
    #Nitrogen emissions####
    Response_var_org %in%   enviro_N2Oemissions  ~ "Climate Mitigation",
    Response_var_org %in%   enviro_NOemissions  ~ "Climate Mitigation",
    
    
    
    ##Crop Yields#######
    ##Grain Quality#### 
    #Corn
    Response_var_org %in%  cropNuptake_grainmaize  ~ "Crop Yields",
    
    #Soybean
    Response_var_org %in% grainquality_soy ~ "Crop Yields",
    Response_var_org %in%  cropNuptake_grainsoy  ~ "Crop Yields",
    
    ##Grain Yields####
    #Corn
    Response_var_org %in% yield_maizegrain ~ "Crop Yields",
    
    #Soybean
    Response_var_org %in% yield_soygrain ~ "Crop Yields",
    
    ##Crop Growth####
    #Corn
    Response_var_org %in% abovegroundbiomass_maize ~ "Crop Yields",
    
    #Soybean
    Response_var_org %in% abovegroundbiomass_soy ~ "Crop Yields",
    
    
    #Corn Nutrient Content###
    Response_var_org %in%  cropNuptake_abvgdbiomass_soy  ~ "Crop Yields", 
    Response_var_org %in%  cropNuptake_abvgdbiomass_maize  ~ "Crop Yields",  
    Response_var_org %in%   cropNuptake_NUE  ~ "Crop Yields",  
    
    ##Stand Count####
    Response_var_org %in% standcount_maize ~ "Crop Yields",
    
    ##Crop Damage####
    Response_var_org %in% standcount_damagedmaize ~ "Crop Yields",
    
    
    
    
    ##########Water Quality#######
    
    ##Runoff####
    Response_var_org %in% runoff_nitrate ~ "Water Quality",
    
    
    ##Drainage####
    Response_var_org %in% drainage ~ "Water Quality"
  )) %>%
  
  #Group_level2####    
mutate(
  group_level2 = case_when( 
    
    #Soil Nutrients####
    #Nitrogen####
    Response_var_org %in%  chem_nitrate_early  ~ "Nitrate",
    Response_var_org %in%  chem_nitrate_mid  ~ "Nitrate",
    Response_var_org %in%  chem_nitrate_harvest  ~ "Nitrate",
    Response_var_org %in%  chem_totalN  ~ "Nitrogen",
    Response_var_org %in%  chem_ammonium_early  ~ "Ammonium",
    Response_var_org %in%  chem_ammonium_mid  ~ "Ammonium",
    Response_var_org %in%  chem_ammonium_harvest  ~ "Ammonium",
    
    
    
    ##Climate Mitigation####
    #Nitrogen emissions ####
    Response_var_org %in%   enviro_N2Oemissions  ~ "Nitrogen Emissions",
    Response_var_org %in%   enviro_NOemissions  ~ "Nitrogen Emissions",
    
    
    
    ##Crop Yields#######
    ##Grain Quality#### 
    #Corn
    Response_var_org %in%  cropNuptake_grainmaize  ~ "Grain Quality",
    
    #Soybean
    Response_var_org %in% grainquality_soy ~ "Grain Quality",
    Response_var_org %in%  cropNuptake_grainsoy  ~ "Grain Quality",
    
    ##Grain Yields####
    #Corn
    Response_var_org %in% yield_maizegrain ~ "Grain Yields",
    
    #Soybean
    Response_var_org %in% yield_soygrain ~ "Grain Yields",
    
    ##Crop Growth####
    #Corn
    Response_var_org %in% abovegroundbiomass_maize ~ "Crop Growth",
    
    #Soybean
    Response_var_org %in% abovegroundbiomass_soy ~ "Crop Growth",
    
    ##Crop Nutrient Content####
    
    Response_var_org %in%  cropNuptake_abvgdbiomass_soy  ~ "Crop Nutrient Content", 
    Response_var_org %in%  cropNuptake_abvgdbiomass_maize  ~ "Crop Nutrient Content",  
    Response_var_org %in%   cropNuptake_NUE  ~ "Crop Nutrient Content",  
    
    ##Stand Count####
    Response_var_org %in% standcount_maize ~ "Stand Count",
    
    ##Crop Damage####
    Response_var_org %in% standcount_damagedmaize ~ "Crop Damage",
    
    
    
    
    ##########Water Quality#######
    
    ##Runoff####
    Response_var_org %in% runoff_nitrate ~ "Runoff from Field",
    
    
    ##Drainage####
    Response_var_org %in% drainage ~ "Infield Drainage"
  )) %>%
  
  
  #Group_level3####    
mutate(
  group_level3 = case_when( 
    
    #Soil Nutrients####
    #Nitrogen####
    Response_var_org %in%  chem_nitrate_early  ~ "Early season (crop stages: preplanting to V6)",
    Response_var_org %in%  chem_nitrate_mid  ~ "Mid season (crop stages: V7 to R1)",
    Response_var_org %in%  chem_nitrate_harvest  ~ "Late season (crop stages: harvest & post harvest)",
    
    Response_var_org %in%  chem_totalN  ~ "Total soil nitrogen",
    
    Response_var_org %in%  chem_ammonium_early  ~ "Early season (crop stages: preplanting to V6)",
    Response_var_org %in%  chem_ammonium_mid  ~ "Mid season (crop stages: V7 to R1)",
    Response_var_org %in%  chem_ammonium_harvest  ~ "Late season (crop stages: harvest & post harvest)",
    
    
    
    ##Climate Mitigation####
    #Nitrogen emissions####
    Response_var_org %in%   enviro_N2Oemissions  ~ "Nitrous oxide (N20)",
    Response_var_org %in%   enviro_NOemissions  ~ "Nitric oxide (NO)",
    
    
    
    ##Crop Yields#######
    ##Grain Quality#### 
    #Corn
    Response_var_org %in%  cropNuptake_grainmaize  ~ "Corn nitrogen uptake",
    
    #Soybean
    Response_var_org %in% grainquality_soy ~ "Soybean oil, fiber, & protein",
    Response_var_org %in%  cropNuptake_grainsoy  ~ "Soybean nitrogen uptake",
    
    ##Grain Yields####
    #Corn
    Response_var_org %in% yield_maizegrain ~ "Corn",
    
    #Soybean
    Response_var_org %in% yield_soygrain ~ "Soybean",
    
    ##Crop Growth####
    #Corn
    Response_var_org %in% abovegroundbiomass_maize ~ "Corn aboveground biomass",
    
    #Soybean
    Response_var_org %in% abovegroundbiomass_soy ~ "Soybean aboveground biomass",
    
    
    #Corn Nutrient Content###
    Response_var_org %in%  cropNuptake_abvgdbiomass_soy  ~ "Soybean nitrogen content of aboveground biomass", 
    Response_var_org %in%  cropNuptake_abvgdbiomass_maize  ~ "Corn nitrogen content of aboveground biomass",  
    Response_var_org %in%   cropNuptake_NUE  ~ "Crop nitrogen use efficiency",  
    
    
    
    ##Stand Count####
    Response_var_org %in% standcount_maize ~ "Corn",
    
    ##Crop Damage####
    Response_var_org %in% standcount_damagedmaize ~ "Corn stand count damage",
    
    
    
    
    ##Water Quality#######
    
    ##Runoff####
    Response_var_org %in% runoff_nitrate ~ "Nitrate",
    
    
    ##Drainage####
    Response_var_org %in% drainage ~ "Discharge amount"
  )) 

###Apply treatment name groups#####

df <- df %>%
  mutate(
    trt1_name = case_when(
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
    trt2_name = case_when(
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

######Organize soil sampling depth and year variables########

unique(levels(df$rv_depth))

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
      
      rv_depth %in% depth_0_30 ~ "0-30 cm",
      rv_depth %in% depth_0_60 ~ "0-60 cm",
      rv_depth %in% depth_0_100 ~ "0-100 cm",
      rv_depth %in% depth_0_150 ~ "0-150 cm"))


###Remove column uneeded after regrouping (info moved to rv_trtspecifics column)
df$Response_var_org <- NULL
df$review <- NULL



#Attach column to Results######
Results <-
  left_join(df, groups_added) 

df2 <- Results %>% mutate(review = "Nutrient Management") %>% 
              select(review, paper_id, duration, rv_year, loc_multi_results,
              group_level1, group_level2, group_level3, rv, rv_trtspecifics, rv_depth, sample_depth,
              rv_units, stat_test, stat_type, trt1, trt1_int, trt1_int2, trt1_value,        
              trt2, trt2_int, trt2_int2, trt2_value,
              significance, effect_norm, finelevel_group, trt1_name, trt1_details, trt2_name,
              trt2_details, nutrient_groups) 

                              


missing <- Results[is.na(Results$group_level1),] #check to see if all rows have an assigned group_level1
missing <- Results[is.na(Results$group_level2),] #check to see if all rows have an assigned group_level2
missing <- Results[is.na(Results$group_level3),] #check to see if all rows have an assigned group_level3
missing <- Results[is.na(Results$nutrient_groups),] #check to see if all rows have an assigned nutrient groups
#94 rows mising a nutrient group assignment
#these rows include treatment comparisons not included in the reported results


####Save Results file with added Group names
write.csv(df2, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Nutrient_ResultsGrouped.csv", row.names = FALSE)

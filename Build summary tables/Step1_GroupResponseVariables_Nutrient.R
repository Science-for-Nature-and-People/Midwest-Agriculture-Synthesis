#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############


#This file:
#Adds grouping variables to all response variables collected during the data extraction process
#There are 4 key agro-environmental  groups:
##1. Soil
##2. Crop Production
##3. Water (infield movement only)
##4. Pest Regulation

#Sub groups for each big order group vary by review.

#Use 'Results' worksheet with this script.


#libraries#####

library("dplyr", lib.loc = "~/R/win-library/3.5")
library("readxl", lib.loc = "~/R/win-library/3.5")
library("tidyverse", lib.loc = "~/R/win-library/3.5")
library("tibble", lib.loc = "~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/")

############Import dataframes######################################################################

Results <- read.csv("Nutrient Review/Nutrient_Results.csv", row.names = NULL)

#add surrogate key to Results
Results$Res_key <- rownames(Results)
Results$Res_key <- as.integer(Results$Res_key)


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
  
  
  ##Nutrient groups####
mutate(
  nutrient_groups = case_when( 
    
    #Application Timing####
    Group_finelevel %in% preplant_plant  ~ "Application Timing",
    Group_finelevel %in% fall_spring  ~ "Application Timing",
    Group_finelevel %in% single_split  ~ "Application Timing",
    
    
    #Fertilizer Placement####
    Group_finelevel %in% uniform_variable  ~ "Fertilizer Placement",
    Group_finelevel %in% broadcast_zone  ~ "Fertilizer Placement",
    Group_finelevel %in% surface_subsurface  ~ "Fertilizer Placement"
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



#Attach column to Results######
Results <-
  left_join(Results, groups_added, by = c("Res_key", "Response_var_org", "Group_finelevel"))

Results <- Results %>% select(Res_key,
                              Paper_id,
                              Duration,
                              Loc_multi_results,
                              Response_var,
                              RV_trtspecifics,
                              RV_depth,
                              RV_year,
                              Response_var_units,
                              Stat_test,
                              Stat_type,
                              Trt_id1, Trt1_interaction, Trt1_interaction2,Trt_id1value,
                              Trt_id2, Trt2_interaction, Trt2_interaction2,Trt_id2value,
                              Sig_level,
                              Group_finelevel,
                              Trt_id1name, Trt_id1description,
                              Trt_id2name, Trt_id2description,
                              group_level1,
                              group_level2,
                              group_level3,
                              nutrient_groups
) %>%
  mutate(Review = "Nutrient Management")

#rename columns for consitency among reviews

Results <- rename(Results,
                  Trt1 = Trt_id1,
                  Trt1_int = Trt1_interaction, 
                  Trt1_int2 = Trt1_interaction2,
                  Trt1_value = Trt_id1value,
                  Trt2 = Trt_id2,
                  Trt2_int = Trt2_interaction, 
                  Trt2_int2 = Trt2_interaction2,
                  Trt2_value = Trt_id2value,
                  significance = Sig_level,
                  finelevel_group = Group_finelevel,
                  Trt1_name = Trt_id1name,
                  Trt1_details = Trt_id1description,
                  Trt2_name = Trt_id2name,
                  Trt2_details = Trt_id2description)



missing <- Results[is.na(Results$group_level1),] #check to see if all rows have an assigned group_level1
missing <- Results[is.na(Results$group_level2),] #check to see if all rows have an assigned group_level2
missing <- Results[is.na(Results$group_level3),] #check to see if all rows have an assigned group_level3
missing <- Results[is.na(Results$nutrient_groups),] #check to see if all rows have an assigned nutrient groups
#94 rows mising a nutrient group assignment
#these rows include treatment comparisons not included in the reported results

write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_ResultsGrouped.csv", row.names = FALSE)




###Examine data to see if any group labels are missing####################
missing <- Results[is.na(Results$group_level1),] #check to see if all rows have an assigned group_level1
missing <- Results[is.na(Results$group_level2),] #check to see if all rows have an assigned group_level2
missing <- Results[is.na(Results$group_level3),] #check to see if all rows have an assigned group_level3

###Export CSV####################
write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_ResultsGrouped.csv", row.names = FALSE)
















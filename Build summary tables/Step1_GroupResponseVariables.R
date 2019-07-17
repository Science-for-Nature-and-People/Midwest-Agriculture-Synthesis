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


#All related results 
Results <-read.csv("PestMgmt Review/PestMgmt_Review_Results.csv", row.names = NULL)

#add surrogate key to Results
Results$Res_key = rownames(Results)

#remove any unwanted response variables

Results <-read.csv("Cover Crop Review/CoverCrop_Results.csv", row.names = NULL)
#for cover crop data frame
Results <- filter(Results,!(Response_var == "cover crop leaf N content")) #set dataframe to work with - remove cover crop nitrogen input data (incomplete dataset)

#add surrogate key to Results
Results$Res_key = rownames(Results)


Results <- read.csv("Nutrient Review/Nutrient_Results.csv", row.names = NULL)

#add surrogate key to Results
Results$Res_key = rownames(Results)

Results <- read.csv("Tillage Review/Tillage_Results.csv", row.names = NULL)

Results <- Results[Results$Paper_id != 209, ] #paper was not peer reviewed

#add surrogate key to Results
Results$Res_key = as.integer(rownames(Results))

###################################################################################################

#######ARE DATAFRAME KEYS UNIQUE?########################################################################
#if n is greater than 1 the key is not unique. When this occurs add a surrogate key.

Ref %>%
  count(DOI) %>%
  filter(n > 1) #zero duplicates

ExpD_Loc %>%
  count(Paper_id, Loc_multi) %>%
  filter(n > 1) #zero duplicates

CashCrop %>%
  count(Paper_id, Duration) %>%
  filter(n > 1) #zero duplicates

CoverCrop %>%
  count(Paper_id, Duration, Year, Trt_id) %>%
  filter(n > 1) #zero duplicates

Results %>%
  count(
    Paper_id,
    Response_var,
    Trt_id1,
    Trt_id2,
    Trt1_interaction,
    Trt1_interaction2,
    Trt2_interaction,
    Trt2_interaction2
  ) %>%
  filter(n > 1) #there isn't a unique key for this tibble


####################################################################################



##################ARE ANY OF THE PRIMARY-FOREIGN KEYS MISMATCHED?###################
#use anti_join to check for mismatched primary-foreign keys between tibbles
#inspect unique key
Results %>%
  count(Res_key) %>%
  filter(n > 1) #zero duplicates

##################################################################################



##############Add Metric Column and Groupings

#Paper_ID & Duration are the two keys that work across all dataframes

######Filter data based on response variable groupings [Crop Production, Soil, Water, Pest Regulation]####


#filter tibble for Group_Rv
#These lists are used for groups below
        #Groups are also included as worksheets for each review

Soil <- Results %>%
      filter(Group_RV == "Soil") %>%
      distinct(Response_var)

  unique(Soil$Response_var)  
  
  
Production <- Results %>%
              filter(Group_RV == "Crop Production")%>%
              distinct(Response_var)
unique(Production$Response_var)
            
Water <- Results %>%
        filter(Group_RV == "Water")%>%
        distinct(Response_var)
unique(Water$Response_var)

Pest <- Results %>%
        filter(Group_RV == "Pest Regulation")%>%
        distinct(Response_var)
unique(Pest$Response_var)

library(openxlsx)
write.xlsx(Soil, file="C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Soil_RVlist.xlsx", sheetName="Soil", row.names=FALSE, append=TRUE)
write.xlsx(Production, file="C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Production_RVlist.xlsx", sheetName="Production", append=TRUE, row.names=FALSE)
write.xlsx(Pest, file="C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Pest_RVlist.xlsx", sheetName="Pest", append=TRUE, row.names=FALSE)
write.xlsx(Water, file="C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Water_RVlist.xlsx", sheetName="Water", append=TRUE, row.names=FALSE)

        
##### Naming of all Grouping Variables Below########################################################
######Organized by Review ##########################################################################


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




#Review specifications (groups)####

#Fertilizer Application####
uniform_variable <- "uniform_variable"

#Fertilizer Placement####

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

#Fertilizer Timing####

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


##########Water#######

##Runoff####

runoff_nitrate <- c("flow-weighted nitrate concentration in drainage tiles",
                    "annual nitrate load in drainage tiles",
                    "yield-scaled nitrate load",
                    "yield-scaled nitrate concentration")

##Drainage####

drainage <- "annual tile drainage discharge volume"



###Apply metric & grouping labels to dataframe#####################################3


metric_labels <- Results %>%
  select(Response_var, Group_finelevel, Res_key) %>%
  mutate(
    group_metric = case_when( 
      
      ####Soils####
      ##Soil Chemical Properties####
      
      Response_var %in%  chem_nitrate_early  ~ "Nitrate (preplanting)",
      Response_var %in%  chem_nitrate_mid  ~ "Nitrate (midseason)",
      Response_var %in%  chem_nitrate_harvest  ~ "Nitrate (harvest)",
      Response_var %in%  chem_totalN  ~ "Total Nitrogen (0-60+ cm) ",
      Response_var %in%  chem_ammonium_early  ~ "Ammonium (preplanting) ",
      Response_var %in%  chem_ammonium_mid  ~ "Ammonium (midseason)", 
      Response_var %in%  chem_ammonium_harvest  ~ "Ammonium (harvest) ",
      
      
      
      ##Soil Environmental Properties####
      
      Response_var %in%   enviro_N2Oemissions  ~ "Nitrous oxide emissions",
      Response_var %in%   enviro_NOemissions  ~ "Nitric oxide emissions",
      
     
      
      ##########Crop Production#######
      ##Crop Nitrogen Uptake#### 
      
      Response_var %in%  cropNuptake_grainsoy  ~ "Grain (soybean)",
      Response_var %in%  cropNuptake_grainmaize  ~ "Grain (maize)",
      Response_var %in%  cropNuptake_abvgdbiomass_soy  ~ "Aboveground biomass (soybean)", 
      Response_var %in%  cropNuptake_abvgdbiomass_maize  ~ "Aboveground biomass (maize)", 
      Response_var %in%   cropNuptake_NUE  ~ "Nitrogen use efficiency", 
       
      
      ##############Crop Production####
      ##Stand Count####
      Response_var %in% standcount_maize ~ "Stand count (maize)",
      Response_var %in% standcount_damagedmaize ~ "Damaged stalks (maize)",
      
      
      ##Yields####
      Response_var %in% yield_maizegrain ~ "Grain (maize)",
      Response_var %in% yield_soygrain ~ "Grain (soybean)",
      Response_var %in% grainquality_soy ~ "Grain quality (soybean)",
      Response_var %in% abovegroundbiomass_maize ~ "Aboveground biomass (maize)",
      Response_var %in% abovegroundbiomass_soy ~ "Aboveground biomass (soybean)",
      
       
      ##########Water#######
      
      ##Runoff####
      Response_var %in% runoff_nitrate ~ "Nitrate",
      
       
      ##Drainage####
      Response_var %in% drainage ~ "Drainage"
    )) %>%
      
      
      #Create Main Groupings #####
    
    mutate(
      main_group = case_when( 
        
        ####Soils####
        ##Soil Chemical Properties####
        
        Response_var %in%  chem_nitrate_early  ~ "Chemical",
        Response_var %in%  chem_nitrate_mid  ~ "Chemical",
        Response_var %in%  chem_nitrate_harvest  ~ "Chemical",
        Response_var %in%  chem_totalN  ~ "Chemical",
        Response_var %in%  chem_ammonium_early  ~ "Chemical",
        Response_var %in%  chem_ammonium_mid  ~ "Chemical",
        Response_var %in%  chem_ammonium_harvest  ~ "Chemical",
        
        
        
        ##Soil Environmental Properties####
        
        Response_var %in%   enviro_N2Oemissions  ~ "Environmental",
        Response_var %in%   enviro_NOemissions  ~ "Environmental",
        
        
        
        ##########Crop Production#######
        ##Crop Nitrogen Uptake#### 
        
        Response_var %in%  cropNuptake_grainsoy  ~ "Crop Nitrogen Uptake",
        Response_var %in%  cropNuptake_grainmaize  ~ "Crop Nitrogen Uptake",
        Response_var %in%  cropNuptake_abvgdbiomass_soy  ~ "Crop Nitrogen Uptake", 
        Response_var %in%  cropNuptake_abvgdbiomass_maize  ~ "Crop Nitrogen Uptake",  
        Response_var %in%   cropNuptake_NUE  ~ "Crop Nitrogen Uptake",  
        
        
        ##############Crop Production####
        ##Stand Count####
        Response_var %in% standcount_maize ~ "Stand Count",
        Response_var %in% standcount_damagedmaize ~ "Stand Count",
        
        
        ##Yields####
        Response_var %in% yield_maizegrain ~ "Yields",
        Response_var %in% yield_soygrain ~ "Yields",
        Response_var %in% grainquality_soy ~ "Yields",
        Response_var %in% abovegroundbiomass_maize ~ "Yields",
        Response_var %in% abovegroundbiomass_soy ~ "Yields",
        
        
        ##########Water#######
        
        ##Runoff####
        Response_var %in% runoff_nitrate ~ "Runoff",
        
        
        ##Drainage####
        Response_var %in% drainage ~ "Drainage"
      )) %>%



##Add Management Practice Specifics to Review ####
mutate(
  Review_specific = case_when( 
    Group_finelevel %in% uniform_variable  ~ "Application (Variable Rate)",
    Group_finelevel %in% broadcast_zone  ~ "Placement (Banding)",
    Group_finelevel %in% surface_subsurface  ~ "Placement (Subsurface)",
    Group_finelevel %in% preplant_plant  ~ "Timing (Pre/Post- Planting)",
    Group_finelevel %in% fall_spring  ~ "Timing (Fall & Spring)",
    Group_finelevel %in% single_split  ~ "Application (Split)"
  ))





#Attach column to Results######
Results <-
  left_join(Results, metric_labels, by = c("Res_key", "Response_var", "Group_finelevel"))




########################Pest Management Review####################################################

#Bulk Groups####
                        ####Soils####
                        ##Soil Biological Properties####
                        biol_AMF <- "soybean root mycorrhizal colonization (arbuscular mycorrhizae)"
                        
                        
                        ##########Crop Production#######
                        ##Crop Yields####                                                                          
                        
                        yield_soybean <- c("normalized soybean yield", 
                                           "soybean grain yield",
                                           "soybean yield (2 year average)")
                        
                        grain_quality_soybean <- c("soybean seeds per gram",
                                                   "soybean grain moisture",
                                                   "soybean seed mass",
                                                   "soybean grain oil",
                                                   "soybean grain protein")
                        
                        yield_maize <-c("maize yield",
                                        "maize grain yield",
                                        "maize yield when soybean aphid populations reached economically damaging levels")
                        
                        
                        
                        ##Stand Count####
                        stand_count_soybean <- c("total number soybean plants per 1.5 m",
                                                 "soybean stand count (2 year average)",
                                                 "soybean stand count")
                        
                        
                        stand_count_maize <- c("maize stand count")
                        
                        lodging_soybean <- c("soybean lodging score (range: 1 = no lodging to 5 = completely lodged)")
                        
                        lodging_maize <- c("lodged maize plants")
                        
                        ##Crop Growth####
                        plant_growth_soybean <- c("NDVI (normalized difference vegetation index)",
                                                  "SRVI (simple ratio vegetation index)",
                                                  "leaf area index (July 26, 2001)",
                                                  "cumulative intercepted photosynthetically active radiation",
                                                  "cumulative normalized difference vegetative index")
                        
                        plant_height_soybean <- c("soybean plant height")
                        
                        seedling_development_soybean <- c("photosynthetic rates of seedling soybean",
                                                          "stomatal conductance of seedling soybean",
                                                          "transpiration rates of seedling soybean",
                                                          "Y-plants (a plant with two growing points which then develops into a plant with two stems)")
                        
                        plant_height_maize <- c("maize plant height")
                        
                        vigor_reduction_maize <- c("maize vigor reduction (visual rating of 0 to 100, where 0 is no effect and 100 is total crop destruction)",
                                                   "maize vigor reduction at 6 weeks (visual rating of 0 to 100, where 0 is no effect and 100 is total crop destruction)")
                        
                        
                        insect_damage_maize <- c("maize plant death due to black cutworms", 
                                                 "maize yield when soybean aphid populations reached economically damaging levels")
                        
                        
                        
                        
                        
                        
                        ############Pest Regulation####
                        ## Disease/Pathogens ####
                        disease_root_maize <- c("maize root rating (pathogen infection)",
                                                "root injury rating [from 0 to 3 where 0 = no injury and 3 = 3 nodes missing] for early May maize planting",
                                                "root injury rating [from 0 to 3 where 0 = no injury and 3 = 3 nodes missing] for late May maize planting",
                                                "root injury rating [from 0 to 3 where 0 = no injury and 3 = 3 nodes missing] for early June maize planting",
                                                "maize root rot index at V2 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                "maize root rot index at V4 stage (where 0 = healthy and 5 = completely rotted tissue)")
                        
                        
                        disease_leaftissue_maize <- c("mesocotyl rot index at V2 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "mesocotyl rot index at V4 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "crown rot index at V2 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "crown rot index at V4 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "stalk rot index at V2 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "stalk rot index at R6 stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "stalk rot index at R6  stage (where 0 = healthy and 5 = completely rotted tissue)",
                                                      "incidence of Fusarium infection at V2 stage",
                                                      "incidence of Fusarium infection at V4 stage",
                                                      "incidence of Fusarium infection at V6 stage",
                                                      "Stewart's wilt incidence (maize)")
                        
                        disease_soil_soybean <- c("Fusarium solani soil colony forming unit (0-15 cm depth)",
                                                  "Fusarium oxysporium soil colony forming unit (0-15 cm depth)")
                        
                        disease_tissue_soybean <- c("soybean disease severity at V4 growth",
                                                    "soybean seedlings percent dampening off")
                        
                        disease_BPMV_soybean <- c("proportion of Bean pod mottle virus-infected soybean in early August",
                                                  "average total Bean pod mottle virus (Comoviridae)")
                        
                        ## Invertebrate Pests & Natural Enemies ####
                        
                        pests_aphids_soybean <- c("number of soybean aphids per leaf")
                        
                        pests_aphiddays_soybean <- c("cumulative aphid days (soybean)")
                        
                        pests_thrips_soybean <- c("number of Thripidae per soybean plant")
                        
                        pests_mites_soybean <- c("number of spider mites per cm sq. (soybean)")
                        
                        pests_beanleafbeetles_soybean <- c("total counts of teneral adult (soon after molting) bean leaf beetles (Cerotoma trifurcata)",
                                                           "total counts of adult bean leaf beetles (Cerotoma trifurcata)")
                        
                        pests_SCM_maize <- c("adult seedcorn maggots per trap")
                        
                        pests_WCRW_maize <- c("number of female Western Corn Rootworm beetles",
                                              "number of male Western Corn Rootworm beetles")
                        
                        pests_WCRWemergence_maize <- c("mean Julian date for 50% Western Corn Rootworm beetle emergence")
                        
                        pests_NCRW_maize <- c("number of female Northern Corn Rootworm beetles",
                                              "number of male Northern Corn Rootworm beetles")
                        
                        preds_abundance_soybeans <- c("predator taxa abundance (Anthocoridae) in soybean",
                                                      "predator taxa abundance (Syrphidae) in soybean",
                                                      "predator taxa abundance (Chrysopidae) in soybean",
                                                      "predator taxa abundance (Coccinellidae) in soybean",
                                                      "predator taxa abundance (Dolichopodidae) in soybean",
                                                      "number of predators (hover flies) in soybean",
                                                      "number of predators (lacewings) in soybean",
                                                      "number of predators (lady beetles) in soybean",
                                                      "number of predators (insidious flower bugs) in soybean")
                        
                        
                        
                        
                        
                        
                        
                        ###############Water##### 
                        #(none)

                 
      
      
      ###Apply metric & grouping labels to dataframe#####################################3
      
      
      metric_labels <- Results %>%
        select(Response_var, Res_key) %>%
        mutate(
          group_metric = case_when( 
          
          ##############Soils####
          ##Soil Biological Properties####
          Response_var %in% biol_AMF ~ "Root Mycorrhizal Colonization (Arbuscular)",
          
          
          
          ##############################################################################################3
          
          
          
          
          
          ##########Crop Production#######
          
          ##Crop Yields####                                                                          
          Response_var %in% yield_soybean ~ "Grain Yield (Soybean)",
          Response_var %in% grain_quality_soybean ~ "Grain Quality (Soybean)",
          Response_var %in% yield_maize ~ "Grain Yield (Maize)",
          
          ##Stand Count####
          Response_var %in% stand_count_soybean ~ "Stand Count (Soybean)",
          Response_var %in% stand_count_maize ~ "Stand Count (Maize)",
          Response_var %in% lodging_soybean ~ "Soybean Lodging",
          Response_var %in% lodging_maize ~ "Maize Lodging (#)",
          
          
          ##Crop Growth####
          
          Response_var %in% plant_growth_soybean  ~ "Growth (Soybean)",
          Response_var %in% plant_height_soybean ~ "Plant Height (Soybean)",
          Response_var %in% seedling_development_soybean ~ "Seedling Development  (Soybean)",
          Response_var %in% plant_height_maize ~ "Plant Height (Maize)",
          Response_var %in% vigor_reduction_maize ~ "Reduction in Vigor (Maize)",
          Response_var %in% insect_damage_maize ~ "Black Cutworm Damage (Maize)",
          
          
          ############Pest Regulation####
          ## Disease/Pathogens ####
          Response_var %in% disease_root_maize ~ "Pathogenic Root Injury (Maize)",
          Response_var %in% disease_leaftissue_maize ~ "Pathogenic Injury to Leaf Tissue (Maize)",
          Response_var %in% disease_soil_soybean ~ "Pathogenic Colonies in Soil (Soybean)",
          Response_var %in% disease_tissue_soybean ~ "Pathogenic Injury to Leaf Tissue (Soybean)",
          Response_var %in% disease_BPMV_soybean ~ "Bean Pod Mottle Virus Infection (Soybean)",
          
          
          ## Invertebrate Pests & Natural Enemies ####
          
          Response_var %in% pests_aphids_soybean ~ "Soybean Aphids (#)",
          Response_var %in% pests_aphiddays_soybean ~ "Soybean Aphids (Cumulative Aphid Days)",
          Response_var %in% pests_thrips_soybean ~ "Thrips (#)",
          Response_var %in% pests_mites_soybean ~ "Spider Mites (Density in Soybean)",
          Response_var %in% pests_beanleafbeetles_soybean ~ "Bean Leaf Beetles (#)",
          Response_var %in% pests_SCM_maize ~ "Seed Corn Maggot (#)",
          Response_var %in% pests_WCRW_maize ~ "Western Corn Rootworm (#)",
          Response_var %in% pests_WCRWemergence_maize ~ "Western Corn Rootworm (Emergence Date)",
          Response_var %in% pests_NCRW_maize ~ "Northern Corn Rootworm (#)",
          Response_var %in% preds_abundance_soybeans ~ "Natural Enemies (#)"
        )) %>%
      
      
      
                        
      

#Create Main Groupings #####

mutate(
  main_group = case_when( 
    
    
    ####Soils####
    ##Soil Biological Properties####
    Response_var %in% biol_AMF ~ "Biological",
    
    ##########Crop Production#######
    
    ##Crop Yields####                                                                          
    Response_var %in% yield_soybean ~ "Crop Yield",
    Response_var %in% grain_quality_soybean ~ "Crop Yield",
    Response_var %in% yield_maize ~ "Crop Yield",
    
    ##Stand Count####
    Response_var %in% stand_count_soybean ~ "Stand Density",
    Response_var %in% stand_count_maize ~ "Stand Density",
    Response_var %in% lodging_soybean ~ "Stand Density",
    Response_var %in% lodging_maize ~ "Stand Density",
    
    
    ##Crop Growth####
    
    Response_var %in% plant_growth_soybean  ~ "Crop Growth",
    Response_var %in% plant_height_soybean ~ "Crop Growth",
    Response_var %in% seedling_development_soybean ~ "Crop Growth",
    Response_var %in% plant_height_maize ~ "Crop Growth",
    Response_var %in% vigor_reduction_maize ~ "Crop Growth",
    Response_var %in% insect_damage_maize ~ "Crop Growth",
    
    
    ############Pest Regulation####
    ## Disease/Pathogens ####
    Response_var %in% disease_root_maize ~ "Pathogens",
    Response_var %in% disease_leaftissue_maize ~ "Pathogens",
    Response_var %in% disease_soil_soybean ~ "Pathogens",
    Response_var %in% disease_tissue_soybean ~ "Pathogens",
    Response_var %in% disease_BPMV_soybean ~ "Pathogens",
    
    
    ## Invertebrate Pests & Natural Enemies ####
    
    Response_var %in% pests_aphids_soybean ~ "Invertebrates",
    Response_var %in% pests_aphiddays_soybean ~ "Invertebrates",
    Response_var %in% pests_thrips_soybean ~ "Invertebrates",
    Response_var %in% pests_mites_soybean ~ "Invertebrates",
    Response_var %in% pests_beanleafbeetles_soybean ~ "Invertebrates",
    Response_var %in% pests_SCM_maize ~ "Invertebrates",
    Response_var %in% pests_WCRW_maize ~ "Invertebrates",
    Response_var %in% pests_WCRWemergence_maize ~ "Invertebrates",
    Response_var %in% pests_NCRW_maize ~ "Invertebrates",
    Response_var %in% preds_abundance_soybeans ~ "Invertebrates"
  ))




##############################################################################################3



##############################################################################################3












                        
                        
    #Attach column to Results
    Results <-
      left_join(Results, metric_labels, by = c("Res_key", "Response_var"))
    
                        
                        
    
    
    
    
    
    
    
  
    
    
      
########################Cover Crops Review####################################################
#Bulk groupings####

                ####Soils####
                ##Soil Chemical Properties####
                chem_som <- c(
                  "particulate organic carbon",
                  "total POM",
                  "particulate organic matter (fPOM) including macro orgnaic matter fractions",
                  "particulate organic matter (oPOM)",
                  "soil organic matter",
                  "soil organic matter level from the top 50 mm of soil (aggregates < 0.42 mm)",                                      
                  "soil organic matter level from the top 50 mm of soil (aggregates between 0.42 and 0.84 mm)",                       
                  "soil organic matter level from the top 50 mm of soil (aggregates between 0.84 and 2.0 mm)",                        
                  "soil organic matter level from the top 50 mm of soil (aggregates between 2.0 and 6.4 mm)",                         
                  "soil organic matter level from the top 50 mm of soil (aggregates between 6.4 and 19.2 mm)",                        
                  "soil organic matter level from the top 50 mm of soil (aggregates > 19.2 mm)",                                      
                  "fine particulate organic matter level from the top 50 mm of soil (aggregates < 0.42 mm)",                          
                  "fine particulate organic matter level from the top 50 mm of soil (aggregates between 0.42 and 0.84 mm)",           
                  "fine particulate organic matter level from the top 50 mm of soil (aggregates between 0.84 and 2.0 mm)",            
                  "fine particulate organic matter level from the top 50 mm of soil (aggregates between 2.0 and 6.4 mm)",             
                  "fine particulate organic matter level from the top 50 mm of soil (aggregates between 6.4 and 19.2 mm)",            
                  "fine particulate organic matter level from the top 50 mm of soil (aggregates > 19.2 mm)",                          
                  "total particulate organic matter level from the top 50 mm of soil (aggregates < 0.42 mm)",                         
                  "total particulate organic matter level from the top 50 mm of soil (aggregates between 0.42 and 0.84 mm)",          
                  "total particulate organic matter level from the top 50 mm of soil (aggregates between 0.84 and 2.0 mm)",           
                  "total particulate organic matter level from the top 50 mm of soil (aggregates between 2.0 and 6.4 mm)",            
                  "total particulate organic matter level from the top 50 mm of soil (aggregates between 6.4 and 19.2 mm)",           
                  "total particulate organic matter level from the top 50 mm of soil (aggregates > 19.2 mm)"                         
                )
                
                chem_carbon_020 <-  c(
                  "soil organic carbon in topsoil",
                  "water extractable organic carbon (0-5 cm)",
                  "water extractable organic carbon (5-20 cm)",
                  "soil organic carbon (SOC) in fall following cover crop, 0-15cm",
                  "organic carbon (0-15 cm)",
                  "soil organic carbon in soybean (0-5 cm soil depth)",                                                               
                  "soil organic carbon in maize (0-5 cm soil depth)",                                                                 
                  "soil organic carbon in soybean (5-15 cm soil depth)",                                                              
                  "soil organic carbon (0-10 cm depth)",                                                                             
                  "soil organic carbon (10-20 cm depth)",
                  "soil carbon concentration (0-5 cm depth)",
                  "soil organic carbon (0-5 cm depth)",
                  "soil organic carbon (0-15 cm)"
                )
                
                #datatoexplore <- filter(Results,Response_var %in% chem_carbon_020)
                #write.xlsx(datatoexplore, file="C:/Users/LWA/Desktop/CC_SOC020.xlsx", row.names=FALSE, append=TRUE)
                #colnames(Results)
                
                chem_carbon_2060<-  c(
                  "soil organic carbon (15-75 cm)",
                  "soil organic carbon (20-40 cm depth)",                                                                             
                  "soil organic carbon (40-60 cm depth)"
                )
                
                chem_carbon_075<-  c(  
                  "total carbon",
                  "total carbon (0-50 cm depth)",
                  "total carbon (0-30 cm depth)",
                  "total carbon (0-45 cm depth)",
                  "soil organic carbon (0-75 cm)",
                  "soil organic carbon 0-30 cm depth (spring)",
                  "soil organic carbon (SOC), spring sample (0-60 cm depth)"
                )
                
                
                chem_nitrate_spring <- c(
                  "soil nitrate (NO3-N)",
                  "soil nitrate (NO3--N) preceeding maize",
                  "soil nitrate (NO3--N) preceeding soybean",
                  "average soil nitrate in 0-60 cm layer in late spring (preplanting of maize)",
                  "average soil nitrate in 0-60 cm layer in late spring (preplanting of soybean)",
                  "soil nitrate nitrogen (NO3-N) concentration in Spring, 0-60 cm",
                  "nitrate (NO3-N) in soil following cover crop",
                  "nitrate (NO3-N)",
                  "nitrogen in macro organic matter (0.5 mm) following cover crop",
                  "nitrogen in macro organic matter (2 mm) following cover crop",
                  "soil nitrate (NO3-N) post cover crop termination, pre cash crop planting (maize)",
                  "soil nitrate (NO3-N) post winter cover crop, before planting cash crop",
                  "soil nitrate 0-30 cm depth (spring)",
                  "soil nitrate NO3 (0-15 cm)",
                  "soil nitrate in spring (0-80 cm depth)"                                                                           
                  
                )
                
                chem_nitrate_maize <-
                  c(
                    "soil nitrate (NO3--N) following maize",
                    "total NO3-N from soil profile (0-120 cm) in crop row",
                    "total NO3-N from soil profile (0-120 cm) in crop interrow",
                    "soil nitrate nitrogen (NO3-N) concentration in Fall 0-30 cm",
                    "soil nitrate nitrogen (NO3-N) concentration in Fall, 30-60 cm",
                    "soil nitrate nitrogen (NO3-N) concentration in Fall, 60-90 cm",
                    "nitrate (NO3-N) at maize V6",
                    "soil nitrate (NO3-N) early June (maize)"
                    
                  )
                
                chem_nitrate_soybean <-
                  c("soil nitrate (NO3--N) following soybean")
                
                chem_nitrate_fall <-
                  c(
                    "soil nitrate (NO3-N) post harvest (maize)",
                    "soil nitrate (NO3-N) post harvest (soybean)",
                    "postharvest soil profile nitrate (0-90 cm)"
                  )
                
                chem_ammonium_spring <- c(
                  "ammonium (NH4-N)",
                  "ammonium (NH4-N) in soil following cover crop",
                  "soil ammonium 0-30 cm depth (spring)",
                  "soil ammonium NH4 (0-15 cm)",
                  "soil ammonium in spring (0-80 cm depth)"                                                                          
                )
                
                chem_totalN <- c(
                  "denitrification",
                  "nitrogen removal",
                  "nitrogen inputs",
                  "nitrogen balance",
                  "total (NO3+NH4)-N from soil profile (0-120 cm) in crop row",
                  "total (NO3+NH4)-N from soil profile (0-120 cm) in crop interrow",
                  "total nitrogen, spring sample",
                  "fraction of soil organic nitrogen mineralized in 30 days (frozen before incubation)",
                  "fraction of soil organic nitrogen mineralized in 30 days (not frozen before incubation)",
                  "total soil nitrogen following cover crop (to 1 m depth)",
                  "total soil nitrogen",
                  "total inorganic nitrogen (TIN)",
                  "total nitrogen (0-15 cm)",
                  "total nitrogen",
                  "soil organic nitrogen 0-30 cm depth (spring)",
                  "nitrate and ammonium (NO3-N + NH4-N) in soil following cover crop",
                  "soil inorganic nitrogen concentrations, after maize planting (0-3 in. depth)",
                  "soil inorganic nitrogen concentrations, after maize planting (3-6 in. depth)"
                  
                )
                
                chem_phosphorus <- c(
                  "plant available phosphorus",
                  "phosphorus removal",
                  "phosphorus inputs",
                  "phosphorus balance",
                  "Olsen phosphorus (Olsen P) concentration in Fall, 0-30 cm",
                  "Olsen phosphorus (Olsen P) concentration in Fall, 30-60 cm",
                  "Olsen phosphorus (Olsen P) concentration in Fall, 60-90 cm",
                  "Olsen phosphorus (Olsen P) concentration in Spring, 0-60 cm"
                  
                )
                
                chem_potassium <- c(
                  "potassium removal",
                  "potassium inputs",
                  "potassium balance",
                  "exchangable potassium (K)"
                )
                
                chem_acidity <- c(
                  "pH",
                  "pH (0-15 cm)")
                
                
                ##Soil Physical Properties####
                
                phy_erosion <- c(
                  "soil loss",
                  "interrill erosion rate",
                  "rill erosion rate",
                  "erodible fraction from the top 50 mm of soil (soil aggregates < 0.84 mm)",
                  "sediment load following fall manure",
                  "sediment load following spring manure"
                  
                )
                
                phy_compaction <- c(
                  "soil penetration resistance",
                  "soil penetration resistance (Spring)",
                  "soil penetration resistance (Fall)"
                )
                
                phy_pores <- c(
                  "total pore spaces",
                  "air filled pore space",
                  "volumetric air content",
                  "water filled pore space",
                  "pore tortuosity factor",
                  "relative gas diffusion coefficient"
                )
                
                phy_aggregates <- c(
                  "mean weight diameter of water stable aggregates",
                  "water aggregate stability",
                  "water stable aggregates in soybean",
                  "water stable aggregates in maize",                                                                                 
                  "water stable mean weight diameter (0-10 cm depth)",                                                                 
                  "water stable mean weight diameter (10-20 cm depth)",                                                                
                  "water stable mean weight diameter (20-40 cm depth)",                                                               
                  "water stable mean weight diameter (40-60 cm depth)",                                                                
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates < 0.42 mm)",               
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 0.42 and 0.84 mm)",
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 0.84 and 2.0 mm)", 
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 2.0 and 6.4 mm)",  
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 6.4 and 19.2 mm)", 
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates > 19.2 mm)"               
                )
                
                phy_bulkdensity <- c(
                  "bulk density (0-15 cm depth)",                                                                                            
                  "bulk density (15-30 cm depth)",                                                                                           
                  "bulk density 0-30 cm depth (spring)",
                  "bulk density",
                  "bulk density (0-10 cm depth)",                                                                                
                  "bulk density (10-20 cm depth)",                                                                               
                  "bulk density (20-40 cm depth)",                                                                               
                  "bulk density (40-60 cm depth)"
                )
                
                phy_texture <- c(
                  "percent sand  (0-15 cm depth)",                                                                                    
                  "percent sand  (15-30 cm depth)",                                                                                   
                  "percent silt  (0-15 cm depth)",                                                                                  
                  "percent silt  (15-30 cm depth)",                                                                                   
                  "percent clay  (0-15 cm depth)",                                                                                  
                  "percent clay  (15-30 cm depth)"                                                                                   
                )
                
                phy_watercontent <- c(
                  "volumetric water content",
                  "soil water content at cash crop emergence, 10 cm depth",
                  "soil water content at cash crop emergence, 20 cm depth",
                  "soil water content before planting of cash crop, 10 cm depth",
                  "soil water content before planting of cash crop, 20 cm depth",
                  "soil water storage  at cash crop emergence, 80 cm depth",
                  "soil water storage before planting of cash crop, 80 cm depth",
                  "gravimetric water content of soil",
                  "soil moisture",
                  "volumetric water saturation content (0-15 cm depth)",                                                              
                  "volumetric water saturation content (15-30 cm depth)",                                                             
                  "field capacity (0-15 cm)",                                                                                        
                  "field capacity (15-30 cm)",                                                                                        
                  "permanent wilting point (0-15 cm)",
                  "permanent wilting point (15-30 cm)",                                                                               
                  "plant available water (0-15 cm)",                                                                                  
                  "plant available water (15-30 cm)",                                                                                 
                  "soil water retention", 
                  "soil water content, after maize planting (0-3 in. depth)",
                  "soil water content, after maize planting (3-6 in. depth)",
                  "soil water content, after maize planting (6-12 in. depth)",
                  "soil water content, after maize planting (12-24 in. depth)"
                  
                )
                
                ##Soil Biological Properties####
                
                biol_microbes <- c(
                  "microbial biomass",
                  "microbial biomass nitrogen (MBN)",
                  "enzyme activity in soybean (Hydrolysis of Fluroescein Diacetate)",                                                 
                  "enzyme activity in maize (Hydrolysis of Fluroescein Diacetate)"                                                   
                )
                
                
                
                ##Soil Environmental Properties####
                
                envir_temp <- c("soil temperature")
                
                envir_CO2 <- c("carbon dioxide emissions (CO2-C)")
                
                
                envir_N2O <- c(
                  "N2O emissions when cover crop present",
                  "N2O emissions over entire year",
                  "cumulative N2O-N emissions from soil",
                  "cumulative nitrous oxide flux from soil April 2003-Mar 2004",
                  "cumulative nitrous oxide flux from soil April 2004 - Feb 2004",
                  "nitrous oxide emissions (N2O-N)"
                )
                
                
                
                ####Pest Regulation####
                ## Weeds ####
                weed_Palmeramaranth <-
                  c(
                    "control of Palmer amaranth (21 days post planting)",
                    "control of Palmer amaranth (at harvest)",
                    "density of Palmer amaranth (42 days post planting)",
                    "density of Palmer amaranth (at harvest)"
                  )
                
                weed_foxtail <-
                  c(   
                    "control of giant foxtail (weeds)"
                  )
                
                weed_cocklebur <-
                  c(   
                    "control of common cocklebur (weeds)"
                  )
                
                weed_deadnettle <-
                  c(   
                    "density of Lamim spp. (Henbit and purple deadnettle) in spring",
                    "density of Lamim spp. (Henbit and purple deadnettle) after 5 years"
                  )
                
                weed_waterhemp <-
                  c(     
                    "density of early season waterhemp",
                    "density of late season waterhemp",
                    "control of common waterhemp (21 days post planting)",
                    "control of common waterhemp (at harvest)"
                  )
                
                
                weed_community_density <- c(
                  
                  "weed density",
                  "total density of winter weeds in spring",
                  "total density of winter weeds after 5 years",
                  "weed density after termination of cover crops",
                  "density of winter annual weeds (before cash crop planting)",
                  "density of late season summer annual weeds"
                )
                weed_community_biomass <- c(
                  
                  "weed biomass",
                  "weed biomass after termination of cover crops"
                )
                
                ## Invertebrates ####
                
                invert_pests_SCN <-
                  c(
                    "density of soybean cyst nematode eggs in spring",
                    "density of soybean cyst nematode eggs after 5 years",
                    "soybean cyst nematode egg counts",
                    "density of Heterodera glycines eggs at harvest (maize)",
                    "density of Heterodera glycines eggs at harvest (soybean)")
                
                invert_pests_Aglycines <-
                  c(
                    "infestation rating of soybean fields with Aphis glycines (0 = 0 aphids/plant; 1 = 1-10 /plant; 2 = 11-100/plant; 3 = 101-1,000/plant; 4 = >1,000/plant)",
                    "proportion of soybean plants infested with Aphis glycines per field (30 plants assessed / field)",
                    "abundance of aphids (Aphis glycines) over time",
                    "exposure of aphid population to predators")
                
                invert_pests_seedcornmaggot <-
                  c( "abundance adult seedcorn maggots per trap")
                
                invert_pests_cornrootworm <-
                  c("Western corn rootworm abundance in first year (1st larval instar)",
                    "Western corn rootworm abundance in first year (2nd larval instar)",
                    "Western corn rootworm abundance in first year (3rd larval instar)",
                    "Western corn rootworm abundance in first year (adult)"
                  )
                
                invert_pests_cornrootworm_damage <-
                  c("maize root damage from corn rootworm")
                
                
                invert_pests_seedcornmaggot_damage <-
                  c(  "seedcorn maggot damage on crops (Y-plants)")
                
                invert_preds<-
                  c(
                    "total abundance of spiders (Araneae) on aboveground tissue (sweep net in soybean)",
                    "total abundance of wolf spiders (Lycosidae) on soil surface (pitfall traps in maize)",
                    "total abundance of wolf spiders (Lycosidae) on soil surface (pitfall traps in soybean)",
                    "total abundance of Harvestmen arachnids (Opiliones) on soil surface (pitfall traps in maize)",
                    "total abundance of lady beetles (Coccinellidae) on aboveground tissue (sweep net in soybean)",
                    "total abundance of green lacewings (Chrysopidae)on aboveground tissue (sweep net in soybean)",
                    "total abundance of damsel bugs (Nabidae)on aboveground tissue (sweep net in soybean)",
                    "total abundance of Minute Pirate Bugs (Anthocoridae) on aboveground tissue (sweep net in soybean)"
                  )
                
                invert_preds_soilcomm_abund<-
                  c(
                    "total species of predator taxa collected from soil surface",
                    "total species of predator taxa collected from soil column",
                    "total abundance of invertebrates on soil surface (pitfall traps in maize)",
                    "total abundance of invertebrates on soil surface (pitfall traps in soybean)"
                  )
                
                invert_preds_soilcomm_div <-
                  c(
                    "diversity of soil surface predators (Shannon's H)",
                    "diversity of predators in soil column (Shannon's H)",
                    "diversity of soil surface predators (Evenness J)",
                    "diversity of predators in soil column (Evenness J)"
                  )
                
                invert_preds_vegcomm_abund <-
                  c("abundance of generalist foliage-foraging predators over times",
                    "mean predator abundance per vacuum sample",
                    "mean predator abundance per vacuum sample (five sampling events)",
                    "total abundance of invertebrates  on aboveground plant tissue (sweep net in soybean)"
                  )
                
                invert_preds_vegcomm_div <-
                  c(
                    "mean predator diversity (Simpson's 1-D) per vacuum sample (four sampling events)"
                  )
                
                invert_preds_activity <-
                  c("mean percent sentinel egg removal per 48 hours (3 sampling events)")
                
                invert_nonpredpest <-
                  c(
                    "total abundance of ants (Formicidae) on soil surface (pitfall traps in maize)",
                    "total abundance of ants (Formicidae) on soil surface (pitfall traps in soybean)",
                    "total abundance of beetles (Coleoptera)on soil surface (pitfall traps in maize)",
                    "total abundance of beetles (Coleoptera)on soil surface (pitfall traps in soybean)",
                    "total abundance of millipedes (Diplopoda) on soil surface (pitfall traps in maize)",
                    "total abundance of millipedes (Diplopoda) on soil surface (pitfall traps in soybean)",
                    "total abundance of crickets (Gryllidae) on soil surface (pitfall traps in maize)",
                    "total abundance of crickets (Gryllidae) on soil surface (pitfall traps in soybean)",
                    "total abundance of hoverflies (Syrphidae) on aboveground tissue (sweep net in soybean)",
                    "total abundance of Chalcid wasps (Chalcidoidae) on aboveground tissue (sweep net in soybean)",
                    "total abundance of parasitic flies (Tachinidae) on aboveground tissue (sweep net in soybean)",
                    "total abundance of invertebrates on aboveground plant tissue (sweep net in soybean)"
                    
                  )
                
                pathogen <- c(
                  "soybean root rot (general)"                                                                                                                             
                )
                
                
                ####Crop Production####
                
                ## Yields ####
                
                yields_grainsoy <- c(
                  "soybean grain yield per 1.8 m row",
                  "soybean grain yield (years 5 & 6)",
                  "soybean grain yield (7 year average)",                           
                  "soybean grain yield",
                  "soybean grain yield (4 year average)",
                  "soybean yield (3 year average)"
                )
                
                yields_grainmaize <- c(
                  "maize grain yield",
                  "maize grain yield after soybean overseeded with cc (years 5 & 6)",
                  "maize grain yield (4 year average)",
                  "maize grain yield, weight of kernels",
                  "maize grain yield (7 year average)" ,                            
                  "barren maize stalk",
                  "maize grain moisture content (4 year average)",             
                  "Harvest Index (maize)",
                  "maize grain dry weight (3 year average)"
                )
                
                yields_biomass_abvgrd <- c(
                  "maize silage dry matter yield",
                  "aboveground dry biomass (maize) at harvest",
                  "plant height at maize reproductive stage 1",
                  "plant height at maize reproductive stage 2",
                  "plant height at maize reproductive stage 3",
                  "plant height at maize reproductive stage 4",
                  "plant height at maize reproductive stage 5",
                  "plant height at maize reproductive stage 6",
                  "maize silage yield" ,                                       
                  "maize stover yield",
                  "maize stover dry weight (3 year average)"
                )
                
                yields_biomass_blwgrd <- c("maize root biomass (0-100 cm)")
                
                ##Crop Nitrogen Content ####
                
                crop_N_maizestalk <- c(
                  "maize stalk nitrate",
                  "total aboveground N uptake (maize)",
                  "maize silage N removal",
                  "maize stover nitrogen uptake" ,
                  "total nitrogen uptake by maize stover (3 year average)",
                  "aboveground plant nitrogen uptake")
                
                crop_N_maizegrain <- c( 
                  "maize grain nitrogen uptake",
                  "maize nitrogen uptake",                                     
                  "total nitrogen uptake by maize grain (3 year average)")
                
                crop_N_soybean <- c(  
                  "total aboveground N uptake (soybean)"
                )
                
                ##Crop seedling density####
                
                seedling_density <- c(
                  "soybean stand count",                                       
                  "maize stand count (4 year average)",                   
                  "maize stand count"
                )
                
                ####Water Movement####
                
                ## Drainage ####
                
                drainage <- c(
                  "average drainage",
                  "infiltration rate",
                  "runoff rate",
                  "drainage discharge (maize)",
                  "drainage discharge (soybean)",
                  "average annual subsurface drainage (maize)",
                  "average annual subsurface drainage (soybean)",
                  "average annual soil water storage (maize)",
                  "average annual soil water storage (soybean)"
                )
                
                ## Runoff ####
                
                runoff_nitrate <-
                  c(
                    "subsurface drainage nitrate (NO3-N) annual yields",
                    "subsurface drainage annual flow weighted concentrations of nitrate (NO3-N)",
                    "subsurface drainage nitrate (NO3-N) yield per amount of N-fertilizer applied during spring months",
                    "average flow weighted nitrate drainage",
                    "cumulative annual nitrate load of drainage water",
                    "concentration of total NO3-N in ground water under maize",
                    "total NO3-N lost from soil under maize",
                    "concentration of total NO3-N in ground water under soybean",
                    "total NO3-N lost from soil under soybean",
                    "flow-weighted average NO3-N concentration in subsurface drainage over 4 years (maize)",
                    "flow-weighted average NO3-N concentration in subsurface drainage over 4 years (soybean)",
                    "average NO3-N loss through subsurface drainage over 4 years (maize)",
                    "average NO3-N loss through subsurface drainage over 4 years (soybean)",
                    "subsurface drainage nitrate (NO3-N) annual yields",
                    "dissolved organic nitrogen (DON)",
                    "normalized NO3-N losses",
                    "runoff quantity following fall manure",
                    "runoff quantity following spring manure",
                    "time to runoff following fall manure",
                    "time to runoff following spring manure"
                    
                  )
                
                runoff_phosphorus <-
                  c(
                    "subsurface drainage total reactive phosphorus (TRP) annual yields",
                    "Subsurface drainage total reactive phosphorus (TRP) annual flow weighted concentrations",
                    "subsurface drainage total reactive phosphorus (TRP) annual concentrations",
                    "reactive phosphorus load runoff following fall manure",
                    "reactive phosphorus load runoff following spring manure",
                    "total phosphorus load runoff following fall manure",
                    "total phosphorus load runoff following spring manure"
                    
                  )
                
                
                



                
###Apply metric & grouping labels to dataframe#####################################


              metric_labels <- Results %>%
                select(Response_var, Res_key) %>%
                mutate(
                  group_metric = case_when(
                    
                    #Soils####
                    #Chemical Properties####
                    
                    Response_var %in% chem_nitrate_spring ~ "Nitrate (Preplant)",
                    Response_var %in% chem_nitrate_maize ~ "Nitrate (Maize)",
                    Response_var %in% chem_nitrate_soybean ~ "Nitrate (Soybean)",
                    Response_var %in% chem_nitrate_fall ~ "Nitrate (Post Harvest)",
                    Response_var %in% chem_ammonium_spring ~ "Ammonium (Preplant)",
                    Response_var %in% chem_totalN ~ "Total Nitrogen",
                    Response_var %in% chem_phosphorus ~ "phosphorus",
                    Response_var %in% chem_potassium ~ "Postassium",
                    Response_var %in% chem_acidity ~ "pH",
                    Response_var %in% chem_carbon_020 ~ "Soil Carbon, 0-20 cm depth",
                    Response_var %in% chem_carbon_2060 ~ "Soil Carbon, 20-60 cm depth",
                    Response_var %in% chem_carbon_075 ~ "Soil Carbon, 0-75 cm depth",
                    Response_var %in% chem_som ~ "Soil Organic Matter",
                    
                    #Physical Properties####
                    Response_var %in% phy_compaction ~ "Compaction",
                    Response_var %in% phy_pores ~ "Soil Pores",
                    Response_var %in% phy_aggregates ~ "Soil Aggregates",
                    Response_var %in% phy_bulkdensity ~ "Soil Bulk Density",
                    Response_var %in% phy_texture ~ "Soil Texture",
                    Response_var %in% phy_watercontent ~ "Soil Water Content",
                    
                    #Biological Properties####
                    
                    Response_var %in% biol_microbes ~ "Microbial Biomass",
                    
                    
                    #Environmental Properties####
                    Response_var %in% envir_temp ~ "Soil Temperature",
                    Response_var %in% envir_CO2 ~ "Carbon Dioxide Emissions",
                    Response_var %in% envir_N2O ~ "Nitrous Oxide Emissions",
                    Response_var %in% phy_erosion ~ "Erosion",
                    
                    #Pest Regulation####
                    #Weeds####
                    Response_var %in% weed_waterhemp ~ "Waterhemp",
                    Response_var %in% weed_deadnettle ~ "Deadnettle",
                    Response_var %in% weed_cocklebur ~ "Cocklebur",
                    Response_var %in% weed_foxtail ~ "Giant Foxtail",
                    Response_var %in% weed_Palmeramaranth ~ "Pigweed",
                    
                    
                    Response_var %in% weed_community_biomass ~ "Weed community (Aboveground Growth)",
                    Response_var %in% weed_community_density ~ "Weed community (Abundance)",
                    
                    #Invertebrates####
                    Response_var %in% invert_pests_cornrootworm ~ "Corn Rootworm (#)",
                    Response_var %in% invert_pests_seedcornmaggot ~ "Seedcorn Maggot (#)",
                    Response_var %in% invert_pests_Aglycines ~ "Soybean Aphid (#)",
                    Response_var %in% invert_pests_SCN ~ "Soybean Cyst Nematode (#)",  
                    
                    Response_var %in% invert_pests_seedcornmaggot_damage ~ "Seedcorn Maggot (Crop Damage)",
                    Response_var %in% invert_pests_cornrootworm_damage ~ "Corn Rootworm (Crop Damage)",
                    
                    Response_var %in% invert_preds ~ "Predators (#)",
                    
                    Response_var %in% invert_preds_vegcomm_div ~ "Predator community in Foliage (#)",
                    Response_var %in% invert_preds_vegcomm_abund ~ "Predator community in Foliage (#)",
                    Response_var %in% invert_preds_soilcomm_div ~ "Predator community in Soils (Diversity)",
                    Response_var %in% invert_preds_soilcomm_abund ~ "Predator community in Soils (#)",
                    
                    Response_var %in% invert_preds_activity ~ "Predator Activity",
                    Response_var %in% invert_nonpredpest ~ "Non-predators & Non-pests",
                    Response_var %in% pathogen ~ "Pathogens",
                    
                    #Crop Production####
                    #Yields####
                    Response_var %in% yields_grainsoy ~ "Grain (Soybean)",
                    Response_var %in% yields_grainmaize ~ "Grain (Maize)",
                    Response_var %in% yields_biomass_abvgrd ~ "Stover Biomass",
                    Response_var %in% yields_biomass_blwgrd ~ "Root biomass",
                    
                    #Crop Nitrogen Yields####
                    Response_var %in% crop_N_maizegrain ~ "Grain (Maize)",
                    Response_var %in% crop_N_maizestalk ~ "Stalk/Stover (Maize)",
                    Response_var %in% crop_N_soybean ~ "Grain (Soybean)",
                    
                    #Crop Seedling Density####
                    Response_var %in% seedling_density ~ "Seedling Density",
                    
                    
                    #Water Movement####
                    #Drainage####
                    Response_var %in% drainage ~ "Drainage",
                    
                    #Runoff####
                    Response_var %in% runoff_nitrate ~ "Nitrate",
                    Response_var %in% runoff_phosphorus ~ "phosphorus"
                    # TRUE                      ~  "other"
                               ) ) %>%
                     
                
                                
                  
                  
                  
  #Create Main Groupings ############################################
   
mutate(
        main_group = case_when(
      
                  #Soils####
                    #Chemical Properties####
                    
                    Response_var %in% chem_nitrate_spring ~ "Chemical",
                    Response_var %in% chem_nitrate_maize ~ "Chemical",
                    Response_var %in% chem_nitrate_soybean ~ "Chemical",
                    Response_var %in% chem_nitrate_fall ~ "Chemical",
                    Response_var %in% chem_ammonium_spring ~ "Chemical",
                    Response_var %in% chem_totalN ~ "Chemical",
                    Response_var %in% chem_phosphorus ~ "Chemical",
                    Response_var %in% chem_potassium ~ "Chemical",
                    Response_var %in% chem_acidity ~ "Chemical",
                    Response_var %in% chem_carbon_020 ~ "Chemical",
                    Response_var %in% chem_carbon_075 ~ "Chemical",
                    Response_var %in% chem_carbon_2060 ~ "Chemical",
                    Response_var %in% chem_som ~ "Chemical",
                    
                    #Physical Properties####
                    Response_var %in% phy_compaction ~ "Physical",
                    Response_var %in% phy_pores ~ "Physical",
                    Response_var %in% phy_aggregates ~ "Physical",
                    Response_var %in% phy_bulkdensity ~ "Physical",
                    Response_var %in% phy_texture ~ "Physical",
                    Response_var %in% phy_watercontent ~ "Physical",
                    
                    #Biological Properties####
                    
                    Response_var %in% biol_microbes ~ "Biological",
                    
                    
                    #Environmental Properties####
                    Response_var %in% envir_temp ~ "Environmental",
                    Response_var %in% envir_CO2 ~ "Environmental",
                    Response_var %in% envir_N2O ~ "Environmental",
                    Response_var %in% phy_erosion ~ "Environmental",
                        
                    #Pest Regulation####
                        
                    #Weeds####
                    Response_var %in% weed_waterhemp ~ "Weeds",
                    Response_var %in% weed_deadnettle ~ "Weeds",
                    Response_var %in% weed_cocklebur ~ "Weeds",
                    Response_var %in% weed_foxtail ~ "Weeds",
                    Response_var %in% weed_Palmeramaranth ~ "Weeds",
                    
                    
                    Response_var %in% weed_community_biomass ~ "Weeds",
                    Response_var %in% weed_community_density ~ "Weeds",
                    
                    #Invertebrates####
                    Response_var %in% invert_pests_cornrootworm ~ "Invertebrates",
                    Response_var %in% invert_pests_seedcornmaggot ~ "Invertebrates",
                    Response_var %in% invert_pests_Aglycines ~ "Invertebrates",
                    Response_var %in% invert_pests_SCN ~ "Invertebrates",  
                    
                    Response_var %in% invert_pests_seedcornmaggot_damage ~ "Invertebrates",
                    Response_var %in% invert_pests_cornrootworm_damage ~ "Invertebrates",
                    
                    Response_var %in% invert_preds ~ "Invertebrates",
                    
                    Response_var %in% invert_preds_vegcomm_div ~ "Invertebrates",
                    Response_var %in% invert_preds_vegcomm_abund ~ "Invertebrates",
                    Response_var %in% invert_preds_soilcomm_div ~ "Invertebrates",
                    Response_var %in% invert_preds_soilcomm_abund ~ "Invertebrates",
                    
                    Response_var %in% invert_preds_activity ~ "Invertebrates",
                    Response_var %in% invert_nonpredpest ~ "Invertebrates",
                    Response_var %in% pathogen ~ "Pathogens",
                        
                    
                    
                    #Crop Production####
                    #Yields####
                    Response_var %in% yields_grainsoy ~ "Yields",
                    Response_var %in% yields_grainmaize ~ "Yields",
                    Response_var %in% yields_biomass_abvgrd ~ "Yields",
                    Response_var %in% yields_biomass_blwgrd ~ "Yields",
                    
                    #Crop Nitrogen Yields####
                    Response_var %in% crop_N_maizegrain ~ "Crop Nitrogen Uptake",
                    Response_var %in% crop_N_maizestalk ~ "Crop Nitrogen Uptake",
                    Response_var %in% crop_N_soybean ~ "Crop Nitrogen Uptake",
                    
                    #Crop Seedling Density####
                    Response_var %in% seedling_density ~ "Stand Count",
                    
                        
                    #Water Movement####
                    #Drainage####
                    Response_var %in% drainage ~ "Drainage",
                    
                    #Runoff####
                    Response_var %in% runoff_nitrate ~ "Runoff",
                    Response_var %in% runoff_phosphorus ~ "Runoff"
                    # TRUE                      ~  "other"
                ))
              
              
                
                
                
                #Attach column to Results######
                Results <-
                  left_join(Results, metric_labels, by = c("Res_key", "Response_var"))
                
                


#############################################################################################

missing <- Results[is.na(Results$group_metric),] #check to see if all rows have an assigned group_metric
missing <- Results[is.na(Results$Review_specific),] #check to see if all rows have an assigned


########################Tillage Review####################################################
#Bulk groupings####

####Soils####
##Soil Chemical Properties####
chem_som <- c("coarse particulate soil organic matter",
              "coarse partiuclate organic matter associated carbon",
              "soil organic matter content (May)",
              "soil organic matter in soil aggregate size <0.4 mm",
              "soil organic matter in soil aggregate size >19 mm",
              "soil organic matter in soil aggregate size 0.4-0.8 mm",
              "soil organic matter in soil aggregate size 0.8-2.0 mm",
              "soil organic matter in soil aggregate size 2.0-6.0 mm",
              "soil organic matter in soil aggregate size 6-19 mm",
              "soil organic matter, total carbon",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size <0.4 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size >19 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 0.4-0.8 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 0.8-2.0 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 2.0-6.0 mm",
              "ratio of fine particulate organic matter to soil organic matter (fPOM/SOM) in soil aggregate size 6-19 mm",
              "particulate organic matter carbon",
              "particulate organic matter carbon ",
              "particulate organic matter nitrogen",
              "fine particulate organic matter in soil aggregate size <0.4 mm",
              "fine particulate organic matter in soil aggregate size >19 mm",
              "fine particulate organic matter in soil aggregate size 0.4-0.8 mm",
              "fine particulate organic matter in soil aggregate size 0.8-2.0 mm",
              "fine particulate organic matter in soil aggregate size 2.0-6.0 mm",
              "fine particulate organic matter in soil aggregate size 6-19 mm",
              "fine particulate soil organic matter",
              "total soil organic matter (sum of loose and occluded POM) after harvest",
              "total soil organic matter (sum of loose and occluded POM) before planting",
              "loose organic matter isotopic composition and fractional proportion of carbon derived from labeled residues",
              "occluded particulate organic matter isotopic composition and fractional proportion of carbon derived from labeled residues",
              "humified organic matter isotopic composition and fractional proportion of carbon derived from labeled residues",
              "soil organic matter content",
              "soil organic matter",
              "total particulate organic matter",
              "total particulate organic matter in soil aggregate size <0.4 mm",
              "total particulate organic matter in soil aggregate size >19 mm",
              "total particulate organic matter in soil aggregate size 0.4-0.8 mm",
              "total particulate organic matter in soil aggregate size 0.8-2.0 mm",
              "total particulate organic matter in soil aggregate size 2.0-6.0 mm",
              "total particulate organic matter in soil aggregate size 6-19 mm",
              "soil particulate organic matter",
              "soil carbon concentration in particulate organic matter"
)

chem_maize_SOC <- c(
  "average annual carbon input from cash crops",
  "corn-derived carbon (C4-C)",
  "eroded soil carbon stock from maize (C4-C) in topsoil",
  "efficiency of returned carbon from maize root and shoot biomass",
  "efficiency of returned carbon from maize root biomass",
  "carbon in maize root and shoot biomass",
  "carbon in maize root biomass",
  "cob biomass carbon content yield",
  "concentration of carbon in maize residue",
  "concentration of carbon in soybean residue",
  "maize carbon input (aboveground residue)",
  "soybean carbon input (aboveground residue)",
  "stover biomass carbon content yield",
  "total crop carbon input",
  "total crop carbon input (maize and soybean residue)"
  
)

chem_crop_SON <- c(
      "annual nitrogen input from maize",
      "annual nitrogen input from soybean",
      "average annual nitrogen input from cash crops"
)


chem_SOC <-  c("total carbon (C4--C & C3-C)",
               "total carbon concentration",
               "total carbon in soil",
              "soil organic carbon by weight mid-season",
               "soil organic carbon contents ",
               "organic carbon ",
               "organic carbon in particulate organic matter (POM)",
               "mineral associated organic carbon",
               "percent carbon in particulate organic matter (POM) ",
               "volumetric soil organic content",
               "soil organic stock",
               "total soil organic stock",
               "soil organic carbon concentration (after harvest",
                "soil organic carbon storage after harvest",
                "soil organic carbon concentration in spring",
                "soil organic carbon storage in the spring",
                "soil carbon in loose-particulate organic matter after harvest",
                "soil carbon in loose-particulate organic matter before planting",
                "soil carbon in occluded-particulate organic matter after harvest",
                "soil carbon in occluded-particulate organic matter before planting",
                "soil organic carbon concentration",
                "soil organic carbon stocks",
                "macroaggregate protected carbon",
                "macroaggregate protected organic carbon",
                "total soil carbon",
                "carbon content in <0.25 mm aggregate size fraction",
                "carbon content in 0.25-0.5 mm aggregate size fraction",
                "carbon content in 0.5-1 mm aggregate size fraction",
                "carbon content in 1-2 mm aggregate size fraction",
                "carbon content in 2-5 mm aggregate size fraction",
                "carbon content in 5-8 mm aggregate size fraction",
                "% soil organic carbon loss as flux (180 days during fallow period)",
                "active carbon concentration",
                "soil organic carbon",
                "delta 13 C in soil",
                "delta-13C value in soil",
                "cumulative stock of soil organic carbon",
                "soil organic carbon ",
                "soil organic carbon concentration ",
                "soil organic carbon concentration on a mass basis",
                "soil organic carbon concentration on an area basis",
                "soil organic carbon content",
                "soil organic carbon content in Crosby silt loam after harvest",
                "soil organic carbon content in Wooster silt loam after harvest",
                "soil organic carbon derived from maize root and shoot biomass",
                "soil organic carbon derived from maize root biomass",
                "soil organic carbon in g/kg",
                "soil organic carbon in March",
                "soil organic carbon in Mg/ha",
                "soil organic carbon of soil aggregate size <0.4 mm",
                "soil organic carbon of soil aggregate size >19 mm",
                "soil organic carbon of soil aggregate size 0.4-0.8 mm",
                "soil organic carbon of soil aggregate size 0.8-2.0 mm",
                "soil organic carbon of soil aggregate size 2.0-6.0 mm",
                "soil organic carbon of soil aggregate size 6-19 mm",
                "soil organic carbon on area basis",
                "soil organic carbon pool",
                "soil organic carbon pool, C3",
                "soil organic carbon pool, C4",
                "soil organic carbon reserves",
                "soil organic carbon stock",
                "soil organic carbon stock ",
                "soil organic carbon stock (C3-C) in topsoil",
                "soil carbon stock from maize (C4-C) in topsoil",
                "soil carbon stored in heavy fractions ",
                "eroded soil organic carbon stock (C3-C) in topsoil",
                "soil carbon stored in light fractions ",
                "mass of soil organic carbon",
                "maximum mineralizable soil carbon (Michaelis-Menten equation)",
                "old carbon (C3-C)",
                "organic carbon pool in soil",
                "light fraction of carbon",
                "soil total carbon",
                "soil total organic carbon",
                "weighted soil organic carbon",
                "total soil organic carbon",
                "total soil organic carbon pool in soil",
                "half-life of C-soil organic carbon",
                "minearlizable soil carbon",
                "mineral fraction carbon",
                "natural abundance of 13-C in soil (delta 13-C)",
                "equivalent mass of soil organic carbon",
                "total soil carbon ",
                "change in soil organic carbon concentrations",
                "potassium sulfate extractable field-moist soil carbon",
                "potassium sulfate extractable microwaved soil carbon",
                "passive organic carbon concentration",
                "passive organic carbon concentration/active carbon",
                "soil carbon",
                "soil carbon concentration",
                "soil carbon concentration whole soil"
)


chem_CNratio <- c("C:N ratio ",
                  "C:N ratio in particulate organic matter (POM)",
                  "mineral associated C:N ratio",
                  "C:N ratio",
                  "carbon to nitrogen ratio in of soil aggregate size <0.4 mm",
                  "carbon to nitrogen ratio in of soil aggregate size >19 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 0.4-0.8 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 0.8-2.0 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 2.0-6.0 mm",
                  "carbon to nitrogen ratio in of soil aggregate size 6-19 mm",
                  "carbon to nitrogen ratio in soil",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size <0.4 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 0.4-0.8 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 0.8-2.0 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 2.0-6.0 mm",
                  "carbon/nitrogen ratio of humic acid in soil aggregate size 6-19 mm"
                  
)

chem_nitrogen_SOM <-
  c(
    "nitrogen in particulate organic matter (POM)",
    "percent nitrogen in particulate organic matter (POM) ",
    "coarse partiuclate organic matter associated nitrogen",
    "nitrogen content in <0.25 mm aggregate size fraction",
    "nitrogen content in 0.25-0.5 mm aggregate size fraction",
    "nitrogen content in 0.5-1 mm aggregate size fraction",
    "nitrogen content in 1-2 mm aggregate size fraction",
    "nitrogen content in 2-5 mm aggregate size fraction",
    "nitrogen content in 5-8 mm aggregate size fraction",
    "soil carbon to nitrogen ratio",
    "soil carbon to nitrogen ratio ",
    "soil nitrogen concentration in particulate organic matter",
    "soil organic matter, total nitrogen",
    "soil organic nitrogen ",
    "soil organic nitrogen on area basis"
  )

chem_totalN <-
  c("total nitrogen ",
    "total nitrogen",
    "total soil nitrogen",
    "mineral associated nitrogen",
    "nitrogen pool in soil",
    "cumulative stock of nitrogen",
    "soil nitrogen",
    "soil nitrogen concentration",
    "soil nitrogen concentration whole soil",
    "soil nitrogen content",
    "soil nitrogen mineralization",
    "soil nitrogen pool",
    "soil nitrogen stored in heavy fractions ",
    "soil nitrogen stored in light fractions ",
    "soil total nitrogen",
    "soil total nitrogen concentration",
    "soil total nitrogen concentration ",
    "soil total nitrogen pool",
    "soil inorganic nitrogen (early fall)",
    "soil inorganic nitrogen (late fall)",
    "soil inorganic nitrogen (late summer)",
    "soil inorganic nitrogen (mid summer)",
    "light fraction of nitrogen",
    "inorganic nitrogen early August",
    "inorganic nitrogen early July",
    "inorganic nitrogen early September",
    "net N-mineralization",
    "potentially mineralizable nitrogen",
    "total inorganic nitrogen",
    "total nitrogen concentration",
    "total nitrogen concentration in soil",
    "total nitrogen in soil",
    "mass of soil nitrogen",
    "equivalent mass of soil nitrogen",
    "mineralizable nitrogen",
    "soil potentially mineralizable nitrogen",
    "soil profile of inorganic nitrogen",
    "total available inorganic nitrogen pool",
    "total soil nitrogen intensity (nitrate & ammonium)",
    "apparent nitrogen mineralization"
  )

chem_pH <-
  c(
    "soil pH mid-season",
    "soil pH",
    "pH",
    "soil total base saturation"
  )

chem_CEC <- c(
  "CEC mid-season",
  "cation exchange capacity",
  "cation-exchange capacity in Crosby silt loam after harvest",
  "cation-exchange capacity in Wooster silt loam after harvest",
  "electirical conductivity",
  "electrical conductivity",
  "electrical conductivity of soil",
  "soil cation exchange capacity",
  "soil CEC in March",
  "soil electical conductivity",
  "soil electircal conductivity",
  "soil electrical conductivity"
)

chem_phosphorus <- c(
  "soil phosphorus mid-season",
  "soil phosphorus",
  "soil phosphorus [Bray P1]",
  "soil phosphorus with Bray P1 extraction",
  "available phosphorus",
  "available soil phosphorus",
  "Bray-extractable phosphorus",
  "extractable phosphorus",
  "extractable soil phosphorus",
  "extractable soil phosphorus levels (Bray P)",
  "soil phosphorus (Bray)",
  "soil phosphorus (Bray-P1)",
  "soil phosphorus (Olsen P)",
  "soil phosphorus content",
  "soil phosphorus content in interrow after 4 years of broadcast fertilizing",
  "soil phosphorus content in row after 4 years of broadcast fertilizing",
  "water-extractable phosphorus",
  "soil phosphate"
)

chem_potassium <- c(
  "soil potassium mid-season",
  "soil potassium",
  "extractable potassium",
  "exchangeable potassium",
  "exchangeable potassium in soil",
  "exchangeable soil potassium",
  "soil potassium base saturation",
  "soil potassium content",
  "soil potassium content in interrow after 4 years of broadcast fertilizing",
  "soil potassium content in row after 4 years of broadcast fertilizing",
  "soil base saturation-Potassium",
  "soil exchangeable potassium"
)

chem_calcium <- c(
  "soil calcium mid-season",
  "soil calcium",
  "exchangeable calcium",
  "soil base saturation-Calcium",
  "soil Ca concentrations in March",
  "soil calcium base saturation",
  "soil exchangeable calcium"
)

chem_magnesium <- c(
  "soil magnesium mid-season",
  "soil magnesium",
  "exchangeable magnesium",
  "soil Mg concentrations in March",
  "soil magnesium base saturation",
  "soil base saturation-Magnesium",
  "soil exchangeable magnesium"
)


chem_misc_nutrients <- c(
    "soil cobalt",
    "soil copper",
    "soil exchangeable sodium",
    "soil hydrogen base saturation",
    "soil molybdenum",
    "soil nickel",
    "soil zinc",
    "soil sodium",
    "soil sulfate",
    "soil sulfur",
    "soil boron",
    "soil iron",
    "soil aluminium",
    "soil manganese"
)

chem_ammonium <- c(
    "soil ammonium mid-season",
    "soil ammonium",
    "soil ammonium (110 Julian day)",
    "soil ammonium (200 Julian day)",
    "soil ammonium (300 Julian day)",
    "soil ammonium concentration in April",
    "soil ammonium concentration in August",
    "soil ammonium in July",
    "soil ammonium in October"
)

chem_nitrate <- c("soil nitrate mid-season",
                  "soil nitrate 36 days after planting",
                  "soil nitrate 64 days after planting",
                  "soil nitrate 92 days after planting",
                  "soil nitrate 42 days after planting",
                  "soil nitrate 72 days after planting",
                  "soil nitrate 103 days after planting",
                  "after-harvest total residual soil nitrate",
                  "soil nitrate",
                  "soil nitrate (110 Julian day)",
                  "soil nitrate (200 Julian day)",
                  "soil nitrate (300 Julian day)",
                  "soil nitrate 02 October",
                  "soil nitrate 11 May",
                  "soil nitrate 13 November",
                  "soil nitrate 18 June",
                  "soil nitrate 18 September",
                  "soil nitrate 2 October",
                  "soil nitrate 23 June",
                  "soil nitrate 23 September",
                  "soil nitrate 25 September",
                  "soil nitrate 29 April",
                  "soil nitrate 30 May",
                  "soil nitrate 30 October",
                  "soil nitrate concentration",
                  "soil nitrate concentration in April",
                  "soil nitrate concentration in August",
                  "soil nitrate content",
                  "soil nitrate in July",
                  "soil nitrate in October",
                  "soil nitrate-N following maize",
                  "soil nitrate-N following soybean",
                  "soil nitrogen intensity of ammonium",
                  "soil nitrogen intensity of nitrate",
                  "soil inorganic nitrate nitrogen concentration"#,
                  #"overwinter changes in soil nitrate following maize",  ####Excluded -> change in value rather than raw value
                  #"overwinter changes in soil nitrate following soybean" ####Excluded -> change in value rather than raw value
)

##Soil Physical Properties####

phy_silt <- c(
  "percent silt",
  "percent silt soil texture",
  "silt concentrations",
  "silt content",
  "silt particle size distribution in spring"
)

phy_clay <- c(
  "percent clay",
  "clay concentrations",
  "clay content",
  "clay particle size distribution in spring",
  "percent clay soil texture"
)

phy_sand <- c(
  "sand concentrations",
  "sand content",
  "sand particle size distribution in spring",
  "percent sand",
  "percent sand soil texture"
)

phy_waterinfiltration <- c(
  "infiltration rate in crop row",
  "infiltration rate in crop interrow",
  "cumulative infiltration in soil",
  "cumulative infiltration over 190 min",
  "infiltration rate",
  "infiltration rate in soil after 2.5 hrs",
  "infiltration rate in soil after 5 minutes",
  "mean cumulative infiltration rate at 3 h",
  "water drop pentration test",
  "water infiltration rate at 140 mins in August",
  "water infiltration rate at 40 mins in August",
  "water infiltration rate at 5 mins in August",
  "water infiltration time of first 2.5 cm water in soybeans",
  "water infiltration time of second 2.5 cm water",
  "steady-state infiltration of soil",
  "soil water sorptivity",
  "sorptivity of soil",
  "sorptivity rate",
  "cumulative infiltration",
  "water infiltration rate",
  "water percolation through soil during growing season",
  "water percolation through soil during non-growing season",
  "annual drainage",
  "drainage",
  "annual subsurface drainage flow"
)

phy_aggregation_diameter <- c( 
  "soil aggregate dry mean weight diameter ",
  "soil aggregate wet mean weight diameter",
  "geometric mean diameter of aggregates",
  "mean weight diameter of aggregates",
  "mean weight diameter in Crosby silt loam after harvest",
  "mean weight diameter in Wooster silt loam after harvest",
  "mean weight diameter of aggregates",
  "mean weight diameter of aggregates (0 kg N/ha applied)",
  "mean weight diameter of aggregates (160 kg N/ha applied)",
  "mean weight diameter of aggregates (80 kg N/ha applied)",
  "mean weight diameter of water-stable aggregates in spring",
  "geometric mean weight diameter of sand-free water stable aggregates"
)


phy_aggregation_stability <- c(
  "wet soil aggregate stability",
  "water stable aggregates ",
  "water aggregate stability (>0.25 mm) of whole soil in maize",
  "percent water stable aggregates",
  "water aggregate stability",
  "water aggregate stability in crop row",
  "water aggregate stability in crop interrow",
  "water stable aggregates",
  "aggregate mean weight diameter in August",
  "aggregate stability",
  "aggregate stability in August",
  "aggregate tensile strength",
  "density of soil aggregates (10 mm diameter) in soil clod",
  "macro-aggregate (0.25-8 mm) distribution",
  "macroaggregates",
  "soil water stable aggregates < 53 um ",
  "soil water stable aggregates > 2000 um ",
  "soil water stable aggregates 250-2000 um ",
  "soil water stable aggregates 53-250 um ",
  "wet soil macroaggregate (0.5 to 2 mm) stability (240 min of sieving)",
  "wet soil macroaggregate (0.5 to 2 mm) stability (3 min of sieving)",
  "wet soil microaggregate (0.053 to 0.25 mm) stability (240 min of sieving)",
  "wet soil microaggregate (0.053 to 0.25 mm) stability (3 min of sieving)",
  "soil aggregation in August ",
  "soil aggregation in March",
  "percent aggregation > 1 mm",
  "total percent aggregation",
  "water stable aggregates",
  "water stable macroaggregate fraction",
  "water-soluble aggregates in Crosby silt loam after harvest",
  "water-soluble aggregates in Wooster silt loam after harvest",
  "water-stable aggregation size 0.4-0.8 mm",
  "water-stable aggregation size 0.8-2.0 mm",
  "water-stable aggregation size 10 mm",
  "water-stable aggregation size 2.0-6.0 mm",
  "wet aggregate stability",
  "soil stability (slake rating)"
)
phy_compaction <- c(
  "cone penetration resistance adjusted for soil moisture one month after planting",
  "cone penetration resistance one month after planting",
  "cone penetration resistance adjusted for soil moisture at planting",
  "soil penetration resistance",
  "soil compaction",
  "penetration resistance in Crosby silt loam after harvest",
  "penetration resistance in Wooster silt loam after harvest",
  "soil penetration resistance in August",
  "soil penetrometer resistance"
  )

phy_watercontent <- c(
  "soil water content one month after planting",
  "soil water content midseason",
  "soil water content at planting",
  "soil water content",
  "soil moisture in crop midrow 36-49 days after planting",
  "soil moisture in crop midrow 43-79 days after planting",
  "soil moisture in crop midrow 49-71 days after planting",
  "soil moisture in crop row 1-36 days after planting",
  "soil moisture in crop row 36-49 days after planting",
  "soil moisture in crop row 36-58 days after planting",
  "soil moisture in crop row 43-79 days after planting",
  "soil moisture in crop row 49-71 days after planting",
  "soil moisture in crop row 58-78 days after planting",
  "soil moisture in crop row 6-36 days after planting",
  "soil water 36 days after planting",
  "soil water 64 days after planting",
  "soil water 92 days after planting",
  "soil water 42 days after planting",
  "soil water 72 days after planting",
  "soil water 103 days after planting",
  "soil moisture at maize emergence",
  "soil moisture at maize mid-silk",
  "available water capacity",
  "available water capacity (AWC)",
  "available water capacity in summer",
  "available water-holding capacity in Crosby silt loam after harvest",
  "available water-holding capacity in Wooster silt loam after harvest",
  "field capacity",
  "gravimetric soil moisture (autumn season)",
  "gravimetric soil moisture (spring season)",
  "gravimetric soil moisture (summer season)",
  "gravimetric soil moisture (winter season)",
  "gravimetric soil moisture early August",
  "gravimetric soil moisture early July",
  "gravimetric soil moisture early September",
  "gravimetric water content of soil",
  "permanent wilting point",
  "plant available soil water",
  "plant available water",
  "soil water content",
  "soil water content in soybean",
  "soil moisture",
  "soil moisture after wetting",
  "soil moisture at post-emergence",
  "soil moisture at preharvest",
  "soil moisture at tasseling",
  "soil moisture content in fall",
  "soil moisture content in summer",
  "soil moisture in April",
  "soil moisture in August",
  "soil moisture retention at 0.03 Mpa",
  "soil moisture retention at 0.033 MPa pressure in August ",
  "soil moisture retention at 1.5 Mpa",
  "volumetric soil water content",
  "volumetric water content",
  "volumetric water content (19 June)",
  "volumetric water content (2 October)",
  "volumetric water content (24 July)",
  "volumetric water content when soil matric potential at -10 kPa",
  "water-filled pore space after 60 min",
  "water holding capacity",
  "water uptake rate after 10 min",
  "water repellency index",
  "soil water holding capacity"
)

phy_bulkdensity <- c(
  "soil bulk density at planting",
  "soil bulk density one month after planting",
  "soil bulk density from soil clod one month after planting",
  "soil bulk density from soil clod one month after planting (6 year average)",
  "soil bulk density one month after planting (6 year average)",
  "soil bulk density after harvest",
  "soil bulk density in the spring",
  "soil mass in the spring",
  "soil bulk density",
  "soil bulk density",
  "dry soil bulk density",
  "soil bulk denity during summer",
  "soil bulk density ",
  "soil bulk density in Crosby silt loam after harvest",
  "soil bulk density in March",
  "soil bulk density in summer",
  "soil bulk density in Wooster silt loam after harvest",
  "soil bulk density stover residue partially left in field",
  "wet soil bulk density",
  "weighted bulk density",
  "particle density in Crosby silt loam after harvest",
  "particle density in Wooster silt loam after harvest",
  "mass of soil "
)

phy_airfilled_pores <- c(
  "air-filled porosity at planting",
  "air-filled porosity one month after planting",
  "air-filled porosity two months after planting",
  "air-filled porosity one month after planting ",
  "air-filled porosity midseason ",
  "air filled pore space",
  "air-filled porosity (fa)",
  "oxygen diffusion rate (12 June)",
  "oxygen diffusion rate (19 June)",
  "oxygen diffusion rate (25 June)",
  "air entry or bubbling pressure",
  "soil air content",
  "volumetric air content",
  "soil relative gas diffusion"
)

phy_waterfilled_pores <- c(
  "water-filled porosity at planting",
  "water-filled porosity one month after planting",
  "water-filled porosity one month after planting (clod sample)",
  "water-filled porosity two months after planting",
  "water-filled porosity one month after planting ",
  "water-filled porosity midseason ",
  "water filled pore space",
  "water filled pore space after irrigation",
  "water filled pore space before irrigation"
)

phy_totalpores <- c(
  "total porosity one month after planting ",
  "total porosity midseason ",
  "total porosity in soil",
  "mean volume of residual pores",
  "mean volume of storage pores",
  "mean volume of transmission pores",
  "median pore radius",
  "saturated hydraulic conductivity",
  "saturated hydraulic conductivity (Ks)",
  "saturated hydraulic conductivity in Crosby silt loam after harvest",
  "saturated hydraulic conductivity in Wooster silt loam after harvest",
  "saturated hydraulic conductivity rate",
  "effective porosity (fe)",
  "soil macropores",
  "pore size distribution parameter",
  "pore space in soil clod",
  "pore tortuosity factor",
  "porosity in Crosby silt loam after harvest",
  "porosity in Wooster silt loam after harvest",
  "relative gas diffusion coefficient",
  "soil aeration porosity (August)",
  "soil aeration porosity (July)",
  "soil aeration porosity (June)",
  "soil aeration porosity (September)",
  "soil micropores",
  "soil fine mesopores",
  "total pore space",
  "total porosity (ft)",
  "transmissiivty of soil",
  "volume of transmission (TrP)",
  "soil coarse mesopores",
  "soil porosity in March",
  "soil total porosity",
  "storage pores (StP)"
)

phy_erosion <- c(
  "calculated soil loss (Universal Soil Loss Equation)",
  "soil loss (calculated using the universal soil loss equation)",
  "soil loss"
)

phy_surfaceresidue <- c(
  "residue cover",
  "residue cover after planting on Readlyn soil",
  "residue cover after planting on Haig soil",
  "residue cover after planting on Webster soil",
  "residue remaining on soil from previous crop",
  "residue percent cover after planting (mean of maize and soybean years)",
  "proportion of soil surface covered by residues immediately after planting",
  "% surface covered with stover",
  "residue cover after planting",
  "residue coverage before planting",
  "percent of soil surface cover by residue",
  "surface residue cover before planting",
  "surface residue cover post-cultivation",
  "surface residue cover post-planting",
  "soil residue",
  "soil surface cover (May)",
  "soil surface cover (September)"
)

phy_surfaceresidue_decomp <- c(
  "first order decay rate (k) of aboveground litter"
)




##Soil Biological Properties####


biol_fungi_abund <- c(
  "abundance of basidiomycetes in soil in soil aggregate size 0.8-2.0 mm",
  "abundance of basidiomycetes in soil in soil aggregate size 2.0-6.0 mm",
  "abundance of basidiomycetes in soil in soil aggregate size 6-19 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size <0.4 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size >19 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 0.4-0.8 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 0.8-2.0 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 2.0-6.0 mm",
  "concentration of immunoreactive total glomalin in soil aggregate size 6-19 mm"
)

biol_enzyme_activity <- c(
  "aspartase activity in soils",
  "change in Amidase enzyme activity/change in soil pH",
  "change in arylamidase enzyme activity/change in soil pH",
  "change in L-Asparaginase enzyme activity/change in soil pH",
  "change in L-Aspartase enzyme activity/change in soil pH",
  "change in L-Glutaminase enzyme activity/change in soil pH",
  "change in Urease enzyme activity/change in soil pH",
  "soil enzyme activity (acid phos)",
  "soil enzyme activity (alk phos)",
  "soil enzyme activity (alpha glu)",
  "soil enzyme activity (arylsulf)",
  "soil enzyme activity (urease)",
  "microbial community utilization of substrates"
)

biol_respiration <- c(
  "soil respiration",
  "soil respiration (CO2-C) in in April",
  "soil respiration (CO2-C) in in April ",
  "soil respiration (CO2-C) in in August",
  "soil respiration (CO2-C) in in August ",
  "soil respiration after irrigation",
  "soil respiration before irrigation",
  "basal respiration rate",
  "specific maintenance respiration rate (qCO2)"
)

biol_microbial_biomass <- c(
  "microbial biomass nitrogen",
  "microbial nitrate",
  "soil microbial biomass carbon",
  "soil microbial biomass carbon ",
  "microbial carbon",
  "microbial biomass carbon",
  "soil microbial biomass",
  "soil microbial biomass on 20 May",
  "soil microbial biomass on 25 July",
  "microbial biomass loss",
  "microbial community utilization of substrates"
)


##Soil Environmental Properties####

envir_temp <- c(
  "soil temperature one month after planting",
  "soil temperature in crop row 6-36 days after planting",
  "soil temperature in crop midrow 6-36 days after planting",
  "soil temperature in crop row 36-58 days after planting",
  "soil temperature in crop midrow 36-58 days after planting",
  "soil temperature in crop row 58-78 days after planting",
  "soil temperature in crop midrow 58-78 days after planting",
  "soil temperature in crop row 13-36 days after planting",
  "soil temperature in crop midrow 13-36 days after planting",
  "soil temperature in crop row 36-49 days after planting",
  "soil temperature in crop midrow 36-49 days after planting",
  "soil temperature in crop row 49-71 days after planting",
  "soil temperature in crop midrow 49-71 days after planting",
  "soil temperature",
  "soil temperature (April-October",
  "soil temperature (autumn season)",
  "soil temperature (spring season)",
  "soil temperature (summer season)",
  "soil temperature (winter season)"
)

envir_CO2 <- c(
  "carbon dioxide concentration in soil 41 days after tillage",
  "carbon dioxide concentration in soil 7 days after tillage",
  "carbon dioxide concentration in soil 79 days after tillage",
  "carbon dioxide emisisons",
  "carbon dioxide emissions (180 days during fallow period)",
  "carbon dioxide equivalent emissions (180 days during fallow period)",
  "carbon dioxide flux",
  "carbon dioxide flux (180 days during fallow period)",
  "carbon dioxide flux (autumn season)",
  "carbon dioxide flux (spring season)",
  "carbon dioxide flux (summer season)",
  "carbon dioxide flux (winter season)",
  "carbon dioxide flux from soil 1 hr after tillage",
  "carbon dioxide flux from soil 2 hr after tillage",
  "carbon dioxide flux from soil 25 days after tillage",
  "carbon dioxide flux from soil 3 hr after tillage",
  "carbon dioxide flux from soil 4 hr after tillage",
  "carbon dioxide flux from soil 45 days after tillage",
  "carbon dioxide flux from soil 5 days after tillage",
  "carbon dioxide flux from soil 65 days after tillage",
  "carbon dioxide flux from soil 85 days after tillage",
  "carbon dioxide flux from soil at tillage",
  "carbon dioxide flux from soil before tillage",
  "annual carbon dioxide fluxes",
  "annual carbon input from maize",
  "annual carbon input from soybean",
  "annual soil carbon dioxide flux",
  "average weekly soil carbon dioxide emissions",
  "cumulative carbon dioxide emissions (April-October)",
  "soil surface carbon dioxide flux (Julian day = 140)",
  "soil surface carbon dioxide flux (Julian day = 180)",
  "soil surface carbon dioxide flux (Julian day = 220)",
  "soil surface carbon dioxide flux (Julian day = 260)",
  "total soil carbon dioxide emissions (20 day total)",
  "soil carbon dioxide emissions",
  "total carbon flux (180 days during fallow period)"
)


envir_N2O <- c(
  "annual nitrous oxide fluxes",
  "annual soil nitrous oxide flux",
  "averaged daily nitrous oxide-nitrogen emissions (continuous maize)",
  "averaged daily nitrous oxide-nitrogen emissions (maize-soybean rotation)",
  "cumulative nitrous oxide emissions",
  "cumulative nitrous oxide flux (April 2003-March 2004)",
  "cumulative nitrous oxide flux (April 2004-Feb 2005)",
  "nitrous oxide emisisons",
  "nitrous oxide emissions",
  "nitrous oxide emissions (180 days during fallow period)",
  "cumulative annual nitrous oxide emissions from soil",
  "nitrous oxide emission flux from soil (autumn)",
  "nitrous oxide emission flux from soil (spring)",
  "nitrous oxide emission flux from soil (summer)",
  "nitrous oxide emission flux from soil (winter)",
  "nitrous oxide emissions per unit area",
  "nitrous oxide emissions per unit grain yield",
  "nitrous oxide emissions per unit nitrogen grain yield",
  "nitrous oxide flux"
)

envir_CH4 <- c(
  "annual methane fluxes",
  "annual soil methane flux",
  "average annual soil methane emissions",
  "methane emissions (180 days during fallow period)",
  "cumulative annual methane emissions from soil",
  "methane emission flux from soil (autumn)",
  "methane emission flux from soil (spring)",
  "methane emission flux from soil (summer)",
  "methane emission flux from soil (winter)",
  "methane flux",
  "methane flux (180 days during fallow period)",
  "soil methane emissions",
  "soil methane emissions before fall tilling",
  "soil methane oxidation (CH4-C) in in April",
  "soil methane oxidation (CH4-C) in in April ",
  "soil methane oxidation (CH4-C) in in August",
  "soil methane oxidation (CH4-C) in in August ",
  "soil methane emission flux"
)

envir_globalwarmingpotential <- c(
  "annual global warming potential",
  "global warming impact",
  "greenhouse gas intensity (N2O/grain)"
)

####Pest Regulation####
## Weeds ####
weed_broadleaf <- c("abundance of Canada thistle,  Cirsium arvense",
    "abundance of Common dandelion, Taraxacum officinale",
    "abundance of Pennsylvania smartweed, Polygonum pensylvanicum",
    "abundance of Quackgrass, Agropyron repens",
    "abundance of broadleaf species",
    "density of common chickweed seeds",
    "density of corn speedwell seeds",
    "density of yellow woodsorrel seeds"
  )

weed_community_abundance <-
  c(   
    "number of weed seeds",
    "percent of weed seeds",
    "percent weed cover in maize row",
    "percent weed cover between maize rows",
    "escaped weeds after second and third year of tillage",
    "total abundance of weeds",
    "total number of germinable seeds ",
    "number of weed species ",
    "weed density"
  )

weed_grass <-
  c(   
    "abundance of grass species",
    "density of large crabgrass seeds"
  )

weed_community_diversity <-
  c(   "weed seed diversity (Shanon-Weiner heterogeneity)",
       "weed seed diversity (Simpson's)",
      "mean number of weed species"
  )

weed_lambsquarters <-
  c(
    "abundance of Common Lambsquarters",
    "abundance of Common lambsquarters, Chenopodium album",
    "density of common lambsquarters seeds"
  )


weed_amaranthus <- c(
  "abundance of Amaranthus species",
  "Amaranthus species density (weeds)",
  "abundance of Pigweed, Amaranthus spp.",
  "density of redroot pigweed seeds"
  )

weed_fallpanicum <- c(
  "abundance of Fall panicum",
  "abundance of Fall panicum, Panicum dichotomiflorum"
)

weed_velvetleaf <- c(
  "abundance of Velvetleaf"
  )

## Invertebrates ####

invert_pests_cornrootworm <-
  c(
    "adult counts of Western corn rootworm during highest infestation"
  )
invert_pests_seedcornmaggot <-
  c(
    "adult seedcorn maggot abundance",
    "seedcorn maggot (pest)"
  )

invert_pests_Aglycines <-
  c( "soybean aphid adult density 24 hr after release",
     "soybean aphid nymph density 24 hr after release",
     "soybean aphid adult density 6 days after release",
     "soybean aphid nymph density 14 days after release",
     "soybean aphid density at peak (13 Aug)"
  )

invert_pests_cornborer <-
  c("mature larval density, O. nubilalis",
    "density of European corn borer larvae (first generation)",
    "density of European corn borer larvae (second generation)",
    "Percent of Ostrinia nubilialis eggs hatched",
    "Density of aphids on maize when pest at first generation larval state (O. nubilalis)",
    "mature larval density per injured plant of first-generation O. nubilalis"
  )

invert_pests_damage <-
  c(  "Western corn rootworm  root damage evaluation",
      "slug damage to plants",
      "armyworm damage to plants",
      "cutworms damage to plants"
      
  )

invert_pests_miscabundance <- c(
        "abundance of Green cloverworm (herbivore)",
        "abundance of Bean leaf beetle (herbivore)",
        "abundance of Japanese beetle (herbivore)",
        "abundance of Grasshoppers (herbivore)",
        "abundance of Striped flea beetles (herbivore)",
        "abundance of Grape colaspis (herbivore)",
        "abundance of Tarnished plant bug (herbivore)",
        "abundance of Homopterans (herbivore)",
        "abundance of potato leafhoppers (pest)",
        "green cloverworm (pest)",
        "potato leafhopper (pest)",
        "bean leaf beetle (pest)",
        "tarnished plant bug (pest)",
        "redlegged grasshopper (pest)",
        "differential grasshopper (pest)",
        "green stink bug (pest)",
        "brown stink bug (pest)"
)

invert_preds_activity <- c(
  "Percent of sentinel eggs preyed upon by Coleomegilla maculata (Coleoptera: Coccinellidae)",
  "Percent of sentinel eggs preyed upon by Chyrsoptera sp. (Neuroptera: Chrusopidae)",
  "Percent of sentinel eggs preyed upon by other sucking predators"

)

invert_preds<-
  c(
    "damsel bugs (predator)",
    "ladybird density, coccinella septempuncata, at peak (13 Aug)",
    "ladybird density, Harmonia axyridis, at peak (13 Aug)",
    "ladybird larvae density, Coccinellid larvae, at peak (13 Aug)",
    "insiduous flower bug density, Orius insidiosus, at peak (30 July)",
    "Syrphid fly larvae density at peak (30 July, predators)",
    "abundance of Harpalus pensylvanicus (predator, ground beetle)",
    "abundance of Cyclotrachelus sodalis (predator, ground beetle)",
    "abundance of Pterostichus chalcites (predator, ground beetle)",
    "abundance of Scarites substriatus (predator, ground beetle)",
    "insidious flower bug (predator)",
    "spined soldier bug (predator)",
    "green lacewings (predator)",
    "ladybeetles, 3 species (predator)",
    "wolf spider abundance (Pardosa milvina Hentz)",
    "wolf spider abundance (Hogna helluo (Walckenaer))",
    "Density of Coleomegilla maculata (Coleoptera: Coccinellidae) when pest at first generation larval state (O. nubilalis)"
    
  )


invert_preds_soilcomm_abund<-
  c(
    "total species of predator taxa collected from soil surface",
    "total species of predator taxa collected from soil column",
    "total abundance of invertebrates on soil surface (pitfall traps in maize)",
    "total abundance of invertebrates on soil surface (pitfall traps in soybean)"
  )

invert_nonpredpest <-
  c(
    "prey abundance (all taxa)",
    "prey abundance (diptera only)",
    "prey abundance (hemiptera only)"
  )

invert_earthworms <-
  c("earthworm density (spring)",
    "earthworm density (fall)"
  )


pathogen_soybean <- c(
  "prevelance of brown stem rot in soybean",
  "prevelance of Phytophthora sojae in soybean",
  "prevelance of Heterodera glycines in soybean"
)

pathogen_maize <- c(
  "maize with Trichoderma spp. in subcrown mesoctyl",
  "maize with Fusarium spp. in subcrown mesoctyl",
  "maize with Trichoderma spp. in crown",
  "maize with Fusarium spp. in crown",
  "maize with Trichoderma spp. in stalk",
  "maize with Fusarium spp. in stalk",
  "mean recovery of Phytophythora sojae in soils",
  "percentage of fields with brown stem rot",
  "stems with brown stem rot",
  "percentage of fields with Phytophthora sojae",
  "leaf disks on soil colonized with Phytophthora sojae",
  "maize stalk rot",
  "maize stalk rot occurrence"
)

abuscularmycorrhizae_maize <- c(
  "maize root concentration of arbuscular mycorrhizae (C16:1cis11) at R6 stage"
)

abuscularmycorrhizae_soybean <- c(
  "soybean root concentration of arbuscular mycorrhizae (C16:1cis11) at R6 stage"
)

nematode_herbivores <- c(
  "herbivore nematode density (spring)",
  "herbivore nematode density (fall)"
)

nematode_nonpredpest <- c("omnivore nematode density (fall)",
                     "microbivore nematode density (fall)",
                     "fungivore nematode density (fall)",
                     "omnivore nematode density (spring)",
                     "microbivore nematode density (spring)",
                     "fungivore nematode density (spring)")

nematode_density <- c(
  "nematode density (fall)",
  "nematode density (spring)"
  )

nematode_soybeancyst <- c(
  "percentage of fields with Heterodera glycines",
  "Heterodera glycines density",
  "population density of soybean cyst nematode (Heterodera glycine)",
  "annual population changes of soybean cyst nematode (Heterodera glycine)",
  "population density of soybean cyst nematode (Heterodera glycines) in cyst stage (Fall)",
  "population density of soybean cyst nematode (Heterodera glycines) in cyst stage (Spring)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at planting",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles in midseason",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at harvest",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 1 month after planting",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 2 months after planting",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 2 weeks after planting (CV1)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles in midseason (CV1)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at harvest (CV1)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles 2 weeks after planting (CV2)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles in midseason (CV2)",
  "population density of soybean cyst nematode (Heterodera glycines) second-stage juveniles at harvest (CV2)",
  "population density of soybean cyst nematodes (Heterodera glycines)",
  "distance soybean cyst nematodes (Heterodera glycines) travelled following tillage",
  "Lloyd's index of patchiness for cyst nematodes (Heterodera glycines)",
  "natural log of H. glycines egg population density at planting",
  "natural log of H. glycines egg population density at harvest",
  "natural log of reproductive factor (Pf/Pi) refl ects seasonal H. glycines population change",
  "natural log of H. glycines egg population density at harvest (first year soybean after maize)",
  "natural log of H. glycines egg population density at harvest (second year soybean)",
  "natural log of H. glycines egg population density at harvest (third year soybean)",
  "natural log of H. glycines egg population density at harvest (continuous soybean)",
  "natural log of H. glycines egg population density at harvest (maize-soybean rotation)",
  "mean population of Heterodera glycine eggs"
)


####Crop Production####

## Yields ####

yields_grainsoy <- c(
  "soybean grain yield",
  "soybean seed yield",
  "soybean yield"
)

yields_grainsoy_quality <- c(
  "soybean grain moisture",
  "soybean grain oil",
  "soybean grain oil content",
  "soybean grain oil yield",
  "soybean grain protein",
  "soybean grain protein content",
  "soybean grain protein yield"
)

yields_grainsoy_growth <- c(
  "soybean plant height",
  "soybean plant height 4 weeks after planting",
  "soybean plant height at midseason"
)

yields_grainmaize <- c(
  "maize grain yield",
  "cob biomass yield",
  "cob potential cellulsoic ethanol yield",
  "maize grain harvest index",
  "maize grain yield ",
  "maize grain yield after 2 yr of tillage",
  "maize harvest index",
  "maize yield",
  "maize yields",
  "sweet maize ear biomass",
  "sweet maize grain yield (fresh weight)",
  "stover potential cellulsoic ethanol yield",
  "maize grain yield on Readlyn soil",
  "maize grain yield on Haig soil",
  "maize grain yield on Webster soil"
  )

yields_grainmaize_quality <- c(
  "maize ear moisture", 
  "maize grain moisture",
  "maize grain moisture on Readlyn soil",
  "maize grain moisture on Haig soil",
  "maize grain moisture on Webster soil",
  "maize harvest moisture"
)

yields_grainmaize_growth <- c(
  "relative plant height", 
  "maize plant height",
  "maize plant height at 8 weeks after planting",
  "maize plant height at midseason",
  "number of days after planting to 50% silking"
)

yields_maize_wateruse <- c(
  "maize seasonal water use",
  "maize water productivity",
  "maize water use"
)

yields_soy_wateruse <- c(
  "soybean water use"
)

yields_cashcrop <- c(
  "cash crop yields (maize and soybeans)"
)

yields_maize_biomass_abvgrd <- c(
  "maize dry matter biomass",
  "maize residue yield",
  "maize silage yield",
  "maize silage yield ",
  "maize stover yield",
  "maize total mature dry matter",
  "stover biomass yield",
  "sweet maize non-reproductive tissue biomass",
  "maize biomass",
  "maize biomass carbon",
  "maize aboveground production",
  "maize aboveground biomass at tasseling",
  "maize aboveground dry matter"
)

yields_soybean_biomass_abvgrd <- c(
  "soybean aboveground net primary production",
  "soybean dry matter biomass",
  "average annual soybean residue biomass"
)

yields_cashcrop_biomass_abvgrd <- c(
  "average annual cash crop residue biomass",
  "average annual maize residue biomass"
)

yields_biomass_blwgrd <- c(
  "maize belowground dry matter production",
  "maize root diameter between rows",
  "maize root diameter within rows",
  "maize root length density",
  "maize root length density between rows",
  "maize root length density within rows",
  "maize root mass between rows",
  "maize root mass within rows",
  "root length density in crop midrow",
  "root length density in crop row",
  "specific root length of maize between rows",
  "specific root length of maize within rows"
)

##Crop Nutrient Content ####

crop_N_maizestalk <- c(
  "concentration of nitrogen in maize residue",
  "maize aboveground tissue nitrogen concentrations",
  "maize leaf nitrogen",
  "maize leaf nitrogen concentration ",
  "maize stover nitrogen uptake",
  "nitrogen content of maize tissue",
  "total maize nitrogen accumulation",
  "total nitrogen uptake in maize",
  "total nitrogen yield",
  "total per plant mass nitrogen (whole plant)",
  "maize aboveground plant nitrogen accumulation",
  "apparent nitrogen recovery in maize",
  "grain nitrogen recovery",
  "maize grain nitrogen removal ",
  "nitrogen fertilizer recovery efficiency",
  "nitrogen recovery efficiency",
  "apparent nitrogen recovery in maize",
  "maize silage nitrogen removal ",
  "maize total nitrogen uptake",
  "maize canopy normalized difference vegetative index (NDVI)",
  "normalized difference vegetative index (NDVI)",
  "relative leaf chlorophyll ",
  "relative leaf chlorophyll at V10 stage"
  )

crop_N_maizeroot <- c(
  "maize belowground plant nitrogen accumulation"
)

crop_N_maizegrain <- c( 
  "maize grain nitrogen concentration", 
  "maize grain nitrogen uptake",
  "maize grain nitrogen yield",
  "maize grain total nitrogen uptake"
)

crop_P_maizeabvgrd <- c(
  "maize grain total phosphorus uptake",
  "maize phosphorus uptake at R6 stage",
  "grain phosphorus recovery",
  "cumulative phosphorus removal (grain + crop resiude)"
)

crop_miscNutrients_maize <- c(
  "maize V6 whole plant boron",
  "maize V6 whole plant calcium",
  "maize V6 whole plant copper",
  "maize V6 whole plant iron",
  "maize V6 whole plant magnesium",
  "maize V6 whole plant manganese",
  "maize V6 whole plant nitrogen",
  "maize V6 whole plant phosphorus",
  "maize V6 whole plant potassium",
  "maize V6 whole plant sulfur",
  "maize V6 whole plant zinc"
)

crop_N_soybean <- c(  
  "concentration of nitrogen in soybean residue",
  "nitrogen content of soybean tissue"
)

crop_P_soybean <- c(
  "soybean phosphorus uptake at R6 stage"
)

##Crop stand count####

standcount_maize <- c(
  "% maize emergence",
  "maize emergence",
  "maize emergence rate index",
  "maize harvest plant density",
  "maize plant population ",
  "maize stand count",
  "stand count (maize)",
  "cash crop emergence rate index on Readlyn soil",
  "cash crop emergence rate index on Haig soil",
  "cash crop emergence rate index on Webster soil",
  "emerged cash crop plant population on Readlyn soil",
  "emerged cash crop plant population on Haig soil",
  "emerged cash crop plant population on Webster soil"
)

standcount_maize_damage <- c(
  "barren stalks (maize)",
  "maize broken stalks",
  "number of maize plants injured"
)


standcount_soy <- c(
  "soybean emergence",
  "soybean lodging",
  "soybean stand count",
  "stand count (soybean)"
)

####Water Movement####
## Runoff ####

runoff_nitrate <-
  c(
    "annual nitrate concentration in 1.2 m shallow groundwater",
    "annual nitrate concentration in 1.8 m shallow groundwater",
    "annual nitrate concentration in 2.4 m shallow groundwater",
    "annual nitrate loss with subsurface drainage flow",
    "annual transport of nitrate in surface runoff (maize years)",
    "annual transport of nitrate in surface runoff (soybean/rye years)",
    "dissolved nitrogen in runoff",
    "flow weighted annual (May-April) concentrations of nitrate in surface runoff (maize years)",
    "flow weighted annual (May-April) concentrations of nitrate in surface runoff (soybean/rye years)",
    "flow-weighted average nitrate concentrations in subsurface drainage",
    "nitrate concentrations in subsurface water",
    "nitrate flow-weighted nitrate concentrations in subsurface drainage",
    "nitrate leaching loses to subsurface drainage",
    "nitrate loss in subsurface water",
    "nitrogen in sediment runoff",
    "soil water nitrate concentrations",
    "volume weighted nitrate anions in leachate",
    "annual inorganic nitrogen leaching losses",
    "average nitrate concentration leached collected after many rainfall events",
    "dissolved organic nitrogen"
  )

runoff_phosphorus <-
  c(
    "bioavailable phosphorus in runoff",
    "dissolved organic phosphorus in runoff",
    "dissolved total phosphorus in runoff",
    "total phosphorus in runoff",
    "total phosphorus in sediment runoff",
    "total soil phosphorus in runoff",
    "volume weighted phosphorus anions in leachate"
  )

runoff_pesticides <- c(
  "atrazine concentrations in runoff (24 June)",
  "atrazine concentrations in runoff (27 May)",
  "Atrazine loss",
  "atrazine mass loss in runoff (24 June)",
  "atrazine mass loss in runoff (27 May)",
  "monensin antibiotic losses in leachate",
  "tylosin antibiotic losses in leachate"
)

runoff_carbon <- c(
  "dissolved organic carbon in runoff",
  "organic carbon in runoff sediment",
  "volume weighted dissolved organic carbon in leachate",
  "dissolved organic carbon"
)

runoff_miscnutrients <- c(
  "volume weighted aluminum cations in leachate",
  "volume weighted ammonium cations in leachate",
  "volume weighted boron anions in leachate",
  "volume weighted calcium cations in leachate",
  "volume weighted chloride anions in leachate",
  "volume weighted copper cations in leachate",
  "volume weighted iron cations in leachate",
  "volume weighted magnesium cations in leachate",
  "volume weighted manganese cations in leachate",
  "volume weighted potassium cations in leachate",
  "volume weighted sodium cations in leachate",
  "volume weighted sulfur anions in leachate",
  "volume weighted zinc cations in leachate"
)

runoff_water <- c(
  "daily runoff volume within 24 hours of rain event (21 June)",
  "daily runoff volume within 24 hours of rain event (24 June)",
  "daily runoff volume within 24 hours of rain event (25 May)",
  "daily runoff volume within 24 hours of rain event (26 May)",
  "daily runoff volume within 24 hours of rain event (27 May)",
  "mean time to runoff",
  "subsurface drainage flow",
  "subsurface drainage water flow",
  "surface runoff",
  "water runoff (May)",
  "water runoff (September)",
  "water runoff after inital rainfall simulation of 9.5 cm/h",
  "water runoff after inital rainfall simulation of 9.5 cm/h within 24 hrs",
  "water runoff after seocond rainfall simulation of 9.5 cm/h within 24 hrs",
  "water runoff during growing season",
  "water runoff during non-growing season",
  "cumulative water loss 5 hrs after tillage event  (12 August)",
  "cumulative water loss 5 hrs after tillage event  (13 August)",
  "cumulative water loss 5 hrs after tillage event  (17 August)",
  "cumulative water loss 5 hrs after tillage event  (18 August)"
)

#"total depth of runoff" <- needs to be added to a group

runoff_sediment <- c(
  "sediment concentration after inital rainfall simulation of 9.5 cm/h",
  "sediment concentration after inital rainfall simulation of 9.5 cm/h within 24 hrs",
  "sediment concentration after seocond rainfall simulation of 9.5 cm/h within 24 hrs",
  "sediment in runoff",
  "sediment loss after inital rainfall simulation of 9.5 cm/h",
  "sediment loss after inital rainfall simulation of 9.5 cm/h within 24 hrs",
  "sediment loss after seocond rainfall simulation of 9.5 cm/h within 24 hrs",
  "total suspended solids",
  "soil loss"
)

runoff_manure <- c(
  "water runoff manures (May)"
)

###Apply metric & grouping labels to dataframe#####################################


grouped_table <- Results %>%
  select(Response_var, Res_key) %>%

  ################group_level1##################
  
  mutate(
    group_level1 = case_when(
      
      #Soil Nutrients####
      #Nitrogen####
      Response_var %in% chem_nitrogen_SOM ~ "Soil Nutrients",
      Response_var %in% chem_crop_SON ~ "Soil Nutrients",
      Response_var %in% chem_totalN ~ "Soil Nutrients",
      Response_var %in% chem_ammonium ~ "Soil Nutrients",
      Response_var %in% chem_nitrate ~ "Soil Nutrients",
      Response_var %in% chem_CNratio ~ "Soil Nutrients",
      
      #Phosphorus####
      Response_var %in% chem_phosphorus ~ "Soil Nutrients",
      
      #Potassium####
      Response_var %in% chem_potassium ~ "Soil Nutrients",
      
      #Micro-nutrients####
      Response_var %in% chem_calcium ~ "Soil Nutrients",
      Response_var %in% chem_magnesium ~ "Soil Nutrients",
      Response_var %in% chem_misc_nutrients ~ "Soil Nutrients",
      
      
      
      ### Other Soil Properties####
      #Chemical Properties####
      Response_var %in% chem_pH ~ "Other Soil Properties",
      Response_var %in% chem_CEC ~ "Other Soil Properties",
      
      #Physical Properties
      Response_var %in% chem_som ~ "Other Soil Properties",
      Response_var %in% phy_aggregation_stability ~ "Other Soil Properties",
      Response_var %in% phy_aggregation_diameter ~ "Other Soil Properties",
      
      Response_var %in% phy_silt ~ "Other Soil Properties",
      Response_var %in% phy_clay ~ "Other Soil Properties",
      Response_var %in% phy_sand ~ "Other Soil Properties",
      Response_var %in% phy_compaction ~ "Other Soil Properties",
      Response_var %in% phy_bulkdensity ~ "Other Soil Properties",
      
      Response_var %in% phy_surfaceresidue ~ "Other Soil Properties",
      Response_var %in% phy_surfaceresidue_decomp ~ "Other Soil Properties",
      Response_var %in% phy_erosion ~ "Other Soil Properties", 
      Response_var %in% phy_airfilled_pores ~ "Other Soil Properties",
      Response_var %in% phy_totalpores ~ "Other Soil Properties",
      
      #Abiotic Factors####
      Response_var %in% envir_temp ~ "Other Soil Properties",
      Response_var %in% phy_waterinfiltration ~ "Other Soil Properties",
      Response_var %in% phy_watercontent ~ "Other Soil Properties",
      Response_var %in% phy_waterfilled_pores ~ "Other Soil Properties",
      
      #Biotic Factors#####
      Response_var %in% biol_respiration ~ "Other Soil Properties",
      Response_var %in% biol_fungi_abund ~ "Other Soil Properties",
      Response_var %in% biol_enzyme_activity ~ "Other Soil Properties",
      Response_var %in% biol_microbial_biomass ~ "Other Soil Properties",
      Response_var %in% abuscularmycorrhizae_soybean ~ "Other Soil Properties",
      Response_var %in% abuscularmycorrhizae_maize ~ "Other Soil Properties",
      
      
      
      
      ##Climate Mitigation####
      #Carbon Mitigation####
      Response_var %in% chem_maize_SOC ~ "Climate Mitigation",
      Response_var %in% chem_SOC ~ "Climate Mitigation",
      Response_var %in% envir_CO2 ~ "Climate Mitigation",
      Response_var %in% envir_CH4 ~ "Climate Mitigation",
      
      #Nitrogen Mitigation
      Response_var %in% envir_N2O ~ "Climate Mitigation",
      
      #Global Warming Potential#####
      Response_var %in% envir_globalwarmingpotential ~ "Climate Mitigation",
      
      
      ###Pests####
      #Weeds ####
      
      Response_var %in% weed_broadleaf ~ "Pests",
      Response_var %in% weed_grass ~ "Pests",
      
      Response_var %in% weed_community_abundance ~ "Pests",
      Response_var %in% weed_community_diversity ~ "Pests",
      
      Response_var %in% weed_lambsquarters ~ "Pests",
      Response_var %in% weed_amaranthus ~ "Pests",
      Response_var %in% weed_fallpanicum ~ "Pests",
      Response_var %in% weed_velvetleaf ~ "Pests",
      
      
      #Invertebrates ####
      
      Response_var %in% invert_pests_cornrootworm ~ "Pests",
      Response_var %in% invert_pests_seedcornmaggot ~ "Pests",
      Response_var %in% invert_pests_Aglycines ~ "Pests",
      Response_var %in% invert_pests_cornborer ~ "Pests",
      Response_var %in% invert_pests_miscabundance ~ "Pests",
      
      Response_var %in% invert_preds ~ "Pests",
      Response_var %in% invert_preds_activity ~ "Pests",
      Response_var %in% invert_preds_soilcomm_abund ~ "Pests",
      Response_var %in% invert_nonpredpest ~ "Pests",
      
      Response_var %in% invert_earthworms ~ "Pests",
      
      Response_var %in% pathogen_soybean ~ "Pests",
      Response_var %in% pathogen_maize ~ "Pests",
      
      
      Response_var %in% nematode_herbivores ~ "Pests",
      Response_var %in% nematode_density ~ "Pests",
      Response_var %in% nematode_soybeancyst ~ "Pests",
      Response_var %in% nematode_nonpredpest ~ "Pests",
      
      
      
      ####Crop Yields####
      #Grain Production ####
      Response_var %in% yields_grainsoy ~ "Crop Yields",
      Response_var %in% yields_cashcrop ~ "Crop Yields",
      Response_var %in% yields_grainmaize ~ "Crop Yields",
      
      #Grain Quality ####
      Response_var %in% yields_grainsoy_quality ~ "Crop Yields",
      Response_var %in% yields_grainmaize_quality ~ "Crop Yields",
      
      #Crop Growth ####
      Response_var %in% yields_grainmaize_growth ~ "Crop Yields",
      Response_var %in% yields_maize_biomass_abvgrd ~ "Crop Yields",
      Response_var %in% yields_grainsoy_growth ~ "Crop Yields",
      Response_var %in% yields_soybean_biomass_abvgrd ~ "Crop Yields",
      Response_var %in% yields_cashcrop_biomass_abvgrd ~ "Crop Yields",
      Response_var %in% yields_biomass_blwgrd ~ "Crop Yields",
      
      #Water Use ####
      Response_var %in% yields_maize_wateruse ~ "Crop Yields",
      Response_var %in% yields_soy_wateruse ~ "Crop Yields",
      
      #Crop Nutrient Content ####
      Response_var %in% crop_N_maizestalk ~ "Crop Yields",
      Response_var %in% crop_N_maizeroot ~ "Crop Yields",
      Response_var %in% crop_N_maizegrain ~ "Crop Yields",
      
      Response_var %in% crop_P_maizeabvgrd ~ "Crop Yields",
      Response_var %in% crop_miscNutrients_maize ~ "Crop Yields",
      
      Response_var %in% crop_N_soybean ~ "Crop Yields",
      Response_var %in% crop_P_soybean ~ "Crop Yields",
      
      #Crop stand count####
      Response_var %in% standcount_maize ~ "Crop Yields",
      Response_var %in% standcount_soy ~ "Crop Yields",
      
      
      
      #Crop Damage ####
      Response_var %in% invert_pests_damage ~ "Crop Yields",
      Response_var %in% standcount_maize_damage ~ "Crop Yields",
      
      
      ####Water Movement####
      #Nutrient Runoff ####
      Response_var %in% runoff_nitrate ~ "Water Quality",
      Response_var %in% runoff_phosphorus ~ "Water Quality",
      Response_var %in% runoff_pesticides ~ "Water Quality",
      Response_var %in% runoff_carbon ~ "Water Quality",
      Response_var %in% runoff_miscnutrients ~ "Water Quality",
      Response_var %in% runoff_manure ~ "Water Quality",
      
      #Flow quantity ####
      Response_var %in% runoff_water ~ "Water Quality",
      
      #Sediment ####
      Response_var %in% runoff_sediment ~ "Water Quality"
    )) %>%
  
  
  
  
  ##############group_level2 ########
    mutate(
    group_level2 = case_when(
   
          
      #Soil Nutrients####
      #Nitrogen####
      Response_var %in% chem_nitrogen_SOM ~ "N",
      Response_var %in% chem_crop_SON ~ "N",
      Response_var %in% chem_totalN ~ "N",
      Response_var %in% chem_ammonium ~ "N",
      Response_var %in% chem_nitrate ~ "N",
      Response_var %in% chem_CNratio ~ "N",
      
      #Phosphorus####
      Response_var %in% chem_phosphorus ~ "P & K",
      
      #Potassium####
      Response_var %in% chem_potassium ~ "P & K",
      
      #Micro-nutrients####
      Response_var %in% chem_calcium ~ "Micro-nutrients",
      Response_var %in% chem_magnesium ~ "Micro-nutrients",
      Response_var %in% chem_misc_nutrients ~ "Micro-nutrients",
      
      
      
      ### Other Soil Properties####
      #Chemical Properties####
      Response_var %in% chem_pH ~ "Chemical Properties",
      Response_var %in% chem_CEC ~ "Chemical Properties",
      
      #Physical Properties
      Response_var %in% chem_som ~ "Physical Properties",
      Response_var %in% phy_aggregation_stability ~ "Physical Properties",
      Response_var %in% phy_aggregation_diameter ~ "Physical Properties",
      
      Response_var %in% phy_silt ~ "Physical Properties",
      Response_var %in% phy_clay ~ "Physical Properties",
      Response_var %in% phy_sand ~ "Physical Properties",
      Response_var %in% phy_compaction ~ "Physical Properties",
      Response_var %in% phy_bulkdensity ~ "Physical Properties",
      
      Response_var %in% phy_surfaceresidue ~ "Physical Properties",
      Response_var %in% phy_surfaceresidue_decomp ~ "Physical Properties",
      Response_var %in% phy_erosion ~ "Physical Properties", 
      Response_var %in% phy_airfilled_pores ~ "Physical Properties",
      Response_var %in% phy_totalpores ~ "Physical Properties",
      
      #Abiotic Factors####
      Response_var %in% envir_temp ~ "Abiotic Factors",
      Response_var %in% phy_waterinfiltration ~ "Abiotic Factors",
      Response_var %in% phy_watercontent ~ "Abiotic Factors",
      Response_var %in% phy_waterfilled_pores ~ "Abiotic Factors",
      
      #Biotic Factors#####
      Response_var %in% biol_respiration ~ "Biotic Factors",
      Response_var %in% biol_fungi_abund ~ "Biotic Factors",
      Response_var %in% biol_enzyme_activity ~ "Biotic Factors",
      Response_var %in% biol_microbial_biomass ~ "Biotic Factors",
      Response_var %in% abuscularmycorrhizae_soybean ~ "Biotic Factors",
      Response_var %in% abuscularmycorrhizae_maize ~ "Biotic Factors",
      
      
      
      
      ##Climate Mitigation####
      #Carbon Mitigation####
      Response_var %in% chem_maize_SOC ~ "Carbon Mitigation",
      Response_var %in% chem_SOC ~ "Carbon Mitigation",
      Response_var %in% envir_CO2 ~ "Carbon Mitigation",
      Response_var %in% envir_CH4 ~ "Carbon Mitigation",
      
      #Nitrogen Mitigation
      Response_var %in% envir_N2O ~ "Nitrogen Mitigation",
      
      #Global Warming Potential#####
      Response_var %in% envir_globalwarmingpotential ~ "Global Warming Potential",
      
      
      ###Pests####
      #Weeds ####
      
      Response_var %in% weed_broadleaf ~ "Weeds",
      Response_var %in% weed_grass ~ "Weeds",
      
      Response_var %in% weed_community_abundance ~ "Weeds",
      Response_var %in% weed_community_diversity ~ "Weeds",
      
      Response_var %in% weed_lambsquarters ~ "Weeds",
      Response_var %in% weed_amaranthus ~ "Weeds",
      Response_var %in% weed_fallpanicum ~ "Weeds",
      Response_var %in% weed_velvetleaf ~ "Weeds",
      
      
      #Invertebrate Pests ####
      
      Response_var %in% invert_pests_cornrootworm ~ "Invertebrate Pests",
      Response_var %in% invert_pests_seedcornmaggot ~ "Invertebrate Pests",
      Response_var %in% invert_pests_Aglycines ~ "Invertebrates Pests",
      Response_var %in% invert_pests_cornborer ~ "Invertebrates Pests",
      Response_var %in% invert_pests_miscabundance ~ "Invertebrates Pests",
      
     #Pest Natural Enemies ####
      Response_var %in% invert_preds ~ "Pest Natural Enemies",
      Response_var %in% invert_preds_activity ~ "Pest Natural Enemies",
      Response_var %in% invert_preds_soilcomm_abund ~ "Pest Natural Enemies",
      
     #Non-Predators & Pests #####
      Response_var %in% invert_nonpredpest ~ "Non-Predators & Pests",
      Response_var %in% nematode_nonpredpest ~ "Non-Predators & Pests",
      Response_var %in% invert_earthworms ~ "Non-Predators & Pests",
      
     #Pathogens####
      Response_var %in% pathogen_soybean ~ "Pathogens",
      Response_var %in% pathogen_maize ~ "Pathogens",
      
      #Nematodes####
      Response_var %in% nematode_herbivores ~ "Nematodes",
      Response_var %in% nematode_density ~ "Nematodes",
      Response_var %in% nematode_soybeancyst ~ "Nematodes",
      
      
      
      
      ####Crop Yields####
      #Grain Production ####
      Response_var %in% yields_grainsoy ~ "Grain Production",
      Response_var %in% yields_cashcrop ~ "Grain Production",
      Response_var %in% yields_grainmaize ~ "Grain Production",
      
      #Grain Quality ####
      Response_var %in% yields_grainsoy_quality ~ "Grain Quality",
      Response_var %in% yields_grainmaize_quality ~ "Grain Quality",
      
      #Crop Growth ####
      Response_var %in% yields_grainmaize_growth ~ "Crop Growth",
      Response_var %in% yields_maize_biomass_abvgrd ~ "Crop Growth",
      Response_var %in% yields_grainsoy_growth ~ "Crop Growth",
      Response_var %in% yields_soybean_biomass_abvgrd ~ "Crop Growth",
      Response_var %in% yields_cashcrop_biomass_abvgrd ~ "Crop Growth",
      Response_var %in% yields_biomass_blwgrd ~ "Crop Growth",
      
      #Water Use ####
      Response_var %in% yields_maize_wateruse ~ "Water Use",
      Response_var %in% yields_soy_wateruse ~ "Water Use",
      
      #Corn Nutrient Content ####
      Response_var %in% crop_N_maizestalk ~ "Corn Nutrient Content",
      Response_var %in% crop_N_maizeroot ~ "Corn Nutrient Content",
      Response_var %in% crop_N_maizegrain ~ "Corn Nutrient Content",
      Response_var %in% crop_P_maizeabvgrd ~ "Corn Nutrient Content",
      Response_var %in% crop_miscNutrients_maize ~ "Corn Nutrient Content",
      
      #Soybean Nutrient Content ####
      Response_var %in% crop_N_soybean ~ "Soybean Nutrient Content",
      Response_var %in% crop_P_soybean ~ "Soybean Nutrient Content",
      
      #Crop stand count####
      Response_var %in% standcount_maize ~ "Crop Yields",
      Response_var %in% standcount_soy ~ "Crop Yields",
      
     #Crop Damage####
     Response_var %in% standcount_maize_damage ~ "Crop Damage",
     Response_var %in% invert_pests_damage ~ "Crop Damage",
     
      
      ####Water Movement####
      #Nutrient Runoff ####
      Response_var %in% runoff_nitrate ~ "Nutrient Runoff",
      Response_var %in% runoff_phosphorus ~ "Nutrient Runoff",
      Response_var %in% runoff_pesticides ~ "Agrochemical Runoff",
      Response_var %in% runoff_carbon ~ "Nutrient Runoff",
      Response_var %in% runoff_miscnutrients ~ "Nutrient Runoff",
      Response_var %in% runoff_manure ~ "Nutrient Runoff",
      
      #Flow quantity ####
      Response_var %in% runoff_water ~ "Flow Quantity",
      
      #Sediment ####
      Response_var %in% runoff_sediment ~ "Sediment Runoff"
    )) %>%
  
  
  
#######group_level3 #################3

mutate(
  group_level3 = case_when(
    
    
    
    #Soil Nutrients####
    #N####
    Response_var %in% chem_nitrogen_SOM ~ "N content of Soil Organic Matter",
    Response_var %in% chem_crop_SON ~ "Crop derived N",
    Response_var %in% chem_totalN ~ "Total N",
    Response_var %in% chem_ammonium ~ "Ammonium (NH4)",
    Response_var %in% chem_nitrate ~ "Nitrate (NO4)",
    Response_var %in% chem_CNratio ~ "C to N ratio",
    
    #P & K####
    Response_var %in% chem_phosphorus ~ "Phosphorus",
    
    #Potassium####
    Response_var %in% chem_potassium ~ "Potassium",
    
    #Micro-nutrients####
    Response_var %in% chem_calcium ~ "Calcium",
    Response_var %in% chem_magnesium ~ "Magnesium",
    Response_var %in% chem_misc_nutrients ~ "Misc. micro-nutrients",
    
    
    
    ### Other Soil Properties####
    #Chemical Properties####
    Response_var %in% chem_pH ~ "pH",
    Response_var %in% chem_CEC ~ "CEC",
    
    #Physical Properties
    Response_var %in% chem_som ~ "Soil Organic Matter Content",
    Response_var %in% phy_aggregation_stability ~ "Aggregate Stability",
    Response_var %in% phy_aggregation_diameter ~ "Aggregate Size",
    
    Response_var %in% phy_silt ~ "Silt Content",
    Response_var %in% phy_clay ~ "Clay Content",
    Response_var %in% phy_sand ~ "Sand Content",
    Response_var %in% phy_compaction ~ "Compaction",
    Response_var %in% phy_bulkdensity ~ "Bulk Density",
    
    Response_var %in% phy_surfaceresidue ~ "Surface Residue Coverage",
    Response_var %in% phy_surfaceresidue_decomp ~ "Decomposition Rate of Surface Residue",
    Response_var %in% phy_erosion ~ "Erosion", 
    Response_var %in% phy_airfilled_pores ~ "Air-filled pore space",
    Response_var %in% phy_totalpores ~ "Total pore space",
    
    #Abiotic Factors####
    Response_var %in% envir_temp ~ "Temperature",
    Response_var %in% phy_waterinfiltration ~ "Water infiltration",
    Response_var %in% phy_watercontent ~ "Moisture Content",
    Response_var %in% phy_waterfilled_pores ~ "Water-filled pore space",
    
    #Biotic Factors#####
    Response_var %in% biol_respiration ~ "Respiration",
    Response_var %in% biol_fungi_abund ~ "Fungal abundance",
    Response_var %in% biol_enzyme_activity ~ "Enzyme activity",
    Response_var %in% biol_microbial_biomass ~ "Microbial biomass",
    Response_var %in% abuscularmycorrhizae_soybean ~ "Soybean mycorrhizal colonization",
    Response_var %in% abuscularmycorrhizae_maize ~ "Corn mycorrhizal colonization",
    
    
    ##Climate Mitigation####
    #Carbon Mitigation####
    Response_var %in% chem_maize_SOC ~ "Soil organic carbon derived from corn",
    Response_var %in% chem_SOC ~ "Soil organic carbon",
    Response_var %in% envir_CO2 ~ "Carbon dioxide emissions",
    Response_var %in% envir_CH4 ~ "Methane emissions",
    
    #Nitrogen Mitigation
    Response_var %in% envir_N2O ~ "Nitrous oxide emissions",
    
    #Global Warming Potential#####
    Response_var %in% envir_globalwarmingpotential ~ "Methane and carbon dioxide emissions",
    
    
    ###Pests####
    #Weeds ####
    
    Response_var %in% weed_broadleaf ~ "Broadleafs",
    Response_var %in% weed_grass ~ "Grasses",
    
    Response_var %in% weed_community_abundance ~ "Weed community (#)",
    Response_var %in% weed_community_diversity ~ "Weed community (diversity)",
    
    Response_var %in% weed_lambsquarters ~ "Lambsquarters (#)",
    Response_var %in% weed_amaranthus ~ "Amaranthus (#)",
    Response_var %in% weed_fallpanicum ~ "Fall Panicum (#)",
    Response_var %in% weed_velvetleaf ~ "Velvetleaf (#)",
    
    
    #Invertebrate Pests ####
    
    Response_var %in% invert_pests_cornrootworm ~ "Corn rootworm (#)",
    Response_var %in% invert_pests_seedcornmaggot ~ "Seedcorn maggot (#)",
    Response_var %in% invert_pests_Aglycines ~ "Soybean aphid (#)",
    Response_var %in% invert_pests_cornborer ~ "Corn borer (#)",
    Response_var %in% invert_pests_miscabundance ~ "General pests (#)",
    
    #Pest Natural Enemies ####
    Response_var %in% invert_preds ~ "Natural enemy (#)",
    Response_var %in% invert_preds_activity ~ "Natural enemy (activity)",
    Response_var %in% invert_preds_soilcomm_abund ~ "Soil-inhabiting natural enemy (#)",
    
    #Non-Predators & Pests #####
    Response_var %in% invert_nonpredpest ~ "Invertebrates (#)",
    Response_var %in% nematode_nonpredpest ~ "Nematodes (#)",
    Response_var %in% invert_earthworms ~ "Earthworms (#)",
    
    #Pathogens####
    Response_var %in% pathogen_soybean ~ "Soybean infections",
    Response_var %in% pathogen_maize ~ "Corn infections",
    
    #Nematodes####
    Response_var %in% nematode_herbivores ~ "Herbivores",
    Response_var %in% nematode_density ~ "Nematode (#)",
    Response_var %in% nematode_soybeancyst ~ "Soybean cyst nematode (#)",
    
    
    
    
    ####Crop Yields####
    #Grain Production ####
    Response_var %in% yields_grainsoy ~ "Soybean",
    Response_var %in% yields_cashcrop ~ "Corn and soybean",
    Response_var %in% yields_grainmaize ~ "Corn",
    
    #Grain Quality ####
    Response_var %in% yields_grainsoy_quality ~ "Soybean",
    Response_var %in% yields_grainmaize_quality ~ "Corn",
    
    #Crop Growth ####
    Response_var %in% yields_grainmaize_growth ~ "Corn (height)",
    Response_var %in% yields_maize_biomass_abvgrd ~ "Corn stover (biomass)",
    Response_var %in% yields_grainsoy_growth ~ "Soybean (height)",
    Response_var %in% yields_soybean_biomass_abvgrd ~ "Soybean tissue (biomass)",
    Response_var %in% yields_cashcrop_biomass_abvgrd ~ "Corn and soybean tissue (biomass) ",
    Response_var %in% yields_biomass_blwgrd ~ "Corn root (biomass)",
    
    #Water Use ####
    Response_var %in% yields_maize_wateruse ~ "Corn",
    Response_var %in% yields_soy_wateruse ~ "Soybean",
    
    #Corn Nutrient Content ####
    Response_var %in% crop_N_maizestalk ~ "Stover nitrogen",
    Response_var %in% crop_N_maizeroot ~ "Root nitrogen",
    Response_var %in% crop_N_maizegrain ~ "Grain nitrogen",
    Response_var %in% crop_P_maizeabvgrd ~ "Stover phosphorus",
    Response_var %in% crop_miscNutrients_maize ~ "Stover micro-nutrients",
    
    #Soybean Nutrient Content ####
    Response_var %in% crop_N_soybean ~ "Tissue nitrogen",
    Response_var %in% crop_P_soybean ~ "Tissue phosphorus",
    
    #Crop stand count####
    Response_var %in% standcount_maize ~ "Corn",
    Response_var %in% standcount_soy ~ "Soybean",
    
    #Crop Damage####
    Response_var %in% standcount_maize_damage ~ "Corn (stand count)",
    Response_var %in% invert_pests_damage ~ "Invertebrate damage",
    
    
    ####Water Movement####
    #Nutrient Runoff ####
    Response_var %in% runoff_nitrate ~ "Nitrate",
    Response_var %in% runoff_phosphorus ~ "Phosphorus",
    Response_var %in% runoff_pesticides ~ "Pesticides",
    Response_var %in% runoff_carbon ~ "Carbon",
    Response_var %in% runoff_miscnutrients ~ "Micro-nutrients",
    Response_var %in% runoff_manure ~ "Manure",
    
    #Flow quantity ####
    Response_var %in% runoff_water ~ "Water",
    
    #Sediment ####
    Response_var %in% runoff_sediment ~ "Sediment Runoff"
  )) 

#Attach column to Results######
Results <-
  left_join(Results, grouped_table, by = c("Res_key", "Response_var"))



  #Create Main Groupings ############################################



#Attach column to Results######
Results <-
  left_join(Results, metric_labels, by = c("Res_key", "Response_var"))




#############################################################################################

missing <- Results[is.na(Results$group_metric),] #check to see if all rows have an assigned group_metric
 
missing <- Results[is.na(Results$Review_specific),] #check to see if all rows have an assigned

missing <- Results[is.na(Results$group_level1),] #check to see if all rows have an assigned group_level1
missing <- Results[is.na(Results$group_level2),] #check to see if all rows have an assigned group_level2
missing <- Results[is.na(Results$group_level3),] #check to see if all rows have an assigned group_level3
                


####Save Results file with added Group names

write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/PestMgmt Review/PestMgmt_ResultsGrouped.csv", row.names = FALSE)
write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Cover Crop Review/CC_ResultsGrouped.csv", row.names = FALSE)
write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_ResultsGrouped.csv", row.names = FALSE)
write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_ResultsGrouped.csv", row.names = FALSE)


#######################################################################################################################


#Download pdfs from bibsource file####
install.package(devtools)
library(devtools)
devtools::install_github("Science-for-Nature-and-People/BibScan")
library(BibScan)

source1 = "C:/Users/LWA/Desktop/bib_files"
output = "C:/Users/LWA/Desktop/Cover_Crop_pdfs"
screened = "C:/Users/LWA/Desktop/ColandrOut/screened_abstracts.csv"


article_pdf_download(source1, output, screened)

?article_pdf_download


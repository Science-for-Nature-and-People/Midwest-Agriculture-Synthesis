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

#Publication reference
#Ref <- read.csv("PestMgmt Review/PestMgmt_Review_Reference.csv")

#Experimental design information and location of experiment
#ExpD_Loc <-read.csv("PestMgmt Review/PestMgmt_Review_ExpD_Location.csv")

#Details about cash crops planted in experiment
#CashCrop <- read.csv("PestMgmt Review/PestMgmt_Review_CashCrop.csv")

#Details about treatments
#Treatment <- read.csv("PestMgmt Review/PestMgmt_Review_Treatment.csv")

#All related results 
Results <-read.csv("PestMgmt Review/PestMgmt_Review_Results.csv")

#remove any unwanted response variables

Results <-read.csv("Cover Crop Review/CoverCrop_Results.csv")
#for cover crop data frame
#Results = filter(Results,!(Response_var == "cover crop leaf N content")) #set dataframe to work with - remove cover crop nitrogen input data (incomplete dataset)


Results<- read.csv("Nutrient Review/Nutrient_Results.csv")

#add surrogate key to Results
Results$Res_key = rownames(Results)

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
Ref %>%
  anti_join(ExpD_Loc, by = "DOI") %>%
  count(DOI, sort = TRUE)

#inspect unique key
Results %>%
  count(key) %>%
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
                             "broadcast_injected_ridge"
                    )

surface_subsurface <- c("band_knife",
                        "band_injection",
                        "surfaceband_belowsurface")

#Fertilizer Timing####

preplant_plant <- c("preplant_postplant",
                    "split_preplant_plant",
                    "timing_preplant_plant",
                    "timing_plant_V8",
                    "timing_preplant_V6",
                    "timing_spring_V3"
                    )

fall_spring <- c("timing_fall_preplant",
                 "timing_fall_spring",
                 "timing_fall_V3"
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
          Response_var %in% lodging_soybean ~ "Soybean Lodging (Scored 1 = no lodging to 5 = completely lodged)",
          Response_var %in% lodging_maize ~ "Maize Lodging (#)",
          
          
          ##Crop Growth####
          
          Response_var %in% plant_growth_soybean  ~ "Soybean Growth (including photosynthetic capacity and leaf area index)",
          Response_var %in% plant_height_soybean ~ "Plant Height (Soybean)",
          Response_var %in% seedling_development_soybean ~ "Soybean Seedling Development",
          Response_var %in% plant_height_maize ~ "Plant Height (Maize)",
          Response_var %in% vigor_reduction_maize ~ "Reduction in Vigor (Maize)",
          Response_var %in% insect_damage_maize ~ "Damage to Plant due to Black Cutworms (Maize)",
          
          
          ############Pest Regulation####
          ## Disease/Pathogens ####
          Response_var %in% disease_root_maize ~ "Pathogenic Root Injury (Maize)",
          Response_var %in% disease_leaftissue_maize ~ "Pathogenic Injury to Leaf Tissue (Maize)",
          Response_var %in% disease_soil_soybean ~ "Fusarium spp. Soil Forming Colonies (Soybean)",
          Response_var %in% disease_tissue_soybean ~ "Pathogenic Injury to Leaf Tissue (Soybean)",
          Response_var %in% disease_BPMV_soybean ~ "Bean Pod Mottle Virus Infections (Soybean)",
          
          
          ## Invertebrate Pests & Natural Enemies ####
          
          Response_var %in% pests_aphids_soybean ~ "Soybean Aphids (# per Leaf)",
          Response_var %in% pests_aphiddays_soybean ~ "Soybean Aphids (Cumulative Aphid Days)",
          Response_var %in% pests_thrips_soybean ~ "Thrips (# per Soybean Plant)",
          Response_var %in% pests_mites_soybean ~ "Spider Mites (# per cm. sq. in Soybean)",
          Response_var %in% pests_beanleafbeetles_soybean ~ "Bean Leaf Beetles (#)",
          Response_var %in% pests_SCM_maize ~ "Seed Corn Maggot (#)",
          Response_var %in% pests_WCRW_maize ~ "Western Corn Rootworm (#)",
          Response_var %in% pests_WCRWemergence_maize ~ "Western Corn Rootworm (Mean Emergence Date)",
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
                  "soil organic carbon (0-5 cm depth)"
                )
                
                chem_carbon_2060<-  c(
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
                
                chem_phosphorous <- c(
                  "plant available phosphorous",
                  "phosphorous removal",
                  "phosphorous inputs",
                  "phosphorous balance",
                  "Olsen phosphorous (Olsen P) concentration in Fall, 0-30 cm",
                  "Olsen phosphorous (Olsen P) concentration in Fall, 30-60 cm",
                  "Olsen phosphorous (Olsen P) concentration in Fall, 60-90 cm",
                  "Olsen phosphorous (Olsen P) concentration in Spring, 0-60 cm"
                  
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
                
                runoff_phosphorous <-
                  c(
                    "subsurface drainage total reactive phosphorous (TRP) annual yields",
                    "Subsurface drainage total reactive phosphorous (TRP) annual flow weighted concentrations",
                    "subsurface drainage total reactive phosphorous (TRP) annual concentrations",
                    "reactive phosphorous load runoff following fall manure",
                    "reactive phosphorous load runoff following spring manure",
                    "total phosphorous load runoff following fall manure",
                    "total phosphorous load runoff following spring manure"
                    
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
                    Response_var %in% chem_phosphorous ~ "Phosphorous",
                    Response_var %in% chem_potassium ~ "Postassium",
                    Response_var %in% chem_acidity ~ "pH",
                    Response_var %in% chem_carbon_020 ~ "Soil Carbon, 0-20 cm depth",
                    Response_var %in% chem_carbon_2060 ~ "Soil Carbon, 20-60 cm depth",
                    Response_var %in% chem_carbon_075 ~ "Soil Carbon, 0-75 cm depth",
                    Response_var %in% chem_som ~ "Soil Organic Matter",
                    
                    #Physical Properties####
                    Response_var %in% phy_erosion ~ "Erosion",
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
                    
                    #Pest Regulation####
                    #Weeds####
                    Response_var %in% weed_waterhemp ~ "Waterhemp",
                    Response_var %in% weed_deadnettle ~ "Deadnettle",
                    Response_var %in% weed_cocklebur ~ "Cocklebur",
                    Response_var %in% weed_foxtail ~ "Giant Foxtail",
                    Response_var %in% weed_Palmeramaranth ~ "Pigweed",
                    
                    
                    Response_var %in% weed_community_biomass ~ "Aboveground growth of weed community",
                    Response_var %in% weed_community_density ~ "Weed community (abundance of weeds)",
                    
                    #Invertebrates####
                    Response_var %in% invert_pests_cornrootworm ~ "Corn Rootworm (#)",
                    Response_var %in% invert_pests_seedcornmaggot ~ "Seedcorn Maggot (#)",
                    Response_var %in% invert_pests_Aglycines ~ "Soybean Aphid (#)",
                    Response_var %in% invert_pests_SCN ~ "Soybean Cyst Nematode (#)",  
                    
                    Response_var %in% invert_pests_seedcornmaggot_damage ~ "Seedcorn Maggot (Damage to Crop)",
                    Response_var %in% invert_pests_cornrootworm_damage ~ "Corn Rootworm (Damage to Crop)",
                    
                    Response_var %in% invert_preds ~ "Predators (#)",
                    
                    Response_var %in% invert_preds_vegcomm_div ~ "Predator community inhabiting foliage (#)",
                    Response_var %in% invert_preds_vegcomm_abund ~ "Predator community inhabiting foliage (#)",
                    Response_var %in% invert_preds_soilcomm_div ~ "Predator community inhabiting soils (Diversity)",
                    Response_var %in% invert_preds_soilcomm_abund ~ "Predator community inhabiting soils (#)",
                    
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
                    Response_var %in% runoff_phosphorous ~ "Phosphorous"
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
                    Response_var %in% chem_phosphorous ~ "Chemical",
                    Response_var %in% chem_potassium ~ "Chemical",
                    Response_var %in% chem_acidity ~ "Chemical",
                    Response_var %in% chem_carbon_020 ~ "Chemical",
                    Response_var %in% chem_carbon_075 ~ "Chemical",
                    Response_var %in% chem_carbon_2060 ~ "Chemical",
                    Response_var %in% chem_som ~ "Chemical",
                    
                    #Physical Properties####
                    Response_var %in% phy_erosion ~ "Physical",
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
                    Response_var %in% runoff_phosphorous ~ "Runoff"
                    # TRUE                      ~  "other"
                ))
              
              
                
                
                
                #Attach column to Results######
                Results <-
                  left_join(Results, metric_labels, by = c("Res_key", "Response_var", "Group_finelevel"))
                
                


#############################################################################################

missing <- Results[is.na(Results$group_metric),] #check to see if all rows have an assigned group_metric

                


####Save Results file with added Group names

write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Nutrient Review/Nutrient_ResultsGrouped.csv")





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


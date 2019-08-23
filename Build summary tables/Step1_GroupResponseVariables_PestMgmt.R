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


##Grain Production####                                                                          

#libraries#####

library("dplyr", lib.loc = "~/R/win-library/3.5")
library("readxl", lib.loc = "~/R/win-library/3.5")
library("tidyverse", lib.loc = "~/R/win-library/3.5")
library("tibble", lib.loc = "~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/")

############Import dataframes######################################################################

Results <-read.csv("PestMgmt Review/PestMgmt_Review_Results.csv", row.names = NULL)

#add surrogate key to Results
Results$Res_key = rownames(Results)


########################Pest Management Review####################################################

##Groups of Response Variables####
####Climate Mitigation####
##NONE####
####Soil Nutrients####
##NONE####

####Crop Yields####

yield_soybean <- c("normalized soybean yield", 
                   "soybean grain yield",
                   "soybean yield (2 year average)")


yield_maize <-c("maize yield",
                "maize grain yield",
                "maize yield when soybean aphid populations reached economically damaging levels")


##Grain Quality####
grain_quality_soybean <- c("soybean seeds per gram",
                           "soybean grain moisture",
                           "soybean seed mass",
                           "soybean grain oil",
                           "soybean grain protein")





##Stand Count####
stand_count_soybean <- c("total number soybean plants per 1.5 m",
                         "soybean stand count (2 year average)",
                         "soybean stand count")


stand_count_maize <- c("maize stand count")

##Crop Damage####

lodging_soybean <- c("soybean lodging score (range: 1 = no lodging to 5 = completely lodged)")

lodging_maize <- c("lodged maize plants")

insect_damage_maize <- c("maize plant death due to black cutworms", 
                         "maize yield when soybean aphid populations reached economically damaging levels")


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


#####Other Soil Properties####
##Biotic Factors####
biol_AMF <- "soybean root mycorrhizal colonization (arbuscular mycorrhizae)"



############Pests####
## Pathogens ####
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

## Invertebrate Pests####

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


## & Natural Enemies ####
preds_abundance_soybeans <- c("predator taxa abundance (Anthocoridae) in soybean",
                              "predator taxa abundance (Syrphidae) in soybean",
                              "predator taxa abundance (Chrysopidae) in soybean",
                              "predator taxa abundance (Coccinellidae) in soybean",
                              "predator taxa abundance (Dolichopodidae) in soybean",
                              "number of predators (hover flies) in soybean",
                              "number of predators (lacewings) in soybean",
                              "number of predators (lady beetles) in soybean",
                              "number of predators (insidious flower bugs) in soybean")


###############Water Quality##### 
#NONE####


##Groups of Pesticide Type###

unique(Results$Trt_id2name)

type_fungicide <- c("mefenoxam; fludioxonil (ApronMaxx)",                                                                                         
                    "azoxystrobin; metalaxyl",                                                                                                    
                    "captan",                                                                                                                     
                    "pyraclostrobin; metalaxyl (Accerleron)",                                                                                     
                    "captan; pentachloronitrobenzene; thiabendazole",                                                                             
                    "Bacillus pumilus GB 34",                                                                                                     
                    "Bacillus subtilis MB1600 (biofungicide); Bradyrhizobium japonicum RCR3407 (nitrogen fixing bacteria)",                       
                    "chlorothalonil",                                                                                                             
                    "diazinon; lindane; captan (Agrox D-L Plus)",                                                                                 
                    "fluazinam",                                                                                                                  
                    "metalaxyl",                                                                                                                  
                    "metalaxyl; Captan 400D",                                                                                                     
                    "fludioxonil",                                                                                                                
                    "mefenoxam",                                                                                                                  
                    "mefenoxam; fludioxonil",                                                                                                     
                    "mefenoxam; fludioxonil (ApronMAXX RFC)",                                                                                     
                    "imazalil",                                                                                                                   
                    "trifloxystrobin; metalaxyl (Trilex 2000)", 
                    "triadimefon",                                                                                                                
                    "thiabendazole",                                                                                                              
                    "pentachloronitrobenzene",                                                                                                    
                    "prothioconazole; penflufen; metalaxyl (EverGol Energy)"                                                                     
                    )

type_organo_pyrethroid_fungi <- c("pyraclostrobin; lambda-cyhalothrin"                                                                                         
)


type_neonic_fungi <- c("clothianidin; fungicide",                                                                                                    
                       "imidacloprid; mefenoxam; fludioxonil (ApronMaxx; Leverage)",                                                                 
                       "mefenoxam; fludioxonil; thiamethoxam; other fungicide; imidacloprid (CruiserMaxx; fungicides; Leverage)",                    
                       "mefenoxam; fludioxonil; thiamethoxam; other fungicides (CruiserMaxx)",                                                       
                       "thiamethoxam; mefenoxam; fludioxonil",                                                                                       
                       "thiamethoxam; mefenoxam; fludioxonil (CruiserMaxx)",                                                                         
                       "azoxystrobin; fludioxonil; mefenoxam; thiamethoxam (Cruiser Extreme 250)",                                                   
                       "abamectin; thiamethoxam; mefenoxam; fludioxonil (Avicta Complete Beans 500)",                                                
                       "imidicloprid; clothianidin; trifloxystrobin; metalaxyl; biofungicide (Trilex 2000; Yield Shield; Gaucho 600; Poncho/VOTiVO)",
                       "mefenoxam; fludioxonil; thiamethoxam (CruiserMaxx)",                                                                         
                       "prothioconazole; penflufen; metalaxyl; clothianidin; Bacillus firmus (EverGol Energy; Poncho/VOTiVO)"                       
                       )

type_otherinsecticide <- c("carbofurans",                                                                                                                
                           "spinosad",                                                                                                                   
                           "carbaryl (Sevin)",                                                                                                           
                           "unidentified insecticide"                                                                                                   
)

type_pyrethroid <- c("flucythrinate; phorate (Astar 15G)",                                                                                         
                     "bifenthrin (Capture 2EC)",                                                                                                   
                     "lambda-cyhalothrin",                                                                                                         
                     "lambda-cyhalothrin (Headline)",                                                                                              
                     "lamda-cyhalothrin, bifenthrin, or chlorpyrifos (Warrior II with Zeon Technology, Tundra EC, or Lorsban Advanced)",           
                     "permethrin (Ambush 2E)",                                                                                                     
                     "permethrin (Pounce)",                                                                                                        
                     "phorate",                                                                                                                    
                     "esfenvalerate",                                                                                                              
                     "fonofos; pyrethroid (Dyfonate II; ICIA0321 20.2G)",                                                                          
                     "pyrethroid (Force CS)",                                                                                                      
                     "pyrethroid (ICIA0321 1E)"                                                                                                   
)

type_organophosphate <- c("chlorethoxyfos",                                                                                                             
                          "fonofos",                                                                                                                    
                          "fonofos (Dyfonate II 15CG)",                                                                                                 
                          "fonofos (Dyfonate II 20G)",                                                                                                  
                          "terbufos",                                                                                                                   
                          "terbufos (Counter 15G)",                                                                                                     
                          "tefluthrin",                                                                                                                 
                          "tefluthrin (Force 1.5G)",                                                                                                    
                          "chlorphyrifos (Lorsban 15G)",                                                                                                
                          "chlorphyrifos (Lorsban)",                                                                                                    
                          "chylorpyrifos",                                                                                                              
                          "organophosphate (ICIA8882 10G)",                                                                                             
                          "organophosphate (XRM-4901)"                                                                                                 
)

type_neonicotinoid <- c( "clothianidin (Poncho)",                                                                                                      
                         "thiamethoxam (Cruiser FS)",                                                                                                  
                         "imidacloprid (Gaucho)",                                                                                                      
                         "thiamethoxam",                                                                                                               
                         "thiamethoxam (Cruiser 5 FS)",                                                                                                
                         "clothianidin",                                                                                                               
                         "imidacloprid"                                                                                                               
)

type_neonic_pyrethroid <- c("thiamethoxam; lambda-cyhalothrin"                                                                                           
)

type_organo_pyrethroid <- c( "ethoprop; phorate (Holdem 20G)"                                                                                             
)

##Groups of Pesticide Placement####
unique(Results$Group_finelevel)

              placement_seed <- c(
                "untreated_seedIF",
                "untreated_seedF",  
                "untreated_seedI",  
                "untreated_seedIFN",
                "untreated_seedunknown",
                "untreated_seedFbio"
              )
              
              placement_foliar <- c(
                "untreated_foliarI",
                "untreated_foliarIF"
              )
                       
              placement_soil <- c(
                "untreated_bandI",
                "untreated_furrowI",
                "untreated_broadcastI",      
                "untreated_soilI"           
              )
              
              placement_seedfoliar <- c(
                "untreated_seedfoliarI"         
              )
  

              placement_exclude <- c("seedIF_seedIF",
                                    "seedIF_seedF",              
                                    "seedIF_foliarI",
                                    "seedF_foliarI",
                                    "seedF_seedIF",
                                    "seedF_seedFfoliarI",
                                    "seedIF_seedFfoliarI",
                                    "seedF_seedIFfoliarI",
                                    "seedIF_seedIFfoliarI",
                                    "seedFfoliarI_seedIFfoliarI",
                                    "seedf_seedFfoliarI",
                                    "seedIF_seedIFfoliari",      
                                    "seedI_seedI",    
                                    "foliarI_seedI")
              Results <- Results %>% filter(!(Group_finelevel == "seedIF_seedIF"|
                                                         Group_finelevel == "seedIF_seedF"|              
                                                         Group_finelevel == "seedIF_foliarI"|
                                                         Group_finelevel == "seedF_foliarI"|
                                                         Group_finelevel == "seedF_seedIF"|
                                                         Group_finelevel == "seedF_seedFfoliarI"|
                                                         Group_finelevel == "seedIF_seedFfoliarI"|
                                                         Group_finelevel == "seedF_seedIFfoliarI"|
                                                         Group_finelevel == "seedIF_seedIFfoliarI"|
                                                         Group_finelevel == "seedFfoliarI_seedIFfoliarI"|
                                                         Group_finelevel == "seedf_seedFfoliarI"|
                                                         Group_finelevel == "seedIF_seedIFfoliari"|      
                                                         Group_finelevel == "seedI_seedI"|    
                                                         Group_finelevel == "foliarI_seedI"))%>% 
                                              droplevels()
              
              

###Apply grouping levels#####################################


groups_added <- Results %>%
  select(Group_finelevel, Response_var, Res_key, Trt_id2name) %>%
  
  
  #Group_level1####    
mutate(
  group_level1 = case_when( 
    #Climate Mitigation####
      ##NONE####
    #Soil Nutrients####
    ##NONE####
    
    ####Water Quality####
    ##NONE####
    
    ####Crop Yields####
    
    ##Grain Production####  
    Response_var %in%  yield_soybean  ~ "Crop Yields",
    Response_var %in%  yield_maize  ~ "Crop Yields",
    
    ##Grain Quality####
    Response_var %in%  grain_quality_soybean  ~ "Crop Yields",
    
    ##Stand Count####
    Response_var %in%  stand_count_soybean  ~ "Crop Yields",
    Response_var %in%  stand_count_maize  ~ "Crop Yields",
    
    ##Crop Damage####
    Response_var %in%  lodging_soybean  ~ "Crop Yields",
    Response_var %in%  lodging_maize  ~ "Crop Yields",
    Response_var %in%  insect_damage_maize  ~ "Crop Yields",
     
    ##Crop Growth####
    Response_var %in%  plant_growth_soybean  ~ "Crop Yields",
    Response_var %in%  plant_height_soybean  ~ "Crop Yields",
    Response_var %in%  seedling_development_soybean  ~ "Crop Yields",
    Response_var %in%  plant_height_maize  ~ "Crop Yields",
    Response_var %in%   vigor_reduction_maize  ~ "Crop Yields",
 
    #####Other Soil Properties####
    ##Biotic Factors####
    Response_var %in%  biol_AMF  ~ "Other Soil Properties",
    
    
    ############Pests####
    ## Pathogens ####
    Response_var %in%  disease_root_maize  ~ "Pests",
    Response_var %in%  disease_leaftissue_maize  ~ "Pests",
    Response_var %in%  disease_soil_soybean  ~ "Pests", #Fusarium
    Response_var %in%  disease_tissue_soybean  ~ "Pests",
    Response_var %in%  disease_BPMV_soybean  ~ "Pests", #Bean pod mottle virus
    
    ## Invertebrate Pests####
    Response_var %in%  pests_aphids_soybean  ~ "Pests", #(number)
    Response_var %in%  pests_aphiddays_soybean  ~ "Pests",
    Response_var %in%  pests_thrips_soybean  ~ "Pests",
    Response_var %in%  pests_mites_soybean  ~ "Pests",
    Response_var %in%  pests_beanleafbeetles_soybean  ~ "Pests",
    Response_var %in%  pests_SCM_maize  ~ "Pests",
    Response_var %in%  pests_WCRW_maize  ~ "Pests",
    Response_var %in%  pests_WCRWemergence_maize  ~ "Pests",
    Response_var %in%  pests_NCRW_maize  ~ "Pests",
    
    ##Natural Enemies ####
    Response_var %in%  preds_abundance_soybeans  ~ "Pests" )) %>%

    

#Group_level2####    
mutate(
  group_level2 = case_when( 
    #Climate Mitigation####
    ##NONE####
    #Soil Nutrients####
    ##NONE####
    
    ####Water Quality####
    ##NONE####
    
    
    ####Crop Yields####
    
    ##Grain Production####  
    Response_var %in%  yield_soybean  ~ "Grain Production",
    Response_var %in%  yield_maize  ~ "Grain Production",
    
    ##Grain Quality####
    Response_var %in%  grain_quality_soybean  ~ "Grain Quality",
    
    ##Stand Count####
    Response_var %in%  stand_count_soybean  ~ "Stand Count",
    Response_var %in%  stand_count_maize  ~ "Stand Count",
    
    ##Crop Damage####
    Response_var %in%  lodging_soybean  ~ "Crop Damage",
    Response_var %in%  lodging_maize  ~ "Crop Damage",
    Response_var %in%  insect_damage_maize  ~ "Crop Damage",
    
    ##Crop Growth####
    Response_var %in%  plant_growth_soybean  ~ "Crop Growth",
    Response_var %in%  plant_height_soybean  ~ "Crop Growth",
    Response_var %in%  seedling_development_soybean  ~ "Crop Growth",
    Response_var %in%  plant_height_maize  ~ "Crop Growth",
    Response_var %in%   vigor_reduction_maize  ~ "Crop Growth",
    
    #####Other Soil Properties####
    ##Biotic Factors####
    Response_var %in%  biol_AMF  ~ "Biotic Factors",
    
    
    ############Pests####
    ## Pathogens ####
    Response_var %in%  disease_root_maize  ~ "Pathogens",
    Response_var %in%  disease_leaftissue_maize  ~ "Pathogens",
    Response_var %in%  disease_soil_soybean  ~ "Pathogens", #Fusarium
    Response_var %in%  disease_tissue_soybean  ~ "Pathogens",
    Response_var %in%  disease_BPMV_soybean  ~ "Pathogens", #Bean pod mottle virus
    
    ## Invertebrate Pests####
    Response_var %in%  pests_aphids_soybean  ~ "Invertebrate Pests", #(number)
    Response_var %in%  pests_aphiddays_soybean  ~ "Invertebrate Pests",
    Response_var %in%  pests_thrips_soybean  ~ "Invertebrate Pests",
    Response_var %in%  pests_mites_soybean  ~ "Invertebrate Pests",
    Response_var %in%  pests_beanleafbeetles_soybean  ~ "Invertebrate Pests",
    Response_var %in%  pests_SCM_maize  ~ "Invertebrate Pests",
    Response_var %in%  pests_WCRW_maize  ~ "Invertebrate Pests",
    Response_var %in%  pests_WCRWemergence_maize  ~ "Invertebrate Pests",
    Response_var %in%  pests_NCRW_maize  ~ "Invertebrate Pests",
    
    ##Natural Enemies ####
    Response_var %in%  preds_abundance_soybeans  ~ "Natural Enemies" )) %>%
  
  #Group_level3####    
mutate(
  group_level3 = case_when( 
    #Climate Mitigation####
    ##NONE####
    #Soil Nutrients####
    ##NONE####
    
    ####Water Quality####
    ##NONE####
    
    
    ####Crop Yields####
    
    ##Grain Production####  
    Response_var %in%  yield_soybean  ~ "Soybean",
    Response_var %in%  yield_maize  ~ "Corn",
    
    ##Grain Quality####
    Response_var %in%  grain_quality_soybean  ~ "Soybean",
    
    ##Stand Count####
    Response_var %in%  stand_count_soybean  ~ "Soybean",
    Response_var %in%  stand_count_maize  ~ "Corn",
    
    ##Crop Damage####
    Response_var %in%  lodging_soybean  ~ "Soybean (# lodged)",
    Response_var %in%  lodging_maize  ~ "Corn (# lodged)",
    Response_var %in%  insect_damage_maize  ~ "Invertebrate damage in corn",
    
    ##Crop Growth####
    Response_var %in%  plant_growth_soybean  ~ "Soybean (greenness/chlorophyll content) ",
    Response_var %in%  plant_height_soybean  ~ "Soybean (crop height)",
    Response_var %in%  seedling_development_soybean  ~ "Soybean (seedling development)",
    Response_var %in%  plant_height_maize  ~ "Corn (crop height)",
    Response_var %in%   vigor_reduction_maize  ~ "Corn (reduction in vigor)",
    
    #####Other Soil Properties####
    ##Biotic Factors####
    Response_var %in%  biol_AMF  ~ "Soybean root mycorrhizal colonization",
    
    
    ############Pests####
    ## Pathogens ####
    Response_var %in%  disease_root_maize  ~ "Corn (infection of roots)",
    Response_var %in%  disease_leaftissue_maize  ~ "Corn (infection of leaf tissue)",
    Response_var %in%  disease_soil_soybean  ~ "Soybeans (pathogens in soil)", #Fusarium
    Response_var %in%  disease_tissue_soybean  ~ "Soybean (infection of leaf tissue)",
    Response_var %in%  disease_BPMV_soybean  ~ "Bean pod mottle virus", #Bean pod mottle virus
    
    ## Invertebrate Pests####
    Response_var %in%  pests_aphids_soybean  ~ "Soybean aphids (#)", #(number)
    Response_var %in%  pests_aphiddays_soybean  ~ "Soybean aphids (cumulative aphid days)",
    Response_var %in%  pests_thrips_soybean  ~ "Thrips on soybeans (#)",
    Response_var %in%  pests_mites_soybean  ~ "Mites on soybeans (#)",
    Response_var %in%  pests_beanleafbeetles_soybean  ~ "Bean leaf beetles on soybeans (#)",
    Response_var %in%  pests_SCM_maize  ~ "Seed corn maggot (#)",
    Response_var %in%  pests_WCRW_maize  ~ "Western corn rootworm (#)",
    Response_var %in%  pests_WCRWemergence_maize  ~ "Western corn rootworm (# days for 50% emergence)",
    Response_var %in%  pests_NCRW_maize  ~ "Northern corn rootworm (#)",
    
    ##Natural Enemies ####
    Response_var %in%  preds_abundance_soybeans  ~ "Invertebrate predators in soyban (#)" )) %>%
                
                
                #pm_group1####    
              mutate(
                pm_group1 = case_when( 
                  ##Insecticides####
                  Trt_id2name %in%  type_neonicotinoid  ~ "Neonicotinoid (Insecticide)",
                  Trt_id2name %in%  type_pyrethroid  ~ "Pyrethroid (Insecticide)",
                  Trt_id2name %in%  type_organophosphate  ~ "Organophosphate (Insecticide)",
                  Trt_id2name %in%  type_otherinsecticide  ~ "Other Insecticides",
                  ##Fungicides####
                  Trt_id2name %in%  type_fungicide  ~ "Fungicide",
                  ###Insecticide-Fungicide Mixture####
                  Trt_id2name %in%  type_organo_pyrethroid  ~ "Organophosphate & Pyrethroid (Insecticide)",
                  Trt_id2name %in%  type_neonic_pyrethroid  ~ "Neonicotinoid & Pyrethroid (Insecticide)",
                  Trt_id2name %in%  type_neonic_fungi  ~ "Neonicotinoid & Fungicide",
                  Trt_id2name %in%  type_organo_pyrethroid_fungi  ~ "Organophosphate, Pyrethroid, & Fungicide"
                  
                  
                )) %>%
                
                #pm_group2####    
              mutate(
                pm_group2 = case_when( 
                  ##Seed####
                  Group_finelevel %in%  placement_seed  ~ "Seed",
                  ##Soil####
                  Group_finelevel %in%  placement_soil  ~ "Soil",
                  ##Foliar####
                  Group_finelevel %in%  placement_foliar  ~ "Foliar",
                  ##Seed & Foliar####
                  Group_finelevel %in%  placement_seedfoliar  ~ "Seed & Foliar"
                ))
  
    
##############################################################################################

#Attach column to Results######
Results <-
  left_join(Results, groups_added) 

Results <- Results %>% select(Res_key,
                              Paper_id,
                              Duration,
                              Loc_multi_results,
                              Response_var,
                              #RV_trtspecifics,
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
                              #trt_specific,
                              pm_group1,
                              pm_group2
) %>%
  mutate(Review = "Early Season Pest Management")

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
missing <- Results[is.na(Results$pm_group1),] #check to see if all rows have an assigned pm_group1
missing <- Results[is.na(Results$pm_group2),] #check to see if all rows have an assigned pm_group2

Results <- Results %>% filter(Trt2_name  != "growth hormones; nutrient mixture (Addamax)") %>% droplevels()


###Export CSV####################
write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/PestMgmt Review/PestMgmt_ResultsGrouped.csv", row.names = FALSE)

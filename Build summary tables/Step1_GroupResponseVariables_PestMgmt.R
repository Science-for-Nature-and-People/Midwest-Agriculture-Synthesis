#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############


#libraries#####

library("dplyr", lib.loc = "~/R/win-library/3.5")
library("readxl", lib.loc = "~/R/win-library/3.5")
library("tidyverse", lib.loc = "~/R/win-library/3.5")
library("tibble", lib.loc = "~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/")

############Import dataframes######################################################################

df <-read.csv("PestMgmt Review/PestMgmt_Review_Results.csv", row.names = NULL)


#Remove unneeded columns
df$Year_result <- NULL
df$Effect <- NULL
df$Authors_comments <- NULL
df$Reviewers_results_short <- NULL
df$Reviewers_results_long <- NULL
df$Group_RV <- NULL
df$Trt_id1name_org <- NULL
df$Trt_id2name_org <- NULL
df$review <- NULL

#Rename column rows to be consistent###
df <- rename(df, paper_id = Paper_id, duration = Duration, loc_multi_results = Loc_multi_results, 
             rv = Response_var, rv_depth = RV_depth, rv_year = RV_year,
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
df$review = paste("Early Season Pest Management")

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

unique(df$trt1_name)

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
unique(df$finelevel_group)

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
              df <- df %>% filter(!(finelevel_group == "seedIF_seedIF"|
                                                         finelevel_group == "seedIF_seedF"|              
                                                         finelevel_group == "seedIF_foliarI"|
                                                         finelevel_group == "seedF_foliarI"|
                                                         finelevel_group == "seedF_seedIF"|
                                                         finelevel_group == "seedF_seedFfoliarI"|
                                                         finelevel_group == "seedIF_seedFfoliarI"|
                                                         finelevel_group == "seedF_seedIFfoliarI"|
                                                         finelevel_group == "seedIF_seedIFfoliarI"|
                                                         finelevel_group == "seedFfoliarI_seedIFfoliarI"|
                                                         finelevel_group == "seedf_seedFfoliarI"|
                                                         finelevel_group == "seedIF_seedIFfoliari"|      
                                                         finelevel_group == "seedI_seedI"|    
                                                         finelevel_group == "foliarI_seedI"))%>% 
                                              droplevels()
              
              

###Apply grouping levels#####################################


groups_added <- df %>%
  select(finelevel_group, rv, review_key, trt2_name) %>%
  
  
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
    rv %in%  yield_soybean  ~ "Crop Yields",
    rv %in%  yield_maize  ~ "Crop Yields",
    
    ##Grain Quality####
    rv %in%  grain_quality_soybean  ~ "Crop Yields",
    
    ##Stand Count####
    rv %in%  stand_count_soybean  ~ "Crop Yields",
    rv %in%  stand_count_maize  ~ "Crop Yields",
    
    ##Crop Damage####
    rv %in%  lodging_soybean  ~ "Crop Yields",
    rv %in%  lodging_maize  ~ "Crop Yields",
    rv %in%  insect_damage_maize  ~ "Crop Yields",
     
    ##Crop Growth####
    rv %in%  plant_growth_soybean  ~ "Crop Yields",
    rv %in%  plant_height_soybean  ~ "Crop Yields",
    rv %in%  seedling_development_soybean  ~ "Crop Yields",
    rv %in%  plant_height_maize  ~ "Crop Yields",
    rv %in%   vigor_reduction_maize  ~ "Crop Yields",
 
    #####Other Soil Properties####
    ##Biotic Factors####
    rv %in%  biol_AMF  ~ "Other Soil Properties",
    
    
    ############Pests####
    ## Pathogens ####
    rv %in%  disease_root_maize  ~ "Pests",
    rv %in%  disease_leaftissue_maize  ~ "Pests",
    rv %in%  disease_soil_soybean  ~ "Pests", #Fusarium
    rv %in%  disease_tissue_soybean  ~ "Pests",
    rv %in%  disease_BPMV_soybean  ~ "Pests", #Bean pod mottle virus
    
    ## Invertebrate Pests####
    rv %in%  pests_aphids_soybean  ~ "Pests", #(number)
    rv %in%  pests_aphiddays_soybean  ~ "Pests",
    rv %in%  pests_thrips_soybean  ~ "Pests",
    rv %in%  pests_mites_soybean  ~ "Pests",
    rv %in%  pests_beanleafbeetles_soybean  ~ "Pests",
    rv %in%  pests_SCM_maize  ~ "Pests",
    rv %in%  pests_WCRW_maize  ~ "Pests",
    rv %in%  pests_WCRWemergence_maize  ~ "Pests",
    rv %in%  pests_NCRW_maize  ~ "Pests",
    
    ##Natural Enemies ####
    rv %in%  preds_abundance_soybeans  ~ "Pests" )) %>%

    

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
    rv %in%  yield_soybean  ~ "Grain Production",
    rv %in%  yield_maize  ~ "Grain Production",
    
    ##Grain Quality####
    rv %in%  grain_quality_soybean  ~ "Grain Quality",
    
    ##Stand Count####
    rv %in%  stand_count_soybean  ~ "Stand Count",
    rv %in%  stand_count_maize  ~ "Stand Count",
    
    ##Crop Damage####
    rv %in%  lodging_soybean  ~ "Crop Damage",
    rv %in%  lodging_maize  ~ "Crop Damage",
    rv %in%  insect_damage_maize  ~ "Crop Damage",
    
    ##Crop Growth####
    rv %in%  plant_growth_soybean  ~ "Crop Growth",
    rv %in%  plant_height_soybean  ~ "Crop Growth",
    rv %in%  seedling_development_soybean  ~ "Crop Growth",
    rv %in%  plant_height_maize  ~ "Crop Growth",
    rv %in%   vigor_reduction_maize  ~ "Crop Growth",
    
    #####Other Soil Properties####
    ##Biotic Factors####
    rv %in%  biol_AMF  ~ "Biotic Factors",
    
    
    ############Pests####
    ## Pathogens ####
    rv %in%  disease_root_maize  ~ "Pathogens",
    rv %in%  disease_leaftissue_maize  ~ "Pathogens",
    rv %in%  disease_soil_soybean  ~ "Pathogens", #Fusarium
    rv %in%  disease_tissue_soybean  ~ "Pathogens",
    rv %in%  disease_BPMV_soybean  ~ "Pathogens", #Bean pod mottle virus
    
    ## Invertebrate Pests####
    rv %in%  pests_aphids_soybean  ~ "Invertebrate Pests", #(number)
    rv %in%  pests_aphiddays_soybean  ~ "Invertebrate Pests",
    rv %in%  pests_thrips_soybean  ~ "Invertebrate Pests",
    rv %in%  pests_mites_soybean  ~ "Invertebrate Pests",
    rv %in%  pests_beanleafbeetles_soybean  ~ "Invertebrate Pests",
    rv %in%  pests_SCM_maize  ~ "Invertebrate Pests",
    rv %in%  pests_WCRW_maize  ~ "Invertebrate Pests",
    rv %in%  pests_WCRWemergence_maize  ~ "Invertebrate Pests",
    rv %in%  pests_NCRW_maize  ~ "Invertebrate Pests",
    
    ##Natural Enemies ####
    rv %in%  preds_abundance_soybeans  ~ "Natural Enemies" )) %>%
  
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
    rv %in%  yield_soybean  ~ "Soybean",
    rv %in%  yield_maize  ~ "Corn",
    
    ##Grain Quality####
    rv %in%  grain_quality_soybean  ~ "Soybean",
    
    ##Stand Count####
    rv %in%  stand_count_soybean  ~ "Soybean",
    rv %in%  stand_count_maize  ~ "Corn",
    
    ##Crop Damage####
    rv %in%  lodging_soybean  ~ "Soybean (# lodged)",
    rv %in%  lodging_maize  ~ "Corn (# lodged)",
    rv %in%  insect_damage_maize  ~ "Invertebrate damage in corn",
    
    ##Crop Growth####
    rv %in%  plant_growth_soybean  ~ "Soybean (greenness/chlorophyll content) ",
    rv %in%  plant_height_soybean  ~ "Soybean (crop height)",
    rv %in%  seedling_development_soybean  ~ "Soybean (seedling development)",
    rv %in%  plant_height_maize  ~ "Corn (crop height)",
    rv %in%   vigor_reduction_maize  ~ "Corn (reduction in vigor)",
    
    #####Other Soil Properties####
    ##Biotic Factors####
    rv %in%  biol_AMF  ~ "Soybean root mycorrhizal colonization",
    
    
    ############Pests####
    ## Pathogens ####
    rv %in%  disease_root_maize  ~ "Corn (infection of roots)",
    rv %in%  disease_leaftissue_maize  ~ "Corn (infection of leaf tissue)",
    rv %in%  disease_soil_soybean  ~ "Soybeans (pathogens in soil)", #Fusarium
    rv %in%  disease_tissue_soybean  ~ "Soybean (infection of leaf tissue)",
    rv %in%  disease_BPMV_soybean  ~ "Bean pod mottle virus", #Bean pod mottle virus
    
    ## Invertebrate Pests####
    rv %in%  pests_aphids_soybean  ~ "Soybean aphids (#)", #(number)
    rv %in%  pests_aphiddays_soybean  ~ "Soybean aphids (cumulative aphid days)",
    rv %in%  pests_thrips_soybean  ~ "Thrips on soybeans (#)",
    rv %in%  pests_mites_soybean  ~ "Mites on soybeans (#)",
    rv %in%  pests_beanleafbeetles_soybean  ~ "Bean leaf beetles on soybeans (#)",
    rv %in%  pests_SCM_maize  ~ "Seed corn maggot (#)",
    rv %in%  pests_WCRW_maize  ~ "Western corn rootworm (#)",
    rv %in%  pests_WCRWemergence_maize  ~ "Western corn rootworm (# days for 50% emergence)",
    rv %in%  pests_NCRW_maize  ~ "Northern corn rootworm (#)",
    
    ##Natural Enemies ####
    rv %in%  preds_abundance_soybeans  ~ "Invertebrate predators in soyban (#)" )) %>%
                
                
                #pm_group1####    
              mutate(
                pm_group1 = case_when( 
                  ##Insecticides####
                  trt2_name %in%  type_neonicotinoid  ~ "Neonicotinoid (Insecticide)",
                  trt2_name %in%  type_pyrethroid  ~ "Pyrethroid (Insecticide)",
                  trt2_name %in%  type_organophosphate  ~ "Organophosphate (Insecticide)",
                  trt2_name %in%  type_otherinsecticide  ~ "Other Insecticides",
                  ##Fungicides####
                  trt2_name %in%  type_fungicide  ~ "Fungicide",
                  ###Insecticide-Fungicide Mixture####
                  trt2_name %in%  type_organo_pyrethroid  ~ "Organophosphate & Pyrethroid (Insecticide)",
                  trt2_name %in%  type_neonic_pyrethroid  ~ "Neonicotinoid & Pyrethroid (Insecticide)",
                  trt2_name %in%  type_neonic_fungi  ~ "Neonicotinoid & Fungicide",
                  trt2_name %in%  type_organo_pyrethroid_fungi  ~ "Organophosphate, Pyrethroid, & Fungicide"
                  
                  
                )) %>%
                
                #pm_group2####    
              mutate(
                pm_group2 = case_when( 
                  ##Seed####
                  finelevel_group %in%  placement_seed  ~ "Seed",
                  ##Soil####
                  finelevel_group %in%  placement_soil  ~ "Soil",
                  ##Foliar####
                  finelevel_group %in%  placement_foliar  ~ "Foliar",
                  ##Seed & Foliar####
                  finelevel_group %in%  placement_seedfoliar  ~ "Seed & Foliar"
                ))
              
#####soil depth groupings#####
#These will display such that it is always displying the results from more shallow sampling depths + deepest depth
#Organized by means of sampling depth

######Organize soil sampling depth and year variables########

unique(levels(df$rv_depth))
              
              
##No soil depths included in this dataset thus far.####

depth_0_30 <- c()
depth_0_60 <- c()
depth_0_100 <- c()
depth_0_150 <- c()

#####Apply soil depth groupings####

df$sample_depth <- NA

##Use the following code for soil depths####
#df <- df %>%
#mutate(
#sample_depth = case_when(

# RV_depth %in% depth_0_30 ~ "0-30 cm",
#  RV_depth %in% depth_0_60 ~ "0-60 cm",
# RV_depth %in% depth_0_100 ~ "0-100 cm",
#  RV_depth %in% depth_0_150 ~ "0-150 cm"))


  colnames(df2)
    
##############################################################################################

#Attach column to Results######
df2 <-
  left_join(df, groups_added) 

df2 <- select(df2, review, paper_id, duration, rv_year, loc_multi_results,
              group_level1, group_level2, group_level3, rv, rv_depth, sample_depth,
              rv_units, stat_test, stat_type, trt1, trt1_int, trt1_int2, trt1_value,        
              trt2, trt2_int, trt2_int2, trt2_value,
              significance, effect_norm, finelevel_group, trt1_name, trt1_details, trt2_name,
              trt2_details, pm_group1, pm_group2)
              

missing <- df2[is.na(df2$group_level1),] #check to see if all rows have an assigned group_level1
missing <- df2[is.na(df2$group_level2),] #check to see if all rows have an assigned group_level2
missing <- df2[is.na(df2$group_level3),] #check to see if all rows have an assigned group_level3
missing <- df2[is.na(df2$pm_group1),] #check to see if all rows have an assigned pm_group1
missing <- df2[is.na(df2$pm_group2),] #check to see if all rows have an assigned pm_group2

df2 <- df2 %>% filter(trt2_name  != "growth hormones; nutrient mixture (Addamax)") %>% droplevels()


###Export CSV####################
write.csv(df2, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/PestMgmt_ResultsGrouped.csv", row.names = FALSE)

###All Pest Management Review Material


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
                    
                    
                    insect_damage_maize <- c("maize plant death due to black cutworms")
                    
                    
                    
                    
                    ############Pest Regulation####
                    ## Disease/Pathogens ####
                    disease_root_maize <- c("maize root rating (pathogen infection)",
                                            "root injury rating [from 0 to 3 where 0 = no injury and 3 = 3 nodes missing] for early May maize planting",
                                            "root injury rating [from 0 to 3 where 0 = no injury and 3 = 3 nodes missing] for late May maize planting",
                                            "root injury rating [from 0 to 3 where 0 = no injury and 3 = 3 nodes missing] for early June maize planting",
                                            "maize root rot index at V2 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                            "maize root rot index at V4 stage (where 0 = healthy, and 5 = completely rotted tissue)")
                    
                    
                    disease_leaftissue_maize <- c("mesocotyl rot index at V2 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                                  "mesocotyl rot index at V4 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                                  "crown rot index at V2 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                                  "crown rot index at V4 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                                  "stalk rot index at V2 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                                  "stalk rot index at R6 stage (where 0 = healthy, and 5 = completely rotted tissue)",
                                                  "stalk rot index at R6  stage (where 0 = healthy, and 5 = completely rotted tissue)",
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
                          

                          
###Apply metric & grouping labels to dataframe#####################################
                          
                          
  metric_labels <- Results %>%
    select(Response_var, Res_key) %>%
    mutate(
      group_metric = case_when(                       
                            
                  ####Soils####
                  ##Soil Biological Properties####
                  Response_var %in% biol_AMF ~ "Biological",
                  
                  ##########Crop Production#######
                  
                  ##Crop Yields####                                                                          
                  Response_var %in% yield_soybean ~ "Grain Yield",
                  Response_var %in% grain_quality_soybean ~ "Grain Quality",
                  Response_var %in% yield_maize ~ "Grain Yield",
                  
                  ##Stand Count####
                  Response_var %in% stand_count_soybean ~ "Stand Count",
                  Response_var %in% stand_count_maize ~ "Stand Count",
                  Response_var %in% lodging_soybean ~ "Lodging (Scored 1 = no lodging to 5 = completely lodged)",
                  Response_var %in% lodging_maize ~ "Lodging (#)",
                  
                  
                  ##Crop Growth####
                  
                  Response_var %in% plant_growth_soybean  ~ "Plant Growth (including photosynthetic capacity and leaf area index)",
                  Response_var %in% plant_height_soybean ~ "Plant Height",
                  Response_var %in% seedling_development_soybean ~ "Seedling Development",
                  Response_var %in% plant_height_maize ~ "Plant Height",
                  Response_var %in% vigor_reduction_maize ~ "Reduction in Vigor",
                  Response_var %in% insect_damage_maize ~ "Damage to Plant due to Black Cutworms",
                  
                  
                  ############Pest Regulation####
                  ## Disease/Pathogens ####
                  Response_var %in% disease_root_maize ~ "Pathogenic Root Injury",
                  Response_var %in% disease_leaftissue_maize ~ "Pathogenic Injury to Leaf Tissue",
                  Response_var %in% disease_soil_soybean ~ "Fusarium spp. Soil Forming Colonies",
                  Response_var %in% disease_tissue_soybean ~ "Pathogenic Injury to Leaf Tissue",
                  Response_var %in% disease_BPMV_soybean ~ "Bean Pod Mottle Virus Infections",
                  
                  
                  ## Invertebrate Pests & Natural Enemies ####
                  
                  Response_var %in% pests_aphids_soybean ~ "Soybean Aphids (# per Leaf)",
                  Response_var %in% pests_aphiddays_soybean ~ "Soybean Aphids (Cumulative Aphid Days)",
                  Response_var %in% pests_thrips_soybean ~ "Thrips (# per Soybean Plant)",
                  Response_var %in% pests_mites_soybean ~ "Spider Mites (# per cm. sq.)",
                  Response_var %in% pests_beanleafbeetles_soybean ~ "Bean Leaf Beetles (#)",
                  Response_var %in% pests_SCM_maize ~ "Seed Corn Maggot (#)",
                  Response_var %in% pests_WCRW_maize ~ "Western Corn Rootworm (#)",
                  Response_var %in% pests_WCRWemergence_maize ~ "Western Corn Rootworm (Mean Emergence Date)",
                  Response_var %in% pests_NCRW_maize ~ "Northern Corn Rootworm (#)",
                  Response_var %in% preds_abundance_soybeans ~ "Natural Enemies (#)"
                ))
            
            
            
            
            ##############################################################################################3
            
            
            
            ##############################################################################################3
            
            
            
            

#Create Main Groupings #####

mutate(
  main_group = case_when(
    
                ##############Soils####
                ##Soil Biological Properties####
                Response_var %in% biol_AMF ~ "Root Mycorrhizal Colonization (Arbuscular)",
                
                
                
                ##############################################################################################3
                
                
                
                
                
                ##########Crop Production#######
                
                yield_grainmaize <- c("maize yield")
                
                yield_grainsoybean <- c("normalized soybean yields", 
                                        "soybean grain yield")
                
                yield_grainquality_soy <- c("soybean seeds per gram",
                                            "soybean seed mass",
                                            "soybean grain moisture",
                                            "soybean grain oil",                                               
                                            "soybean grain protein")
                
                yield_plantht <- c("maize plant height",                                                 
                                   "soybean plant height")                                                  
                
                yield_leafmetrics <- c("NDVI (normalized difference vegetation index)",                         
                                       "SRVI (simple ratio vegetation index)",                                  
                                       "leaf area index (July 26, 2001)") 
                
                
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
              ))
            
            
            
            ##Crop Yields####                                                                          
            Response_var %in% yield_soybean ~ "Soybean: Yields",
            Response_var %in% grain_quality_soybean ~ "Soybean: Yields",
            Response_var %in% yield_maize ~ "Maize: Yields",
            
            ##Stand Count####
            Response_var %in% stand_count_soybean ~ "Soybean: Stand Density",
            Response_var %in% stand_count_maize ~ "Maize: Stand Density",
            Response_var %in% lodging_soybean ~ "Soybean: Stand Density",
            Response_var %in% lodging_maize ~ "Soybean: Stand Density",
            
            
            ##Crop Growth####
            
            Response_var %in% plant_growth_soybean  ~ "Soybean: Plant Growth",
            Response_var %in% plant_height_soybean ~ "Soybean: Plant Growth",
            Response_var %in% seedling_development_soybean ~ "Soybean: Plant Growth",
            Response_var %in% plant_height_maize ~ "Maize: Plant Growth",
            Response_var %in% vigor_reduction_maize ~ "Maize: Plant Growth",
            Response_var %in% insect_damage_maize ~ "Maize: Plant Growth",
            
            
            ############Pest Regulation####
            ## Disease/Pathogens ####
            Response_var %in% disease_root_maize ~ "Maize: Crop Disease",
            Response_var %in% disease_leaftissue_maize ~ "Maize: Crop Disease",
            Response_var %in% disease_soil_soybean ~ "Soybean: Crop Disease",
            Response_var %in% disease_tissue_soybean ~ "Soybean: Crop Disease",
            Response_var %in% disease_BPMV_soybean ~ "Soybean: Crop Disease",
            
            
            ## Invertebrate Pests & Natural Enemies ####
            
            Response_var %in% pests_aphids_soybean ~ "Soybean: Invertebrates Pests",
            Response_var %in% pests_aphiddays_soybean ~ "Soybean: Invertebrate Pests",
            Response_var %in% pests_thrips_soybean ~ "Soybean: Invertebrate Pests",
            Response_var %in% pests_mites_soybean ~ "Soybean: Invertebrate Pests",
            Response_var %in% pests_beanleafbeetles_soybean ~ "Soybean: Invertebrate Pests",
            Response_var %in% pests_SCM_maize ~ "Maize: Invertebrate Pests",
            Response_var %in% pests_WCRW_maize ~ "Maize: Invertebrate Pests",
            Response_var %in% pests_WCRWemergence_maize ~ "Maize: Invertebrate Pests",
            Response_var %in% pests_NCRW_maize ~ "Maize: Invertebrate Pests",
            Response_var %in% preds_abundance_soybeans ~ "Soybean: Predators"
            ))
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
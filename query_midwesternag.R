#Midwestern Ag Synthesis - SNAPP
#Database Querying 2

#Download pdfs from bibsource file
library(devtools)
devtools::install_github("Science-for-Nature-and-People/BibScan")
library(BibScan)

source1 = "C:/Users/LWA/Desktop/bib_files"
output = "C:/Users/LWA/Desktop/Cover_Crop_pdfs"
screened = "C:/Users/LWA/Desktop/ColandrOut/screened_abstracts.csv"


article_pdf_download(source1, output, screened)



#libraries

library("dplyr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")
library("stringi", lib.loc="~/R/win-library/3.4")
library("forcats", lib.loc="~/R/win-library/3.4")

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

############Import dataframes######################################################################

  #Publication reference
Ref <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Reference") 
  
   #Experimental design information and location of experiment
ExpD_Loc <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "ExpD_Location") 
 
   #Details about cash crops planted in experiment
CashCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CashCrop")
 
  #Details about treatments with particular attention to cover crops
Treatment <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Treatment")
  
  #All cover crop related results f
Results <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Results")
    Results = filter(Results, !(Response_var == "cover crop leaf N content")) #set dataframe to work with - remove cover crop nitrogen input data (incomplete dataset)              
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
  count(Paper_id, Response_var, Trt_id1, Trt_id2, Trt1_interaction,  Trt1_interaction2, Trt2_interaction, Trt2_interaction2) %>% 
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




####Filtering Data Files###############################################################

Results <- filter(Results, Review_id == "Cover crop") #Cover crop review only
#Results <- filter(Results, Review_id == "Tillage") #Soil Management review only
#Results <- filter(Results, Review_id == "Seed protection") #Seed & Seedling Protection review only
#Results <- filter(Results, Review_id == "Fertilizer") #Fertilizer use review only

##############Add Metric Column and Groupings

#Paper_ID & Duration are the two keys that work across all dataframes

######Filter data based on response variable groupings [Crop Production, Soil, Water, Pest Regulation]     
    

#filter tibble for Group_Rv
      CC_Soil <- filter(Results, Group_RV == "Soil")
      CC_Production <- filter(Results, Group_RV == "Crop Production")
      CC_Water <- filter(Results, Group_RV == "Water")
      CC_Pest <- filter(Results, Group_RV == "Pest Regulation")
      
      
   #Identify variables included in each grouping 
      #itemized list of unique metrics (Response_var)
      (uniqueSoils <- unique(CC_Soil$Response_var))
      
      (uniquePestR <- unique(CC_Pest$Response_var))
      
      (uniqueProduction <- unique(CC_Production$Response_var))
      
      (uniqueWater <- unique(CC_Water$Response_var))
      

      
    ##### Metric Groupings ####
          ##continue adding groups to this list
   
      ####Soils####
      ##Soil Chemical Properties####
      
      chem_nitrate_spring <- c( "soil nitrate (NO3-N)",
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
                              "soil nitrate 0-30cm depth (spring)",
                              "soil nitrate NO3 (0-15 cm)"
                                )
  
      chem_nitrate_maize <- c("soil nitrate (NO3--N) following maize",
                              "total NO3-N from soil profile (0-120 cm) in crop row",
                              "total NO3-N from soil profile (0-120 cm) in crop interrow",
                              "soil nitrate nitrogen (NO3-N) concentration in Fall 0-30 cm",
                              "soil nitrate nitrogen (NO3-N) concentration in Fall, 30-60 cm",
                              "soil nitrate nitrogen (NO3-N) concentration in Fall, 60-90 cm",
                              "nitrate (NO3-N) at maize V6",
                              "soil nitrate (NO3-N) early June (maize)"
                                )
      
      chem_nitrate_soybean <- c("soil nitrate (NO3--N) following soybean"
                                  )
      
      chem_nitrate_fall <- c("soil nitrate (NO3-N) post harvest (maize)",
                              "soil nitrate (NO3-N) post harvest (soybean)"
                                )
      
      chem_ammonium_spring <- c("ammonium (NH4-N)",
                                "ammonium (NH4-N) in soil following cover crop",
                                "soil ammonium 0-30cm depth (spring)",
                                "soil ammonium NH4 (0-15 cm)"
                                  )
      
      chem_totalN <- c("denitrification",
                        "nitrogen removal",
                        "nitrogen inputs",
                        "nitrogen balance",
                        "total (NO3+NH4)-N from soil profile (0-120 cm) in crop row",
                        "total (NO3+NH4)-N from soil profile (0-120 cm) in crop interrow",
                        "total nitrogen, spring sample",
                        "fraction of soil organic nitrogen mineralized in 30 days  (frozen before incubation)",
                        "fraction of soil organic nitrogen mineralized in 30 days (not frozen before incubation)",
                        "total soil nitrogen following cover crop (to 1 m depth)",
                        "total soil nitrogen",
                        "total inorganic nitrogen (TIN)",
                        "total nitrogen (0-15 cm)",
                        "total nitrogen",
                        "soil organic nitrogen 0-30cm depth (spring)",
                        "nitrate and ammonium (NO3-N + NH4-N) in soil following cover crop"
                          )
      
      chem_phosphorous <- c("plant available phosphorous",
                            "phosphorous removal",
                            "phosphorous inputs",
                            "phosphorous balance",
                            "Olsen phosphorous (Olsen P) concentration in Fall, 0-30cm",
                            "Olsen phosphorous (Olsen P) concentration in Fall, 30-60 cm",
                            "Olsen phosphorous (Olsen P) concentration in Fall, 60-90 cm",
                            "Olsen phosphorous (Olsen P) concentration in Spring, 0-60cm"
                              )
      
      chem_potassium <- c("potassium removal",
                          "potassium inputs",
                          "potassium balance",
                          "exchangable potassium (K)"
                            )
      
      chem_acidity <- c("pH",
                        "pH (0-15 cm)"
                          )
      
      ##Soil Physical Properties####
      
      phy_erosion <- c("soil loss",
                      "interrill erosion rate",
                      "rill erosion rate"
                      )
      
      phy_structure <- c("soil penetration resistance",
                          "bulk density",
                          "water aggregate stability",
                          "soil penetration resistance (Spring)",
                          "soil penetration resistance (Fall)",
                          "air filled pore space",
                          "bulk density 0-30cm depth (spring)",
                          "volumetric air content",
                          "water filled poor space",
                          "total pore spaces",
                          "mean weight diameter of water stable aggregates",
                          "pore tortuosity factor",
                          "relative gas diffusion coefficient"
                          )
      
      phys_watercontent <- c("volumetric water content",
                              "soil water content at cash crop emergence, 10 cm depth",
                              "soil water content at cash crop emergence, 20 cm depth",
                              "soil water content before planting of cash crop, 10 cm depth",
                              "soil water content before planting of cash crop, 20 cm depth",
                              "soil water storage  at cash crop emergence, 80 cm depth",
                              "soil water storage before planting of cash crop, 80 cm depth",
                              "gravimetric water content of soil",
                              "soil moisture"
                              )
      
            ##Soil Biological Properties####

      biol_carbon <-  c("soil organic carbon in topsoil",
                  "soil organic carbon (0-75 cm)",
                  "water extractable organic carbon (0-5 cm)",
                  "water extractable organic carbon (5-20 cm)",
                  "total carbon",
                  "soil organic carbon (SOC) in fall following cover crop, 0-15cm",
                  "soil organic carbon (SOC), spring sample",
                  "soil organic carbon 0-30cm depth (spring)",
                  "soil organic carbon",
                  "organic carbon (0-15 cm)",
                  "soil carbon concentration")
        
      biol_microbes <- c("microbial biomass",
                          "microbial biomass nitrogen (MBN)"
                             )
      
        biol_som <- c("particulate organic carbon",
                      "total POM",
                      "particulate organic matter (fPOM) including macro orgnaic matter fractions",
                      "particulate organic matter (oPOM)",
                      "soil organic matter"
                      )
        
            ##Soil Environmental Properties####
        
        envir_temp <- c("soil temperature"
                        )
        
        envir_CO2 <- c("carbon dioxide emissions (CO2-C)")
        
        
        envir_N2O <- c("N2O emissions when cover crop present",
                        "N2O emissions over entire year",
                        "cumulative N2O-N emissions from soil",
                        "cumulative nitrous oxide flux from soil April 2003-Mar 2004",
                        "cumulative nitrous oxide flux from soil April 2004 - Feb 2004",
                        "nitrous oxide emissions (N2O-N)"
                        )


        ####Pest Regulation####      
        
        ## Weeds ####
        weeds_species <- c("control of Palmer amaranth (21 days post planting)",
                            "control of Palmer amaranth (at harvest)",
                            "density of Palmer amaranth (42 days post planting)",
                            "density of Palmer amaranth (at harvest)",
                            "control of common waterhemp (21 days post planting)",
                            "control of common waterhemp (at harvest)",
                            "control of giant foxtail (weeds)",
                            "control of common cocklebur (weeds)",
                            "density of Lamim spp. (Henbit and purple deadnettle) in spring",
                            "density of Lamim spp. (Henbit and purple deadnettle) after 5 years",
                            "density of early season waterhemp",
                            "density of late season waterhemp"
                            )
        
        weeds_community <- c("weed biomass",
                              "weed density",
                              "total density of winter weeds in spring",
                              "total density of winter weeds after 5 years",
                              "weed density after termination of cover crops",
                              "weed biomass after termination of cover crops",
                              "density of winter annual weeds (before cash crop planting)",
                              "density of late season summer annual weeds"
                              )
        
        ## Invertebrates ####    
        
        invert_pests_species <- c("density of soybean cyst nematode eggs in spring",
                                  "density of soybean cyst nematode eggs after 5 years",
                                  "abundance adult seedcorn maggots per trap",
                                  "seedcorn maggot damage on crops (Y-plants)",
                                  "pest abundance in first year (1st larval instar)",
                                  "pest abundance in first year (2nd larval instar)",
                                  "pest abundance in first year (3rd larval instar)",
                                  "pest abundance in first year (adult)",
                                  "maize root damage from corn rootworm",
                                  "infestation rating of soybean fields with Aphis glycines (0 = 0 aphids/plant; 1 = 1-10 /plant; 2 = 11-100/plant; 3 = 101-1,000/plant; 4 = >1,000/plant)",
                                  "proportion of soybean plants infested with Aphis glycines per field (30 plants assessed / field)",
                                  "abundance of aphids (Aphis glycines) over time",
                                  "exposure of aphid population to predators",
                                  "density of Heterodera glycines eggs at harvest (maize)",
                                  "density of Heterodera glycines eggs at harvest (soybean)"
                                  )
        
        invert_preds_species <- c("total abundance of spiders (Araneae) on aboveground tissue (sweep net in soybean)",
                                  "total abundance of wolf spiders (Lycosidae) on soil surface (pitfall traps in maize)",
                                  "total abundance of wolf spiders (Lycosidae) on soil surface (pitfall traps in soybean)",
                                  "total abundance of green lacewings (Chrysopidae)on aboveground tissue (sweep net in soybean)",
                                  "total abundance of lady beetles (Coccinellidae) on aboveground tissue (sweep net in soybean)",
                                  "total abundance of damsel bugs (Nabidae)on aboveground tissue (sweep net in soybean)",
                                  "total abundance of Harvestmen arachnids (Opiliones) on soil surface (pitfall traps in maize)",
                                  "total abundance of Minute Pirate Bugs (Anthocoridae) on aboveground tissue (sweep net in soybean)"
                                  )
        
        invert_preds_comm <- c("total species of predator taxa collected from soil surface",
                                "total species of predator taxa collected from soil column",
                                "diversity of soil surface predators (Shannon's H)",
                                "diversity of predators in soil column (Shannon's H)",
                                "diversity of soil surface predators (Evenness J)",
                                "diversity of predators in soil column (Evenness J)",
                                "abundance of generalist foliage-foraging predators over times",
                                "mean predator abundance per vacuum sample",
                                "mean predator abundance per vacuum sample (five sampling events)",
                                "mean predator diversity (Simpson's 1-D) per vacuum sample (four sampling events)",
                                "total abundance of invertebrates  on aboveground plant tissue (sweep net in soybean)",
                                "total abundance of invertebrates on soil surface (pitfall traps in maize)",
                                "total abundance of invertebrates on soil surface (pitfall traps in soybean)"
                                )
        
        invert_pred_activity <- c("mean percent sentinel egg removal per 48 hours (3 sampling events)"
                                  )
        
        invert_nonpredpest <- c("total abundance of ants (Formicidae) on soil surface (pitfall traps in maize)",
                                "total abundance of ants (Formicidae) on soil surface (pitfall traps in soybean)",
                                "total abundance of beetles (Coleoptera)on soil surface (pitfall traps in maize)",
                                "total abundance of beetles (Coleoptera)on soil surface (pitfall traps in soybean)",
                                "total abundance of millipedes (Diplopoda) on soil surface (pitfall traps in maize)",
                                "total abundance of millipedes (Diplopoda) on soil surface (pitfall traps in soybean)",
                                "total abundance of crickets (Gryllidae) on soil surface (pitfall traps in maize)",
                                "total abundance of crickets (Gryllidae) on soil surface (pitfall traps in soybean)",
                                "total abundance of hoverflies (Syrphidae) on aboveground tissue (sweep net in soybean)",
                                "total abundance of Chalcid wasps (Chalcidoidae) on aboveground tissue (sweep net in soybean)",
                                "total abundance of parasitic flies (Tachinidae) on aboveground tissue (sweep net in soybean)"
                                )
   ####Crop Production####      
        
        ## Yields ####      
     
        yields_grain <- c("cash crop yield",
                          "soybean yield",
                          "maize yield",
                          "maize yield (grain)",
                          "soybean yield per 1.8 m row",
                          "soybean yield (years 5 & 6)",
                          "maize yield after soybean overseeded with cc (years 5 & 6)",
                          "maize yield (average over 4 years)",
                          "soybean yield (average over 4 years)",
                          "Harvest Index (corn)",
                          "grain biomass",
                          "grain yield (maize)",
                          "grain yield (soybean)",
                          "maize yield, weight of kernels"
                          )
        
        yields_biomass_abvgrd <- c("maize silage dry matter yield",
                                  "corn silage yield",
                                  "aboveground dry biomass (maize) at harvest",
                                  "plant height at maize reproductive stage 1",
                                  "plant height at maize reproductive stage 2",
                                  "plant height at maize reproductive stage 3",
                                  "plant height at maize reproductive stage 4",
                                  "plant height at maize reproductive stage 5",
                                  "plant height at maize reproductive stage 6"
                                  )
        
        yields_biomass_blwgrd <- c("root biomass (0-100 cm)"
                                    )
        ##Crop Nitrogen Content ####
        
        crop_N <- c("corn stalk nitrate",
                    "total aboveground N uptake (Maize)",
                    "total aboveground N uptake (Soybean)",
                    "maize silage dry matter N removal",
                    "grain nitrogen uptake",
                    "aboveground plant nitrogen uptake"
                    )
        
        ##Crop seedling density####
        
        seedling_density <- c("cash crop density",
                              "cash crop population density (soybean)"
                              )
        
   ####Water Movement####      
        
        ## Drainage ####        
        
        drainage <- c("average drainage",
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
        
        runoff_nitrate <- c("subsurface drainage nitrate (NO3-N) annual yields",
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
                            "normalized NO3-N losses"
                            )
        
        runoff_phosphorous <- c("Subsurface drainage total reactive phosphorous (TRP) annual yields",
                                "Subsurface drainage total reactive phosphorous (TRP) annual flow weighted concentrations"
                                )
        
        
        
      ###Apply metric labels to dataframe####
      

          metric_labels <- Results %>%
            select(Response_var, key) %>%
            mutate(
              group_metric = case_when(
                #Soils
                  #Chemical Properties
                  Response_var %in% biol_carbon ~ "soil carbon (spring)",
                  Response_var %in% chem_nitrate_spring ~ "soil nitrate (spring)",
                  Response_var %in% chem_nitrate_maize ~ "soil nitrate (maize)",
                  Response_var %in% chem_nitrate_soybean ~ "soil nitrate (soybean)",
                  Response_var %in% chem_nitrate_fall ~ "soil nitrate(fall)",
                  Response_var %in% chem_ammonium_spring ~"soil ammonium (spring)",
                  Response_var %in% chem_totalN ~ "soil total nitrogen",
                  Response_var %in% chem_phosphorous ~ "soil phosphorous",
                  Response_var %in% chem_potassium ~ "soil postassium",
                  Response_var %in% chem_acidity ~"pH",
                
                  #Physical Properties
                  Response_var %in% phy_erosion ~ "soil erosion",
                  Response_var %in% phy_structure ~ "soil structure",
                  Response_var %in% phys_watercontent ~ "soil water content",
                
                  #Biological Properties
                  Response_var %in% biol_microbes ~ "soil microbial biomass",
                  Response_var %in% biol_som ~ "soil organic matter",
                
                  #Environmental Properties
                  Response_var %in% envir_temp ~ "soil temperature",
                  Response_var %in% envir_CO2 ~ "carbon dioxide emissions",
                  Response_var %in% envir_N2O ~ "nitrous oxide emissions",
                
                #Pest Regulation
                  #Weeds
                  Response_var %in% weeds_species ~ "weeds (species-level)",
                  Response_var %in% weeds_community ~ "weeds (community-level)",
                  
                  #Invertebrates
                  Response_var %in% invert_pests_species ~ "invertebrate pests (species-level)",
                  Response_var %in% invert_preds_species ~ "invertebrate predators (species-level)",
                  Response_var %in% invert_preds_comm ~ "invertebrate predators (community-level)",
                  Response_var %in% invert_pred_activity ~ "invertebrate predators (activity)",
                  Response_var %in% invert_nonpredpest ~ "invertebrate non-predators & non-pests)",
                
                #Crop Production
                  #Yields
                  Response_var %in% yields_grain ~ "grain yield",
                  Response_var %in% yields_biomass_abvgrd ~ "cash crop aboveground biomass",
                  Response_var %in% yields_biomass_blwgrd ~ "cash crop belowground biomass",
                
                  #Crop Nitrogen Yields
                  Response_var %in% crop_N ~ "crop nitrogen content",
                  
                  #Crop Seedling Density
                  Response_var %in% seedling_density ~ "crop seedling density",
                
                #Water Movement
                  #Drainage
                  Response_var %in% drainage ~ "water drainage",
                
                  #Runoff
                  Response_var %in% runoff_nitrate ~ "nitrate runoff",
                  Response_var %in% runoff_phosphorous ~ "phosphorous runoff"
               # TRUE                      ~  "other"
              ))
            

      #Attach column to Results
      Results <- left_join(Results, metric_labels, by = c("key", "Response_var"))
          
#############################################################################################
      
      
      
      #File with all results included from Cover Crop review
      CC_Ref2 <- left_join(Ref, ExpD_Loc)
      CC_Cash <- left_join(CC_Ref2, CashCrop)
      CC_Trt <- left_join(CC_Cash, Treatment) 
       
      CC_all <- left_join(CC_Trt, Results)#, by = "Paper_id", "Duration")
      
      # CCall_Cash$Year <- as.numeric(CCall_Cash$Year)  
      
      #add surrogate key to Results
              CC_all$master_key = rownames(CC_all)
      
      write.csv(CC_all, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv")

      
      
    #Filter by CoverCrop:CC_max_diversity

      monocultures <- filter(CoverCrop, CC_max_diversity == "single")
      mixtures <- filter(CoverCrop, CC_max_diversity == "mixture")

#Join data frames (must make mega dataframe so it can be later filtered based on response variables) 

      mono_ExpD <- left_join(monocultures, ExpD_Loc)
      mono_Ref <- left_join(mono_ExpD, Ref)
      mono_Cash <- left_join(mono_Ref, CashCrop) 
        mono_Cash$Year = as.numeric(mono_Cash$Year)
  
  #Master file for all monocultures = mono_all
mono_all <- left_join(mono_Cash, Results, by = "Paper_id", "Duration") 
        write.csv(mono_all, file = "C:/Users/LWA/github/midwesternag_synthesis/monocultures_data.csv")


      mix_ExpD <- left_join(mixtures, ExpD_Loc)
      mix_Ref <- left_join(mix_ExpD, Ref)
      mix_Cash <- left_join(mix_Ref, CashCrop) 
        mix_Cash$Year = as.numeric(mix_Cash$Year)
        
  #Master file for all mixtures = mix_all
mix_all <- left_join(mix_Cash, Results, by = "Paper_id", "Duration") 
        write.csv(mix_all, file = "C:/Users/LWA/github/midwesternag_synthesis/mixtures_data.csv")


#####Side note###
    #to match column keys with different names use by = c("a" = "b")
    #this will be useful for joining covercrop to results tibble as "Trt_id" = "Trt1_id" or "Trt2_id"

#number of observations for each response variable 
    groups_RVs <- mono_all  %>%
              count(Group_RV, sort = TRUE)
              groups_RVs
            #1 Pest Regulation  3308
            #2 Soil             2402
            #3 Crop Production  2330
            #4 Water             213
              
    #filter tibble for Group_Rv = Soil
      mono_Soil <- filter(mono_Results, Group_RV == "Soil")
      mono_Production <- filter(mono_Results, Group_RV == "Crop Production")
      mono_Water <- filter(mono_Results, Group_RV == "Water")
      mono_Pest <- filter(mono_Results, Group_RV == "Pest Regulation")
      
      
   #Identify variables included in Soil grouping 
      #itemized list of unique soil metrics (Response_var)
      (uniqueSoils <- unique(mono_Soil$Response_var))
      
      (uniquePestR <- unique(mono_Pest$Response_var))
      
      (uniqueProduction <- unique(mono_Production$Response_var))
      
      (uniqueWater <- unique(mono_Water$Response_var))
      
      ##########################################################Begin here########################
      
    
      
      
# number of unique studies for each metric
  #within CC_max_diversity = single species
      (unique(ss_soil_om$Paper_id))
       (unique(ss_soil_erosion$Paper_id))
       (unique(ss_soil_nutrients$Paper_id))
       (unique(ss_soil_ghg$Paper_id))       
              
  #within CC_max_diversity = mixture
      (unique(mix_soil_om$Paper_id))      
      (unique(mix_soil_erosion$Paper_id))
      (unique(mix_soil_nutrients$Paper_id))
      (unique(mix_soil_ghg$Paper_id))
  
        
 
        
        
        
        
        
  
    
   
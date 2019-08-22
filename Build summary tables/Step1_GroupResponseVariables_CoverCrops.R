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


#remove any unwanted response variables

Results <-read.csv("Cover Crop Review/CoverCrop_Results.csv", row.names = NULL)
#for cover crop data frame
Results <- filter(Results,!(Response_var == "cover crop leaf N content")) #set dataframe to work with - remove cover crop nitrogen input data (incomplete dataset)

#add surrogate key to Results
Results$Res_key = rownames(Results)

    ########################Cover Crops Review####################################################
#Bulk groupings####

    ####Climate Mitigation####
        ##Carbon Emissions#####
    
    chem_carbon <-  c(
      "soil organic carbon in topsoil",
      "particulate organic carbon",
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
      "soil organic carbon (0-15 cm)",
      "soil organic carbon (15-75 cm)",
      "soil organic carbon (20-40 cm depth)",                                                                             
      "soil organic carbon (40-60 cm depth)",
      "total carbon",
      "total carbon (0-50 cm depth)",
      "total carbon (0-30 cm depth)",
      "total carbon (0-45 cm depth)",
      "soil organic carbon (0-75 cm)",
      "soil organic carbon 0-30 cm depth (spring)",
      "soil organic carbon (SOC), spring sample (0-60 cm depth)"
    )
    
    envir_CO2 <- c("carbon dioxide emissions (CO2-C)")
    
        ##Nitrogen emissions####
    envir_N2O <- c(
      "N2O emissions when cover crop present",
      "N2O emissions over entire year",
      "cumulative N2O-N emissions from soil",
      "cumulative nitrous oxide flux from soil April 2003-Mar 2004",
      "cumulative nitrous oxide flux from soil April 2004 - Feb 2004",
      "nitrous oxide emissions (N2O-N)"
    )
    
    
    
    #####Soil Nutrients####                
        ###Nitrogen#######
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
            
            
        ##Phosphorus#####
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
          
        ##Potassium####    
                chem_potassium <- c(
                  "potassium removal",
                  "potassium inputs",
                  "potassium balance",
                  "exchangable potassium (K)"
                )
                
      #####Other Soil Properties####
                
                ##Physical properties####
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
                
                phy_totalpores <- c(
                  "total pore spaces"
                )
                
                phy_airpores <- c(
                  "air filled pore space",
                  "volumetric air content",
                  "pore tortuosity factor",
                  "relative gas diffusion coefficient")
                
                phy_waterpores <- c("water filled pore space")
                
                phy_aggregatestability <- c(
                  "mean weight diameter of water stable aggregates",
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates < 0.42 mm)",               
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 0.42 and 0.84 mm)",
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 0.84 and 2.0 mm)", 
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 2.0 and 6.4 mm)",  
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates between 6.4 and 19.2 mm)", 
                  "fraction of dry soil aggregate size distribution from the top 50 mm of soil (aggregates > 19.2 mm)"               
                )
                phy_aggregatesize <- c(
                  "water aggregate stability",
                  "water stable aggregates in soybean",
                  "water stable aggregates in maize",                                                                                 
                  "water stable mean weight diameter (0-10 cm depth)",                                                                 
                  "water stable mean weight diameter (10-20 cm depth)",                                                                
                  "water stable mean weight diameter (20-40 cm depth)",                                                               
                  "water stable mean weight diameter (40-60 cm depth)"
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
                
                
                phy_sand <- c(
                  "percent sand  (0-15 cm depth)",                                                                                    
                  "percent sand  (15-30 cm depth)")
                phy_silt <- c(
                  "percent silt  (0-15 cm depth)",                                                                                  
                  "percent silt  (15-30 cm depth)")
                phy_clay <- c(
                  "percent clay  (0-15 cm depth)",                                                                                  
                  "percent clay  (15-30 cm depth)"                                                                                   
                ) 
                
                phy_som <- c(
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
                
                ##Chemical Properties#####
                
                chem_acidity <- c(
                  "pH",
                  "pH (0-15 cm)")            
                
        ##Abiotic Factors######          
                
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
                  "soil water content, after maize planting (12-24 in. depth)",
                  "average annual soil water storage (maize)",
                  "average annual soil water storage (soybean)"
        
                )
                
                
                
                envir_temp <- c("soil temperature")
                
                
        ##Biotic Factors####
                biol_microbes <- c(
                  "microbial biomass",
                  "microbial biomass nitrogen (MBN)"
                              )
                biol_enzymeactivity <- c(
                  "enzyme activity in soybean (Hydrolysis of Fluroescein Diacetate)",                                                 
                  "enzyme activity in maize (Hydrolysis of Fluroescein Diacetate)" 
                  )
                
                
                
    ####Pests####
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
                
        ##Pest Natural Enemies####
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
                
        ##Pathogens####
                pathogen <- c(
                  "soybean root rot (general)"                                                                                                                             
                )
                
                
    ####Crop Yields####
                
        ## Grain production####
            ##Soybean####        
                yields_grainsoy <- c(
                  "soybean grain yield per 1.8 m row",
                  "soybean grain yield (years 5 & 6)",
                  "soybean grain yield (7 year average)",                           
                  "soybean grain yield",
                  "soybean grain yield (4 year average)",
                  "soybean yield (3 year average)"
                )
            ##Corn####   
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
        ##Crop Growth####
            ##Corn####
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
            ##Corn####        
                crop_N_maizestalk <- c(
                  "maize stalk nitrate",
                  "total aboveground N uptake (maize)",
                  "maize silage N removal",
                  "maize stover nitrogen uptake" ,
                  "total nitrogen uptake by maize stover (3 year average)",
                  "aboveground plant nitrogen uptake")
             
            ##Soybean####   
                crop_N_soybean <- c(  
                  "total aboveground N uptake (soybean)"
                )
                
        ##Grain Quality####
            ##Corn####
                crop_N_maizegrain <- c( 
                  "maize grain nitrogen uptake",
                  "maize nitrogen uptake",                                     
                  "total nitrogen uptake by maize grain (3 year average)")
                
                
                
        ##Stand Count####
            ##Corn####    
                standcount_corn <- c(
                  "maize stand count (4 year average)",                   
                  "maize stand count"
                )
            
            ##Soybean####
                standcount_soybean <- c(
                  "soybean stand count"
                )
                
    ####Water Quality####
                
        ## Flow Quantity####
            ##Water####  
                water_discharge <- c(
                  "runoff rate",
                  "drainage discharge (maize)",
                  "drainage discharge (soybean)"
                )
                
               water_drainage <- c(
                 "average drainage",
                "infiltration rate",
                "average annual subsurface drainage (maize)",
                "average annual subsurface drainage (soybean)"
               )
                
               
                
        ##Nutrient Runoff####
            ##Nitrate####
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
            ##Phosphorus####
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
                  #Group_level1####    
                  mutate(
                    group_level1 = case_when( 
                      
                      
                      ####Climate Mitigation####
                      ##Carbon Emissions#####
                      Response_var %in% chem_carbon ~ "Climate Mitigation",      
                      ##Carbon Emissions####
                      Response_var %in% envir_CO2 ~ "Climate Mitigation",       
                      ##Nitrogen emissions####
                      Response_var %in% envir_N2O ~ "Climate Mitigation",       
                        
      #####Other Soil Properties####
                      
                      ##Physical properties####
                      Response_var %in% phy_erosion ~ "Other Soil Properties",       
                      Response_var %in% phy_compaction ~ "Other Soil Properties",       
                      Response_var %in% phy_totalpores ~ "Other Soil Properties",       
                      Response_var %in% phy_airpores ~ "Other Soil Properties",
                      Response_var %in% phy_waterpores ~ "Other Soil Properties", 
                      Response_var %in% phy_aggregatestability ~ "Other Soil Properties",                
                      Response_var %in% phy_aggregatesize ~ "Other Soil Properties",                
                      Response_var %in% phy_bulkdensity ~ "Other Soil Properties",                
                      Response_var %in% phy_sand ~ "Other Soil Properties",                
                      Response_var %in% phy_silt ~ "Other Soil Properties",                
                      Response_var %in% phy_clay ~ "Other Soil Properties",                
                      Response_var %in% phy_som ~ "Other Soil Properties",                
      
      ##Abiotic Factors######          
      Response_var %in% phy_watercontent ~ "Other Soil Properties",                                                    
      Response_var %in% envir_temp ~ "Other Soil Properties",                                                    
      
      ##Biotic Factors####
      Response_var %in% biol_microbes ~ "Other Soil Properties",                                                    
      Response_var %in% biol_enzymeactivity ~ "Other Soil Properties",                  
      
      ##Chemical Properties#####
      Response_var %in% chem_acidity ~ "Other Soil Properties",                
                      
      
      #####Soil Nutrients####                
                      ###Nitrogen#######
     ##Nitrate####
       Response_var %in% chem_nitrate_spring ~ "Soil Nutrients",                
      Response_var %in% chem_nitrate_maize ~ "Soil Nutrients",                                 
      Response_var %in% chem_nitrate_soybean ~ "Soil Nutrients",                                 
      Response_var %in% chem_nitrate_fall ~ "Soil Nutrients",                                 
    ##Ammonium####                  
    Response_var %in% chem_ammonium_spring ~ "Soil Nutrients",                                 
    
    ##Total Nitrogen####
    Response_var %in% chem_totalN ~ "Soil Nutrients",                                                    
                
                      
                      ##Phosphorus#####
    Response_var %in% chem_phosphorus ~ "Soil Nutrients",                                                    
                       
                      ##Potassium####    
    Response_var %in% chem_potassium ~ "Soil Nutrients",                                                    
                      
                      ####Pests####
                      ## Weeds ####
    ##Broadleaves####
    Response_var %in% weed_Palmeramaranth ~ "Pests",
    Response_var %in% weed_cocklebur ~ "Pests",
    Response_var %in% weed_deadnettle ~ "Pests",                  
    Response_var %in% weed_waterhemp ~ "Pests",                                    
    
    #Grassess#####
    Response_var %in% weed_foxtail ~ "Pests",
    
    ###Weed community####
    Response_var %in% weed_community_density ~ "Pests",                  
    Response_var %in% weed_community_biomass ~ "Pests",                                    
                      
    ## Nematodes ####
    Response_var %in% invert_pests_SCN ~ "Pests",                                    
    ## Invertebrates ####                  
    Response_var %in% invert_pests_Aglycines ~ "Pests",                                    
    Response_var %in% invert_pests_seedcornmaggot ~ "Pests",                                    
    Response_var %in% invert_pests_cornrootworm ~ "Pests",                                    
    
    ##Invertebrate Damage####
    Response_var %in% invert_pests_cornrootworm_damage ~ "Pests",                                                       
    Response_var %in% invert_pests_seedcornmaggot_damage ~ "Pests",                                                                         
                       
    
    ##Pest Natural Enemies####
    Response_var %in% invert_preds ~ "Pests",                                                                         
    Response_var %in% invert_preds_soilcomm_abund ~ "Pests", #found in soil                                                                         
    Response_var %in% invert_preds_soilcomm_div ~ "Pests", #found in soil                                                                         
    Response_var %in% invert_preds_vegcomm_abund ~ "Pests", #found on leaves
    Response_var %in% invert_preds_vegcomm_div ~ "Pests", #found on leaves
    Response_var %in% invert_preds_activity ~ "Pests", 
    Response_var %in% invert_nonpredpest ~ "Pests",                   
                       
                      ##Pathogens####
    Response_var %in% pathogen ~ "Pests",                                     
                      
                      
                      ####Crop Yields####
                      
                      ## Grain production####
                      ##Soybean####        
    Response_var %in% yields_grainsoy ~ "Crop Yields",                                     
    
                      ##Corn####   
    Response_var %in% yields_grainmaize ~ "Crop Yields",                                     
    
                      ##Crop Growth####
                      ##Corn####
    Response_var %in% yields_biomass_abvgrd ~ "Crop Yields",                                     
    Response_var %in% yields_biomass_blwgrd ~ "Crop Yields",                                     
                      
                      ##Crop Nitrogen Content ####
                      ##Corn####        
    Response_var %in% crop_N_maizestalk ~ "Crop Yields",                                     
                       
                      ##Soybean####   
    Response_var %in% crop_N_soybean ~ "Crop Yields",                                     
    
                      ##Grain Quality####
                      ##Corn####
    Response_var %in% crop_N_maizegrain ~ "Crop Yields",                                     
    
                      ##Stand Count####
                      ##Corn####    
    Response_var %in% standcount_corn ~ "Crop Yields",                                     
    
                      ##Soybean####
    Response_var %in% standcount_soybean ~ "Crop Yields",                                                       
                      
                      ####Water Quality####
                      
                      ## Flow Quantity####
                      ##Water####  
    Response_var %in% water_discharge ~ "Water Quality",                                                       
    Response_var %in% water_drainage ~ "Water Quality",                                                       
    
                      ##Nutrient Runoff####
                      ##Nitrate####
    Response_var %in% runoff_nitrate ~ "Water Quality" ,                                                     
                      ##Phosphorus####
    Response_var %in% runoff_phosphorus ~ "Water Quality"                                                       
                            )) %>%
                 #Group_level2####    
               mutate(group_level2 = case_when( 
                   
                   
                   ####Climate Mitigation####
                   ##Carbon Mitigation#####
                   Response_var %in% chem_carbon ~ "Carbon Mitigation",      
                   Response_var %in% envir_CO2 ~ "Carbon Mitigation",       
                   ##Nitrogen Mitigation####
                   Response_var %in% envir_N2O ~ "Nitrogen Mitigation",       
                   
                   #####Other Soil Properties####
                   
                   ##Physical properties####
                   Response_var %in% phy_erosion ~ "Physical Properties",       
                   Response_var %in% phy_compaction ~ "Physical Properties",              
                   Response_var %in% phy_totalpores ~ "Physical Properties",         
                   Response_var %in% phy_airpores ~ "Physical Properties",       
                   Response_var %in% phy_waterpores ~ "Physical Properties",       
                   Response_var %in% phy_aggregatestability ~ "Physical Properties",                       
                   Response_var %in% phy_aggregatesize ~ "Physical Properties",                      
                   Response_var %in% phy_bulkdensity ~ "Physical Properties",                
                   Response_var %in% phy_sand ~ "Physical Properties",           
                   Response_var %in% phy_silt ~ "Physical Properties",                  
                   Response_var %in% phy_clay ~ "Physical Properties",                  
                   Response_var %in% phy_som ~ "Physical Properties",                 
                   
                   ##Chemical Properties#####
                   Response_var %in% chem_acidity ~ "Chemical Properties",                       
              
                   ##Abiotic Factors######          
                   Response_var %in% phy_watercontent ~ "Abiotic Factors",                                                    
                   Response_var %in% envir_temp ~ "Abiotic Factors",                                                    
                   
                   ##Biotic Factors####
                   Response_var %in% biol_microbes ~ "Biotic Factors",                                                    
                   Response_var %in% biol_enzymeactivity ~ "Biotic Factors",
                   
                        
                   #####Soil Nutrients####                
                   ###Nitrogen#######
                   ##Nitrate####
                   Response_var %in% chem_nitrate_spring ~ "Nitrogen",                
                   Response_var %in% chem_nitrate_maize ~ "Nitrogen",                                 
                   Response_var %in% chem_nitrate_soybean ~ "Nitrogen",                                 
                   Response_var %in% chem_nitrate_fall ~ "Nitrogen",                                 
                   ##Ammonium####                  
                   Response_var %in% chem_ammonium_spring ~ "Nitrogen",                                 
                   
                   ##Total Nitrogen####
                   Response_var %in% chem_totalN ~ "Nitrogen",                                                    
                   
                   
                   ##Phosphorus#####
                   Response_var %in% chem_phosphorus ~ "P & K",                                                    
                   
                   ##Potassium####    
                   Response_var %in% chem_potassium ~ "P & K",                                                    
                   
                   
                   ####Pests####
                   ## Weeds ####
                   ##Broadleaves####
                   Response_var %in% weed_Palmeramaranth ~ "Weeds",
                   Response_var %in% weed_cocklebur ~ "Weeds",
                   Response_var %in% weed_deadnettle ~ "Weeds",                  
                   Response_var %in% weed_waterhemp ~ "Weeds",                                    
                   
                   #Grasses#####
                   Response_var %in% weed_foxtail ~ "Weeds",
                   
                   ###Weed community####
                   Response_var %in% weed_community_density ~ "Weeds",                  
                   Response_var %in% weed_community_biomass ~ "Weeds",                                    
                   
                   ## Nematodes ####
                   Response_var %in% invert_pests_SCN ~ "Nematodes",                                    
                   ## Invertebrates ####                  
                   Response_var %in% invert_pests_Aglycines ~ "Invertebrate Pests",                                    
                   Response_var %in% invert_pests_seedcornmaggot ~ "Invertebrate Pests",                                    
                   Response_var %in% invert_pests_cornrootworm ~ "Invertebrate Pests",                                    
                   
                   ##Crop Damage####
                   Response_var %in% invert_pests_cornrootworm_damage ~ "Crop Damage",                                                       
                   Response_var %in% invert_pests_seedcornmaggot_damage ~ "Crop Damage",                                                                         
                   
                   
                   ##Pest Natural Enemies####
                   Response_var %in% invert_preds ~ "Pest Natural Enemies",                                                                         
                   Response_var %in% invert_preds_soilcomm_abund ~ "Pest Natural Enemies", #found in soil                                                                         
                   Response_var %in% invert_preds_soilcomm_div ~ "Pest Natural Enemies", #found in soil                                                                         
                   Response_var %in% invert_preds_vegcomm_abund ~ "Pest Natural Enemies", #found on leaves
                   Response_var %in% invert_preds_vegcomm_div ~ "Pest Natural Enemies", #found on leaves
                   Response_var %in% invert_preds_activity ~ "Pest Natural Enemies", 
                   Response_var %in% invert_nonpredpest ~ "Pest Natural Enemies",                   
                   
                   ##Pathogens####
                   Response_var %in% pathogen ~ "Pathogens",                                     
                   
                   
                   ####Crop Yields####
                   
                   ## Grain production####
                   ##Soybean####        
                   Response_var %in% yields_grainsoy ~ "Grain Production",                                     
                   
                   ##Corn####   
                   Response_var %in% yields_grainmaize ~ "Grain Production",                                     
                   
                   ##Crop Growth####
                   ##Corn####
                   Response_var %in% yields_biomass_abvgrd ~ "Crop Growth",                                     
                   Response_var %in% yields_biomass_blwgrd ~ "Crop Growth",                                     
                   
                   ##Crop Nitrogen Content ####
                   ##Corn####        
                   Response_var %in% crop_N_maizestalk ~ "Crop Nitrogen Content",                                     
                   
                   ##Soybean####   
                   Response_var %in% crop_N_soybean ~ "Crop Nitrogen Content",                                     
                   
                   ##Grain Quality####
                   ##Corn####
                   Response_var %in% crop_N_maizegrain ~ "Grain Quality",                                     
                   
                   ##Stand Count####
                   ##Corn####    
                   Response_var %in% standcount_corn ~ "Stand Count",                                     
                   
                   ##Soybean####
                   Response_var %in% standcount_soybean ~ "Stand Count",                                                       
                   
                   ####Water Quality####
                   
                   ## Flow Quantity####
                   ##Water####  
                 Response_var %in% water_discharge ~ "Flow Quantity",                                                       
                   Response_var %in% water_drainage ~ "Flow Quantity",                                                       
                   
                   ##Nutrient Runoff####
                   ##Nitrate####
                 Response_var %in% runoff_nitrate ~ "Nutrient Runoff" ,
                   ##Phosphorus####
                   Response_var %in% runoff_phosphorus ~ "Nutrient Runoff"                                                       
                 )) %>%
                 #Group_level3####    
               mutate(
                 group_level3 = case_when( 
                   
                   
                   ####Climate Mitigation####
                   ##Carbon Mitigation#####
                   Response_var %in% chem_carbon ~ "Soil organic carbon",      
                   Response_var %in% envir_CO2 ~ "Carbon dioxide (CO2)",       
                   ##Nitrogen Mitigation####
                   Response_var %in% envir_N2O ~ "Nitrous oxide (N2O)",       
                   
                   #####Other Soil Properties####
                   
                   ##Physical properties####
                   Response_var %in% phy_erosion ~ "Erosion",       
                   Response_var %in% phy_compaction ~ "Compaction",              
                   Response_var %in% phy_totalpores ~ "Total pore space",         
                   Response_var %in% phy_airpores ~ "Air-filled pores",       
                   Response_var %in% phy_waterpores ~ "Water-filled pores",       
                   Response_var %in% phy_aggregatestability ~ "Aggregate stability",                       
                   Response_var %in% phy_aggregatesize ~ "Aggregate size",                      
                   Response_var %in% phy_bulkdensity ~ "Soil bulk density",                
                   Response_var %in% phy_sand ~ "Sand content",           
                   Response_var %in% phy_silt ~ "Silt content",                  
                   Response_var %in% phy_clay ~ "Clay content",                  
                   Response_var %in% phy_som ~ "Soil organic matter content",                 
                   
                   ##Chemical Properties#####
                   Response_var %in% chem_acidity ~ "pH",                       
                   
                   
                   ##Abiotic Factors######          
                   Response_var %in% phy_watercontent ~ "Soil moisture",                                                    
                   Response_var %in% envir_temp ~ "Soil temperature",                                                    
                   
                   ##Biotic Factors####
                   Response_var %in% biol_microbes ~ "Microbial biomass",                                                    
                   Response_var %in% biol_enzymeactivity ~ "Enzyme activity",
                   
                   #####Soil Nutrients####                
                   ###Nitrogen#######
                   ##Nitrate####
                   Response_var %in% chem_nitrate_spring ~ "Nitrate (NO3)",                
                   Response_var %in% chem_nitrate_maize ~ "Nitrate (NO3)",                                 
                   Response_var %in% chem_nitrate_soybean ~ "Nitrate (NO3)",                                 
                   Response_var %in% chem_nitrate_fall ~ "Nitrate (NO3)",                                 
                   ##Ammonium####                  
                   Response_var %in% chem_ammonium_spring ~ "Ammonium (NH4)",                                 
                   
                   ##Total Nitrogen####
                   Response_var %in% chem_totalN ~ "Total nitrogen",                                                    
                   
                   
                   ##Phosphorus#####
                   Response_var %in% chem_phosphorus ~ "Phosphorus",                                                    
                   
                   ##Potassium####    
                   Response_var %in% chem_potassium ~ "Potassium",                                                    
                   
                   ####Pests####
                   ## Weeds ####
                   ##Broadleaves####
                   Response_var %in% weed_Palmeramaranth ~ "Pigweed (#)",
                   Response_var %in% weed_cocklebur ~ "Cocklebur (#)",
                   Response_var %in% weed_deadnettle ~ "Dead nettle (#)",                  
                   Response_var %in% weed_waterhemp ~ "Waterhemp (#)",                                    
                   
                   #Grasses#####
                   Response_var %in% weed_foxtail ~ "Foxtail (#)",
                   
                   ###Weed community####
                   Response_var %in% weed_community_density ~ "Weed community (#)",                  
                   Response_var %in% weed_community_biomass ~ "Weeed community (biomass)",                                    
                   
                   ## Nematodes ####
                   Response_var %in% invert_pests_SCN ~ "Soybean cyst nematode (#)",                                    
                   ## Invertebrates ####                  
                   Response_var %in% invert_pests_Aglycines ~ "Soybean aphid (#)",                                    
                   Response_var %in% invert_pests_seedcornmaggot ~ "Seedcorn maggot (#)",                                    
                   Response_var %in% invert_pests_cornrootworm ~ "Corn rootworm (#)",                                    
                   
                   ##Crop Damage####
                   Response_var %in% invert_pests_cornrootworm_damage ~ "Damage caused by corn rootworm",                                                       
                   Response_var %in% invert_pests_seedcornmaggot_damage ~ "Damage caused by seed corn maggot",                                                                         
                   
                   
                   ##Pest Natural Enemies####
                   Response_var %in% invert_preds ~ "Invertebrate predator (#)",                                                                         
                   Response_var %in% invert_preds_soilcomm_abund ~ "Invertebrate soil community (#)", #found in soil                                                                         
                   Response_var %in% invert_preds_soilcomm_div ~ "Invertebrate soil community (diversity)", #found in soil                                                                         
                   Response_var %in% invert_preds_vegcomm_abund ~ "Invertebrate foliar community (#)", #found on leaves
                   Response_var %in% invert_preds_vegcomm_div ~ "Invertebrate foliar community (diversity)", #found on leaves
                   Response_var %in% invert_preds_activity ~ "Invertebrate predator (activity)", 
                   Response_var %in% invert_nonpredpest ~ "Invertebrate non-predator & pests (#)",                   
                   
                   ##Pathogens####
                   Response_var %in% pathogen ~ "Pathogens",                                     
                   
                   
                   ####Crop Yields####
                   
                   ## Grain production####
                   ##Soybean####        
                   Response_var %in% yields_grainsoy ~ "Soybean",                                     
                   
                   ##Corn####   
                   Response_var %in% yields_grainmaize ~ "Corn",                                     
                   
                   ##Crop Growth####
                   ##Corn####
                   Response_var %in% yields_biomass_abvgrd ~ "Corn stalk biomass",                                     
                   Response_var %in% yields_biomass_blwgrd ~ "Corn root biomass",                                     
                   
                   ##Crop Nitrogen Content ####
                   ##Corn####        
                   Response_var %in% crop_N_maizestalk ~ "Corn stalk nitrogen content",                                     
                   
                   ##Soybean####   
                   Response_var %in% crop_N_soybean ~ "Soybean stalk nitrogen content",                                     
                   
                   ##Grain Quality####
                   ##Corn####
                   Response_var %in% crop_N_maizegrain ~ "Corn nitrogen content",                                     
                   
                   ##Stand Count####
                   ##Corn####    
                   Response_var %in% standcount_corn ~ "Corn",                                     
                   
                   ##Soybean####
                   Response_var %in% standcount_soybean ~ "Soybean",                                                       
                   
                   ####Water Quality####
                   
                   ## Flow Quantity####
                   ##Water####  
                   Response_var %in% water_discharge ~ "Water discharge",                                                       
                   Response_var %in% water_drainage ~ "Drainage",                                                       
                   
                   ##Nutrient Runoff####
                   ##Nitrate####
                   Response_var %in% runoff_nitrate ~ "Nitrate" ,
                   ##Phosphorus####
                   Response_var %in% runoff_phosphorus ~ "Phosphorus"                                                       
                 )) 
                      
                                    #Attach column to Results######
                Results <-
                  left_join(Results, metric_labels, by = c("Res_key", "Response_var"))
                

               missing <- Results[is.na(Results$group_level1),] #check to see if all rows have an assigned group_level1
               missing <- Results[is.na(Results$group_level2),] #check to see if all rows have an assigned group_level2
               missing <- Results[is.na(Results$group_level3),] #check to see if all rows have an assigned group_level3
               
               
               ####Save Results file with added Group names
               write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Cover Crop Review/CC_ResultsGrouped.csv", row.names = FALSE)
               
                               


#############################################################################################


missing <- Results[is.na(Results$group_level1),] #check to see if all rows have an assigned group_level1
missing <- Results[is.na(Results$group_level2),] #check to see if all rows have an assigned group_level2
missing <- Results[is.na(Results$group_level3),] #check to see if all rows have an assigned group_level3


####Save Results file with added Group names

write.csv(Results, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Cover Crop Review/CC_ResultsGrouped.csv", row.names = FALSE)


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


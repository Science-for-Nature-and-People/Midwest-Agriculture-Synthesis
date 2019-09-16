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

df <-read.csv("Cover Crop Review/CoverCrop_Results.csv", row.names = NULL)

#Remove unneeded columns
df$Year_result <- NULL
df$Effect <- NULL
df$Authors_comments <- NULL
df$Reviewers_results_short <- NULL
df$Reviewers_results_long <- NULL
df$Group_RV <- NULL

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


#for cover crop data frame
df <- filter(df,!(rv == "cover crop leaf N content")) #set dataframe to work with - remove cover crop nitrogen input data (incomplete dataset)

#add surrogate key to Results
df$review_key = rownames(df)

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


              metric_labels <- df %>%
                select(rv, review_key) %>%
                  #Group_level1####    
                  mutate(
                    group_level1 = case_when( 
                      
                      
                      ####Climate Mitigation####
                      ##Carbon Emissions#####
                      rv %in% chem_carbon ~ "Climate Mitigation",      
                      ##Carbon Emissions####
                      rv %in% envir_CO2 ~ "Climate Mitigation",       
                      ##Nitrogen emissions####
                      rv %in% envir_N2O ~ "Climate Mitigation",       
                        
      #####Other Soil Properties####
                      
                      ##Physical properties####
                      rv %in% phy_erosion ~ "Other Soil Properties",       
                      rv %in% phy_compaction ~ "Other Soil Properties",       
                      rv %in% phy_totalpores ~ "Other Soil Properties",       
                      rv %in% phy_airpores ~ "Other Soil Properties",
                      rv %in% phy_waterpores ~ "Other Soil Properties", 
                      rv %in% phy_aggregatestability ~ "Other Soil Properties",                
                      rv %in% phy_aggregatesize ~ "Other Soil Properties",                
                      rv %in% phy_bulkdensity ~ "Other Soil Properties",                
                      rv %in% phy_sand ~ "Other Soil Properties",                
                      rv %in% phy_silt ~ "Other Soil Properties",                
                      rv %in% phy_clay ~ "Other Soil Properties",                
                      rv %in% phy_som ~ "Other Soil Properties",                
      
      ##Abiotic Factors######          
      rv %in% phy_watercontent ~ "Other Soil Properties",                                                    
      rv %in% envir_temp ~ "Other Soil Properties",                                                    
      
      ##Biotic Factors####
      rv %in% biol_microbes ~ "Other Soil Properties",                                                    
      rv %in% biol_enzymeactivity ~ "Other Soil Properties",                  
      
      ##Chemical Properties#####
      rv %in% chem_acidity ~ "Other Soil Properties",                
                      
      
      #####Soil Nutrients####                
                      ###Nitrogen#######
     ##Nitrate####
      rv %in% chem_nitrate_spring ~ "Soil Nutrients",                
      rv %in% chem_nitrate_maize ~ "Soil Nutrients",                                 
      rv %in% chem_nitrate_soybean ~ "Soil Nutrients",                                 
      rv %in% chem_nitrate_fall ~ "Soil Nutrients",                                 
    ##Ammonium####                  
    rv %in% chem_ammonium_spring ~ "Soil Nutrients",                                 
    
    ##Total Nitrogen####
    rv %in% chem_totalN ~ "Soil Nutrients",                                                    
                
                      
                      ##Phosphorus#####
    rv %in% chem_phosphorus ~ "Soil Nutrients",                                                    
                       
                      ##Potassium####    
    rv %in% chem_potassium ~ "Soil Nutrients",                                                    
                      
                      ####Pests####
                      ## Weeds ####
    ##Broadleaves####
    rv %in% weed_Palmeramaranth ~ "Pests",
    rv %in% weed_cocklebur ~ "Pests",
    rv %in% weed_deadnettle ~ "Pests",                  
    rv %in% weed_waterhemp ~ "Pests",                                    
    
    #Grassess#####
    rv %in% weed_foxtail ~ "Pests",
    
    ###Weed community####
    rv %in% weed_community_density ~ "Pests",                  
    rv %in% weed_community_biomass ~ "Pests",                                    
                      
    ## Nematodes ####
    rv %in% invert_pests_SCN ~ "Pests",                                    
    ## Invertebrates ####                  
    rv %in% invert_pests_Aglycines ~ "Pests",                                    
    rv %in% invert_pests_seedcornmaggot ~ "Pests",                                    
    rv %in% invert_pests_cornrootworm ~ "Pests",                                    
    
    ##Invertebrate Damage####
    rv %in% invert_pests_cornrootworm_damage ~ "Pests",                                                       
    rv %in% invert_pests_seedcornmaggot_damage ~ "Pests",                                                                         
                       
    
    ##Pest Natural Enemies####
    rv %in% invert_preds ~ "Pests",                                                                         
    rv %in% invert_preds_soilcomm_abund ~ "Pests", #found in soil                                                                         
    rv %in% invert_preds_soilcomm_div ~ "Pests", #found in soil                                                                         
    rv %in% invert_preds_vegcomm_abund ~ "Pests", #found on leaves
    rv %in% invert_preds_vegcomm_div ~ "Pests", #found on leaves
    rv %in% invert_preds_activity ~ "Pests", 
    rv %in% invert_nonpredpest ~ "Pests",                   
                       
                      ##Pathogens####
    rv %in% pathogen ~ "Pests",                                     
                      
                      
                      ####Crop Yields####
                      
                      ## Grain production####
                      ##Soybean####        
    rv %in% yields_grainsoy ~ "Crop Yields",                                     
    
                      ##Corn####   
    rv %in% yields_grainmaize ~ "Crop Yields",                                     
    
                      ##Crop Growth####
                      ##Corn####
    rv %in% yields_biomass_abvgrd ~ "Crop Yields",                                     
    rv %in% yields_biomass_blwgrd ~ "Crop Yields",                                     
                      
                      ##Crop Nitrogen Content ####
                      ##Corn####        
    rv %in% crop_N_maizestalk ~ "Crop Yields",                                     
                       
                      ##Soybean####   
    rv %in% crop_N_soybean ~ "Crop Yields",                                     
    
                      ##Grain Quality####
                      ##Corn####
    rv %in% crop_N_maizegrain ~ "Crop Yields",                                     
    
                      ##Stand Count####
                      ##Corn####    
    rv %in% standcount_corn ~ "Crop Yields",                                     
    
                      ##Soybean####
    rv %in% standcount_soybean ~ "Crop Yields",                                                       
                      
                      ####Water Quality####
                      
                      ## Flow Quantity####
                      ##Water####  
    rv %in% water_discharge ~ "Water Quality",                                                       
    rv %in% water_drainage ~ "Water Quality",                                                       
    
                      ##Nutrient Runoff####
                      ##Nitrate####
    rv %in% runoff_nitrate ~ "Water Quality" ,                                                     
                      ##Phosphorus####
    rv %in% runoff_phosphorus ~ "Water Quality"                                                       
                            )) %>%
                 #Group_level2####    
               mutate(group_level2 = case_when( 
                   
                   
                   ####Climate Mitigation####
                   ##Carbon Mitigation#####
                   rv %in% chem_carbon ~ "Carbon Mitigation",      
                   rv %in% envir_CO2 ~ "Carbon Mitigation",       
                   ##Nitrogen Mitigation####
                   rv %in% envir_N2O ~ "Nitrogen Mitigation",       
                   
                   #####Other Soil Properties####
                   
                   ##Physical properties####
                   rv %in% phy_erosion ~ "Physical Properties",       
                   rv %in% phy_compaction ~ "Physical Properties",              
                   rv %in% phy_totalpores ~ "Physical Properties",         
                   rv %in% phy_airpores ~ "Physical Properties",       
                   rv %in% phy_waterpores ~ "Physical Properties",       
                   rv %in% phy_aggregatestability ~ "Physical Properties",                       
                   rv %in% phy_aggregatesize ~ "Physical Properties",                      
                   rv %in% phy_bulkdensity ~ "Physical Properties",                
                   rv %in% phy_sand ~ "Physical Properties",           
                   rv %in% phy_silt ~ "Physical Properties",                  
                   rv %in% phy_clay ~ "Physical Properties",                  
                   rv %in% phy_som ~ "Physical Properties",                 
                   
                   ##Chemical Properties#####
                   rv %in% chem_acidity ~ "Chemical Properties",                       
              
                   ##Abiotic Factors######          
                   rv %in% phy_watercontent ~ "Abiotic Factors",                                                    
                   rv %in% envir_temp ~ "Abiotic Factors",                                                    
                   
                   ##Biotic Factors####
                   rv %in% biol_microbes ~ "Biotic Factors",                                                    
                   rv %in% biol_enzymeactivity ~ "Biotic Factors",
                   
                        
                   #####Soil Nutrients####                
                   ###Nitrogen#######
                   ##Nitrate####
                   rv %in% chem_nitrate_spring ~ "Nitrogen",                
                   rv %in% chem_nitrate_maize ~ "Nitrogen",                                 
                   rv %in% chem_nitrate_soybean ~ "Nitrogen",                                 
                   rv %in% chem_nitrate_fall ~ "Nitrogen",                                 
                   ##Ammonium####                  
                   rv %in% chem_ammonium_spring ~ "Nitrogen",                                 
                   
                   ##Total Nitrogen####
                   rv %in% chem_totalN ~ "Nitrogen",                                                    
                   
                   
                   ##Phosphorus#####
                   rv %in% chem_phosphorus ~ "P & K",                                                    
                   
                   ##Potassium####    
                   rv %in% chem_potassium ~ "P & K",                                                    
                   
                   
                   ####Pests####
                   ## Weeds ####
                   ##Broadleaves####
                   rv %in% weed_Palmeramaranth ~ "Weeds",
                   rv %in% weed_cocklebur ~ "Weeds",
                   rv %in% weed_deadnettle ~ "Weeds",                  
                   rv %in% weed_waterhemp ~ "Weeds",                                    
                   
                   #Grasses#####
                   rv %in% weed_foxtail ~ "Weeds",
                   
                   ###Weed community####
                   rv %in% weed_community_density ~ "Weeds",                  
                   rv %in% weed_community_biomass ~ "Weeds",                                    
                   
                   ## Nematodes ####
                   rv %in% invert_pests_SCN ~ "Nematodes",                                    
                   ## Invertebrates ####                  
                   rv %in% invert_pests_Aglycines ~ "Invertebrate Pests",                                    
                   rv %in% invert_pests_seedcornmaggot ~ "Invertebrate Pests",                                    
                   rv %in% invert_pests_cornrootworm ~ "Invertebrate Pests",                                    
                   
                   ##Crop Damage####
                   rv %in% invert_pests_cornrootworm_damage ~ "Crop Damage",                                                       
                   rv %in% invert_pests_seedcornmaggot_damage ~ "Crop Damage",                                                                         
                   
                   
                   ##Pest Natural Enemies####
                   rv %in% invert_preds ~ "Pest Natural Enemies",                                                                         
                   rv %in% invert_preds_soilcomm_abund ~ "Pest Natural Enemies", #found in soil                                                                         
                   rv %in% invert_preds_soilcomm_div ~ "Pest Natural Enemies", #found in soil                                                                         
                   rv %in% invert_preds_vegcomm_abund ~ "Pest Natural Enemies", #found on leaves
                   rv %in% invert_preds_vegcomm_div ~ "Pest Natural Enemies", #found on leaves
                   rv %in% invert_preds_activity ~ "Pest Natural Enemies", 
                   rv %in% invert_nonpredpest ~ "Pest Natural Enemies",                   
                   
                   ##Pathogens####
                   rv %in% pathogen ~ "Pathogens",                                     
                   
                   
                   ####Crop Yields####
                   
                   ## Grain production####
                   ##Soybean####        
                   rv %in% yields_grainsoy ~ "Grain Production",                                     
                   
                   ##Corn####   
                   rv %in% yields_grainmaize ~ "Grain Production",                                     
                   
                   ##Crop Growth####
                   ##Corn####
                   rv %in% yields_biomass_abvgrd ~ "Crop Growth",                                     
                   rv %in% yields_biomass_blwgrd ~ "Crop Growth",                                     
                   
                   ##Crop Nitrogen Content ####
                   ##Corn####        
                   rv %in% crop_N_maizestalk ~ "Crop Nitrogen Content",                                     
                   
                   ##Soybean####   
                   rv %in% crop_N_soybean ~ "Crop Nitrogen Content",                                     
                   
                   ##Grain Quality####
                   ##Corn####
                   rv %in% crop_N_maizegrain ~ "Grain Quality",                                     
                   
                   ##Stand Count####
                   ##Corn####    
                   rv %in% standcount_corn ~ "Stand Count",                                     
                   
                   ##Soybean####
                   rv %in% standcount_soybean ~ "Stand Count",                                                       
                   
                   ####Water Quality####
                   
                   ## Flow Quantity####
                   ##Water####  
                 rv %in% water_discharge ~ "Flow Quantity",                                                       
                   rv %in% water_drainage ~ "Flow Quantity",                                                       
                   
                   ##Nutrient Runoff####
                   ##Nitrate####
                 rv %in% runoff_nitrate ~ "Nutrient Runoff" ,
                   ##Phosphorus####
                   rv %in% runoff_phosphorus ~ "Nutrient Runoff"                                                       
                 )) %>%
                 #Group_level3####    
               mutate(
                 group_level3 = case_when( 
                   
                   
                   ####Climate Mitigation####
                   ##Carbon Mitigation#####
                   rv %in% chem_carbon ~ "Soil organic carbon",      
                   rv %in% envir_CO2 ~ "Carbon dioxide (CO2)",       
                   ##Nitrogen Mitigation####
                   rv %in% envir_N2O ~ "Nitrous oxide (N2O)",       
                   
                   #####Other Soil Properties####
                   
                   ##Physical properties####
                   rv %in% phy_erosion ~ "Erosion",       
                   rv %in% phy_compaction ~ "Compaction",              
                   rv %in% phy_totalpores ~ "Total pore space",         
                   rv %in% phy_airpores ~ "Air-filled pores",       
                   rv %in% phy_waterpores ~ "Water-filled pores",       
                   rv %in% phy_aggregatestability ~ "Aggregate stability",                       
                   rv %in% phy_aggregatesize ~ "Aggregate size",                      
                   rv %in% phy_bulkdensity ~ "Soil bulk density",                
                   rv %in% phy_sand ~ "Sand content",           
                   rv %in% phy_silt ~ "Silt content",                  
                   rv %in% phy_clay ~ "Clay content",                  
                   rv %in% phy_som ~ "Soil organic matter content",                 
                   
                   ##Chemical Properties#####
                   rv %in% chem_acidity ~ "pH",                       
                   
                   
                   ##Abiotic Factors######          
                   rv %in% phy_watercontent ~ "Soil moisture",                                                    
                   rv %in% envir_temp ~ "Soil temperature",                                                    
                   
                   ##Biotic Factors####
                   rv %in% biol_microbes ~ "Microbial biomass",                                                    
                   rv %in% biol_enzymeactivity ~ "Enzyme activity",
                   
                   #####Soil Nutrients####                
                   ###Nitrogen#######
                   ##Nitrate####
                   rv %in% chem_nitrate_spring ~ "Nitrate (NO3)",                
                   rv %in% chem_nitrate_maize ~ "Nitrate (NO3)",                                 
                   rv %in% chem_nitrate_soybean ~ "Nitrate (NO3)",                                 
                   rv %in% chem_nitrate_fall ~ "Nitrate (NO3)",                                 
                   ##Ammonium####                  
                   rv %in% chem_ammonium_spring ~ "Ammonium (NH4)",                                 
                   
                   ##Total Nitrogen####
                   rv %in% chem_totalN ~ "Total nitrogen",                                                    
                   
                   
                   ##Phosphorus#####
                   rv %in% chem_phosphorus ~ "Phosphorus",                                                    
                   
                   ##Potassium####    
                   rv %in% chem_potassium ~ "Potassium",                                                    
                   
                   ####Pests####
                   ## Weeds ####
                   ##Broadleaves####
                   rv %in% weed_Palmeramaranth ~ "Pigweed (#)",
                   rv %in% weed_cocklebur ~ "Cocklebur (#)",
                   rv %in% weed_deadnettle ~ "Dead nettle (#)",                  
                   rv %in% weed_waterhemp ~ "Waterhemp (#)",                                    
                   
                   #Grasses#####
                   rv %in% weed_foxtail ~ "Foxtail (#)",
                   
                   ###Weed community####
                   rv %in% weed_community_density ~ "Weed community (#)",                  
                   rv %in% weed_community_biomass ~ "Weeed community (biomass)",                                    
                   
                   ## Nematodes ####
                   rv %in% invert_pests_SCN ~ "Soybean cyst nematode (#)",                                    
                   ## Invertebrates ####                  
                   rv %in% invert_pests_Aglycines ~ "Soybean aphid (#)",                                    
                   rv %in% invert_pests_seedcornmaggot ~ "Seedcorn maggot (#)",                                    
                   rv %in% invert_pests_cornrootworm ~ "Corn rootworm (#)",                                    
                   
                   ##Crop Damage####
                   rv %in% invert_pests_cornrootworm_damage ~ "Damage caused by corn rootworm",                                                       
                   rv %in% invert_pests_seedcornmaggot_damage ~ "Damage caused by seed corn maggot",                                                                         
                   
                   
                   ##Pest Natural Enemies####
                   rv %in% invert_preds ~ "Invertebrate predator (#)",                                                                         
                   rv %in% invert_preds_soilcomm_abund ~ "Invertebrate soil community (#)", #found in soil                                                                         
                   rv %in% invert_preds_soilcomm_div ~ "Invertebrate soil community (diversity)", #found in soil                                                                         
                   rv %in% invert_preds_vegcomm_abund ~ "Invertebrate foliar community (#)", #found on leaves
                   rv %in% invert_preds_vegcomm_div ~ "Invertebrate foliar community (diversity)", #found on leaves
                   rv %in% invert_preds_activity ~ "Invertebrate predator (activity)", 
                   rv %in% invert_nonpredpest ~ "Invertebrate non-predator & pests (#)",                   
                   
                   ##Pathogens####
                   rv %in% pathogen ~ "Pathogens",                                     
                   
                   
                   ####Crop Yields####
                   
                   ## Grain production####
                   ##Soybean####        
                   rv %in% yields_grainsoy ~ "Soybean",                                     
                   
                   ##Corn####   
                   rv %in% yields_grainmaize ~ "Corn",                                     
                   
                   ##Crop Growth####
                   ##Corn####
                   rv %in% yields_biomass_abvgrd ~ "Corn stalk biomass",                                     
                   rv %in% yields_biomass_blwgrd ~ "Corn root biomass",                                     
                   
                   ##Crop Nitrogen Content ####
                   ##Corn####        
                   rv %in% crop_N_maizestalk ~ "Corn stalk nitrogen content",                                     
                   
                   ##Soybean####   
                   rv %in% crop_N_soybean ~ "Soybean stalk nitrogen content",                                     
                   
                   ##Grain Quality####
                   ##Corn####
                   rv %in% crop_N_maizegrain ~ "Corn nitrogen content",                                     
                   
                   ##Stand Count####
                   ##Corn####    
                   rv %in% standcount_corn ~ "Corn",                                     
                   
                   ##Soybean####
                   rv %in% standcount_soybean ~ "Soybean",                                                       
                   
                   ####Water Quality####
                   
                   ## Flow Quantity####
                   ##Water####  
                   rv %in% water_discharge ~ "Water discharge",                                                       
                   rv %in% water_drainage ~ "Drainage",                                                       
                   
                   ##Nutrient Runoff####
                   ##Nitrate####
                   rv %in% runoff_nitrate ~ "Nitrate" ,
                   ##Phosphorus####
                   rv %in% runoff_phosphorus ~ "Phosphorus"                                                       
                 )) 
                      
                                    #Attach column to Results######
                df <-
                  left_join(df, metric_labels, by = c("review_key", "rv"))
                

               missing <- df[is.na(df$group_level1),] #check to see if all rows have an assigned group_level1
               missing <- df[is.na(df$group_level2),] #check to see if all rows have an assigned group_level2
               missing <- df[is.na(df$group_level3),] #check to see if all rows have an assigned group_level3
               

#remove rows from cover crop termination review finelevel_group = mgmt, termination, none
df <- df %>% filter(
                !(finelevel_group == "mgmt"), !(finelevel_group == "termination"), !(finelevel_group == "none"))

###Apply treatment name groups to cc_group1 & cc_group2 columns#####

levels(df$finelevel_group)
df <- df %>%
  mutate(
    ##Cover crop diversity####  
    cc_group1 = case_when(
      
      #single species####
      finelevel_group %in% "mono"  ~ "Single species",    
      
      #Two species####
      finelevel_group %in% "mix_2"  ~ "Two species",
      
      #Three or more species####
      finelevel_group %in% "mix_3"  ~ "Three or more species",
      
      #      #Mixture comparisons####
      
           finelevel_group %in% "mono_mono"  ~ "Mixture comparisons",
            finelevel_group %in% "mono_mix_2"  ~ "Mixture comparisons",
            finelevel_group %in% "mix_2_mix_2"  ~ "Mixture comparisons",
            finelevel_group %in% "mono_mix_3"  ~ "Mixture comparisons",
            finelevel_group %in% "mix_2_mix_3"  ~ "Mixture comparisons"
    )) %>%
  
  mutate(
    ##Functional diversity####  
    cc_group2 = case_when(
      
      #Non-legume + Legume Mixture####
      trt2_name %in% "forage radish + hairy vetch + winter rye"  ~ "Non-legume + Legume Mixture",    
      trt2_name %in% "winter rye + hairy vetch"  ~ "Non-legume + Legume Mixture",    
      trt2_name %in% "winter rye + Austrian winter pea"  ~ "Non-legume + Legume Mixture",    
      trt2_name %in% "forage radish + hairy vetch"  ~ "Non-legume + Legume Mixture",    
      
      
      #Non-legume Mixture####
      trt2_name %in% "forage radish + buckwheat"  ~ "Non-legume Mixture",    
      trt2_name %in% "forage radish + winter triticale"  ~ "Non-legume Mixture",    
      trt2_name %in% "winter rye + forage radish"  ~ "Non-legume Mixture",                                
      trt2_name %in% "winter rye + oat"  ~ "Non-legume Mixture",    
      
      #Cover crop rotation####
      trt2_name %in% "hairy vetch (corn) / winter rye (soybean)"  ~ "Rotation of Cover Crops",    
      trt2_name %in% "hairy vetch + winter rye (corn) / winter rye (soybean)"  ~ "Rotation of Cover Crops",    
      trt2_name %in% "oat (corn), clover (soybean)"   ~ "Rotation of Cover Crops",    
      trt2_name %in% "slender wheatgrass (corn), winter lentils (soybean)"  ~ "Rotation of Cover Crops",    
      trt2_name %in% "winter rye (corn), hairy vetch (soybean)"  ~ "Rotation of Cover Crops",    
      
      #Non-legumes####
      trt2_name %in% "barley"  ~ "Non-legume",    
      trt2_name %in% "Canada bluegrass"  ~ "Non-legume",    
      trt2_name %in% "canola"  ~ "Non-legume",    
      trt2_name %in% "chickweed"  ~ "Non-legume",    
      trt2_name %in% "downy brome"  ~ "Non-legume",    
      trt2_name %in% "forage radish"  ~ "Non-legume",    
      trt2_name %in% "forage rape"  ~ "Non-legume",    
      trt2_name %in% "Italian ryegrass"  ~ "Non-legume",    
      trt2_name %in% "mustard"  ~ "Non-legume",    
      trt2_name %in% "oat"  ~ "Non-legume",    
      trt2_name %in% "oat, Italian ryegrass, or forage radish"  ~ "Non-legume",    
      trt2_name %in% "perennial ryegrass"  ~ "Non-legume",    
      trt2_name %in% "rapeseed"  ~ "Non-legume",    
      trt2_name %in% "wheatgrass"  ~ "Non-legume",    
      trt2_name %in% "winter rye"  ~ "Non-legume",    
      trt2_name %in% "winter triticale"  ~ "Non-legume",                                          
      trt2_name %in% "winter wheat"  ~ "Non-legume",       
      
      #Legumes####
      trt2_name %in% "alfalfa"  ~ "Legume",    
      trt2_name %in% "Austrian winter pea"  ~ "Legume",
      trt2_name %in% "clover"  ~ "Legume",
      trt2_name %in% "hairy vetch"  ~ "Legume"))


missing <- df %>% filter(is.na(cc_group1))
missing <- df %>% filter(is.na(cc_group2))

######Organize soil sampling depth and year variables########

unique(levels(df$rv_depth))

#####soil depth groupings#####
#These will display such that it is always displying the results from more shallow sampling depths + deepest depth
#Organized by means of sampling depth

depth_0_30 <- c(
  "0-5 cm" ,
  "0-7.6 cm",
  "0-10 cm",
  "0-15 cm",
  "0-20 cm",
  "0-30 cm",
  "10-20 cm",
  "10 cm",
  "15-30 cm",
  "15.2-30.5 cm",
  "20-40 cm",
  "20 cm",
  "20 cm ",
  "5-15 cm",
  "5-20 cm",
  "7.6-15.2 cm",
  "topsoil"
)

depth_0_60 <- c(
  "0-45 cm",
  "0-46 cm",
  "0-50 cm",
  "0-60 cm",
  "15-75 cm",
  "30-60 cm",
  "30.5-61 cm",
  "40-60 cm",
  "60-90 cm"
)


depth_0_100 <- c(
  "0-75 cm",
  "0-80 cm",
  "0-90 cm",
  "0-100 cm",
  "80 cm"
)

depth_0_150 <- c(
  "0-120 cm", 
  "0-150 cm"
)



#####Apply soil depth groupings####

df <- df %>%
  mutate(
    sample_depth = case_when(
      
      rv_depth %in% depth_0_30 ~ "0-30 cm",
      rv_depth %in% depth_0_60 ~ "0-60 cm",
      rv_depth %in% depth_0_100 ~ "0-100 cm",
      rv_depth %in% depth_0_150 ~ "0-150 cm"))



colnames(df)

df2 <- select(df, review, paper_id, duration, rv_year, loc_multi_results,
              group_level1, group_level2, group_level3, rv, rv_depth, sample_depth,
              rv_units, stat_test, stat_type, trt1, trt1_int, trt1_int2, trt1_value,        
              trt2, trt2_int, trt2_int2, trt2_value,
              significance, effect_norm, finelevel_group, trt1_name, trt1_details, trt2_name,
              trt2_details, cc_group1, cc_group2 
              )



####Save Results file with added Group names
write.csv(df2, file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Files for protocol/Covercrop_ResultsGrouped.csv", row.names = FALSE)

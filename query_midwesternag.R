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

library("dplyr", lib.loc = "~/R/win-library/3.5")
library("readxl", lib.loc = "~/R/win-library/3.5")
library("tidyverse", lib.loc = "~/R/win-library/3.5")
library("ggplot2", lib.loc = "~/R/win-library/3.5")
library("stringr", lib.loc = "~/R/win-library/3.5")
library("stringi", lib.loc = "~/R/win-library/3.5")
library("forcats", lib.loc = "~/R/win-library/3.5")
library("plotrix", lib.loc = "~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

############Import dataframes######################################################################

#Publication reference
Ref <-
  read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Reference")

#Experimental design information and location of experiment
ExpD_Loc <-
  read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "ExpD_Location")

#Details about cash crops planted in experiment
CashCrop <-
  read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CashCrop")

#Details about treatments with particular attention to cover crops
Treatment <-
  read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Treatment")

#All cover crop related results f
Results <-
  read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Results")
Results = filter(Results,!(Response_var == "cover crop leaf N content")) #set dataframe to work with - remove cover crop nitrogen input data (incomplete dataset)
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
CC_Soil <- filter(Results, Group_RV == "Soil")
CC_Production <-
  filter(Results, Group_RV == "Crop Production")
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

biol_carbon <-  c(
  "soil organic carbon in topsoil",
  "soil organic carbon (0-75 cm)",
  "water extractable organic carbon (0-5 cm)",
  "water extractable organic carbon (5-20 cm)",
  "total carbon",
  "soil organic carbon (SOC) in fall following cover crop, 0-15cm",
  "soil organic carbon (SOC), spring sample",
  "soil organic carbon 0-30 cm depth (spring)",
  "soil organic carbon",
  "organic carbon (0-15 cm)",
  "soil carbon concentration",
  "soil organic carbon in soybean (0-5 cm soil depth)",                                                               
  "soil organic carbon in maize (0-5 cm soil depth)",                                                                 
  "soil organic carbon in soybean (5-15 cm soil depth)",                                                              
  "soil organic carbon (0-10 cm depth)",                                                                             
  "soil organic carbon (10-20 cm depth)",                                                                             
  "soil organic carbon (20-40 cm depth)",                                                                             
  "soil organic carbon (40-60 cm depth)"                                                                             
)

biol_microbes <- c(
  "microbial biomass",
  "microbial biomass nitrogen (MBN)",
  "enzyme activity in soybean (Hydrolysis of Fluroescein Diacetate)",                                                 
  "enzyme activity in maize (Hydrolysis of Fluroescein Diacetate)"                                                   
)

biol_som <- c(
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



###Apply metric & grouping labels to dataframe####


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
      
      #Physical Properties####
      Response_var %in% phy_erosion ~ "Erosion",
      Response_var %in% phy_compaction ~ "Compaction",
      Response_var %in% phy_pores ~ "Soil Pores",
      Response_var %in% phy_aggregates ~ "Soil Aggregates",
      Response_var %in% phy_bulkdensity ~ "Soil Bulk Density",
      Response_var %in% phy_texture ~ "Soil Texture",
      Response_var %in% phy_watercontent ~ "Soil Water Content",
      
      #Biological Properties####
      Response_var %in% biol_carbon ~ "Soil Carbon",
      Response_var %in% biol_microbes ~ "Microbial Biomass",
      Response_var %in% biol_som ~ "Soil Organic Matter",
      
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
      Response_var %in% invert_pests_cornrootworm ~ "Pest: Corn Rootworm (# individuals)",
      Response_var %in% invert_pests_seedcornmaggot ~ "Pest: Seedcorn Maggot (# individuals)",
      Response_var %in% invert_pests_Aglycines ~ "Pest: Soybean Aphid (# individuals)",
      Response_var %in% invert_pests_SCN ~ "Pest: Soybean Cyst Nematode (# individuals)",  
      
      Response_var %in% invert_pests_seedcornmaggot_damage ~ "Pest: Seedcorn Maggot (Damage to Crop)",
      Response_var %in% invert_pests_cornrootworm_damage ~ "Pest: Corn Rootworm (Damage to Crop)",
      
      Response_var %in% invert_preds ~ "Predators (# individuals)",
      
      Response_var %in% invert_preds_vegcomm_div ~ "Predator Community inhabiting Foliage (Diversity)",
      Response_var %in% invert_preds_vegcomm_abund ~ "Predator Community inhabiting Foliage (# individuals)",
      Response_var %in% invert_preds_soilcomm_div ~ "Predator Community inhabiting Soils (Diversity)",
      Response_var %in% invert_preds_soilcomm_abund ~ "Predator Community inhabiting Soils (# individuals)",
      
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
       
  
  #Create Main Groupings #####
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
      
      #Physical Properties####
      Response_var %in% phy_erosion ~ "Physical",
      Response_var %in% phy_compaction ~ "Physical",
      Response_var %in% phy_pores ~ "Physical",
      Response_var %in% phy_aggregates ~ "Physical",
      Response_var %in% phy_bulkdensity ~ "Physical",
      Response_var %in% phy_texture ~ "Physical",
      Response_var %in% phy_watercontent ~ "Physical",
      
      #Biological Properties####
      Response_var %in% biol_carbon ~ "Biological",
      Response_var %in% biol_microbes ~ "Biological",
      Response_var %in% biol_som ~ "Biological",
      
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


#Attach column to Results
Results <-
  left_join(Results, metric_labels, by = c("Res_key", "Response_var"))

#############################################################################################

missing <- Results[is.na(Results$group_metric),] #check to see if all rows have an assigned group_metric


#File with all results included from Cover Crop review#####
CC_Ref2 <- left_join(Ref, ExpD_Loc)
CC_Cash <- left_join(CC_Ref2, CashCrop)
CC_Trt <- left_join(CC_Cash, Treatment)

CC_all <-
  left_join(CC_Trt, Results)#, by = "Paper_id", "Duration")

# CCall_Cash$Year <- as.numeric(CCall_Cash$Year)

#add surrogate key to Results
CC_all$master_key = rownames(CC_all)

write.csv(CC_all, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv")
write.csv(CC_Ref2, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_RefExp.csv")
write.csv(CashCrop, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_CashCrop.csv")
write.csv(Treatment, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Trt.csv")
write.csv(Results, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Results.csv")





#######################################################################################################################
####Filtering Data Files###############################################################


#import data
covercrops <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
  covercrops_refexp <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_RefExp.csv", header=TRUE, row.names = "X")
    covercrops_cashcrop <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_CashCrop.csv", header=TRUE, row.names = "X")
      covercrops_trtmt <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Trt.csv", header=TRUE, row.names = "X")
        covercrops_results <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Results.csv", header=TRUE, row.names = "X")
    
#set dataframe to work with - only using comparisons to control (0), excluded groups, and means only (remove SEMs) 
df <- filter(covercrops_results,!(Trt_id1 > 0), Group_finelevel != "none", Stat_type == "mean", !is.na(Trt_id1value) & !is.na(Trt_id2value)) 
        
        df <- arrange(df, Paper_id)
        df_results <- arrange(covercrops_results, Paper_id)
        df_refexp <- covercrops_refexp
        df_trtmt <- covercrops_trtmt

        
#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
        #df <- df %>%
         #       mutate(per_change = ((Trt_id2value - Trt_id1value)/Trt_id1value)*100)
        #df$per_change <- as.numeric(df$per_change)
        
        df <- df %>%
              mutate( Trt_id1value_alt = if_else(Trt_id1value == 0, 0.000000001, Trt_id1value)) %>%      
          mutate(per_change = ((Trt_id2value - Trt_id1value_alt)/Trt_id1value_alt)*100)
        df$per_change <- as.numeric(df$per_change)
        
        
        #df$per_change2 <- as.numeric(if_else(df$per_change == 0, 0.000001, df$per_change ))
        df$per_change <- as.numeric(if_else(df$per_change == Inf, 0.000001, df$per_change ))
        df$Paper_id <- as.factor(df$Paper_id)
        df$main_group <- as.factor(df$main_group)
        
####Replace group_finelevel with a more descriptive description
    df <- df %>%
                        mutate(
                          Cover_crop_diversity = case_when(
                            Group_finelevel %in% "mono" ~ "Monoculture", 
                            Group_finelevel %in% "mix_2" ~ "Mixture (2 Spp.)",
                            Group_finelevel %in% "mix_3" ~ "Mixture (3+ Spp.)"
                        ))
   df$Cover_crop_diversity <- as.factor( df$Cover_crop_diversity)
   
   
##Monoculture Groupings ####
   
####Add broad groupings of monocultured cover crops
   # Mismatches for descriptions of rotated monocultures of cover crops
#levels(df$Trt_id2name)
#str_view(df$Trt_id2name, "rye")
   
#nonlegume <- c("rye", "canola", "rapeseed", "radish", "triticale", "wheat", "oat", "mustard", "buckwheat", "barley")
#legume <- c("hairy vetch", "vetch", "red clover", "white clover", "crimson clover", "alfalfa", "Austrian winter peas")

#   df2 <- df %>%
#             mutate(
#                     monocultures = case_when(
#                            Trt_id2name %in% str_match(Trt_id2name, nonlegume & legume) & 
#                                      Cover_crop_diversity %in% "Monoculture" ~ "Rotation of Non-Legume & Legume", 
#                            Trt_id2name %in% str_match(Trt_id2name, "rye") & Cover_crop_diversity %in% "Monoculture" ~ "Winter Rye", 
#                            Trt_id2name %in% str_match(Trt_id2name, legume) &
#                                      Cover_crop_diversity %in% "Monoculture" ~ "Legume", 
#                            Cover_crop_diversity %in% "Monoculture" ~ "Non-Legume", 
#                            TRUE ~ ""
#                        ))
 
#   rm(df2)
   
           
######Review: Cover crop####
  ####Group_RV: Soil####
        df_soil <- df %>%
                    filter (Group_RV == "Soil")
         
        cc_soil_summary <- df_soil %>%
                select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity, per_change) %>%
                group_by(main_group, group_metric, Cover_crop_diversity) %>%
                summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id))
        
 #Explore data distribution
   #look by Response_var
       
qplot(Response_var, per_change, data=df_soil,  colour=Cover_crop_diversity) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

   outliers <- filter(df_soil, per_change > 100)
        
            write.csv(cc_soil_summary, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv")

        
        
  ####Group_RV: Pest Regulation####
        df_pest <- df %>%
                    filter (Group_RV == "Pest Regulation")      
        
       
        cc_pest_summary <- df_pest %>%
                select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity, per_change) %>%
                group_by(main_group, group_metric, Cover_crop_diversity) %>%
                summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id))
        
     
        #Explore data distribution
   #look by Response_var
  cc_pest_summary2 <- cc_pest_summary %>%
                    filter(!is.na(per_change > 1000))
                    
  
qplot(Response_var, per_change, data=df_pest[df_pest$per_change < 1000,],  colour=Cover_crop_diversity) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
      outliers <- filter(df_pest, per_change > 100)
        
         write.csv(cc_pest_summary, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv")

        
        
  ####Group_RV: Crop Production####
        df_yield <- df %>%
                    filter (Group_RV == "Crop Production")      
        
       
        cc_yield_summary <- df_yield %>%
                select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity, per_change) %>%
                group_by(main_group, group_metric, Cover_crop_diversity) %>%
                summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id))
       
        qplot(Response_var, per_change, data=df_yield,  colour=Cover_crop_diversity) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
        
        outliers <- filter(df_yield, per_change > 100)
  
         
        write.csv(cc_yield_summary, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv")
   
                    
  ####Group_RV: Water####
        df_water <- df %>%
                    filter (Group_RV == "Water")      
        
       
        cc_water_summary <- df_water %>%
                select(Paper_id, Group_RV, main_group, group_metric, Group_finelevel, Cover_crop_diversity, per_change) %>%
                group_by(main_group, group_metric, Cover_crop_diversity) %>%
                summarise(mean_per_change = mean(per_change, na.rm = TRUE), sem_per_change = std.error(per_change, na.rm = TRUE), num_papers = n_distinct(Paper_id), num_comparisons =length(Paper_id))
        
        
        qplot(Response_var, per_change, data=df_water,  colour=Cover_crop_diversity) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
        
        outliers <- filter(df_water, per_change > 100)
  
        write.csv(cc_water_summary, file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv")
         
        

   
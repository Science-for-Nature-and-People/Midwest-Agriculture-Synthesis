# Build Conservation Evidence Report
#Cover Crop Review Summary file

library("dplyr", lib.loc="~/R/win-library/3.5")
library("readxl", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("stringr", lib.loc="~/R/win-library/3.5")
library("stringi", lib.loc="~/R/win-library/3.5")

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis")

#Filter Cover Crop Review articles and identify Paper_id #'s that we can then use to filter summary dataframe


df <- read.csv("CC_report_summary.csv", header=TRUE, row.names = "X")
glimpse(df)
colnames (df)

#Descriptors for querying dataset and producing report

#"Paper_id" <- unique identification number
#"Year_result" <- total number of years study was conducted
#"Group_finelevel"<- Cover crop diversity classification (monoculture, mixture 2 spp, mixture 3+ spp) 
#"Loc_multi_results" 
#"Trt_id1" <- Treatment Identification #
#"Trt1_interaction" <- Identification # of interacting treatment (if there was a treatment interaction)
#"Trt1_interaction2"     <- Identification # of interacting treatment (if there was a second treatment interaction)
#"Trt_id1description" <- Description of treatment 1
#"Trt_id2" <- Treatment Identification #
#"Trt2_interaction" <- Identification # of interacting treatment (if there was a treatment interaction)
#"Trt2_interaction2"    <- Identification # of interacting treatment (if there was a second treatment interaction)
#"Trt_id2description" <- Description of treatment 2


#Rename grouping columns so that they are more obvious
  #group_level 1, 2, 3...?
#"Response_var" <- name of reponse variable measured
#"Group_RV"
#"group_metric"
#"main_group"             
#"Group_finelevel"

#Add back metric grouping information!

#"introduction" <- introduction statement for report
#"citation_short" <- abridged citation "last name (year)" for report
#"results_short" <- condensed results statement for report
#"methods"  <- methods statement for report             
#"Reviewers_results_long" <- comprehensive results statement for report

#"citation" <- full citation for report

#filter dataset based on group(s) of interest and then print summary
#Loop through all grouping levels and print summary paragraph
#don't forget citations that go with each section!

df$Group_RV <- as.factor(df$Group_RV)
df$main_group <- as.factor(df$main_group)
df$group_metric <- as.factor(df$group_metric)
df$Group_finelevel <- as.factor(df$Group_finelevel)


df$summary_statement <- str_c(report$introduction,", ",report$citation_short,". ", report$Reviewers_results_short," ",report$methods," ",report$Reviewers_results_long, sep = "")


#######################
#Creating Crop Production datatables for Report######################
Production <- df %>%
          filter(Group_RV %in% "Crop Production") %>%
          droplevels()
levels(Production$main_group) #"Crop Nitrogen Uptake" "Stand Count"          "Yields" 

###############################################################################################################################
      #####Nitrogen Uptake####
      Production.Nuptake <- Production  %>%
        filter(main_group %in% "Crop Nitrogen Uptake") %>%
        droplevels()
      levels(Production.Nuptake$group_metric) # "Grain (Maize)"        "Grain (Soybean)"      "Stalk/Stover (Maize)"
      
      
############################################################################################################################        
            ####Maize Grain yield####
            Production.Nuptake.Maizegrain <- Production.Nuptake  %>%
              filter(group_metric %in% "Grain (Maize)") %>%
              droplevels()
            levels(Production.Nuptake.Maizegrain$Group_finelevel) # "mono"
            
            ####Soybean Grain yield####
            Production.Nuptake.Soybeangrain <- Production.Nuptake  %>%
              filter(group_metric %in% "Grain (Soybean)") %>%
              droplevels()
            levels(Production.Nuptake.Maizegrain$Group_finelevel) # "mono"
            
            
            ####Stalk/Stover (Maize)####
            Production.Nuptake.Stalkstover <- Production.Nuptake  %>%
              filter(group_metric %in% "Stalk/Stover (Maize)") %>%
              droplevels()
            levels(Production.Nuptake.Maizegrain$Group_finelevel) #"mono"
############################################################################################################################            
      ####Stand Count####
            Production.Standcount <- Production  %>%
              filter(main_group %in% "Stand Count") %>%
              droplevels()
            levels(Production.Standcount$group_metric) # Seedling Density
            
############################################################################################################################        
            ####Seedling Density####
      Production.Standcount.Seedlingdensity <- Production.Standcount  %>%
        filter(group_metric %in% "Seedling Density") %>%
        droplevels()
      levels(Production.Standcount.Seedlingdensity$Group_finelevel) # "mix_2" "mono"

#############################################################################################################################
      
      ####Crop Yields####
      Production.Yields <- Production  %>%
        filter(main_group %in% "Yields") %>%
        droplevels()
      levels(Production.Yields$group_metric) # "Grain (Maize)"   "Grain (Soybean)" "Root biomass"    "Stover Biomass" 

####################################################################################################################
      ####Grain (Maize)####
      Production.Yields.Maizegrain <- Production.Yields  %>%
        filter(group_metric %in% "Grain (Maize)") %>%
        droplevels()
      levels(Production.Yields.Maizegrain$Group_finelevel) # "mix_2" "mix_3" "mono" 
      
      ####Grain (Soybean)####
      Production.Yields.Soybeangrain <- Production.Yields  %>%
        filter(group_metric %in% "Grain (Soybean)") %>%
        droplevels()
      levels(Production.Yields.Soybeangrain$Group_finelevel) # "mix_2" "mix_3" "mono" 
      

      ####Root Biomass####
      Production.Yields.Rootbiomass <- Production.Yields  %>%
        filter(group_metric %in% "Root biomass") %>%
        droplevels()
      levels(Production.Yields.Rootbiomass$Group_finelevel) # "mono" 
      
      
      ####Stover Biomass####
      Production.Yields.Stoverbiomass <- Production.Yields  %>%
        filter(group_metric %in% "Stover Biomass") %>%
        droplevels()
      levels(Production.Yields.Stoverbiomass$Group_finelevel) # "mix_2" "mono"  
      


########################################################################################################################
#####Pest Regulation####      

Pests <- df %>%
        filter(Group_RV %in% "Pest Regulation") %>%
        droplevels()
levels(Pests$main_group) #"Invertebrates" "Pathogens"     "Weeds" 
##########################################################################################################################
      #####Invertebrates##################
      Pests.Invertebrates <- df %>%
        filter(main_group %in% "Invertebrates") %>%
        droplevels()
      levels(Pests.Invertebrates$group_metric) #"Corn Rootworm (#)"   "Corn Rootworm (Damage to Crop)"                 
              # "Non-predators & Non-pests"                       "Predator Activity"                              
              # "Predator community inhabiting foliage (#)"       "Predator community inhabiting soils (#)"        
              # "Predator community inhabiting soils (Diversity)" "Predators (#)"                                  
              # "Seedcorn Maggot (#)"                             "Seedcorn Maggot (Damage to Crop)"               
              # "Soybean Aphid (#)"                               "Soybean Cyst Nematode (#)"  
      
      group_by(Group_finelevel)
      
      #####Pathogens#######################
      Pests.Pathogens <- df %>%
        filter(main_group %in% "Pathogens") %>%
        droplevels()
      levels(Pests.Pathogens$group_metric) # "Pathogens"
      
      
      #####Weeds###########################
      Pests.Weeds <- df %>%
        filter(main_group %in% "Weeds") %>%
        droplevels()
      levels(Pests.Weeds$group_metric) #  "Aboveground growth of weed community" "Cocklebur"   "Deadnettle"                          
      # "Giant Foxtail"  "Pigweed" "Waterhemp"   "Weed community (abundance of weeds)"                        
       







############################################################################################################################

      
#####Soils#####      
Soils <- df %>%
  filter(Group_RV %in% "Soil") %>%
  droplevels()
levels(Soils$main_group) #"Biological"    "Chemical"      "Environmental" "Physical"  
#############################################################################################################################

      #Biological
      Soils.Biological <- Soils %>%
        filter(main_group %in% "Biological") %>%
        droplevels()
      levels(Soils.Biological$group_metric) #"Microbial Biomass" 

      
#############################################################################################################################

      #Chemical
      Soils.Chemical <- Soils %>%
        filter(main_group %in% "Chemical") %>%
        droplevels()
      levels(Soils.Chemical$group_metric) #"Ammonium (Preplant)"         "Nitrate (Maize)"             "Nitrate (Post Harvest)"      "Nitrate (Preplant)"         
      # "Nitrate (Soybean)"           "pH"                          "Phosphorous"                 "Postassium"                 
      # "Soil Carbon, 0-20 cm depth"  "Soil Carbon, 0-75 cm depth"  "Soil Carbon, 20-60 cm depth" "Soil Organic Matter"        
      # "Total Nitrogen"
      
      
      
#############################################################################################################################
      #Environmental
      Soils.Environmental <- Soils %>%
        filter(main_group %in% "Environmental") %>%
        droplevels()
      levels(Soils.Environmental$group_metric) #"Carbon Dioxide Emissions" "Nitrous Oxide Emissions"  "Soil Temperature"
    
      
#############################################################################################################################      
      #Physical
      Soils.Physical <- Soils %>%
        filter(main_group %in% "Physical") %>%
        droplevels()
      levels(Soils.Physical$group_metric) #"Compaction"         "Erosion"            "Soil Aggregates"    "Soil Bulk Density"  "Soil Pores"         "Soil Texture"      
      # "Soil Water Content"
      
      df$Group_RV <- as.factor(df$Group_RV)
      df$main_group <- as.factor(df$main_group)
      df$group_metric <- as.factor(df$group_metric)
      df$Group_finelevel <- as.factor(df$Group_finelevel)
      
      

Water <- df %>%
  filter(Group_RV %in% "Water") %>%
  droplevels()
levels(Water$main_group) #"Drainage" "Physical" "Runoff" 

                 
                 , df$main_group, df$group_metric, df$Group_finelevel)
groups(report$summary_statement)


levels(df$Group_RV)
 #"Crop Production" "Pest Regulation" "Soil"            "Water" 

levels(df$main_group)
levels(df$group_metric)


levels(df$Group_finelevel)
#"mix_2" "mix_3" "mono" 


group1 <- factor(c("Crop Production", "Pest Regulation", "Soil","Water"))













#filter data based on all possible combinations and print results

#print(summary statement and citationz)        

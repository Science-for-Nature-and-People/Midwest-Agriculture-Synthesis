#Midwestern Ag Synthesis - SNAPP
#Database Querying 2
#March 2, 2018

#libraries

library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(forcats)

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

#import dataframes
CC.div.Ref <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Reference")
     
CC.div.ExpD_Loc <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "ExpD_Location")

CC.div.CashCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CashCrop")

CC.div.CoverCrop <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "CoverCrop")

CC.div.Results <- read_excel("CCdiversity_Synthesis_Database_Atwood.xlsx", sheet = "Results")

#check keys
CC.div.Ref %>% 
  count(DOI) %>% 
  filter(n > 1) #zero duplicates

CC.div.ExpD_Loc %>% 
  count(Paper_id, Loc_multi) %>% 
  filter(n > 1) #zero duplicates

CC.div.CashCrop %>% 
  count(Paper_id, Duration) %>% 
  filter(n > 1) #zero duplicates

CC.div.CoverCrop %>% 
  count(Paper_id, Duration, Year, Trt_id) %>% 
  filter(n > 1) #zero duplicates

CC.div.Results %>% 
  count(Paper_id, Response_var, Trt_id1, Trt_id2, Trt1_interaction, Trt2_interaction) %>% 
  filter(n > 1) #there isn't a unique key for this tibble

#add surrogate key to tibble
CC.div.Results$key = rownames(CC.div.Results)

#use anti_join to check for mismatched primary-foreign keys between tibbles
CC.div.Ref %>%
  anti_join(CC.div.ExpD_Loc, by = "DOI") %>%
  count(DOI, sort = TRUE)

#inspect unique key
CC.div.Results %>% 
  count(key) %>% 
  filter(n > 1) #zero duplicates

#set key
#key = "DOI" # not necessary, if you keep by = NULL then R automatically joins by columns with same name
            #caution if column names are same but mean different things

  #Group: Crop Production
    #Ref.Loc <- left_join(CC.div.Ref, CC.div.ExpD_Loc, by = key)
    Ref.Loc <- left_join(CC.div.Ref, CC.div.ExpD_Loc, by = NULL) #should return same key as above, joining, by = "DOI"
    Ref.Loc.Cash <- left_join(Ref.Loc, CC.div.CashCrop, by = NULL)
    Ref.Loc.Cash.Cover <- left_join(Ref.Loc.Cash, CC.div.CoverCrop, by = NULL)
    Ref.Loc.Cash.Cover.Results <- left_join(Ref.Loc.Cash.Cover, CC.div.Results, by = c("Paper_id", "Duration"))
   
     
    
  #to match column keys with different names use by = c("a" = "b")
    #this will be useful for joining covercrop to results tibble as "Trt_id" = "Trt1_id" or "Trt2_id"
    
    
     
    
#number of observations for each response variable group
    groups_RVs <- Ref.Loc.Cash.Cover.Results  %>%
              count(Group_RV, sort = TRUE)
              groups_RVs
  
    #filter tibble for Group_Rv = Soil
      Soil_RVs <- filter(Ref.Loc.Cash.Cover.Results, Group_RV == "Soil")   
      
    #itemized list of unique soil metrics (Response_var)
      (uniqueSoils <- unique(Soil_RVs$Response_var))
      
    
      # add column ("metric") that combines simliar response_var groups 
      soil_om <-  filter(Soil_RVs, Response_var == c("SOC loss in root zone (0-75 cm)",
                                                          "water-extractable organic carbon (0-5 cm)",
                                                          "soil organic carbon in topsoil",
                                                          "total carbon stock"
                                                          ))
      soil_om <-   mutate(soil_om,
                        metric = "soil_organic_matter")
      
      soil_erosion <-  filter(Soil_RVs, Response_var == c("soil loss", "water aggregate stability"))
      soil_erosion <-   mutate(soil_erosion,
                        metric = "erosion_aggregation")
      
      soil_nutrients <-  filter(Soil_RVs, Response_var == c("soil nitrate (N-NO3)",
                                                    "plant available phosphorous (P)",
                                                    "soil ammonium (N-NH4)"
                                                    ))
      soil_nutrients <-  mutate(soil_nutrients,
                        metric = "soil_nutrients")

      
      soil_ghg <-  filter(Soil_RVs, Response_var == c("N2O emissions when cover crop present",
                                              "denitrification",
                                              "N2O emissions over entire year"
                                              ))
      soil_ghg <-  mutate(soil_ghg,
                        metric = "soil_ghg")
    
            #filter by cover crop diversity level (single or mixture)
             #single species
              ss_soil_om  <-  filter(soil_om, CC_max_diversity == "single")
              ss_soil_erosion <-  filter(soil_erosion, CC_max_diversity == "single")
              ss_soil_nutrients  <-  filter(soil_nutrients, CC_max_diversity == "single")
              ss_soil_ghg  <-  filter(soil_ghg, CC_max_diversity == "single")
              
            #mixture of species
              mix_soil_om  <-  filter(soil_om, CC_max_diversity == "mixture")  
              mix_soil_erosion <-  filter(soil_erosion, CC_max_diversity == "mixture")
              mix_soil_nutrients  <-  filter(soil_nutrients, CC_max_diversity == "mixture")
              mix_soil_ghg  <-  filter(soil_ghg, CC_max_diversity == "mixture")


  
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
  #number of observations for each response variable group
    CC_max_diversity <- Ref.Loc.Cash.Cover.Results  %>%
              count(CC_max_diversity, sort = TRUE)
              CC_max_diversity
              
              
#working with factors- library(forcats)
    Ref.Loc.Cash.Cover.Results  %>%
              count(Group_RV, sort = TRUE)
    
    ggplot(Ref.Loc.Cash.Cover.Results, aes(Group_RV)) +
        geom_bar()
    
#grouping the results
    RV_summary <- Ref.Loc.Cash.Cover.Results %>%
  group_by(Response_var) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()
              
              
#working with strings        

df <- tibble(
  word = words, 
  i = seq_along(word)
)
Ref.Loc.Cash.Cover.Results = data.frame(Ref.Loc.Cash.Cover.Results)
Ref.Loc.Cash.Cover.Results %>% 
  filter(str_detect(Ref.Loc.Cash.Cover.Results$Group_RV, "x$"))
              
              
#to report NA for a string result              
str_replace_na() 
#Objects of length 0 are silently dropped. This is particularly useful in conjunction with if:


#to report synthesis use this setup for paragraphs
  name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
    

    
  #Filter out papers of interest
    #based on Cover crop presence/absence...
    #this will include only papers where "CoverCrop: CC_mixture" = fallow and single species
    #must comb all rows associated with each paper and exclude papers that have CC mixture
           
    
    Ref.Loc.Results %>% 
  semi_join(top_dest)
    
    
    
         
         #count number of unique DOIs for Crop Production
           df %>% 
               group_by (Group_RV) %>%
                  summarise(count = n_distinct(DOI)) %>%
                    arrange(CC.div.Ref.Loc.Results, Group_RV)
           
           df %>% 
             filter(Group_RV=="Crop Production") %>%
                summarize(n_distinct(DOI))
           
           CC.div.Ref.Loc.Results %>%
              count(Group_RV == "Crop Production", DOI)
                
          
           
                    dplyr::distinct(CC.div.Ref.Loc.Results, Group_RV, DOI)
               
            
     Crop.Production <- CC.div.Ref.Loc.Results[CC.div.Ref.Loc.Results$Group_RV == "Crop Production"]
        
#draw map of us with coordinates for each research site
    CC.div.ExpD_Loc %>%
              ggplot(aes(Longitude, Latitude)) + #need to change lat/long info so it readily uploads into ggplot & maps
              borders("state") +
              geom_point() +
              coord_quickmap() 
    
    
    
     
#attempt to use SQL database     
library (odbc)
library(DBI)
library(dbplyr)
library(RSQLite)
     
     
     
#connect to database
con <- dbConnect(odbc::odbc()
                  Driver = "SQL Server"
                  Server = "local host"
                  Database = "CCdiversity_Synthesis_Database_Atwood.xlsx"
                  )




#create empty database with no tables
con = dbConnect(RSQLite::SQLite(), dbname = ":memory:")


#inspect empty database
dbListTables(con)
 

#load data into the database
dbWriteTable(con, "Ref", Ref)
 

#inspect loaded dataframe

dbListFields(con, "Ref")


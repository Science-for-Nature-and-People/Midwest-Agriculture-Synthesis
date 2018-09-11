#####Summarize Results###################################################################
#working with strings

library("dplyr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")
library("stringi", lib.loc="~/R/win-library/3.4")
library("forcats", lib.loc="~/R/win-library/3.4")


#import data
monocultures <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/monocultures_data.csv", header=TRUE, row.names = "X")
mixtures <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/mixtures_data.csv", header=TRUE, row.names = "X")

df = filter(monocultures, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)
#df_mix = filter(mixtures, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)

        
###Synthesis Output################################################################################################################
#Run this code for each metric grouping (from query script)   
          
      #####Citations####################################     
         
      unique((df %>% str_glue_data( "{Authors} ({PubYear}). {Title}. {Journal}, {Volume}: {Pages}.{DOI}")))                  

              #need to add issue # <- " (", df$Issue, ") " ...need workaround for pubs with no Issue number (NA). 
              #When these are included in citation creation entire script goes to "NA"
                  #need to italize Journal name <- preferably italize entire column
              
                            #Possible solutions
                              #issue <- stri_replace_na(df$Issue, replacement = ": ")
                              #print_condition(issue, )
                              #stri_replace_na(df$Issue, replacement = ": ")


  
###Paper-level synthesis##################################################################################################           

        #Prepare df####
                  ###Create City, State lists#####
                    
                    #Make column that combines city, state for each row
                    df$city_state <- paste(df$City, df$State, sep=", ")
                    
                    #Then create lists and rename column
                    city_statelist <- df %>%
                                      group_by(Paper_id) %>% 
                                      summarise (city_state_list = paste(unique(city_state), collapse=", "))
                    
                    
                    #rejoin list to original dataset
                    df <- left_join(df, city_statelist, by="Paper_id")
                        
                        
                              #Create Experimental Treatment lists
                              
                              #Make column that combines all treatments for each study
                              df$Trtmts_all <- paste(df$Trtmt_main, df$Trtmt_splitA, df$Trtmt_splitB, df$Trtmt_splitC, sep=", ")
                                    
                              #need to remove NAs (possibly with if else statement + is.na)
                              #need to add *and* before last treatment
      
                        ######Merge replications (different # of replications per site)
                        ######Merge experimental years (different start and end dates per site)...maybe distinguish differences for each site??
                              
     #######INTRODUCTION Statement #################                        
            (intro_1 <- unique(df %>% 
                        str_glue_data("A {Exp_design}, {Exp_arrangement} study with {Reps} replications was conducted from {Year_start} to {Year_start + Years_num} in {city_state_list} investigating the effects of {Trtmts_all} in a {Cash_species} system for paper # {Paper_id}({Authors}, {PubYear}).")))
                    #need to add *and* before last city, state & treatment combo for each list
                    #need to remove NA from list of treatments...possible create treatment list 
                    #need to remove NA for Experiemtnal arrangements

      ##METHODS Statement
                  
                #Describe cover crop treatments                        
                 
                  # create df where only rows listing cover crops are listed
                  # List cover crops included in experiment with winter fallow last
                  df_ccs <- df %>%
                                filter(!is.na(CC_mixture) ) %>%
                                group_by(Paper_id) %>%
                                #arrange(CC_mixture) %>% #need to alter so that winter fallow is always listed last (?specify with ARRANGE?)
                                summarise (CC_trts_list = paste(unique(CC_mixture), collapse=", "))
                                   
                  
                  #add CC Treatment list to main data frame
                  df <- left_join(df, df_ccs, by="Paper_id")
                  

      #NEXT planting details, seeding rates, soil preparation, terminatin type + description
      
    (methods_1 <- unique(df %>%
                  str_glue_data("Plots recieved one of the following cover crop treatments: {CC_trts_list} for paper # {Paper_id}")))

    
      (results_abbrev <- str_c("They found ", ) )
        #need abridged dataframe for each unique paper to synthesize across
      
              
      
      
  
      
      
      
    
              
  ## Experiments with Cover crop mixtures
       colnames(mix_soil_om)
              
    #create abridged table with pertinent information
    # Directionality of results (+/0/-)
         df.mix_soil_om_abridged <- select(mix_soil_om,
           c(Paper_id, Loc_multi.x,  Plot_width,
            Plot_length, Cash_tillage:Cash_genetics,
            Trt_id: Loc_multi.y, Group_RV:Authors_comments))
         
         
   Soil_type , Annual_precip,
         #Trtmt_levels:Trtmt_splitB_levels, 
        
         
         
         
         
         
         
         
         
         
               
        
    
        
        
        
        
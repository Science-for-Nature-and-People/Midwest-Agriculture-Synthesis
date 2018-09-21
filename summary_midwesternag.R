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

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

#import data
covercrops <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
#monocultures <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/monocultures_data.csv", header=TRUE, row.names = "X")
#mixtures <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/mixtures_data.csv", header=TRUE, row.names = "X")

df <- filter(covercrops, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)
#df <- filter(monocultures, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)
#df <- filter(mixtures, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)
df <- arrange(df, Paper_id)


###Synthesis Output################################################################################################################
#Run this code for each metric grouping (from query script)   
          
      #####Citations####################################     
         
     df$citation <- df %>% 
                    str_glue_data( "{Paper_id} {Authors} ({PubYear}). {Title}. {Journal}, {Volume_issue}: {Pages}.{DOI}")                  
(unique(df$citation))
              #need to add issue # <- " (", df$Issue, ") " ...need workaround for pubs with no Issue number (NA). 
              #When these are included in citation creation entire script goes to "NA"
                  #need to italize Journal name <- preferably italize entire column
              
                            #Possible solutions
                              #issue <- stri_replace_na(df$Issue, replacement = ": ")
                              #print_condition(issue, )
                              #stri_replace_na(df$Issue, replacement = ": ")


  
###Paper-level synthesis##################################################################################################           

        #Prepare df####
                  
                ###Create conditional statement for experimental design and experimental arrangement####
            
                            Experiment_row <- df %>%
                                            select(Exp_design, Exp_arrangement, key, Paper_id) %>%
                                            mutate(
                                            Exp_list_row = case_when(
                                                  !is.na(Exp_arrangement)  ~ paste(Exp_arrangement, Exp_design),
                                                  !is.na(Exp_design)  ~ paste(Exp_design)
                                            )) 
              unique(Experiment_row$Exp_list_row)
                    
                            #Generate list of treatments included in each experiment.
                              Exp_list <- Experiment_row %>%
                              group_by(Paper_id) %>%         
                              summarise (Exp_list = paste(unique(Exp_list_row), collapse=", "))
                    
                              #Inspect Treatment list 
                                   View(Exp_list)
                              
                               #Attach column with list of treatments to dataframe
                                df <- left_join(df, Exp_list)
                              

                ###Create City, State lists#####
                    
                    #Make column that combines city, state for each row
                    df$city_state <- paste(df$City, df$State, sep=", ")
                    
                    #Then create lists and rename column
                    city_statelist <- df %>%
                                      group_by(Paper_id) %>% 
                                      summarise (city_state_list = paste(unique(city_state), collapse=", "))
                    
                    
                    #ATtach list of city, states to dataset
                    df <- left_join(df, city_statelist, by="Paper_id")
                        
                        
                  ###Create Experimental Treatment lists####
                              
                              #Make column that combines all treatments for each study
                              Trtmt_list_row <- df %>%
                                            select(Trtmt_main:Trtmt_splitC, key, Paper_id) %>%
                                            mutate(
                                            Trtmt_list_row = case_when(
                                                  !is.na(Trtmt_splitC)  ~ paste(Trtmt_main, ", ", Trtmt_splitA, ", ", Trtmt_splitB, ", and " , Trtmt_splitC, sep = ""),
                                                  !is.na(Trtmt_splitB)  ~ paste(Trtmt_main, ", ", Trtmt_splitA, ", and ", Trtmt_splitB, sep = ""),
                                                  !is.na(Trtmt_splitA)  ~ paste(Trtmt_main," and ", Trtmt_splitA, sep = ""),
                                                  !is.na(Trtmt_main)  ~ paste(Trtmt_main)
                                            ))  
                              
                 
                              
                              #Generate list of treatments included in each experiment.
                              Trtmt_list <- Trtmt_list_row %>%
                              group_by(Paper_id) %>%         
                              summarise (Trtmt_list = paste(unique(Trtmt_list_row), collapse=", "))
                    
                              #Inspect Treatment list 
                                   View(Trtmt_list)
                              
                               #Attach column with list of treatments to dataframe
                                df <- left_join(df, Trtmt_list)
       
                            
                  ###Consolidate Start & End Years across site locations####            
                    
                    #Set dates for end of experiment
                        df$Year_end <- df$Year_start + df$Years_num            
                     
                      #Concatenate year start and year end
                                df$Years_exp <- paste(df$Year_start, df$Year_end, sep = "-")
                                           
                    years_list <- df %>%
                                      group_by(Paper_id) %>% 
                                      summarise (years_list = paste(unique(Years_exp), collapse=" and "))
                    unique(years_list)
                    
                    #Attach list of years study is conducted to dataset
                    df <- left_join(df, years_list, by="Paper_id")
                              
                    
                    
          #Count number of sites for each experiment####
                    df <- df %>%
                    group_by(Paper_id) %>%
                    mutate(unique_locations = n_distinct(Loc_multi))
                    
                   
                
                Unique_locations_text <- df %>%
                                      select(unique_locations, key, Paper_id) %>%
                                      mutate(
                                      Unique_locs_text_row = case_when(
                                            unique_locations == 1  ~ paste(unique_locations, "site"),
                                            unique_locations != 1  ~ paste(unique_locations, "sites")
                                      ))  
                        
           
                        
                        #Generate list of treatments included in each experiment.
                        Unique_locs_text <- Unique_locations_text %>%
                        group_by(Paper_id) %>%         
                        summarise (locs_text = paste(unique(Unique_locs_text_row), collapse=", "))
              
                        #Inspect Treatment list 
                             View(Unique_locs_text)
                        
                         #Attach column with list of treatments to dataframe
                          df <- left_join(df, Unique_locs_text)
                          colnames(df)
                          
                    
          #Count number of replications for each experiment####
                    reps_list <- df %>%
                                      group_by(Paper_id) %>% 
                                      summarise (reps_list = paste(unique(Reps), collapse=" or "))
                    
                   unique(reps_list)
                    
                     #Attach list of years study is conducted to dataset
                    df <- left_join(df, reps_list, by="Paper_id")                  
                            
                              #need to remove NAs (possibly with if else statement + is.na)
                              #need to add *and* before last treatment
      
                ######Dealing with NAs for Exp_arrangement & Exp_design
            Trtmt_list_row <- df %>%
                                    select(Trtmt_main:Trtmt_splitC, key, Paper_id) %>%
                                    mutate(
                                    Trtmt_list_row = case_when(
                                          !is.na(Trtmt_splitC)  ~ paste(Trtmt_main, ", ", Trtmt_splitA, ", ", Trtmt_splitB, ", and " , Trtmt_splitC, sep = ""),
                                          !is.na(Trtmt_splitB)  ~ paste(Trtmt_main, ", ", Trtmt_splitA, ", and ", Trtmt_splitB, sep = ""),
                                          !is.na(Trtmt_splitA)  ~ paste(Trtmt_main," and ", Trtmt_splitA, sep = ""),
                                          !is.na(Trtmt_main)  ~ paste(Trtmt_main)
                                    )) 
            
             #Generate list of treatments included in each experiment.
                              Trtmt_list <- Trtmt_list_row %>%
                              group_by(Paper_id) %>%         
                              summarise (Trtmt_list = paste(unique(Trtmt_list_row), collapse=", "))
                    
                              #Inspect Treatment list 
                                   View(Trtmt_list)
                              
                               #Attach column with list of treatments to dataframe
                                df <- left_join(df, Trtmt_list)
       
                              
                    
     #######INTRODUCTION Statement #################                        
            df$intro <- df %>% 
                        str_glue_data("{Paper_id} A {Exp_list} study with {reps_list} replications was conducted at {locs_text} from {years_list} in {city_state_list} investigating the effects of {Trtmt_list} in a {Cash_species} system ({Authors_abbrev}, {PubYear}).")
unique(df$intro)  


#####START HERE#############

                    #need to add *and* before last city, state & treatment combo for each list
                    #need to remove NA from list of treatments...possible create treatment list 
                    #need to remove NA for Experiemtnal arrangements

      ##METHODS Statement
                  
                #Describe cover crop treatments                        
                 
                  # create df where only rows listing cover crops var. are listed
                  # List cover crops included in experiment with winter fallow first
                  
                              #create column with cover crop + variety
                              df$CC_cultivar <- if_else(is.na(df$CC_type), paste(df$CC_mixture), paste(df$CC_mixture, df$CC_type, sep = " cv. "), missing = NULL)
                                
                                
                  df_ccs <- df %>%
                                filter(!is.na(CC_mixture) ) %>%
                                group_by(Paper_id) %>%
                                #arrange(CC_mixture) %>% #need to alter so that winter fallow is always listed last (?specify with ARRANGE?)
                                summarise (CC_trts_list = paste(unique(CC_cultivar), collapse=", "))
                                   
                  
                  #add CC Treatment list to main data frame
                  df <- left_join(df, df_ccs, by="Paper_id")
                  

      #NEXT planting details, seeding rates, soil preparation, terminatin type + description
      
    df$methods <- df %>%
                  str_glue_data("{Paper_id} Plots received one of the following cover crop treatments: {CC_trts_list}.")
    #(Cover crops were {CC_plantimplement} {CC_plantdate} at a seeding rate of {CC_seeddensity} and terminated {Termination_timing) with {Termination_type} )

    unique(paste(df$intro, df$methods, sep = " "))
    
    
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
        
         
         
         
         
         
         
         
         
         
               
        
    
        
        
        
        
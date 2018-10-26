#####Summarize Results###################################################################
#working with strings

library("dplyr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")
library("stringi", lib.loc="~/R/win-library/3.4")
library("forcats", lib.loc="~/R/win-library/3.4")

setwd("C:/Users/LWA/Desktop/SNAPP_Wood_2017/LiteratureReview")

#import data
covercrops <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
  covercrops_refexp <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_RefExp.csv", header=TRUE, row.names = "X")
    covercrops_cashcrop <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_CashCrop.csv", header=TRUE, row.names = "X")
      covercrops_trtmt <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Trt.csv", header=TRUE, row.names = "X")
        covercrops_results <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Results.csv", header=TRUE, row.names = "X")
      
        ###NOTE: Use Paper_id list to filter papers for synthesis writing
        
        #monocultures <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/monocultures_data.csv", header=TRUE, row.names = "X")
        #mixtures <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/mixtures_data.csv", header=TRUE, row.names = "X")
        
        df <-
          filter(covercrops,!(Trt_id1 > 0)) #set dataframe to work with - only using comparisons to control (0)
        df_results <-
          filter(covercrops_results,!(Trt_id1 > 0)) #set dataframe to work with - only using comparisons to control (0)
        
        
        #df <- filter(monocultures, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)
        #df <- filter(mixtures, !(Trt_id1>0)) #set dataframe to work with - only using comparisions to control (0)
        df <- arrange(df, Paper_id)
        df_results <- arrange(df_results, Paper_id)
        df_refexp <- covercrops_refexp
        df_trtmt <- covercrops_trtmt
        
        ###Synthesis Output################################################################################################################
        #Run this code for each metric grouping (from query script)
        
        #####Citations####################################
        
        df_refexp$citation <- df_refexp %>%
          str_glue_data(
            "{Paper_id} {Authors} ({PubYear}). {Title}. {Journal}, {Volume_issue}: {Pages}. DOI: {DOI}"
          )
        
        #print list of citations
        noquote(unique(df_refexp$citation))
        
        
        #df$citation <- df %>%
        #                   str_glue_data( "{Paper_id} {Authors} ({PubYear}). {Title}. {Journal}, {Volume_issue}: {Pages}. DOI: {DOI}")
        #noquote(unique(df$citation))
        
        #need to italize Journal name <- preferably italize entire column
        
        
        
        ###Paper-level synthesis##################################################################################################
        
        #Prepare df####
        
        ###Create conditional statement for experimental design and experimental arrangement####
        
        Experiment_row <- df %>%
          select(Exp_design, Exp_arrangement, Res_key, Paper_id) %>%
          mutate(Exp_list_row = case_when(
            !is.na(Exp_arrangement)  ~ paste(Exp_arrangement, Exp_design),!is.na(Exp_design)  ~ paste(Exp_design)
          ))
        unique(Experiment_row$Exp_list_row)
        
        
        Experiment_row <- df_refexp %>%
          select(Exp_design, Exp_arrangement, Paper_id) %>%
          mutate(Exp_list_row = case_when(
            !is.na(Exp_arrangement)  ~ paste(Exp_arrangement, Exp_design),!is.na(Exp_design)  ~ paste(Exp_design)
          ))
        unique(Experiment_row$Exp_list_row)
        
        #Generate list of treatments included in each experiment.
        Exp_list <- Experiment_row %>%
          group_by(Paper_id) %>%
          summarise (Exp_list = paste(unique(Exp_list_row), collapse =
              ", "))
        
        #Inspect Treatment list
        View(Exp_list)
        
        #Attach column with list of treatments to dataframe
        df_refexp <-
          left_join(df_refexp, Exp_list)
        
        
        ###Create City, State lists#####
        
        #Make column that combines city, state for each row
        df_refexp$city_state <-
          if_else(
            !is.na(df_refexp$City),
            paste(df_refexp$City, df_refexp$State, sep = ", "),
            paste(df_refexp$State)
          )
        
        #Then create lists and rename column
        city_statelist <- df_refexp %>%
          group_by(Paper_id) %>%
          summarise (city_state_list = paste(unique(city_state), collapse =
              ", "))
        
        
        #ATtach list of city, states to dataset
        df_refexp <-
          left_join(df_refexp, city_statelist, by = "Paper_id")
        
        
        ###Create Experimental Treatment lists####
        
        #Make column that combines all treatments for each study
        Trtmt_list_row <- df_refexp %>%
          select(Trtmt_main:Trtmt_splitC, Paper_id) %>%
          mutate(Trtmt_list_row = case_when(
            !is.na(Trtmt_splitC)  ~ paste(
              Trtmt_main,
              ", ",
              Trtmt_splitA,
              ", ",
              Trtmt_splitB,
              ", and " ,
              Trtmt_splitC,
              sep = ""
            ),!is.na(Trtmt_splitB)  ~ paste(Trtmt_main, ", ", Trtmt_splitA, ", and ", Trtmt_splitB, sep = ""),!is.na(Trtmt_splitA)  ~ paste(Trtmt_main, " and ", Trtmt_splitA, sep = ""),!is.na(Trtmt_main)  ~ paste(Trtmt_main)
          ))
        
        
        
        #Generate list of treatments included in each experiment.
        Trtmt_list <- Trtmt_list_row %>%
          group_by(Paper_id) %>%
          summarise (Trtmt_list = paste(unique(Trtmt_list_row), collapse =
              ", "))
        
        #Inspect Treatment list
        View(Trtmt_list)
        
        #Attach column with list of treatments to dataframe
        df_refexp <-
          left_join(df_refexp, Trtmt_list)
        
        
        ###Consolidate Start & End Years across site locations####
        
        #Set dates for end of experiment
        df_refexp$Year_end <-
          df_refexp$Year_start + df_refexp$Years_num
        
        #Concatenate year start and year end
        df_refexp$Years_exp <-
          paste(df_refexp$Year_start, df_refexp$Year_end, sep = "-")
        
        years_list <- df_refexp %>%
          group_by(Paper_id) %>%
          summarise (years_list = paste(unique(Years_exp), collapse =
              " and "))
        unique(years_list)
        
        #Attach list of years study is conducted to dataset
        df_refexp <-
          left_join(df_refexp, years_list, by = "Paper_id")
        
        
        
        #Count number of sites for each experiment####
        #conditional statement for "in" (city, state) or "at" (research center)
        df_refexp <- df_refexp %>%
          group_by(Paper_id) %>%
          mutate(unique_locations = n_distinct(Loc_multi))
        
        
        
        Unique_locations_text <- df_refexp %>%
          select(unique_locations, Paper_id) %>%
          mutate(Unique_locs_text_row = case_when(
            unique_locations == 1  ~ paste(unique_locations, "site"),
            unique_locations != 1  ~ paste(unique_locations, "sites")
          ))
        
        
        
        #Generate list of treatments included in each experiment.
        Unique_locs_text <-
          Unique_locations_text %>%
          group_by(Paper_id) %>%
          summarise (locs_text = paste(unique(Unique_locs_text_row), collapse =
              ", "))
        
        #Inspect Treatment list
        View(Unique_locs_text)
        
        #Attach column with list of treatments to dataframe
        df_refexp <-
          left_join(df_refexp, Unique_locs_text)
        colnames(df)
        
        
        #Count number of replications for each experiment####
        reps_list <- df_refexp %>%
          group_by(Paper_id) %>%
          summarise (reps_list = paste(unique(Reps), collapse =
              " or "))
        
        unique(reps_list)
        
        #Attach list of years study is conducted to dataset
        df_refexp <-
          left_join(df_refexp, reps_list, by = "Paper_id")
        
        #need to remove NAs (possibly with if else statement + is.na)
        #need to add *and* before last treatment
        
        ######Dealing with NAs for Exp_arrangement & Exp_design
        Trtmt_list_row <- df_refexp %>%
          select(Trtmt_main:Trtmt_splitC, Paper_id) %>%
          mutate(Trtmt_list_row = case_when(
            !is.na(Trtmt_splitC)  ~ paste(
              Trtmt_main,
              ", ",
              Trtmt_splitA,
              ", ",
              Trtmt_splitB,
              ", and " ,
              Trtmt_splitC,
              sep = ""
            ),!is.na(Trtmt_splitB)  ~ paste(Trtmt_main, ", ", Trtmt_splitA, ", and ", Trtmt_splitB, sep = ""),!is.na(Trtmt_splitA)  ~ paste(Trtmt_main, " and ", Trtmt_splitA, sep = ""),!is.na(Trtmt_main)  ~ paste(Trtmt_main)
          ))
        
        #Generate list of treatments included in each experiment.
        Trtmt_list <- Trtmt_list_row %>%
          group_by(Paper_id) %>%
          summarise (Trtmt_list = paste(unique(Trtmt_list_row), collapse =
              ", "))
        
        #Inspect Treatment list
        View(Trtmt_list)
        
        #Attach column with list of treatments to dataframe
        df_refexp <-
          left_join(df_refexp, Trtmt_list)
        
        #Join RefExp with Cash crops
        df2 <-
          left_join(df_refexp, covercrops_cashcrop)
        
        
        
        #######INTRODUCTION Statement #################
        df2$intro <- df2 %>%
          str_glue_data(
            "{Paper_id} A {Exp_list} study with {reps_list} replications was conducted at {locs_text} from {years_list} in {city_state_list} investigating the effects of {Trtmt_list} in a {Cash_species} system ({Authors_abbrev}, {PubYear})."
          )
        unique(df2$intro)
        
        
        
        
        ######Results#####
        #Continue adding results (short & long) to dataframe.
        
        results_abbrev <-
          str_c("They found", {
            df$Reviewers_results_short
          }, {
            df$Reviewers_results_long
          }, sep = " ")
        unique(results_abbrev)
        #need abridged dataframe for each unique paper to synthesize across
        
        
        
        
        
        
        
        
        
        
        ## Experiments with Cover crop mixtures
        colnames(mix_soil_om)
        
        #create abridged table with pertinent information
        # Directionality of results (+/0/-)
        df.mix_soil_om_abridged <- select(
          mix_soil_om,
          c(
            Paper_id,
            Loc_multi.x,
            Plot_width,
            Plot_length,
            Cash_tillage:Cash_genetics,
            Trt_id:Loc_multi.y,
            Group_RV:Authors_comments
          )
        )
        
        
        Soil_type , Annual_precip,
        #Trtmt_levels:Trtmt_splitB_levels,
        

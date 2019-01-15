  ##METHODS for Cover Crop Review#####
    #working with strings

library("dplyr", lib.loc="~/R/win-library/3.5")
library("readxl", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("stringr", lib.loc="~/R/win-library/3.5")
library("stringi", lib.loc="~/R/win-library/3.5")
library("forcats", lib.loc="~/R/win-library/3.5")

  

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis")

#import data
covercrops <- read.csv("CoverCrop_data.csv", header=TRUE, row.names = "X")
  covercrops_refexp <- read.csv("CoverCrop_RefExp.csv", header=TRUE, row.names = "X")
    covercrops_cashcrop <- read.csv("CoverCrop_CashCrop.csv", header=TRUE, row.names = "X")
      covercrops_trtmt <- read.csv("CoverCrop_Trt.csv", header=TRUE, row.names = "X")
        covercrops_results <- read.csv("CoverCrop_Results.csv", header=TRUE, row.names = "X")
      
                
        #Goal: Describe cover crop treatments #####
        #Merge cover crop treatments and experimental design dataframes
        df_methods <- left_join(covercrops_refexp, covercrops_trtmt)
        
        #Create key for cover crop dataframe
        df_methods$keyccs <- rownames(df_methods)
        
        # create df where only rows listing cover crops are listed
        df_ccs <- df_methods %>%
          filter(!is.na(CC_mixture))  %>%
          select(
            Paper_id,
            Duration,
            keyccs,
            CC_mixture,
            CC_type,
            CC_seeddensity,
            CC_plantimplement,
            CC_plantdate,
            Herbicide_type,
            Herbicide_rate,
            Herbicide_rateunits,
            Termination_type,
            Termination_timing,
            Plot_width,
            Plot_length
          )
        
        
        
        #Generate list of seeding rates with associated cover crop (e.g. rye @ 248 kg/ha, hairy vetch @ 124 kg/ha)
        
        #create column with cover crop + variety
        df_ccs$CC_cultivar <-
          if_else(
            is.na(df_ccs$CC_type),
            paste(df_ccs$CC_mixture),
            paste(df_ccs$CC_mixture, df_ccs$CC_type, sep = " cv. "),
            missing = NULL
          )
        
        #Add control indicator
        df_ccs$CC_cultivar <-
          if_else(
            df_ccs$CC_cultivar == "winter fallow",
            paste(df_ccs$CC_cultivar, "(control)", sep = " "),
            paste(df_ccs$CC_cultivar),
            missing = NULL
          )
        
        #unique(df_ccs$CC_cultivar)
        
        
        #add seeding rates
        df_ccs <- df_ccs   %>%
          #select(Paper_id, master_key, CC_cultivar, CC_seeddensity, CC_mixture) %>%
          #filter(!is.na(CC_mixture)) %>%
          mutate(
            CC_cultivar2 = if_else(
              is.na(CC_seeddensity) | CC_cultivar == "winter fallow (control)",
              paste(CC_cultivar),
              paste(CC_cultivar, "@", CC_seeddensity, sep = " "),
              missing = NULL
            )
          ) %>%
          group_by(Paper_id) %>%
          mutate(Cult_seed_list = case_when(!is.na(CC_cultivar2)  ~  paste(unique(CC_cultivar2), collapse =
              ", ")))
        
        #unique(df_ccs$Cult_seed_list)
        
        
        
        
        ####How were the cover crop seeds planted?######
        #filter df_ccs to exclude winter fallow
        
        df_ccs_nofallow <- df_ccs %>%
          filter(CC_mixture != "winter fallow") %>%
          mutate(CC_plantimplement2 = if_else(
            is.na(as.character(CC_plantimplement)),
            paste("planted", sep = ""),
            as.character(CC_plantimplement)
          ))
        
        
        
        plantlist <- df_ccs_nofallow %>%
          group_by(Paper_id) %>%
          summarise (plantinglist = paste(unique(CC_plantimplement2), collapse =
              " and "))
        # unique(plantlist$plantinglist)
        
        
        #ATtach list of planting implement to dataset
        df_ccs <-
          left_join(df_ccs, plantlist, by = "Paper_id")
        
        
        
        ####When were the cover crops planted?####
        #Plant date: create conditional statment to add on/in/between to each cell based on how the date is recorded
        
        months2 <-
          c("August-September", "September-October", "October-November")
        month2 <-
          c("August", "September", "October", "November")
        
        CC_plantdate <- df_ccs %>%
          select(CC_plantdate, Paper_id, Duration, keyccs) %>%
          mutate(
            meth_CC_plantdate = case_when(
              CC_plantdate %in% months2  ~ paste("between", (CC_plantdate)),
              CC_plantdate %in% "fall"  ~ paste("in the", (CC_plantdate)),
              CC_plantdate %in% month2  ~ paste("in", (CC_plantdate))
              #is.na(CC_plantdate) ~ paste(""),
              #TRUE ~ as.character(CC_plantdate)
            )
          )
        
        #need to consolidate unique planting dates and exclude NAs
        
        plantdates <- CC_plantdate %>%
          group_by(Paper_id) %>%
          #na.omit(meth_CC_plantdate) %>%
          mutate (plantdate2 = paste(unique(na.omit(meth_CC_plantdate)), collapse =
              " and "))
        
        plantdates_df <-
          unique(plantdates$plantdate2)
        
        #plantdates <- CC_plantdate %>%
        #          group_by(Paper_id) %>%
        #         filter(!is.na(meth_CC_plantdate)) %>%
        #        summarise (plantingdates = paste(unique(meth_CC_plantdate), collapse=" and "))
        
        #Attach list of planting implement to dataset
        df_ccs <- left_join(df_ccs, plantdates)
        
        
        
        #df_ccs$meth_CC_plantdate <- replace_na(df_ccs$meth_CC_plantdate, paste(""))
        
        #######df_ccs <- left_join(df_ccs_nofall)
        
        #consolidate herbicide type information by Paper_id####
        #exclude rows with NA for herbicide type
        
        Herbicide_list <- df_ccs_nofallow %>%
          select(Herbicide_type, keyccs, Paper_id) %>%
          na.omit(Herbicide_type)
        
        
  
  #unique(Herbicide_list$Herb_list)
  
  df_ccs <-
    left_join(df_ccs, Herbicide_list)
  
  
  
  ###How were the cover crops terminated?####
  
  CC_termination <- df_ccs %>%
    select(Paper_id, Duration, keyccs, Termination_type) %>%
    na.omit(Termination_type) %>%
    group_by(Paper_id) %>%
    mutate(CC_terminationlist = case_when(!is.na(Termination_type) ~ paste(unique(Termination_type), collapse =
        " and ")))
  
  
  unique(CC_termination$CC_terminationlist)
  
  df_ccs <-
    left_join(df_ccs, CC_termination)
  
  
  ##Describe plot sizes for each experiment####
  
  df_ccs_nofallow$plot_size_m2 = as.numeric(df_ccs_nofallow$Plot_width) * as.numeric(df_ccs_nofallow$Plot_width)
  df_ccs_nofallow$plot_size = ifelse(
    !is.na(df_ccs_nofallow$plot_size_m2),
    paste(df_ccs_nofallow$plot_size_m2, "sq m", sep = " "),
    paste("an undescribed size")
  )
  
  
  plotsizem2_list <- df_ccs_nofallow %>%
    group_by(Paper_id) %>%
    summarise (plots_list = paste(unique(plot_size), collapse =
        " or "))
  
  noquote(unique(plotsizem2_list))
  
  #Attach list of plot sizes to dataset
  df_ccs <- left_join(df_ccs, plotsizem2_list)
  
  
  #######Methods Statement####
  
  
  df_ccs2 <- df_ccs %>%
    group_by(Paper_id) %>%
    mutate(
      methods1 = paste(
        Paper_id,
        " Plots received one of the following cover crop treatments: ",
        Cult_seed_list,
        ".",
        sep = ""
      )
    ) #Cover crops were ",
  #na.omit(plantinglist), " ", na.omit(plantdate2), " and terminated with ",  na.omit(CC_terminationlist),
  #" Plots were ", plots_list, ".", sep = "")
  
  
  
  df_ccs2 <- df_ccs2 %>%
    group_by(Paper_id) %>%
    mutate(
      methods2 = case_when(
        plantinglist %in% "planted" &
          plantdate2 %in% "" &
          is.na(CC_terminationlist) ~ paste(""),
        plantinglist %in% "planted" &
          plantdate2 %in% ""  ~ paste(
            Paper_id,
            "Cover crops were terminated with ",
            CC_terminationlist,
            ".",
            sep = ""
          ),
        plantinglist %in% "planted" &
          is.na(CC_terminationlist) ~ paste(Paper_id, "Cover crops were planted ", plantdate2, ".", sep = ""),
        is.na(CC_terminationlist) &
          plantdate2 %in% "" ~ paste(Paper_id, "Cover crops were ", plantinglist, ".", sep = ""),
        is.na(CC_terminationlist) ~ paste(
          Paper_id,
          " Cover crops were ",
          plantinglist,
          " ",
          plantdate2,
          ".",
          sep = ""
        ),
        plantinglist %in% "planted" ~ paste(
          Paper_id,
          "Cover crops were planted ",
          plantdate2,
          " and terminated ",
          CC_terminationlist,
          ".",
          sep = ""
        )
      )
    )
  
  df_ccs2$methods3 <-
    paste(df_ccs2$Paper_id, " Plots were ", df_ccs2$plots_list, ".", sep = "")
  
  
  #str_glue_data("{Paper_id} Plots received one of the following cover crop treatments: {Cult_seed_list}.
  # Cover crops were {plantinglist} {plantdate2} ") #and terminated with {Termination_type} on {Termination_timing}.
  #Plots were {plots_list}.")
  
  noquote(unique(df_ccs2$methods1))
  noquote(unique(df_ccs2$methods2))
  noquote(unique(df_ccs2$methods3))
  
  
  #Write complete methods statement by concatenating methods1, methods2, and methods3
  #need to create only one statement per paper ID.
  #remove paper Id from methods 2 & 3 after single statement is constructed.
  
  #Condititional statement that writes single sentence only if all columns are present
  #then creates new dataframe with just one row for each paper ID
  #remove blanks
  #unique rows only
  
  
  #need to consolidate unique planting dates and exclude NAs
  
  df_ccs2 <- df_ccs2 %>%
    group_by(Paper_id) %>%
    #na.omit(meth_CC_plantdate) %>%
    mutate (methods2 = paste(unique(na.omit(methods2)), collapse = ""))
  
                                                                
  
  #Generate single dataframe with Paper Id and complete Methods Statement. 
    #Includes one line for each unique paper.
  method_statements <- df_ccs2 %>%
                    select(Paper_id, methods1, methods2, methods3) %>%
                    group_by(Paper_id) %>%
                    mutate(methods = stri_paste(methods1, methods2, methods3, sep = " ") ) %>%
                    select(Paper_id, methods) %>%
                    summarise(methods = paste(unique(methods), collapse = " "))
  unique(method_statements$methods)
  
  #Export final methods summary statement
  write.csv(method_statements, file = "CC_methods_summary.csv")
  
  
  
  #Extras
  
  #List of Cover Crops without seeding rate information
  #df_ccs2 <- df_ccs %>%
  #             group_by(Paper_id) %>%
  #            #arrange(CC_mixture) %>% #need to alter so that winter fallow is always listed last (?specify with ARRANGE?)
  #           summarise (CC_trts_list = paste(unique(CC_cultivar), collapse=", "))
  #unique(df_ccs2$CC_trts_list)
  
  #add CC Treatment list to main data frame
  #df_ccs <- left_join(df_ccs, df_ccs2, by="Paper_id")
  
 
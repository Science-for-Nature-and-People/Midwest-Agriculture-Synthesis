#Synthesis of Midwestern Agriculture#######################
#Managing Soil Carbon Working Group - SNAPP, NCEAS ###############

library(dplyr)
library(tidyverse)
library(stringr) #string detection/ working with strings
library(plotrix) # for standard error calculations
library(splitstackshape) #expanding results by # of years the data recorded represent


#This file:
#Summarizes all quantitative data to be used for the Shiny App and data display#####

#######################################################################################################################
####Filtering Data Files###############################################################


#import data
setwd(".")
datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 
df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_ResultsGrouped.csv", row.names = NULL)

df$Stat_type <- as.character(df$Stat_type)
#remove rows with no group metric, blank values, and something other than mean values

df <- df %>% filter(Stat_type == "mean", !is.na(Trt_id1value), !is.na(group_level1)) %>%
        select(Paper_id:Review_id, Response_var:group_level3)


#####Tillage Review ######################################################


#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
              select(Review_id, group_level1, group_level2, group_level3, Response_var_units) %>%
              group_by(Review_id, group_level1, group_level2, group_level3) %>%
              mutate(unit_list = case_when(!is.na(group_level1) ~ paste(unique(Response_var_units), collapse = "; "))) %>%
              select(Review_id, group_level1, group_level2, group_level3, unit_list) %>%
              distinct()

write.csv(unit_list, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_Group_units.csv", row.names = FALSE)

######################################################################################

##Standarize ranking of tillage treatments based on Reicoisky 2015##########
# "Conservation tillage is not conservation agriculture", Journal of Soil and Water Conservation
#Sept 2015, doi: 10.2489/jswc.70.5.103A
#See Fig 2 for list and rankings (Conventional tillage - Conservation tillage - No tillage)

#Tillage type (abbreviation)
# 0. Conventional tillage (conventional) # Authors do not explicitly state the type of tillage used
# 1. Moldboard plow (MP)
# 2. Disc plow (disc)
# 3. Deep ripper (deep)
# 4. Subsoil-HD (subsoil)
# 5. Rotary tillage (rotary)
# 6. Chisel plow (CP)
# 6.5 Conservation tillage (conservation) # Authors do not explicitly state the type of tillage used
# 7. Field cultivator (cultivator)
# 7.5 Deep zonal tillage
# 8. Ridge till (RT)
# 9. Subsoil-LD (subsoil_low)
# 10. Vertical tillage (vertical)
# 11. Reduced tillage (reduced)
# 12. Mulch tillage (mulch)
# 13. Stubble mulch (stubble)
# 14. Strip tillage (ST)
# 15. Slot tillage (slot)
# 16. No tillage- LD & HD (NT)


#Add tillage rankings to new columns [ tilltype_1 & tilltype_2]

df <- df %>%
          mutate(tilltype_1 = if_else(str_detect(Group_finelevel, "conventional_"), 0,
                                      if_else(str_detect(Group_finelevel, "MPCP_"), 6, #chisel plow treatment
                      if_else(str_detect(Group_finelevel, "MP_"), 1,
                              if_else(str_detect(Group_finelevel, "MPmini_"), 1,
                              if_else(str_detect(Group_finelevel, "disc_"), 2, 
                                      if_else(str_detect(Group_finelevel, "deep_"), 3, 
                                              if_else(str_detect(Group_finelevel, "deeptill_"), 3, 
                                                      if_else(str_detect(Group_finelevel, "deep90_"), 3,
                                                              if_else(str_detect(Group_finelevel, "deep60_"), 3,
                                                                      if_else(str_detect(Group_finelevel, "deep40_"), 3, 
                                              if_else(str_detect(Group_finelevel, "subsoil_"), 4, 
                                                      if_else(str_detect(Group_finelevel, "rotary_"), 5, 
                      if_else(str_detect(Group_finelevel, "CP_"), 6,
                        if_else(str_detect(Group_finelevel, "CPnew_"), 6,
                                
                              if_else(str_detect(Group_finelevel, "conservation_"), 6.5, 
                                      if_else(str_detect(Group_finelevel, "cultivator_"), 7,
                                              if_else(str_detect(Group_finelevel, "deepzone_"), 7.5,
                                              if_else(str_detect(Group_finelevel, "RT_"), 8, 
                                                      if_else(str_detect(Group_finelevel, "subsoil_low_"), 9, 
                                                              if_else(str_detect(Group_finelevel, "vertical_"), 10, 
                      if_else(str_detect(Group_finelevel, "reduced_"), 11,
                              if_else(str_detect(Group_finelevel, "mulch_"), 12, 
                                      if_else(str_detect(Group_finelevel, "stubble_"), 13, 
                                              if_else(str_detect(Group_finelevel, "ST_"), 14,
                                                      if_else(str_detect(Group_finelevel, "slot_"), 15, 
                                                              if_else(str_detect(Group_finelevel, "NT_"), 16,
                                                                      if_else(str_detect(Group_finelevel, "NTnew_"), 16,
                                             99999999)))))))))))))))))))))))))))) %>%
  mutate(tilltype_2 = if_else(str_detect(Group_finelevel, "_conventional"), 0,
                              if_else(str_detect(Group_finelevel, "_MPCP"), 6, #chisel plow treatment
                              if_else(str_detect(Group_finelevel, "_MP"), 1,
                                      if_else(str_detect(Group_finelevel, "_MPmini"), 1,
                                              if_else(str_detect(Group_finelevel, "_disc"), 2, 
                                                      if_else(str_detect(Group_finelevel, "_deep"), 3, 
          if_else(str_detect(Group_finelevel, "_deeptill"), 3, 
                  if_else(str_detect(Group_finelevel, "_deep90"), 3,
                          if_else(str_detect(Group_finelevel, "_deep60"), 3,
                                  if_else(str_detect(Group_finelevel, "_deep40"), 3, 
                                          if_else(str_detect(Group_finelevel, "_subsoil"), 4, 
                                                  if_else(str_detect(Group_finelevel, "_rotary"), 5, 
                                                          if_else(str_detect(Group_finelevel, "_CP"), 6,
                                                                  
        if_else(str_detect(Group_finelevel, "_CPnew"), 6,
                if_else(str_detect(Group_finelevel, "_conservation"), 6.5, 
                       if_else(str_detect(Group_finelevel, "_cultivator"), 7,
                              if_else(str_detect(Group_finelevel, "_deepzone"),7.5,
                                      if_else(str_detect(Group_finelevel, "_RT"), 8, 
                                              if_else(str_detect(Group_finelevel, "_subsoil_low"), 9, 
                                                      if_else(str_detect(Group_finelevel, "_vertical"), 10, 
                                                              if_else(str_detect(Group_finelevel, "_reduced"), 11,
                  if_else(str_detect(Group_finelevel, "_mulch"), 12, 
                          if_else(str_detect(Group_finelevel, "_stubble"), 13, 
                                  if_else(str_detect(Group_finelevel, "_ST"), 14,
                                          if_else(str_detect(Group_finelevel, "_slot"), 15, 
                                                  if_else(str_detect(Group_finelevel, "_NT"), 16,
                                                          if_else(str_detect(Group_finelevel, "_NTnew"), 16,
                                                                  999999))))))))))))))))))))))))))))


####Use tillage rankings to reorganize comparisons where higher ranking is listed in tilltype_1########################################################################################
df$Trt_id1 <- as.integer(df$Trt_id1)
df$Trt_id2 <- as.integer(df$Trt_id2)
df$Group_finelevel <- as.character(df$Group_finelevel)
df$tilltype_1 <- as.integer(df$tilltype_1)
df$tilltype_2 <- as.integer(df$tilltype_2)
df$Trt1_interaction <- as.integer(df$Trt1_interaction)
df$Trt2_interaction <- as.integer(df$Trt2_interaction)
df$Trt1_interaction2 <- as.integer(df$Trt1_interaction2)
df$Trt2_interaction2 <- as.integer(df$Trt2_interaction2)
df$Sig_level <- as.character(as.factor(df$Sig_level))
df$Trt_id1name <- as.character(as.factor(df$Trt_id1name))
df$Trt_id2name <- as.character(as.factor(df$Trt_id2name))
df$Trt_id1description <- as.character(as.factor(df$Trt_id1description))
df$Trt_id2description <- as.character(as.factor(df$Trt_id2description))


df2 <- df %>%
      mutate(Trt1 = case_when(tilltype_1 < tilltype_2 ~ Trt_id1,
                              tilltype_1 > tilltype_2 ~ Trt_id2,
                              tilltype_1 == tilltype_2 ~ Trt_id1)) %>%
      mutate(Trt1_int = case_when(tilltype_1 < tilltype_2 ~ Trt1_interaction,
                              tilltype_1 > tilltype_2 ~ Trt2_interaction,
                              tilltype_1 == tilltype_2 ~ Trt_id2)) %>%
      mutate(Trt1_int2 = case_when(tilltype_1 < tilltype_2 ~ Trt1_interaction2,
                                tilltype_1 > tilltype_2 ~ Trt2_interaction2,
                                tilltype_1 == tilltype_2 ~ Trt1_interaction2)) %>%
      mutate(Trt1_value = case_when(tilltype_1 < tilltype_2 ~ Trt_id1value,
                               tilltype_1 > tilltype_2 ~ Trt_id2value,
                               tilltype_1 == tilltype_2 ~ Trt_id1value)) %>%
      mutate(Trt2 = case_when(tilltype_1 < tilltype_2 ~ Trt_id2,
                              tilltype_1 > tilltype_2 ~ Trt_id1,
                              tilltype_1 == tilltype_2 ~ Trt_id2)) %>%
      mutate(Trt2_int = case_when(tilltype_1 < tilltype_2 ~ Trt2_interaction,
                                  tilltype_1 > tilltype_2 ~ Trt1_interaction,
                                  tilltype_1 == tilltype_2 ~ Trt2_interaction)) %>%
      mutate(Trt2_int2 = case_when(tilltype_1 < tilltype_2 ~ Trt2_interaction2,
                                   tilltype_1 > tilltype_2 ~ Trt1_interaction2,
                                   tilltype_1 == tilltype_2 ~ Trt2_interaction2)) %>%
      mutate(Trt2_value = case_when(tilltype_1 < tilltype_2 ~ Trt_id2value,
                                    tilltype_1 > tilltype_2 ~ Trt_id1value,
                                    tilltype_1 == tilltype_2 ~ Trt_id2value)) %>%
     mutate(significance = Sig_level) %>%
      
  #dropping Normative effect - having difficulties coercing it into the opposite value based on criteria below
        #mutate(norm_effect = if_else(tilltype_1 < tilltype_2, Effect_norm,
         #                           if_else(Effect_norm == 0, Effect_norm,
          #                           if_else(is.na(Effect_norm), Effect_norm,
           #                                  if_else(Effect_norm == "", Effect_norm,
            #                         if_else(tilltype_1 > tilltype_2 && Effect_norm %in% "1", paste("-1"),
             #                        if_else(tilltype_1 > tilltype_2 && Effect_norm %in% "-1", paste("1"), "ALBERT"))))))) %>% 
      mutate(finelevel_group = if_else(tilltype_1 < tilltype_2, Group_finelevel,
                                    if_else(tilltype_1 > tilltype_2, paste(Group_finelevel, " reverse"), 
                                            if_else(tilltype_1 == tilltype_2, Group_finelevel, "Albert")))) %>%
      mutate(Trt1_name = case_when(tilltype_1 < tilltype_2 ~ Trt_id1name,
                              tilltype_1 > tilltype_2 ~ Trt_id2name,
                              tilltype_1 == tilltype_2 ~ Trt_id1name)) %>%
        mutate(Trt1_description = case_when(tilltype_1 < tilltype_2 ~ Trt_id1description,
                                     tilltype_1 > tilltype_2 ~ Trt_id2description,
                                     tilltype_1 == tilltype_2 ~ Trt_id1description)) %>%
        mutate(Trt2_name = case_when(tilltype_1 < tilltype_2 ~ Trt_id2name,
                                     tilltype_1 > tilltype_2 ~ Trt_id1name,
                                     tilltype_1 == tilltype_2 ~ Trt_id2name)) %>%
        mutate(Trt2_description = case_when(tilltype_1 < tilltype_2 ~ Trt_id2description,
                                            tilltype_1 > tilltype_2 ~ Trt_id1description,
                                            tilltype_1 == tilltype_2 ~ Trt_id2description)) %>%
         mutate(Tillage_1 = case_when(tilltype_1 < tilltype_2 ~ tilltype_1,
                                      tilltype_1 > tilltype_2 ~ tilltype_2,
                                      tilltype_1 == tilltype_2 ~ tilltype_1)) %>%
        mutate(Tillage_2 = case_when(tilltype_1 < tilltype_2 ~ tilltype_2,
                                    tilltype_1 > tilltype_2 ~ tilltype_1,
                                    tilltype_1 == tilltype_2 ~ tilltype_2)) 

df3 <- df2 %>% filter(tilltype_1 > tilltype_2) %>% select(tilltype_1, tilltype_2, Tillage_1, Tillage_2)


#Now drop columns that these new columns replace####
df3 <- df2 %>%
        select(Res_key, Review_id, Paper_id, Duration, Loc_multi_results, Response_var:Stat_type, Trt1:Trt2_description, Tillage_1, Tillage_2, group_level1:group_level3)

write.csv(df3, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_treatmentsorganized.csv", row.names = FALSE)
##################################################################################################################################

###Use this newly created file for the analysis####
df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_treatmentsorganized.csv", row.names = NULL)

        

#####Calculate Percent Change [(Trtmt-Control)/Control] & Actual Difference [Trtment-Control] for each row
df$Trt1_value <- as.numeric(as.character(df$Trt1_value))
df$Trt2_value <- as.numeric(as.character(df$Trt2_value))

#Calculates percent change and change in abundance based on the order of the tillage types listed in the dataframe
#If the lower ranking tillage type is in tilltype_2 then tilltype_2 is used as the control and tilltype_1 is used as the treatment

df <- df %>%
  mutate(per_change = if_else(Trt1_value == 0,  ((Trt2_value - 0)/1)*100, ((Trt2_value - Trt1_value)/Trt1_value)*100)) %>%      
  mutate(actual_diff = (Trt2_value - Trt1_value))
           #if_else((str_detect(main_group,"Invertebrates") & 
            #          (str_detect(Response_var_units, "# | number")) | (str_detect(Response_var_units, "%"))),
             #      (Trt2_value - Trt1_value), NULL))
         
           #if_else((str_detect(main_group,"Weeds") & (str_detect(Response_var_units, "# | number")) | (str_detect(Response_var_units, "%")))
                                    #(Trt2_value - Trt1_value), NULL)))
#Use number change for changes in Invertebrate Pest and Predator populations
levels(df$group_level1)
levels(df$group_level2)
levels(df$group_level3)

#Change types
#df$per_change <- as.numeric(as.character(df$per_change))
#df$abundance_change <- as.numeric(as.character(df$abundance_change))
#df$Paper_id <- as.factor(df$Paper_id)
#df$main_group <- as.factor(df$main_group)

#df <- df %>%
#  filter(tilltype_1 != tilltype_2) %>%
#  group_by(tilltype_1, tilltype_2) %>%
#  mutate(per_change = case_when (tilltype_1 < tilltype_2 & Trt_id1value == 0 ~ ((Trt_id2value - 0)/1)*100, 
#                                 tilltype_1 < tilltype_2 & Trt_id1value != 0 ~ ((Trt_id2value - Trt_id1value)/Trt_id1value)*100,
#                                tilltype_1 > tilltype_2 & Trt_id2value == 0 ~ ((Trt_id1value - 0)/1)*100,
#                                tilltype_1 > tilltype_2 & Trt_id2value != 0 ~ ((Trt_id1value - Trt_id2value)/Trt_id2value)*100,
#                                TRUE ~ NA_real_)) %>%
#   mutate(abundance_change = case_when(tilltype_1 < tilltype_2 & Trt_id1value == 0 & 
#                                str_detect(main_group,"Invertebrates") & ((str_detect(group_metric, "#") |
#                                str_detect(Response_var_units, "# | number"))) | str_detect(Response_var_units, "%") ~  (Trt_id2value - Trt_id1value),
#                                tilltype_1 > tilltype_2 & Trt_id1value == 0 & 
#                                str_detect(main_group,"Invertebrates") & ((str_detect(group_metric, "#") |
#                                str_detect(Response_var_units, "# | number"))) | str_detect(Response_var_units, "%") ~  (Trt_id1value - Trt_id2value),
#                                TRUE ~ NA_real_))
                                                
####################Expand table to display results for all years####

df <- cSplit(df, splitCols = "RV_year", sep = ";", direction = "long") # all data were expanded based on list of years 
levels(as.factor(df$RV_year))

df <- df %>% filter(RV_year > 0) %>% droplevels() #removes rows where data were collected prior to the adoption of the practice


#Change types
#df$per_change <- as.numeric(as.character(df$per_change))
#df$abundance_change <- as.numeric(as.character(df$abundance_change))
df$Paper_id <- as.factor(df$Paper_id)
#df$main_group <- as.factor(df$main_group)

#determine number of years each mean represents & replicate number of rows to match # years the data represent#######

#df <- df %>%
 # mutate(per_change_yr = ifelse(Year_result == 0, sub('.*-', '', Duration), 1))

####For some experiments this is not calculating correctly (see #152, 154) double check all exp with multiple years that don't include all years of the experiment
  

#  replicate rows so that total number of rows for that datapoint is equal to the # years the datapoint represents
#df <- df %>% 
 # uncount(as.numeric(per_change_yr))



######Organize soil sampling depth and year variables########

unique(levels(df$RV_depth))

#####soil depth groupings#####
  #These will display such that it is always displying the results from more shallow sampling depths + deepest depth
  #Organized by means of sampling depth

depth_0_30 <- c(
  "0-10 cm",
  "0-15 cm",
  "0-20 cm",
  "0-5 cm",
  "0-25 cm",
  "0-7.5 cm",
  "0-16 cm",
  "0-2 cm",
  "0-2.5 cm",
  "0-20 cm",
  "0-23 cm",
  "0-24 cm",
  "0-25 cm",
  "0-8 cm",
  "16 cm",
  "2-5 cm",
  "2 cm",
  "2.5-5 cm",
  "2.5 cm",
  "23 cm",
  "5-10 cm",
  "5-15 cm",
  "5-17.5 cm",
  "5-20 cm",
  "5-7.5 cm",
  "7.5-15 cm",
  "8-16 cm",
  "9 cm",
  "surface layer",
  "clod",
  "10-15 cm",
  "10-20 cm",
  "10-25 cm",
  "10 cm",
  "15-20 cm",
  "15-22.5 cm",
  "15-25 cm",
  "16-24 cm",
  "20-25 cm",
  "20-40 cm",
  "30 cm",
  "15-30 cm",
  "10-30 cm",
  "25-30 cm",
  "20-30 cm",
  "0-35 cm",
  "0-40 cm",
  "0-50 cm",
  "0-30.5 cm",
  "0-38 cm",
  "0-40 cm",
  "0-45 cm",
  "0-53 cm",
  "0-30.5 cm",
  "0-30 cm"
  
  )

depth_0_60 <- c(
  "25-50 cm",
  "30-40 cm",
  "30-45 cm",
  "30-50 cm",
  "40-50 cm",
  "15-45 cm",
  "17.5-30 cm",
  "40-60 cm",
  "45-60 cm",
  "30-60 cm",
  "subsoil layer",
  "50-60 cm",
  "50-70 cm",
  "45-75 cm",
  "subsoil and surface layers",
  "0-60 cm",
  "0-70 cm",
  "0-80 cm",
  "0-100 cm",
  "0-120 cm",
  "0-68 cm",
  "0-75 cm",
  "0-90 cm",
  "20-100 cm")
  
  
  depth_0_100 <- c(
  "60-100 cm",
  "50-75 cm",
  "60-75 cm",
  "60-80 cm",
  "60-90 cm",
  "70-90 cm",
  "75-100 cm",
  "75-105 cm",
  "80-100 cm")
  
  depth_0_150 <- c(
  "90-120 cm",
  "120-150 cm",
  "0-300 cm",
  "150 cm")




#####Apply soil depth groupings####

df <- df %>%
  mutate(
    sample_depth = case_when(
     # RV_depth %in% NA_real_  ~ "Soil Surface",
      RV_depth %in% depth_0_30 ~ "0-30 cm",
      RV_depth %in% depth_0_60 ~ "0-60 cm",
      RV_depth %in% depth_0_100 ~ "0-100 cm",
      RV_depth %in% depth_0_150 ~ "0-150 cm"))


#####Sampling year#####
unique(df$RV_year)

year_1_5 <- c(1:5)
year_6_10 <- c(6:10)
year_11_20 <- c(11:20)
year_21_30 <- c(21:30)
year_31_40 <- c(31:40)
year_41_50 <- c(41:50)

#####Apply sampling year groupings####

#Years need to be displayed cumulatively...these groupings don't do that, but the web tool should
df <- df %>%
  mutate(
    sample_year = case_when(
      
      RV_year %in% year_1_5 ~ "Year 1-5",
      RV_year %in% year_6_10 ~ "Years 6-10",
      RV_year %in% year_11_20 ~ "Years 11-20",
      RV_year %in% year_21_30 ~ "Years 21-30",
      RV_year %in% year_31_40 ~ "Years 31-40",
      RV_year %in% year_41_50 ~ "Years 41-50"
    ))

####Treatment Comparisons#####


#There are very few papers for a majority of the tillage practices x outcomes in this database
  #To maximize the amount of data we can display, Tillage practices will be merged based on similar disturbance regimes


df <- df %>%
  mutate(
    Trt_1name = case_when(
      
      #Replace tilltype_1 rankings with names of tillages
      
      #Group 1: Conventional Tillage <- Moldboard plow, Disc plow, Deep ripper, Dubsoil deep, Rotary tillage
      Tillage_1 %in% 1 ~ "Conventional tillage", #Moldboard plow
      Tillage_1 %in% 0 ~ "Conventional tillage", #name given to tillage practice in paper - no further specifications provided
      Tillage_1 %in% 2 ~ "Conventional tillage", #"Disc plow"
      Tillage_1 %in% 3 ~ "Conventional tillage", #"Deep ripper"
      Tillage_1 %in% 4 ~ "Conventional tillage", #"Subsoil deep"
      Tillage_1 %in% 5 ~ "Conventional tillage", #"Rotary tillage"
      
      #Group 2: Conservation tillage <- Chisel plow, Field cultivation, Subsoil shalow, Vertical tillage, Reduced tillage, Mulch Tillage
      Tillage_1 %in% 6 ~ "Conservation tillage", #Chisel plow
      Tillage_1 %in% 6.5 ~ "Conservation tillage", #name given to tillage practice in paper - no further specifications provided
      Tillage_1 %in% 7 ~ "Conservation tillage", #"Field cultivator"
      Tillage_1 %in% 9 ~ "Conservation tillage", #"Subsoil shallow"
      Tillage_1 %in% 10 ~ "Conservation tillage", #"Vertical tillage"
      Tillage_1 %in% 11 ~ "Conservation tillage", #"Reduced tillage"
      Tillage_1 %in% 12 ~ "Conservation tillage", #"Mulch tillage"
      Tillage_1 %in% 13 ~ "Conservation tillage", #"Stubble mulch"
      
      #Group 3: Zonal tillage <- Deep zonal tillage, Ridge tillage, Strip tillage, 
      Tillage_1 %in% 7.5 ~ "Zonal tillage" , #"Deep zonal tillage"
      Tillage_1 %in% 8 ~ "Zonal tillage", #"Ridge till"
      Tillage_1 %in% 14 ~ "Zonal tillage", #"Strip tillage"
      
      #Group 4: No tillage (Slot tillage)
      Tillage_1 %in% 15 ~ "No tillage", #"Slot tillage"
      Tillage_1 %in% 16 ~ "No tillage",
      
      TRUE ~ "Albert"))

df <- df %>%      
  mutate(
    Trt_2name = case_when(
      
      #Replace tilltype_2 rankings with names of tillages
      #Group 1: Conventional Tillage <- Moldboard plow, Disc plow, Deep ripper, Dubsoil deep, Rotary tillage
      Tillage_2 %in% 1 ~ "Conventional tillage", #Moldboard plow
      Tillage_2 %in% 0 ~ "Conventional tillage", #name given to tillage practice in paper - no further specifications provided
      Tillage_2 %in% 2 ~ "Conventional tillage", #"Disc plow"
      Tillage_2 %in% 3 ~ "Conventional tillage", #"Deep ripper"
      Tillage_2 %in% 4 ~ "Conventional tillage", #"Subsoil deep"
      Tillage_2 %in% 5 ~ "Conventional tillage", #"Rotary tillage"
      
      #Group 2: Conservation tillage <- Chisel plow, Field cultivation, Subsoil shalow, Vertical tillage, Reduced tillage, Mulch Tillage
      Tillage_2 %in% 6 ~ "Conservation tillage", #Chisel plow
      Tillage_2 %in% 6.5 ~ "Conservation tillage", #name given to tillage practice in paper - no further specifications provided
      Tillage_2 %in% 7 ~ "Conservation tillage", #"Field cultivator"
      Tillage_2 %in% 9 ~ "Conservation tillage", #"Subsoil shallow"
      Tillage_2 %in% 10 ~ "Conservation tillage", #"Vertical tillage"
      Tillage_2 %in% 11 ~ "Conservation tillage", #"Reduced tillage"
      Tillage_2 %in% 12 ~ "Conservation tillage", #"Mulch tillage"
      Tillage_2 %in% 13 ~ "Conservation tillage", #"Stubble mulch"
      
      #Group 3: Zonal tillage <- Deep zonal tillage, Ridge tillage, Strip tillage, 
      Tillage_2 %in% 7.5 ~ "Zonal tillage" , #"Deep zonal tillage"
      Tillage_2 %in% 8 ~ "Zonal tillage", #"Ridge till"
      Tillage_2 %in% 14 ~ "Zonal tillage", #"Strip tillage"
      
      #Group 4: No tillage (Slot tillage)
      Tillage_2 %in% 15 ~ "No tillage", #"Slot tillage"
      Tillage_2 %in% 16 ~ "No tillage",
      
      TRUE ~ "Albert"))

#Lists treatments compared for each row
df <- df %>%      
  mutate(Trt_compare = str_c(Trt_1name, Trt_2name, sep = " - ")) %>%
  mutate(Review = paste("Tillage"))
      
levels(as.factor(df$Trt_compare))

#[1] "Conservation tillage - Conservation tillage" "Conservation tillage - No tillage"           "Conservation tillage - Zonal tillage"       
#[4] "Conventional tillage - Conservation tillage" "Conventional tillage - Conventional tillage" "Conventional tillage - No tillage"          
#[7] "Conventional tillage - Zonal tillage"        "No tillage - No tillage"                     "Zonal tillage - Conservation tillage"       
#[10] "Zonal tillage - No tillage"  


unique(df$group_level1)

####Group_RV: Other Soil Properties####
df_othersoilprops <- df %>%
  filter (group_level1 == "Other Soil Properties")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_othersoilprops[df_othersoilprops$actual_diff<1000,],  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_othersoilprops,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Crop Yields####
df_cropyields <- df %>%
  filter (group_level1 == "Crop Yields")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_cropyields,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Climate Mitigation####
df_climatemitigation <- df %>%
  filter (group_level1 == "Climate Mitigation") %>% droplevels()
levels(df_climatemitigation$group_level2)
levels(df_climatemitigation$group_level3)
#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_climatemitigation,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Group_RV: Soil Nutrients####
df_soilnutrients <- df %>%
  filter (group_level1 == "Soil Nutrients")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_soilnutrients,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Pests####
df_pests <- df %>%
  filter (group_level1 == "Pests")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_pests,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

####Group_RV: Water Quality####
df_waterquality <- df %>%
  filter (group_level1 == "Water Quality")

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, actual_diff, data=df_waterquality,  colour=group_level2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


####Join Raw  results back into one file ####
all_data <- rbind(df_othersoilprops, 
                  df_cropyields, 
                  df_climatemitigation,
                  df_pests,
                  df_soilnutrients,
                  df_waterquality)

all_data <- rename(all_data, Trt1_details = Trt1_description, Trt2_details = Trt2_description )

all_data <- all_data %>% mutate(trt_specifics = NA) %>% mutate(nutrient_groups = NA)
all_data$Trt1 <- as.factor(all_data$Trt1)
all_data$Trt2 <- as.factor(all_data$Trt2)
all_data$Trt2_int2 <- as.factor(all_data$Trt2_int2)
all_data$nutrient_groups <- as.factor(all_data$nutrient_groups)



write.csv(all_data, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/TillageMgmt_ALL_raw.csv", row.names = FALSE)

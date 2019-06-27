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

#remove rows with no group metric, blank values, and something other than mean values
df <- df[!is.na(df$group_metric) & !is.na(df$Trt_id1value) & df$Stat_type == "mean",]


#####Tillage Review ######################################################


#Build list of units for each grouping variable #####

#for each Review_id, Group_RV, group_metric build column of list with Response_var_units

unit_list <- df %>%
              select(Review_id, Group_RV, group_metric, Response_var_units) %>%
              group_by(Review_id, Group_RV, group_metric) %>%
              mutate(unit_list = case_when(!is.na(group_metric) ~ paste(unique(Response_var_units), collapse = "; "))) %>%
              select(Review_id, Group_RV, group_metric, unit_list) %>%
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
                                             99999999))))))))))))))))))))))))))) %>%
  mutate(tilltype_2 = if_else(str_detect(Group_finelevel, "_conventional"), 0,
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
                                                                  999999)))))))))))))))))))))))))))



####Use tillage rankings to reorganize comparisons where higher ranking is listed in tilltype_1########################################################################################
df$Trt_id1 <- as.integer(df$Trt_id1)
df$Trt_id2 <- as.integer(df$Trt_id2)
df$Group_finelevel <- as.character(df$Group_finelevel)

df2 <- df %>%
      mutate(Trt1 = case_when(tilltype_1 < tilltype_2 ~ Trt_id1,
                              tilltype_1 > tilltype_2 ~ Trt_id2)) %>%
      mutate(Trt1_int = case_when(tilltype_1 < tilltype_2 ~ Trt1_interaction,
                              tilltype_1 > tilltype_2 ~ Trt2_interaction)) %>%
      mutate(Trt1_int2 = case_when(tilltype_1 < tilltype_2 ~ Trt1_interaction2,
                                tilltype_1 > tilltype_2 ~ Trt2_interaction2)) %>%
      mutate(Trt1_int2 = case_when(tilltype_1 < tilltype_2 ~ Trt1_interaction2,
                               tilltype_1 > tilltype_2 ~ Trt2_interaction2)) %>%
      mutate(Trt1_value = case_when(tilltype_1 < tilltype_2 ~ Trt_id1value,
                               tilltype_1 > tilltype_2 ~ Trt_id2value)) %>%
  
      mutate(Trt2 = case_when(tilltype_1 < tilltype_2 ~ Trt_id2,
                              tilltype_1 > tilltype_2 ~ Trt_id1)) %>%
      mutate(Trt2_int = case_when(tilltype_1 < tilltype_2 ~ Trt2_interaction,
                                  tilltype_1 > tilltype_2 ~ Trt1_interaction)) %>%
      mutate(Trt2_int2 = case_when(tilltype_1 < tilltype_2 ~ Trt2_interaction2,
                                   tilltype_1 > tilltype_2 ~ Trt1_interaction2)) %>%
      mutate(Trt2_int2 = case_when(tilltype_1 < tilltype_2 ~ Trt2_interaction2,
                                   tilltype_1 > tilltype_2 ~ Trt1_interaction2)) %>%
      mutate(Trt2_value = case_when(tilltype_1 < tilltype_2 ~ Trt_id2value,
                                    tilltype_1 > tilltype_2 ~ Trt_id1value)) %>%
      mutate(significance = Sig_level) %>%
      
  #dropping Normative effect - having difficulties coercing it into the opposite value based on criteria below
        #mutate(norm_effect = if_else(tilltype_1 < tilltype_2, Effect_norm,
         #                           if_else(Effect_norm == 0, Effect_norm,
          #                           if_else(is.na(Effect_norm), Effect_norm,
           #                                  if_else(Effect_norm == "", Effect_norm,
            #                         if_else(tilltype_1 > tilltype_2 && Effect_norm %in% "1", paste("-1"),
             #                        if_else(tilltype_1 > tilltype_2 && Effect_norm %in% "-1", paste("1"), "ALBERT"))))))) %>% 
      mutate(finelevel_group = if_else(tilltype_1 < tilltype_2, Group_finelevel,
                                    if_else(tilltype_1 > tilltype_2, paste(Group_finelevel, " reverse"), Group_finelevel))) %>%
      mutate(Trt1_name = case_when(tilltype_1 < tilltype_2 ~ Trt_id1name,
                              tilltype_1 > tilltype_2 ~ Trt_id2name)) %>%
        mutate(Trt1_description = case_when(tilltype_1 < tilltype_2 ~ Trt_id1description,
                                     tilltype_1 > tilltype_2 ~ Trt_id2description)) %>%
        mutate(Trt2_name = case_when(tilltype_1 < tilltype_2 ~ Trt_id2name,
                                     tilltype_1 > tilltype_2 ~ Trt_id1name)) %>%
        mutate(Trt2_description = case_when(tilltype_1 < tilltype_2 ~ Trt_id2description,
                                            tilltype_1 > tilltype_2 ~ Trt_id1description)) %>%
         mutate(Tillage_1 = case_when(tilltype_1 < tilltype_2 ~ tilltype_1,
                                      tilltype_1 > tilltype_2 ~ tilltype_2)) %>%
        mutate(Tillage_2 = case_when(tilltype_1 < tilltype_2 ~ tilltype_2,
                                    tilltype_1 > tilltype_2 ~ tilltype_1)) 

df3 <- df2 %>% filter(tilltype_1 > tilltype_2) %>% select(tilltype_1, tilltype_2, Tillage_1, Tillage_2)

#Now drop columns that these new columns replace####
df3 <- df2 %>%
        select(Paper_id:Group_RV, Response_var:Stat_type, Trt1:Trt2_description, Tillage_1, Tillage_2, Res_key:main_group)

write.csv(df3, file = "C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_treatmentsorganized.csv", row.names = FALSE)
##################################################################################################################################

###Use this newly created file for the analysis####
df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Tillage_treatmentsorganized.csv", row.names = NULL)

        

#####Calculate Percent Change [(Trtmt-Control)/Control] for each row
df$Trt1_value <- as.numeric(as.character(df$Trt1_value))
df$Trt2_value <- as.numeric(as.character(df$Trt2_value))

#Calculates percent change and change in abundance based on the order of the tillage types listed in the dataframe
#If the lower ranking tillage type is in tilltype_2 then tilltype_2 is used as the control and tilltype_1 is used as the treatment

df <- df %>%
  mutate(per_change = if_else(Trt1_value == 0,  ((Trt2_value - 0)/1)*100, ((Trt2_value - Trt1_value)/Trt1_value)*100)) %>%      
  mutate(abundance_change = if_else((str_detect(main_group,"Invertebrates") & ((str_detect(group_metric, "#") | str_detect(Response_var_units, "# | number"))) | (str_detect(Response_var_units, "%"))), 
                                    (Trt2_value - Trt1_value), NULL))
#Use number change for changes in Invertebrate Pest and Predator populations
levels(df$group_metric)

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


#Change types
df$per_change <- as.numeric(as.character(df$per_change))
df$abundance_change <- as.numeric(as.character(df$abundance_change))
df$Paper_id <- as.factor(df$Paper_id)
df$main_group <- as.factor(df$main_group)
df$Year_result <- as.numeric(as.character(df$Year_result))

#determine number of years each mean represents & replicate number of rows to match # years the data represent#######

#df <- df %>%
 # mutate(per_change_yr = ifelse(Year_result == 0, sub('.*-', '', Duration), 1))

####For some experiments this is not calculating correctly (see #152, 154) double check all exp with multiple years that don't include all years of the experiment
  

#  replicate rows so that total number of rows for that datapoint is equal to the # years the datapoint represents
#df <- df %>% 
 # uncount(as.numeric(per_change_yr))

####Legends#####

df <- df %>%
  mutate(
    Legend_1 = case_when(
      
      #Replace tilltype_1 rankings with names of tillages
      Tillage_1 %in% 0 ~ "Conventional tillage",
      Tillage_1 %in% 1 ~ "Moldboard plow",
      Tillage_1 %in% 2 ~ "Disc plow",
      Tillage_1 %in% 3 ~ "Deep ripper",
      Tillage_1 %in% 4 ~ "Subsoil deep",
      Tillage_1 %in% 5 ~ "Rotary tillage",
      Tillage_1 %in% 6 ~ "Chisel plow",
      Tillage_1 %in% 6.5 ~ "Conservation tillage",
      Tillage_1 %in% 7 ~ "Field cultivator",
      Tillage_1 %in% 7.5 ~ "Deep zonal tillage",
      Tillage_1 %in% 8 ~ "Ridge till",
      Tillage_1 %in% 9 ~ "Subsoil shallow",
      Tillage_1 %in% 10 ~ "Vertical tillage",
      Tillage_1 %in% 11 ~ "Reduced tillage",
      Tillage_1 %in% 12 ~ "Mulch tillage",
      Tillage_1 %in% 13 ~ "Stubble mulch",
      Tillage_1 %in% 14 ~ "Strip tillage",
      Tillage_1 %in% 15 ~ "Slot tillage",
      Tillage_1 %in% 16 ~ "No tillage",
      TRUE ~ "Albert"))

df <- df %>%      
  mutate(
    Legend_2 = case_when(
      
      #Replace tilltype_2 rankings with names of tillages
      Tillage_2 %in% 0 ~ "Conventional tillage",
      Tillage_2 %in% 1 ~ "Moldboard plow",
      Tillage_2 %in% 2 ~ "Disc plow",
      Tillage_2 %in% 3 ~ "Deep ripper",
      Tillage_2 %in% 4 ~ "Subsoil deep",
      Tillage_2 %in% 5 ~ "Rotary tillage",
      Tillage_2 %in% 6 ~ "Chisel plow",
      Tillage_2 %in% 6.5 ~ "Conservation tillage",
      Tillage_2 %in% 7 ~ "Field cultivator",
      Tillage_2 %in% 7.5 ~ "Deep zonal tillage",
      Tillage_2 %in% 8 ~ "Ridge till",
      Tillage_2 %in% 9 ~ "Subsoil shallow",
      Tillage_2 %in% 10 ~ "Vertical tillage",
      Tillage_2 %in% 11 ~ "Reduced tillage",
      Tillage_2 %in% 12 ~ "Mulch tillage",
      Tillage_2 %in% 13 ~ "Stubble mulch",
      Tillage_2 %in% 14 ~ "Strip tillage",
      Tillage_2 %in% 15 ~ "Slot tillage",
      Tillage_2 %in% 16 ~ "No tillage",
      TRUE ~ "Albert"))


####Group_RV: Soil####
df_soil <- df %>%
  filter (Group_RV == "Soil")
colnames(df_soil)

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_soil,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_soil,  colour=Legend_2) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_soil, per_change > 200 | per_change < -200)
#294 comparisons with > 200 % change....investigate these for accuracy

write.csv(outliers, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/Tillage Review/Soil_outliers.csv", row.names = FALSE)


soil_summary3 <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Group_finelevel, per_change, abundance_change) %>% #Legend_2, Legend_3
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1 ) %>% #Legend_2, Legend_3
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id)) 

soil_summary2 <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

soil_summary1 <- df_soil %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id))%>%
  mutate(Group_RV = "Soil") %>%
  mutate(Review = "Early Season Pest Management") #%>%
mutate(Review_specific = "Timing (Pre/Post- Planting)")




#Calculates Change in Mean Abundance...maybe useful for pests and % values

#soil_summary0 <- df_soil %>%
# select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
#filter(!is.na(abundance_change)) %>%
#group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
#summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
#mutate(Group_RV = "Soil") %>%
#mutate(Review = "Cover Crop")

soil_summary <- left_join(soil_summary1, soil_summary2)
soil_summary <- left_join(soil_summary, soil_summary3)



####Group_RV: Pest Regulation####
df_pest <- df %>%
  filter (Group_RV == "Pest Regulation")      

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_pest[df_pest$per_change < 1000,],  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
#[df_pest$per_change < 1000,]
qplot(Response_var, abundance_change, data=df_pest[df_pest$abundance_change > -1000,],  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
#[df_pest$abundance_change > -1000,]
outliers <- filter(df_pest, per_change > 2000)


pest_summary3 <- df_pest[df_pest$per_change < 1000,] %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id))


pest_summary2 <- df_pest[df_pest$per_change < 1000,] %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

pest_summary1 <- df_pest[df_pest$per_change < 1000,] %>% #[df_pest$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id)) %>%
  mutate(Group_RV = "Pest Regulation") %>%
  mutate(Review = "Early Season Pest Management") #%>%
mutate(Review_specific = "Timing (Pre/Post- Planting)")




#pest_summary0 <- df_pest[df_pest$abundance_change > -1000,] %>% #[df_pest$abundance_change > -1000,]
# select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
#  filter(!is.na(abundance_change)) %>%
#  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
#  summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
#  mutate(Group_RV = "Pest Regulation") %>%
#  mutate(Review = "Cover Crop")

pest_summary <- left_join(pest_summary1, pest_summary2)
pest_summary <- left_join(pest_summary, pest_summary3)

summary(pest_summary$mean_abundance_change)

#Explore data distribution
#look by Response_var
#cc_pest_summary2 <- cc_pest_summary %>%
#                filter(!is.na(per_change > 1000))
test <- df_pest[!is.na(df_pest$abundance_change),]


####Group_RV: Crop Production####
df_yield <- df %>%
  filter (Group_RV == "Crop Production")      

#Explore data distribution
#look by Response_var
qplot(Response_var, per_change, data=df_yield[df_yield$per_change < 1000,],  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_yield,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)


outliers <- filter(df_yield, per_change > 500)



yield_summary3 <- df_yield[df_yield$per_change < 1000,] %>% #
  select(Paper_id, Review_id, main_group, group_metric, Legend_1,  Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id)) 

yield_summary2 <- df_yield[df_yield$per_change < 1000,] %>% #
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

yield_summary1 <- df_yield[df_yield$per_change < 1000,] %>% #[df_yield$per_change < 1000,]
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id))%>%
  mutate(Group_RV = "Crop Production") %>%
  mutate(Review = "Early Season Pest Management") #%>%
mutate(Review_specific = "Timing (Pre/Post- Planting)")



#Difference in Abundance
#yield_summary2 <- df_yield %>%
# select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
#filter(!is.na(abundance_change)) %>%
#group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
#summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
#mutate(Group_RV = "Crop Production") %>%
#mutate(Review = "Cover Crop")

yield_summary <- left_join(yield_summary1, yield_summary2)
yield_summary <- left_join(yield_summary, yield_summary3)



####Group_RV: Water####
df_water <- df %>%
  filter (Group_RV == "Water")      

#Explore data distribution
#look by Response_var

qplot(Response_var, per_change, data=df_water,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)
qplot(Response_var, abundance_change, data=df_water,  colour=Legend_1) + theme_bw(base_size=16) + stat_smooth(aes(group=1), method="lm", se=FALSE)

outliers <- filter(df_water, per_change > 100)

water_summary3 <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
  summarise(mean_per_change3 = mean(per_change, na.rm = TRUE),
            sem_per_change3 = std.error(per_change, na.rm = TRUE),
            num_papers3 = n_distinct(Paper_id), num_comparisons3 =length(Paper_id)) 

water_summary2 <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, per_change, abundance_change) %>%
  group_by(Review_id, main_group, group_metric, Legend_1, Legend_2) %>%
  summarise(mean_per_change2 = mean(per_change, na.rm = TRUE),
            sem_per_change2 = std.error(per_change, na.rm = TRUE),
            num_papers2 = n_distinct(Paper_id), num_comparisons2 =length(Paper_id))

water_summary1 <- df_water %>% 
  select(Paper_id, Review_id, main_group, group_metric, Legend_1, Group_finelevel, per_change, abundance_change) %>%
  #remove rows that will be used for soil summary 2
  group_by(Review_id, main_group, group_metric, Legend_1) %>%
  summarise(mean_per_change1 = mean(per_change, na.rm = TRUE),
            sem_per_change1 = std.error(per_change, na.rm = TRUE),
            num_papers1 = n_distinct(Paper_id), num_comparisons1 =length(Paper_id))%>%
  mutate(Group_RV = "Water") %>%
  mutate(Review = "Fertilizer") %>%
  mutate(Review_specific = "Timing (Pre/Post- Planting)")


#Calculates mean abundance
#water_summary0 <- df_water %>%
# select(Paper_id, Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3, Group_finelevel, abundance_change) %>%
#filter(!is.na(abundance_change)) %>%
#group_by(Review_id, main_group, group_metric, Legend_1, Legend_2, Legend_3) %>%
#summarise(mean_abundance_change = mean(abundance_change, na.rm = TRUE), sem_abundance_change = std.error(abundance_change, na.rm = TRUE), num_papers_abund = n_distinct(Paper_id), num_comparisons_abund =length(Paper_id)) %>%
#mutate(Group_RV = "Water") %>%
#mutate(Review = "Cover Corp")

water_summary <- left_join(water_summary1, water_summary2)
water_summary <- left_join(water_summary, water_summary3)



####Join Summary results back into one file ####
summary_all <- full_join(soil_summary, yield_summary)
summary_all <- full_join(summary_all, pest_summary)
summary_all <- full_join(summary_all, water_summary)    


#Nutrient Review
summary_all_appsplit <- full_join(soil_summary1, yield_summary1)    
summary_all_appvarrate <- full_join(soil_summary1, yield_summary1) 
summary_all_placement_banding <- full_join(soil_summary1, yield_summary1) 
summary_all_placement_subsurface <- full_join(soil_summary1, yield_summary1)
summary_all_timing_fallspring <- full_join(soil_summary1, yield_summary1)
summary_all_timing_fallspring <- full_join(summary_all_timing_fallspring, water_summary1)
summary_all_timing_prepostplant <- full_join(soil_summary1, yield_summary1)
summary_all_timing_prepostplant <- full_join(summary_all_timing_prepostplant, water_summary1)

#merge all above
summary_all <- full_join(summary_all_appsplit, summary_all_appvarrate)
summary_all2 <- full_join(summary_all_placement_banding, summary_all_placement_subsurface)
summary_all3 <- full_join(summary_all_timing_fallspring, summary_all_timing_prepostplant)
summary_all <- full_join(summary_all, summary_all2)
summary_all <- full_join(summary_all, summary_all3)

summary_all$Review2 <- as.factor(paste(summary_all$Review, summary_all$Review_specific, sep = " "))
summary_all$Review_specific <- NULL



write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/NutrientMgmt_FULL_Summary.csv", row.names = FALSE)

write.csv(summary_all, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/PestMgmt_FULL_Summary.csv", row.names = FALSE)

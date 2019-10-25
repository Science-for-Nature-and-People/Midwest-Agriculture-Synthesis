#Synthesis of Midwestern Agriculture#######################

#Libraries
library("readxl", lib.loc = "~/R/win-library/3.5")
library(dplyr)
library(dbplyr)
library(tidyverse)
library(stringr) #string detection/ working with strings
library(plotrix) # for standard error calculations
library(stringr)
library(ggplot2)

#This file:


#import data
df <- read.csv(file = "C:/Users/LWA/Desktop/SNAPP_Wood_2017/Protocols/Files for protocol/Tillage_raw.csv", row.names = NULL)


#Filter based on SOC stocks and specific units of interest
levels(df$group_level3)


units <- c("Mt/ha", "Mg C/ha in depth", "Mg/ha", "kg/sq m", "Cmax, kg/ha", "t/ha", "kg C/ha", "kg/m^2", "g soil/m^2")

rvs <- c("soil organic stock", "total soil organic stock", "soil organic carbon storage after harvest", "soil organic carbon concentration in spring",
         "soil organic carbon storage in the spring", "soil organic carbon", "soil organic carbon stocks", "total soil carbon",
         "soil organic carbon content", "soil organic carbon pool","cumulative stock of soil organic carbon", "total soil organic carbon",
         "soil organic carbon in Mg/ha", "mass of soil organic carbon", "equivalent mass of soil organic carbon", 
         "soil organic carbon concentration on an area basis", "total soil organic carbon pool in soil", "soil organic carbon on area basis", 
         "total carbon (C4--C & C3-C)", "soil organic carbon stock", "total carbon in soil", 
         "soil organic carbon stock", "total soil carbon")

#these rvs will be excluded from the standard units analysis - double check with Shamitha &/or Steve to ensure these are okay for exclusion                                                                               
 exclude <- c("soil organic carbon derived from maize root and shoot biomass", "soil organic carbon derived from maize root biomass",          
             "half-life of C-soil organic carbon", "mineral fraction carbon",  "maximum mineralizable soil carbon (Michaelis-Menten equation)",
             "soil organic carbon pool, C3",  "soil organic carbon pool, C4", "old carbon (C3-C)",                                            
             "macroaggregate protected carbon", "soil carbon stored in light fractions", "soil carbon stored in heavy fractions",
             "eroded soil organic carbon stock (C3-C) in topsoil", "soil organic carbon stock (C3-C) in topsoil",
             "soil carbon stock from maize (C4-C) in topsoil", "soil organic carbon derived from maize root and shoot biomass", "soil organic carbon derived from maize root biomass",
             "maximum mineralizable soil carbon (Michaelis-Menten equation)", "soil organic carbon pool, C3", 
             "soil organic carbon pool, C4", "old carbon (C3-C)", "macroaggregate protected carbon", "soil carbon stored in light fractions", 
             "eroded soil organic carbon stock (C3-C) in topsoil", "soil organic carbon stock (C3-C) in topsoil", "soil carbon stock from maize (C4-C) in topsoil" 
             )               


unique(df_SOC$rv)

#For total SOC stocks standardize the units to Mg C/ha/yr
df_SOC <- df %>% 
          filter(group_level3 == "Soil organic carbon") %>%
          filter(rv %in% rvs) %>%
          filter( !str_detect(rv_year,";")) %>% #removes data sets with averages across multiple years
                filter(rv_units %in% units) %>%
                         droplevels() %>%
          mutate(
            trt1_value_stdunit = case_when(
              rv_units %in% "g soil/m^2" ~ (trt1_value*(1/(1*10^6))*(10000)),
              rv_units %in% "kg/m^2" ~ (trt1_value*(10000)*(1/1000)),
              rv_units %in% "kg/sq m" ~ (trt1_value*(10000)*(1/1000)),
              rv_units %in% "Cmax, kg/ha" ~ (trt1_value*(1/1000)),
              rv_units %in% "kg C/ha" ~ (trt1_value*(1/1000)),
              rv_units %in% "Mg C/ha in depth" ~ trt1_value,
              rv_units %in% "Mg/ha" ~ trt1_value,
              rv_units %in% "Mt/ha" ~ trt1_value,
              rv_units %in% "t/ha" ~ trt1_value
              )
            ) %>%
            mutate(
              trt2_value_stdunit = case_when(
                rv_units %in% "g soil/m^2" ~ (trt2_value*(1/(1*10^6))*(10000)),
                rv_units %in% "kg/m^2" ~ (trt2_value*(10000)*(1/1000)),
                rv_units %in% "kg/sq m" ~ (trt2_value*(10000)*(1/1000)),
                rv_units %in% "Cmax, kg/ha" ~ (trt2_value*(1/1000)),
                rv_units %in% "kg C/ha" ~ (trt2_value*(1/1000)),
                rv_units %in% "Mg C/ha in depth" ~ trt2_value,
                rv_units %in% "Mg/ha" ~ trt2_value,
                rv_units %in% "Mt/ha" ~ trt2_value,
                rv_units %in% "t/ha" ~ trt2_value
              )
            ) 

unique(df_SOC$paper_id)  #24 papers included in the analysis

#write.csv(df_SOC, file = "C:/Users/LWA/Desktop/MGChayr.csv", row.names = FALSE)


#For each paper and trtmt type, calculate difference based on annual SOC differences (SOC_diff)
#group by paper_id, rv, rv_units,rv_year, trt1, trt2
#Sum SOC_diff for each group
df_SOC <- df_SOC %>%
            mutate(SOC_diff = (trt2_value_stdunit - trt1_value_stdunit)) 

#mean and se for conservation tillage and zonal tlilage

df_SOC_conservation <- df_SOC %>% 
                        filter(trt_compare == "Conventional tillage - Conservation tillage" | 
                                 trt_compare == "Conventional tillage - Zonal tillage") %>%
                        group_by(paper_id, loc_multi_results, rv, rv_units, rv_year, trt1,trt1_int, trt1_int2, trt2, trt2_int, trt2_int2) %>%
                        mutate(rv_depth_list = str_c(str_sort(unique(rv_depth)), collapse= ",")) %>% #consolidate sampling depths to single list
                        mutate(SOC_depth = sum(SOC_diff))%>%
                        select(review, paper_id, duration, rv_year, loc_multi_results, group_level1, group_level2, group_level3,
                               rv, trt1, trt1_int, trt1_int2, trt2, trt2_int, trt2_int2, tillage_1, tillage_2, SOC_depth, rv_depth_list)
df_SOC_conservation2 <- distinct(df_SOC_conservation)

mean(df_SOC_conservation2$SOC_depth) #0.2482143
std.error(df_SOC_conservation2$SOC_depth) #3.448203
unique(df_SOC_conservation2$paper_id) #11 papers
                        

#mean and se for no tillage

df_SOC_notill <- df_SOC %>% 
                filter(trt_compare == "Conventional tillage - No tillage") %>%
                group_by(paper_id, loc_multi_results, rv, rv_units, rv_year, trt1,trt1_int, trt1_int2, trt2, trt2_int, trt2_int2) %>%
                mutate(rv_depth_list = str_c(str_sort(unique(rv_depth)), collapse= ",")) %>% #consolidate sampling depths to single list
                mutate(SOC_depth = sum(SOC_diff))%>%
                select(review, paper_id, duration, rv_year, loc_multi_results, group_level1, group_level2, group_level3,
                       rv, trt1, trt1_int, trt1_int2, trt2, trt2_int, trt2_int2, tillage_1, tillage_2, SOC_depth, rv_depth_list)
df_SOC_notill2 <- distinct(df_SOC_notill)

mean(df_SOC_notill2$SOC_depth) #6.142286
std.error(df_SOC_notill2$SOC_depth) #5.073645
unique(df_SOC_notill2$paper_id) #17 papers



df_SOC2 <- df_SOC %>%
            group_by(paper_id, loc_multi_results, rv, rv_units, rv_year, trt1,trt1_int, trt1_int2, trt2, trt2_int, trt2_int2) %>%
            mutate(rv_depth_list = str_c(str_sort(unique(rv_depth)), collapse= ",")) %>% #consolidate sampling depths to single list
            mutate(SOC_depth = sum(SOC_diff)) %>%
            select(review, paper_id, duration, rv_year, loc_multi_results, group_level1, group_level2, group_level3,
                   rv, trt1, trt1_int, trt1_int2, trt2, trt2_int, trt2_int2, tillage_1, tillage_2, SOC_year, rv_depth_list)

df_SOC3 <- distinct(df_SOC2) #remove duplicate rows
df_SOC3$rv_year <- as.integer(df_SOC3$rv_year)

#Calculate yearly difference
df_SOC4 <- df_SOC3 %>% ungroup() %>%
            select(review, paper_id, duration, rv_year, loc_multi_results, group_level1, group_level2, group_level3,
                   rv, trt1, trt1_int, trt1_int2, trt2, trt2_int, trt2_int2, tillage_1, tillage_2, SOC_year, rv_depth_list) %>%
            group_by(paper_id, loc_multi_results, rv, trt1, trt1_int, trt1_int2, trt2, trt2_int, trt2_int2) %>%
            mutate(SOC_lag = dplyr::lead(SOC_year, order_by = rv_year) )

write.csv(df_SOC3, file = "C:/Users/LWA/Desktop/MGChayr.csv", row.names = FALSE)


data %>%
  group_by(groups) %>%
  mutate(lag.value = dplyr::lag(value, n = 1, default = NA))
            
  ##time series lag analysis???
  
  (time_plot <- ggplot(df_SOC3, aes(x = rv_year, y = SOC_year)) +
     geom_point())
#exclude results if averaged over multiple years. cannot determine yearly change with these data.
#use string detect ";" for rv_year. if detectd remove the row.
#recount # of papers included in analysis
#now run time series lag analysis
            

#sum results 

           



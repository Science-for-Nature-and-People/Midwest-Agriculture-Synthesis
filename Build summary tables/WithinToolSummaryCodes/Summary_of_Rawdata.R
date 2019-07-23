

library(dplyr)
library(tidyverse)
library(stringr) #string detection/ working with strings
library(plotrix) # for standard error calculations
library(forcats)

#In App Summary statements for on-the-fly analysis

#datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 
df <- read.csv("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/TillageMgmt_ALL_raw.csv", row.names = NULL)


#calulate mean per change, sem per change, actual change, sem change, num papers, num comparisons, and list of papers for each grouping
results <- df %>% mutate_if(is.factor,  #converts blank cells in factor cols to NAs
                              fct_explicit_na,
                              na_level = "NA") %>%
            select(Paper_id, Review, group_level1, group_level2, group_level3, sample_depth, sample_year, Trt_1name, Trt_2name, Trt_compare, per_change, actual_diff) %>%
            #adjust grouping level to most coarse
            group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year, Trt_compare, Trt_1name, Trt_2name) %>%
            summarise(mean_per_change = mean(per_change, na.rm = TRUE),
                      sem_per_change = std.error(per_change, na.rm = TRUE), #if results in NA then that needs to go to infinity
                      mean_actual_diff = mean(actual_diff, na.rm = TRUE),
                      sem_actual_diff = std.error(actual_diff, na.rm = TRUE), #if results in NA then that needs to go to infinity
                      num_papers = n_distinct(Paper_id),
                      num_comparisons = length(Paper_id),
                      paper_id_list = paste(unique(Paper_id), collapse = ";")) 

#statements to convert SEMs=0 to Inf (maybe unnecessary because filter statement below removes all these)
#SEMS based on 1 value reuslt in NA <- this converts NA to Infinity to variability in the data  
results$sem_per_change <- if_else(is.na(results$sem_per_change), Inf, results$sem_per_change)
results$sem_per_change <- if_else(is.na(results$sem_actual_diff), Inf, results$sem_actual_diff)

#Exclude data if less than 5 unique comparisons (means across multiple years are excluded)

results2 <- results %>% filter(num_comparisons > 4 & sem_per_change != 0 & sem_actual_diff != 0)


#Decision to merge some tillage practices based on similarity in disturbance regime to increase number of papers for each outcome

results_filtered <- str_subset(results$paper_id_list,";")

colnames(results)

#%>%
 # ungroup() %>%
  #select(Review_id, main_group, group_metric, Legend_1, mean_per_change1, sem_per_change1, num_papers1, num_comparisons1, paper_id_list1) %>%
  #mutate(Group_RV = "Crop Production") %>%
  #mutate(Review = "Early Season Pest Management") 
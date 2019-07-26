
#### Load libraries ####
library(readr)
library(tidyverse) # For reading and manipulating data
library(shiny) # for making Shiny app
library(ggplot2) # for plotting data
library(leaflet) # for mapping
library(shinyjs) # for interactive clicking
library(gdata) # reorder legends
library(shinydashboard)
library(here) # helps us keep the files in order (www)
library(RColorBrewer) #to color the plot
library(grid) #to add annotations to the plot (using grobs)
library(cowplot)
library(plotrix)
library(forcats)

# Main data sets
tillage_data <- read_csv(here("www/data/TillageMgmt_ALL_raw.csv"), col_types = cols(Trt2_int = col_integer(),
                                                                                    Trt1_int2 = col_integer(),
                                                                                    Trt2_int2 = col_integer()))


tillage_results <- df %>% mutate_if(is.factor,  #converts blank cells in factor cols to NAs
                            fct_explicit_na,
                            na_level = "NA") %>%
  #the selection list below will need to be responsive to the user inputs
  select(Paper_id, Review, group_level1, group_level2, group_level3, sample_depth, sample_year, Trt_1name, Trt_2name, Trt_compare, per_change, actual_diff) %>%
  #adjust grouping level to most coarse
  group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year, Trt_compare, Trt_1name, Trt_2name) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE), #means of differences of percent change ((trt-control)/control)
            sem_per_change = std.error(per_change, na.rm = TRUE), #if results in NA then that needs to go to infinity
            mean_actual_diff = mean(actual_diff, na.rm = TRUE), #mean of differences in values
            sem_actual_diff = std.error(actual_diff, na.rm = TRUE), #if results in NA then that needs to go to infinity
            num_papers = n_distinct(Paper_id), # Number of papers for each summary
            num_comparisons = length(Paper_id), #Number of comparisons for each summary
            paper_id_list = paste(unique(Paper_id), collapse = ";")) %>% #List of unique papers for each summry
  ungroup %>%
  mutate(group_facet_level32 = paste(group_level3, group_level2, sep = "_"))
            

#statements to convert SEMs=0 to Inf (maybe unnecessary because filter statement below removes all these)
#SEMS based on 1 value reuslt in NA <- this converts NA to Infinity to variability in the data  
tillage_results$sem_per_change <- if_else(is.na(tillage_results$sem_per_change), Inf, tillage_results$sem_per_change)
tillage_results$sem_per_change <- if_else(is.na(tillage_results$sem_actual_diff), Inf, tillage_results$sem_actual_diff)









map.data <- read_csv(here("www","data", "mapping","site-data_with_counties.csv")) %>%
  mutate()
references <- read_csv(here("www", "data", "references-for-app.csv"), col_types = cols(Paper_id = col_integer())) %>%
  #changing strings to be utf-8 encoded
  mutate(citation = iconv(citation, 'latin1', 'UTF-8', sub = ''))

#using built-in state data to add region to map.data
data(state)
state_region_lookup <- data.frame(State = state.abb, Region = state.region) %>%
  mutate(Region = fct_recode(Region, Midwest = 'North Central'))
map.data <- map.data %>% 
  left_join(state_region_lookup, by = 'State')


#turn the citations into html links using the doi
citation_with_hyperlink <- function(citation){
  doi <- citation %>% 
    #get everything after 'DOI: '
    str_extract('(?<=DOI: ).*')
  #turn into html links
  doi_link <- paste0('<a href=http://dx.doi.org/', doi, '>', citation, '</a>')
  return(doi_link)
}

#replace citations with their html links
references$citation <- citation_with_hyperlink(references$citation)




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
raw_data <- read_csv(here("www/data/TillageMgmt_ALL_raw.csv"), col_types = cols(Trt2_int = col_integer(),
                                                                                    Trt1_int2 = col_integer(),
                                                                                    Trt2_int2 = col_integer(),
                                                                                    Trt1_details = col_character(),
                                                                                    Trt2_details = col_character(),
                                                                                    trt_specifics = col_character(),
                                                                                    nutrient_groups = col_character()))


summary_data <- raw_data %>% mutate_if(is.factor,  #converts blank cells in factor cols to NAs
                                               fct_explicit_na,
                                               na_level = "NA") %>%
  #the selection list below will need to be responsive to the user inputs
  select(Paper_id, Review, group_level1, group_level2, group_level3, Trt1_details, Trt2_details, trt_specifics, nutrient_groups, sample_depth, sample_year, Trt_1name, Trt_2name, Trt_compare, per_change, actual_diff) %>%
  #adjust grouping order based on order of selections in webtool. these will vary by review.
  group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year, Trt_compare, Trt_1name, Trt_2name,
           Trt1_details, Trt2_details, trt_specifics, nutrient_groups) %>%
  summarise(mean_per_change = mean(per_change, na.rm = TRUE), #means of differences of percent change ((trt-control)/control)
            sem_per_change = std.error(per_change, na.rm = TRUE), #if results in NA then that needs to go to infinity
            mean_actual_diff = mean(actual_diff, na.rm = TRUE), #mean of differences in values
            sem_actual_diff = std.error(actual_diff, na.rm = TRUE), #if results in NA then that needs to go to infinity
            num_papers = n_distinct(Paper_id), # Number of papers for each summary
            num_comparisons = length(Paper_id), #Number of comparisons for each summary
            paper_id_list = paste(unique(Paper_id), collapse = ";")) %>% #List of unique papers for each summry
  ungroup %>%
  mutate(group_facet_level32 = paste(group_level3, group_level2, sep = "_")) %>%
  filter(num_comparisons > 4 & sem_per_change != 0 & sem_actual_diff != 0)
            

#statements to convert SEMs=0 to Inf (maybe unnecessary because filter statement below removes all these)
#SEMS based on 1 value reuslt in NA <- this converts NA to Infinity to variability in the data  
summary_data$sem_per_change <- ifelse(is.na(summary_data$sem_per_change), Inf, summary_data$sem_per_change)
summary_data$sem_actual_diff <- ifelse(is.na(summary_data$sem_actual_diff), Inf, summary_data$sem_actual_diff)


# raw_data %>% mutate_if(is.factor,  #converts blank cells in factor cols to NAs
#                            fct_explicit_na,
#                            na_level = "NA") %>%
#   #the selection list below will need to be responsive to the user inputs
#   select(Paper_id, Review, group_level1, group_level2, group_level3, Trt1_details, Trt2_details, trt_specifics, nutrient_groups, sample_depth, sample_year, Trt_1name, Trt_2name, Trt_compare, per_change, actual_diff) %>%
#   #adjust grouping order based on order of selections in webtool. these will vary by review.
#   group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year, Trt_compare, Trt_1name, Trt_2name,
#            Trt1_details, Trt2_details, trt_specifics, nutrient_groups) %>%
#   summarise(n = n())








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
  doi_link <- paste0('<a target=_blank href=http://dx.doi.org/', doi, '>', citation, '</a>')
  return(doi_link)
}

#replace citations with their html links
references$citation <- citation_with_hyperlink(references$citation)



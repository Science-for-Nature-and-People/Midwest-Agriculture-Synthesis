
#### Load libraries ####
library(readr)
library(tidyverse) # For reading and manipulating data
library(shiny) # for making Shiny app
library(ggplot2) # for plotting data
library(leaflet) # for mapping
library(shinyjs) # for interactive clicking
library(gdata) # reorder legends
library(shinydashboard)
library(here) # to deal with tehfact we are using a sub directory (www)
library(RColorBrewer) #to color the plot
library(grid) #to add annotations to the plot (using grobs)
library(cowplot)

# Main data sets
#summary_all <- read_csv(here("www", "data", "data-for-app.csv"))
summary_all <- read_csv(here("www", "data", "data-for-app.csv"), col_types = cols(paper_id_list1 = col_character(), 
                                                                                  paper_id_list2 = col_character(), 
                                                                                  paper_id_list3 = col_character()
                                                                                  ))
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



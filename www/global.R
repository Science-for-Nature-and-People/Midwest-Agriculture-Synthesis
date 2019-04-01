library(readr)
library(here)
# Main data sets
#summary_all <- read_csv(here("www", "data", "data-for-app.csv"))
summary_all <- read_csv(here("www", "data", "data-for-app.csv"), col_types = cols(paper_id_list1 = col_character(), paper_id_list2 = col_character(), paper_id_list3 = col_character()))
map.data <- read_csv(here("www","data", "mapping","site-data_with_counties.csv"))
references <- read_csv(here("www", "data", "references-for-app.csv"), col_types = cols(Paper_id = col_integer()))

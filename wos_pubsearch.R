#devtools::install_github("juba/rwos")
library(rwos)
library(tidyverse)
library(stringdist)
library(janitor)

# read in existing references for comparison
handmade_refs <- read_csv('www/data/references-for-app.csv')

handmade_refs_expanded <- read_csv('www/data/refs_all_expanded.csv')


# API requires a session key
session_id <- wos_authenticate()


#' function to write this particular query, given the practice and the times (default is 1980 - 2017))
#' @param  practice_query is the query terms (with control statements) of 
write_full_query <- function(practice_query, year_start = 1980, year_end = 2017){
  #
  geography_terms <- 'Illinois OR Indiana OR Iowa OR Kansas OR Michigan OR Minnesota OR Missouri OR "North Dakota" OR "South Dakota" OR Nebraska OR Ohio OR Wisconsin OR ((Midwest* AND U.S.) OR (Midwest* AND US) OR (Midwest* AND "United States"))'
  cropping_system_terms <- 'corn OR maize OR soybean OR "Zea mays" OR "Glycine max" OR agricultur* OR agro-ecosystem* OR agroecosystem* OR crop OR "field crop*" OR "cropping system" OR farm* OR "conservation agricult*"'
  year_terms <- "1980-2017"
  
  str_glue(
    "(TS = ({geography_terms}) AND TS = ({cropping_system_terms}) AND TS = ({practice_query})) AND PY = ({year_start}-{year_end}) "
  )
}

#' function to create dataframe of results for a query
#' 
#' @param sid is session id created by `rwos::wos_authenticate()`
#' @references write_full_query()
get_results <- function(practice_query, sid, year_start = 1980, year_end = 2017){
  practice_query %>%
    write_full_query(year_start, year_end) %>%      # Write out the query
    wos_search(sid, .) %>%                          # Search the database to get a list of hits
    wos_retrieve_all()                              # Process that list as a dataframe
}


### Write out the practice specific queries (taken from google doc)
cover_crop_query <- '"cover crop" OR cover-crop* OR covercrop*'
tillage_query <- '"conservation till*" OR "conventional till" OR "no-till" OR "no till*" OR "reduced till*" OR "minimum till*"'
pest_query <- '"pesticide seed treatment"* OR "seed treatment*" OR "systemic insect*" OR neonic* OR pyrethr* OR (foliar AND insecticide*)'
nutrient_mgmt_query <- '(precision AND (fert* OR agr* OR nitrogen OR phosphorous)) OR "variable rate" OR "band* fert*" OR 4R OR ((enhance* OR efficien*) AND (nitrogen OR phosphorous))'

test_query <-  '(precision AND (fert* OR agr* OR nitrogen OR phosphorous)) OR "variable rate" OR "band* fert*" OR 4R OR ((enhance* OR efficien*) AND (nitrogen OR phosphorous)) OR fertilizer'
a <- test_query %>% get_results(sid = session_id, year_start = 1980, year_end = 2017)
############################################

### Create a dataframe with all the hits ###

############################################

results_list <- list(cover_crop_query, tillage_query, pest_query, nutrient_mgmt_query) %>%
  map(~get_results(.x, sid = session_id, year_start = 1980, year_end = 2017)) %>%
  set_names(c("cc", "tillage", "pest", "nutrient"))

# Combine the hits to generate a giant dataframe to compare dois
results_df <- results_list %>%
  bind_rows()


##################################################

### Compare results with what was done by hand ###

###################################################

# Get DOIs from references_for_app.csv
handmade_dois <- handmade_refs$citation %>% 
  #get everything after 'DOI: '
  str_extract('(?<=DOI: ).*')


# how many of the dois are in the results?
(handmade_dois %in% results_df$doi)


###### aobut half are missing, which is not good.

# hand check some missing ones
# first missing: "10.4137/ASWR.S30708" 
results_df %>% filter(str_detect(authors, "Grebli"))



##### ok... so a big problem is that the API is sometimes missing the dois. We need a different way to match
results_df$doi %>% is.na %>% sum 


## IDEA: Match on first author, yaer, and title? (idk if title is overkill)

# title is only guaranteed to be partial (idk how much i trust the separation on ".")
# I'm using separate rather than a replace just so that we can see more in case something goes bad
handmade_refs_separated <- handmade_refs %>%
  separate(citation, into = c("authors", "remainder"),
           sep = "\\(\\d{4}\\)\\.\\s", 
           extra = "merge",
           remove = FALSE) %>%
  separate(remainder, into  = c("title", "endpiece"),
           sep = "\\.", extra = "merge") %>%
  separate(authors, into = c("first_author_last_name", "misc_authors"),
           sep = ",", extra = "merge") %>%
  mutate(doi = handmade_dois,
         year = str_extract(citation_short, "\\d{4}"))



handmade_refs_separated %>%
  filter( (doi %in% results_df$doi) | ( str_detect(first_author_last_name, results_df$authors) & (year == results_df$year) & str_detect(title, results_df$title)  )  )


handmade_refs_separated %>%
  distinct(doi) %>%
  inner_join(distinct(results_df), by = "doi") %>% nrow

handmade_refs_separated %>%
  distinct(title, first_author_last_name, year)


####################################

## Instead of trying to get title through the citation, we can use this new csv that we got from lesley
  # now we can just try directly matching titles (lower cased)

# Formatting the original, hand-matched titles to be lower case
formatted_handmade_refs_expanded <- handmade_refs_expanded %>%
  remove_empty('rows') %>%
  mutate(title = str_to_lower(title) %>% str_trim)

formatted_results_df <- results_df %>%
  mutate(title = str_to_lower(title) %>% str_trim)
# formatting the titles in the results dataframe to also be lower case
query_titles <- formatted_results_df$title %>%
  unique




# First, let???s match by doi. that???s most certain --------------------------

unmatched_by_doi <- formatted_handmade_refs_expanded %>%
  filter(!doi %in% results_df$doi)


# Next, let???s match by the rest of the title and see if we get a hit --------
unmatched_by_doi_title <- unmatched_by_doi %>%
  filter(!title %in% query_titles)

  



# Next, let???s see if we can find a fuzzy-match ----------------------------

# create string distance matrix using levenshtein distance (number of deltetions/insertions/substitutions)
title_dist_matrix <- stringdist::stringdistmatrix(unmatched_by_doi_title$title, query_titles, method = "lv")

# Zeroes are exact matches, but we are looking for fuzzy matches, so we remove exact matches
# We'll say there's definely no match if the lv distance is greater than 30
title_dist_matrix[title_dist_matrix == 0 | title_dist_matrix > 30] <- NA


# pull out the row indices where there is at least one non-NA value
nonzero_index <- rowSums(title_dist_matrix, na.rm = TRUE) > 0

# pull out the rows where there is at least one non-NA value
title_dist_with_match <- title_dist_matrix[nonzero_index,]


# create a dataframe to compare the two titles side by side
title_match_check <- unmatched_by_doi_title %>%
  slice(which(nonzero_index)) %>%
  mutate(matched_title = query_titles[apply(title_dist_with_match, 1, which.min)]) %>%
  select(title, matched_title)





# All the fuzzy matches look like real matches! let???s remove them from the table  --------

unmatched_by_doi_title_fuzzy <- unmatched_by_doi_title %>%
  filter(!title %in% title_match_check$title)

questionable_match <- unmatched_by_doi_title_fuzzy$title %>% stringdistmatrix(query_titles, method = "lv")

questionable_match_lookup <- unmatched_by_doi_title_fuzzy %>%
  mutate(matched_title = query_titles[apply(questionable_match, 1, which.min)]) %>%
  select(title, matched_title) 


# some of these are inconclusive, so let's look at the rest of the data (publication year, authors, ect.)

questionable_match_with_metadata <- questionable_match_lookup %>%
  left_join(unmatched_by_doi_title_fuzzy, by = "title") %>%
  left_join(formatted_results_df, by = c("matched_title" = "title"))



# just to make sure there's no match for these 3 observations, we'll look at year and author in the query data

  # this paper is not in the 4 editions we have access to (SCI, ISTP, ISSHP, IC)
    # confirmed by searching the web of science site
results_df %>% filter(str_detect(str_to_lower(authors), 'grebliunas'))

  # this paper IS in the 4 editions we have access to.. i'm not sure why we aren't picking it up
results_df %>% filter(str_detect(str_to_lower(authors), 'osborne'), year == 2014)
results_df %>% filter(str_detect(str_to_lower(authors), 'ferguson'), year == 2002)
















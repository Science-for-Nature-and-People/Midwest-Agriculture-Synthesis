#devtools::install_github("juba/rwos")
#devtools::install_github("Science-for-Nature-and-People/BibScan")

library(rwos)
library(tidyverse)
library(stringdist)
library(janitor)
library(rcrossref)
library(BibScan)
library(devtools)
library(rcrossref)
library(RCurl)











# read in existing references for comparison
handmade_refs <- read_csv('www/data/references-for-app.csv')

handmade_refs_expanded <- read_csv('www/data/refs_all_expanded.csv')


###############################

###### Helper Functions #######

###############################


# Julien's previous code for getting a doi from an article title. we have to modify one of the functions though (getdoi)
devtools::source_url("https://raw.github.nceas.ucsb.edu/LTER/biblio-analysis/master/get_dois.R?token=AAAABGSW6KKEWWX7LGMNDHS5STRTW")

#' Query the CrossRef API to find the DOI associated with a specific title; repeat several times the query if failed waiting 5sec between trials
#'
#' @param title A character.
#' @param nattempts An integer
#' @return A vector containing: DOI, Referenced Title, Full Citation
#' @examples
#' getdoi("Use of film for community conservation education in primate habitat countries")
#' getdoi("Use of film for community conservation education in primate habitat countries", n=10)
getdoi <- function(my_title, nattempts = 1, threshold = 20.0) {
  #this function uses the crossref API to query the DOI database using titles entered by the reviewer
  if (is.na(my_title)) {
    print("the my_title is not availaible")
    doi <- NA
    title.new <- NA
    # fullCitation <- NA
    count_from_api <- NA
  } else {
    # Query the API
    query <- get_cr(my_title)
    print("Query returned successfully")
    if (nrow(query) < 1){
      j = 0
      while (j <= nattempts) {
        Sys.sleep(5)
        query <- get_cr(my_title)
        j = j+1
      }
    }
    if (nrow(query) > 0 | is.null(query)){
      #Keep information only if the matching score is over 2.0
      if (query$score[1] > threshold){  ## THRESHOLD, might want to try other values based on the whole dataset
        doi <- query$doi[1]
        # print(doi)
        title.new <- query$title[1]
        #queryable_doi = strsplit(doi,"https://doi.org/")[[1]][2] #IM: with cr_works, the returned DOI seems already to have https://doi.org/ removed
        # queryable_doi = doi #added by IM to make fullCitation line run
        #It seems sometimes the API is unresponsive -> squeezing 5 attemps, need to be improved
        # fullCitation <- get_fullcitation(queryable_doi)
        #if the API retuned the 500 error, try 5 more time with 5sec pause in between
        # if (is.null(fullCitation)) {
        #   fullCitation <- NA # prevent errors about zero-length inputs to if, below
        # } else {
        #   tryCatch(
        #     {
        #       if (fullCitation == "" | is.function(fullCitation)){
        #         i = 0
        #         while (i <= nattempts) {
        #           Sys.sleep(5)
        #           fullCitation <- get_fullcitation(queryable_doi)
        #           i = i+1
        #         }
        #       }
        #     },
        #     error = function(err) {
        #       print(err)
        #     },
        #     finally = {
        #       print(fullCitation)
        #     }
        #   )
        # }
        
        #Get the number of citation for that DOI
        
        # count_from_api <- cr_citation_count(doi=doi)$count
        count_from_api <- NA
        tryCatch(
          count_from_api <- cr_citation_count(doi=doi)$count,
          error = function(err) {
            print(err)
          },
          finally = {
            count_from_api <- NA
          }
        )
        
      } else {
        doi <- NA
        title.new <- NA
        # fullCitation <- NA
        count_from_api <- NA
        print("No doi was matched")
      }
    } else {
      doi <- NA
      title.new <- NA
      # fullCitation <- NA
      count_from_api <- NA
      print("No doi was matched")
    }
  }
  
  # Write the output data frame
  final_df <- data.frame(title_zotero = my_title, 
                         doi = as.character(doi), 
                         title_api = as.character(title.new), 
                         # citation = as.character(fullCitation),
                         citation_count = count_from_api,
                         match_score = query$score[1],
                         stringsAsFactors = FALSE
  )
}



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
# also get rid of all spcaes in doi
formatted_handmade_refs_expanded <- handmade_refs_expanded %>%
  remove_empty('rows') %>%
  mutate(title_lower = str_to_lower(title) %>% str_trim,
         doi = str_replace_all(doi, "\\s", ""))

# formatting the titles in the results dataframe to also be lower case
# also get rid of all spaces in doi
formatted_results_df <- results_df %>%
  mutate(title_lower = str_to_lower(title) %>% str_trim,
         doi = str_replace_all(doi, "\\s", ""))

query_titles_lookup <- formatted_results_df %>%
  select(title, title_lower) %>%
  distinct(title_lower, .keep_all = TRUE)





# First, let???s match by doi. that???s most certain --------------------------

unmatched_by_doi <- formatted_handmade_refs_expanded %>%
  filter(!doi %in% formatted_results_df$doi)


# Next, let???s match by the title and see if we get a hit --------
unmatched_by_doi_title <- unmatched_by_doi %>%
  filter(!title_lower %in% query_titles_lookup$title_lower)





# Next, let???s see if we can find a fuzzy-match ----------------------------

# create string distance matrix using levenshtein distance (number of deltetions/insertions/substitutions)
title_dist_matrix <- stringdist::stringdistmatrix(unmatched_by_doi_title$title_lower, query_titles_lookup$title_lower, method = "lv")

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
  mutate(matched_title = query_titles_lookup$title_lower[apply(title_dist_with_match, 1, which.min)]) %>%
  select("original_title" = title, matched_title) %>%
  left_join(formatted_handmade_refs_expanded, by = c("original_title" = "title")) %>%
  left_join(query_titles_lookup, by = c("matched_title" = "title_lower")) %>%
  select(original_title, "matched_title" = title, "original_title_lower" = title_lower, "matched_title_lower" = matched_title)





# All the fuzzy matches look like real matches! let???s remove them from the table  --------

unmatched_by_doi_title_fuzzy <- unmatched_by_doi_title %>%
  filter(!title_lower %in% title_match_check$original_title_lower)

questionable_match <- unmatched_by_doi_title_fuzzy$title_lower %>% stringdistmatrix(query_titles_lookup$title_lower, method = "lv")

questionable_match_lookup <- unmatched_by_doi_title_fuzzy %>%
  mutate(matched_title = query_titles_lookup$title_lower[apply(questionable_match, 1, which.min)]) %>%
  select(title, matched_title) 


# some of these are inconclusive, so let's look at the rest of the data (publication year, authors, ect.)

questionable_match_with_metadata <- questionable_match_lookup %>%
  left_join(unmatched_by_doi_title_fuzzy, by = "title") %>%
  left_join(formatted_results_df, by = c("matched_title" = "title_lower"))



# just to make sure there's no match for these 3 observations, we'll look at year and author in the query data

# this paper is not in the 4 editions we have access to (SCI, ISTP, ISSHP, IC)
# confirmed by searching the web of science site
results_df %>% filter(str_detect(str_to_lower(authors), 'grebliunas'))

# this paper IS in the 4 editions we have access to.. i'm not sure why we aren't picking it up
results_df %>% filter(str_detect(str_to_lower(authors), 'osborne'), year == 2014)
results_df %>% filter(str_detect(str_to_lower(authors), 'ferguson'), year == 2002)






############################

### Write to .bib? ###

############################








############################

### Trying to match DOIs ###

############################

## we want to isolate the files that we found matches for using titles (either exact or fuzzy),
## excluding the papers where we either found match through doi, or found no match

# papers matched exactly
no_doi_exact_title_match <- unmatched_by_doi %>%
  filter(title_lower %in% query_titles_lookup$title_lower) %>%
  mutate(matched_title_lower = title_lower)

# papers matched fuzzy
no_doi_fuzzy_title_match <- unmatched_by_doi_title %>%
  inner_join(title_match_check, by = c("title_lower" = "original_title_lower"))


# combine these two dataframe, and only keep doi, original title, and matched title (lowered)
# then use matched_title_lower to join with the query dataframe
no_doi_any_title_match <- bind_rows(no_doi_exact_title_match, no_doi_fuzzy_title_match) %>%
  select(doi, title, title_lower, matched_title_lower) %>%
  left_join(formatted_results_df, by = c("matched_title_lower" = "title_lower")) %>%
  rename("handmade_doi" = "doi.x", 
         "query_doi" = "doi.y",
         "handmade_title" = "title.x",
         "query_title" = "title.y")





#taking function from the biblio-analysis repo in the LTER org! (see the `source_url` code at the top)
# NOTE: this takes a while to run!
#' i don't have a lot of faith in the score (score is what threshold is for). I think it refers to the "relevance score" in the crossref api, which is a tf-idf?
scrape_doi_title <- function(title, threshold = 20){
  scrape <- getdoi(title, threshold = threshold)
  
  tibble("matched_title_lower"= title, 
         "doi_scraped" = scrape$doi, 
         "title_scraped" = scrape$title_api, 
         "match_score" = scrape$match_score,
         "title_dist" = stringdist::stringdist(matched_title_lower, str_to_lower(title_scraped), method = "lv")) 
}

#add distance between the titles. can set a cutoff like ~10
doi_scraping <- map_df(no_doi_any_title_match$matched_title_lower, ~scrape_doi_title(.x))


# # playing with the threshold (eg tf-idf) I don't think this is the way to go. I prefer using something like "lv" distance between titles
# doi_scraping60 <- map_df(no_doi_any_title_match$matched_title_lower, ~scrape_doi_title(.x, threshold = 60)) %>%
#   mutate(title_dist = stringdist::stringdist(matched_title_lower, str_to_lower(title_scraped), method = "lv"))


#doi_scraping_testlist <- seq(30,60,10) %>% map(function(x) map_df(no_doi_any_title_match$matched_title_lower, function(y) scrape_doi_title(y, threshold = x)))


# ## This was my first attempt, but i didn't want to have to run getdoi twice.
# doi_scraping <- no_doi_any_title_match %>%
#   rowwise() %>%
#   mutate(scraped_doi = getdoi(matched_title_lower)$doi,
#          #scraped_title = 
#          correct = identical(str_to_lower(scraped_doi), str_to_lower(handmade_doi)))


#' check doi scraping results by comparing DOIs / looking at title distance
#' @param table_without_doi_match is a tibble where we were able to string match, but not able to doi match. see no_doi_any_title_match
#' @param doi_scrape_result is the output of a map_df(~scrape_doi_title) (eg doi_scraping)
#' @return A dataframe with the different title versions, different DOIs, title distance, where the DOI doesn't match
check_doi_scrape <- function(table_without_doi_match, doi_scrape_result){
  compare_dois <- doi_scrape_result %>%
    left_join(table_without_doi_match, by = "matched_title_lower") %>%
    select(matched_title_lower, handmade_title, title_scraped, query_doi, handmade_doi, doi_scraped, match_score, title_dist) %>%
    rowwise() %>%
    mutate(correct = identical(str_to_lower(doi_scraped), str_to_lower(handmade_doi))) %>%
    ungroup %>%
    distinct(handmade_title, .keep_all = TRUE)
  
  compare_dois %>%
    filter(correct == FALSE)
}



check_doi20 <-check_doi_scrape(no_doi_any_title_match, doi_scraping) 

#start clicking some links
check_doi20 %>%
  mutate_at(vars(contains("doi")), function(x) str_replace_all(x, x, paste0("https://doi.org/", x))) %>% View




# check_doi60 <- check_doi_scrape(no_doi_any_title_match, doi_scraping60)
# check_doi60 %>%
#   mutate_at(vars(contains("doi")), function(x) str_replace_all(x, x, paste0("https://doi.org/", x))) %>% View
# 
# 




#### check which of the handmade doi's are incorrect


doi_existance_check <- check_doi20 %>%
  filter(handmade_doi != "Unavailable" & !str_detect(handmade_doi, "http")) %>%
  mutate(handmade_doi_url = paste0("https://doi.org/", handmade_doi)) %>%
  rowwise() %>%
  mutate(valid_doi = url.exists(handmade_doi_url))


















#############################

## TEST: a couple of the multi-word terms on the google doc are unquoted
## The queries above in the code have quotes in them. The below code checked the the original versions to make sure we aren't missing anything

##############################

# the original tillage query doesn't have the first term quoted
tillage_query_og <- 'conservation till* OR "conventional till" OR "no-till" OR "no till*" OR "reduced till*" OR "minimum till*"'
tillage_results_og <- get_results(tillage_query_og, sid = session_id)

tillage_results_new <- get_results(tillage_query, sid = session_id)

# Check out the rows the new one misses to see if any of them are important
tillage_results_diff <- anti_join(tillage_results_og, tillage_results_new, by = names(tillage_results_og))
tillage_results_diff %>% 
  filter(title %in% formatted_handmade_refs_expanded$title)

# do the same for pesticide
pest_query_og <- 'pesticide seed treatment* OR "seed treatment*" OR "systemic insect*" OR neonic* OR pyrethr* OR (foliar AND insecticide*)'
pest_results_og <- get_results(pest_query_og, sid = session_id)
pest_results_new <- get_results(pest_query, sid = session_id)

pest_results_diff <- anti_join(pest_results_og, pest_results_new, by = names(pest_results_og))
pest_results_diff %>%
  filter(title %in% formatted_handmade_refs_expanded$title)














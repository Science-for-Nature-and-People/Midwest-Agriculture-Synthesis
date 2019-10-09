#########################################

## Workflow for assisting publication search

#########################################


#devtools::install_github("juba/rwos")
#devtools::install_github("Science-for-Nature-and-People/BibScan")
#devtools::install_github("rich-iannone/blastula")
library(rwos)
library(tidyverse)
library(magrittr)
library(stringdist)
library(janitor)
library(rcrossref)
library(BibScan)
library(devtools)
library(rcrossref)
library(RCurl)
library(lubridate)
library(blastula)


# one time: create credential file for email alert system:
create_smtp_creds_file(
  file = "nathanhwangbo_ucsbedu_cred",
  user = "nathanhwangbo@ucsb.edu",
  provider = "gmail"
)


todays_date <- Sys.Date() %>% str_replace_all("-", "")
# this will be the endpoint in our query
current_year <- Sys.Date() %>% lubridate::year()

# check the newsest old query for the start point in the query
old_year <- list.files(here("auto_pubsearch", "wos_queries"), pattern = "wos_query_\\d+", full.names = F) %>%
  str_sort(decreasing = T, numeric = T) %>%
  first(1) %>%
  str_match("\\d+") %>%
  lubridate::parse_date_time(order = "ymd") %>%
  lubridate::year()

## Step 0: pass in helper functions ==================================================================================


# Julien's previous code for getting a doi from an article title. we have to modify one of the functions though (getdoi)
devtools::source_url("https://raw.github.nceas.ucsb.edu/LTER/biblio-analysis/master/get_dois.R?token=AAAABGVWONEENFX2ISLFG5K5TY6ZY")

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
        title.new <- query$title[1]
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
        count_from_api <- NA
        print("No doi was matched")
      }
    } else {
      doi <- NA
      title.new <- NA
      count_from_api <- NA
      print("No doi was matched")
    }
  }
  
  # Write the output data frame
  final_df <- data.frame(title_zotero = my_title, 
                         doi = as.character(doi), 
                         title_api = as.character(title.new), 
                         citation_count = count_from_api,
                         match_score = query$score[1],
                         stringsAsFactors = FALSE
  )
}


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


#' check doi scraping results by comparing DOIs / looking at title distance
#' @param table_without_doi_match is a tibble where we were able to string match, but not able to doi match. see no_doi_any_title_match
#' @param doi_scrape_result is the output of a map_df(~scrape_doi_title) (eg doi_scraping)
#' @return A dataframe with the different title versions, different DOIs, title distance, whether the dois match, and a column 
#' called `doi_combined` that is the original DOI if it is found, and the scraped DOI otherwise.
check_crossref_match <- function(table_without_match, scrape_result){
  scrape_result %>%
    left_join(table_without_match, by = c("matched_title_lower" = "title_lower")) %>%
    select(matched_title_lower, title_scraped, doi, doi_scraped, match_score, title_dist, review, uid, isi_id, issn) %>%
    rowwise() %>%
    mutate(doi_match = identical(str_to_lower(doi_scraped), str_to_lower(doi)),
           doi_combined = ifelse(!is.na(doi), doi, doi_scraped)) %>%
    ungroup %>%
    distinct(matched_title_lower, .keep_all = TRUE)
}

#' Quick function for going from final_matches to citation
#' @param matchdf A dataframe of matches for a review
#' @param date is the date you want to label as the file
matchlist_to_citation <- function(matchdf, date){
  # the review was the same for all rows of matchdf, so we can pick any element
  review <- matchdf$review[1]
  abstracts <- map(matchdf$doi_combined, ~tryCatch(cr_abstract(.x), error = function(x) x))
  
  
  rcrossref::cr_cn(matchdf$doi_combined, format = "bibtex") %>%
    str_replace(pattern = "\\}$", replacement = "") %>%
    map2(abstracts, ~paste0(.x, "\tabstract = {", .y, "}\n}")) %>%
    paste(collapse = "\n") %T>%
    write_file(here("auto_pubsearch", "Bibfiles", paste0("citations_", review, "_", date, ".bib")))
  
  write(paste0(".bib file for ", review, " written!"), stdout())
}



get_cr <- function(titleq) {
  # q <- cr_works(q = titleq, flq = c(query.title = titleq), limit = 5, sort = "score")
  print("querying the API")
  tryCatch(q <- cr_works(q = trimws(titleq), limit = 5, sort = "score"),
           error = function(err) {
             print(err)
             # return empty data frame
             return(data.frame(doi=as.character(NA),
                               title=as.character(NA),
                               score=as.character(NA),
                               stringsAsFactors = FALSE)
             )
           })
  #Handle the strange case a function is returned from the call....
  if (is.function(q)) {
    # return empty data frame
    return(data.frame(doi=as.character(NA), 
                      title=as.character(NA),
                      score=as.character(NA),
                      stringsAsFactors = FALSE)
    )
  }
  
  # Check if any column is missing
  if (is.null(q$data$doi)) {
    q$data$doi <- as.character(NA)
  }
  if (is.null(q$data$title)) {
    q$data$title <- as.character(NA)
  }
  if (is.null(q$data$score)) {
    q$data$score <- as.character(NA)
  }
  if(!is_tibble(q$data)) {
    q$data <- as_tibble(q$data)
  }
  
  # Remove rows with any NA in those 3 fields (assuming it is wrong)
  data_q <- q$data %>%
    #as_tibble() %>%
    select(doi,title, score) %>%
    drop_na()
  
  # # Create the ouput dataframe
  # query_df <- data.frame(doi=data_q$doi, title=data_q$title, score=as.numeric(data_q$score),
  #                 stringsAsFactors = FALSE)
}





## Step 1: Query. USER UPDATE year_start and year_end ================================================================================= 


### Write out the practice specific queries (taken from google doc)
cover_crop_query <- '"cover crop" OR cover-crop* OR covercrop*'
tillage_query <- '"conservation till*" OR "conventional till" OR "no-till" OR "no till*" OR "reduced till*" OR "minimum till*"'
pest_query <- '"pesticide seed treatment"* OR "seed treatment*" OR "systemic insect*" OR neonic* OR pyrethr* OR (foliar AND insecticide*)'
nutrient_mgmt_query <- '(precision AND (fert* OR agr* OR nitrogen OR phosphorous)) OR "variable rate" OR "band* fert*" OR 4R OR ((enhance* OR efficien*) AND (nitrogen OR phosphorous))'


### Create a dataframe with all the hits 

# Sometimes the API limits us.. it seems random whether it decides to or not, so this code tries a few times
i <- 0
repeat({
  session_id <- wos_authenticate()
  results_list <- try({
    list(cover_crop_query, tillage_query, pest_query, nutrient_mgmt_query) %>%
    map(~get_results(.x, sid = session_id, year_start = old_year, year_end = current_year)) %>%
    set_names(c("cc", "tillage", "pest", "nutrient"))
  })
  
  if(!inherits(results_list, "try-error")){
    write("Query success!", file = stdout())
    break
  }
  if(i > 3) {
    write("Query failed 3 times..Giving up", file = stderr())
    break
  }
  
  write(paste0("Query failed. Trying again. i = ", i), file = stderr())
  i <- i + 1
  Sys.sleep(5)
})



# Combine the hits to generate a giant dataframe
  # Don't remove duplicates, since we are splitting into separate reviews later!
results_df <- results_list %>%
  bind_rows(.id = "review") %>%
  mutate(title_lower = title %>% str_to_lower() %>% str_trim()) %T>%
  #distinct(title_lower, .keep_all = T) %T>%
  write_csv(here("auto_pubsearch", "wos_queries", paste0("wos_query_", todays_date, ".csv")))




## Step 2: Filter out these results (?) Maybe we want to just scrape everything and filter later================================================================

# First, we read in the old query, so that we can remove papers we've already looked at
old_results_file <- list.files(here("auto_pubsearch", "wos_queries"), pattern = "wos_query_\\d+", full.names = T) %>%
    str_sort(decreasing = T, numeric = T) %>%
    first(1)
old_results_df <- read_csv(old_results_file)

# Now, we remove the results that showed up in a previous query
unique_new_results_df <- results_df %>% 
  anti_join(old_results_df, by = "title_lower")



##### NOTE: if there are no new results, then unique_new_results_df will have 0 rows
#####       We only want to execute the rest of the script if there are >20 new rows
#####       Solution: wrap everything below this line in an if statement
#####       Having such a large if statement doesn't look very nice though.. 
#####       Maybe it could be improved by using quit(), or by functionizing everything


if(nrow(unique_new_results_df) >= 20){
  ## Step 3: Pass titles into rcrossref ==============================================================
  match_crossref <- map_df(unique_new_results_df$title_lower, ~scrape_doi_title(.x))
  
  
  
  ## Step 4: Check non-exact matches ================================================================
  # all of the observations next to their matches
  check_crossref <- check_crossref_match(unique_new_results_df, match_crossref) 
  
  # look at all the cases where doi doesn't match. we notice a lot of shared titles though!
  check_crossref %>%
    filter(doi_match == FALSE) %>%
    arrange(title_dist) 
  
  
  # all of the actual crossref matches. THIS STEP MIGHT DEPEND ON WHAT WE SEE IN doi_match == FALSE LINE ABOVE!!!!
  final_matches <- check_crossref %>%
    filter(doi_match == TRUE | title_dist == 0 | !is.na(doi))
  
  # Load in the latest nomatch file, so that we aren't duplicating any results
  old_nomatch_file <- list.files(here("auto_pubsearch", "failed_matches"), pattern = "wos_cr_nomatch_\\d+", full.names = T) %>%
    str_sort(decreasing = T, numeric = T) %>%
    first(1)
  old_nomatch_df <- read_csv(old_nomatch_file)
  
  # Save the date of the last successful query
  last_alert_date <- old_nomatch_file %>%
    str_extract("\\d+") %>%
    parse_date_time(orders = "ymd") 
  
  
  
  
  # print out all the DOIs that weren't matched. also filter out DOIs that weren't matched previously 
   # ie antijoin final matches AND old wos_cr_nomatch.
  check_crossref %>% 
    anti_join(final_matches, by = "matched_title_lower") %>%
    anti_join(old_nomatch_df, by = "matched_title_lower") %T>%
    write_csv(here("auto_pubsearch", "failed_matches", paste0("wos_cr_nomatch_", todays_date, ".csv")))
  
  

  ## Step 5: Get citations from rcrossref, split by review ==========================================================================
  citations <- final_matches %>%
    group_by(review) %>%
    group_map(~matchlist_to_citation(.x, todays_date), keep = TRUE)

  
  
  ## Step 6: Bibscan using the citations=============================================================================
  bibscan_pdfs <- BibScan::article_pdf_download(here("auto_pubsearch", "Bibfiles"), here("auto_pubsearch", "latest_bibscan_results"))
  
  
  
  ## Step 7: Send an alert with the new information =================================================================
  more_than_twenty_alert <- 
  "
  Hello!
  
  There are at least 20 new papers for the Midwest Agriculture Reviews! This alert is counting papers from {last_alert_date} to {today()}.  
  ****
  The API returns:

  Cover crops > {results_df %>% filter(review == 'cc') %>% nrow}  
  Nutrient Management > {results_df %>% filter(review == 'nutrient') %>% nrow}  
  Pest Management > {results_df %>% filter(review == 'pest') %>% nrow}  
  Tillage > {results_df %>% filter(review == 'tillage') %>% nrow}  
  ****

  The queries used to generate these numbers are below:  

  **Cover crops**: {cover_crop_query}  

  **Nutrient Management**: {nutrient_mgmt_query}  

  **Pest Management**: {pest_query}  

  **Tillage**: {tillage_query}
  ****************
  

  Happy reviewing!
  "
  
  
  alert_email <- compose_email(body = more_than_twenty_alert, footer = "Email sent on {add_readable_time()}")
  
  #send email
  smtp_send(alert_email, to = "nathanhwangbo@gmail.com", from = "nathanhwangbo@ucsb.edu", 
            credentials = creds_file("nathanhwangbo_ucsbedu_cred"),
            subject = "Testing the alert system")
  
}















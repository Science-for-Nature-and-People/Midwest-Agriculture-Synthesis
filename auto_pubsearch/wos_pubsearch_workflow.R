#########################################

## Workflow for assisting publication search

#########################################


#devtools::install_github("juba/rwos")
#devtools::install_github("Science-for-Nature-and-People/BibScan")
library(rwos)
library(tidyverse)
library(magrittr)
library(stringdist)
library(janitor)
#library(rcrossref)
#library(BibScan)
library(devtools)
#library(rcrossref)
library(RCurl)
library(lubridate)
library(here)
library(jsonlite)
library(httr)
library(git2r)

# link to github api issue that we want the bot to comment on.
issues_url <- "https://api.github.com/repos/Science-for-Nature-and-People/Midwest-Agriculture-Synthesis/issues/40/comments"
# get the private issue token from local file
issue_token <- readr::read_file(here("auto_pubsearch" , 'machine_git_token.txt'))


# this will be used to title files
todays_date <- Sys.Date() %>% str_replace_all("-", "")
# this will be the endpoint in our query
current_year <- Sys.Date() %>% lubridate::year()


# check the newsest old query for the start point in the query
old_year <- list.files(here("auto_pubsearch", "wos_raw_queries"), pattern = "wos_query_\\d+", full.names = F) %>%
  str_sort(decreasing = T, numeric = T) %>%
  first(1) %>%
  str_match("\\d+") %>%
  lubridate::parse_date_time(order = "ymd") %>%
  lubridate::year()

## Step 0: pass in helper functions ==================================================================================


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
  wos_result <- practice_query %>%
    write_full_query(year_start, year_end) %>%      # Write out the query
    wos_search(sid, .) %>%                          # Search the database to get a list of hits
    wos_retrieve_all()                              # Process that list as a dataframe
  
  # wait for a bit so that the api doesn't get mad
  Sys.sleep(5)
  
  wos_result
  
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
})



## Step 2: Filter the results so we're only left with papers since the last alert. =======================

# First, we read in the old query, so that we can remove papers we've already looked at
old_results_file <- list.files(here("auto_pubsearch", "wos_raw_queries"), pattern = "wos_query_\\d+", full.names = T) %>%
    str_sort(decreasing = T, numeric = T) %>%
    first(1)
old_results_df <- read_csv(old_results_file)


# we use this to mark the time period between papers
old_results_date <- old_results_file %>%
    str_extract("\\d+") 


# Combine the current hits to generate a giant dataframe
# Don't remove duplicates, since we are splitting into separate reviews later!
results_df <- results_list %>%
  bind_rows(.id = "review") %>%
  mutate(title_lower = title %>% str_to_lower() %>% str_trim()) %T>%
  #distinct(title_lower, .keep_all = T) %T>%
  write_csv(here("auto_pubsearch", "wos_raw_queries", paste0("wos_query_", todays_date, ".csv")))



# Now, we remove the results that showed up in a previous query.
# This is necessary because the rwos query only allows a filter by YEAR. so this will help to remove within year papers
unique_new_results_df <- results_df %>% 
  anti_join(old_results_df, by = "title_lower") %T>%
  write_csv(here("auto_pubsearch", "wos_new_refs", paste0("wos_query_between_", old_results_date, "_", todays_date, ".csv")))

  
# Step 3: Write files and send an Alert (github comment) if there are >= 20 new papers ====================================
if(nrow(unique_new_results_df) >= 20){
  
  # First, look at the last date an alert was sent
  alert_date_log <- read_csv(here("auto_pubsearch", "alert_date_log.csv"), col_types = list(col_character()))
  last_alert_date <- alert_date_log$last_alert_date %>% tail(1)
  
  # write a new entry for the new alert we're currently making
  alert_date_log %>%
    rbind(as.character(today())) %>%
    write_csv(path = here("auto_pubsearch", "alert_date_log.csv"))
  
  
  ### Push to repo
  
  # give path to repo
  midwest_repo <- repository(here())
  
  # add files
  add(midwest_repo, here("auto_pubsearch","alert_date_log.csv"))
  add(midwest_repo, here("auto_pubsearch", "wos_raw_queries", paste0("wos_query_", todays_date, ".csv")))
  add(midwest_repo, here("auto_pubsearch", "wos_new_refs", paste0("wos_query_between_", old_results_date, "_", todays_date, ".csv")))
  
  new_commit <- commit(midwest_repo, message = str_glue("auto-updated queries for {now()}"))
  push(midwest_repo, credentials = cred_token(token = "GITHUB_TOKEN"))
  
  
  # Alert text!
  more_than_twenty_alert <- str_glue(
  "Hello!    ",
  
  "There are at least {nrow(unique_new_results_df)} new papers for the Midwest Agriculture Reviews! This alert is counting papers from {last_alert_date} to {today()}.",  
  "******",
  "The API returns:  ",

  "Cover crops > {unique_new_results_df %>% filter(review == 'cc') %>% nrow}",
  "Nutrient Management > {unique_new_results_df %>% filter(review == 'nutrient') %>% nrow}",
  "Pest Management > {unique_new_results_df %>% filter(review == 'pest') %>% nrow}",
  "Tillage > {unique_new_results_df %>% filter(review == 'tillage') %>% nrow}",
  "******",

  "The queries used to generate these numbers are below:  ",

  "**Cover crops**: {cover_crop_query}  ",

  "**Nutrient Management**: {nutrient_mgmt_query}   ",

  "**Pest Management**: {pest_query}  ",

  "**Tillage**: {tillage_query}  ",
  "******  ",
  
  "Happy reviewing!",
  
  "(this alert was generated at {lubridate::now()}), and corresponds to {sha(new_commit)}",
  .sep = '  '
  )
  
  
  # Generate the issue text as json
  json_text <- toJSON(
    list(
      body = unbox(more_than_twenty_alert)
      )
    )
  
  # send the issue to github. (user doesn't show up anywhere, but the parameter can't be NULL)
  issue <- httr::POST(issues_url, 
                      body = json_text, 
                      config = authenticate(user = 'user', password = issue_token))
  
  # show some confirmation text if the issue went through
  if(status_code(issue) == 201){
    write("issue successfully written!", stdout())
  } 
  
}












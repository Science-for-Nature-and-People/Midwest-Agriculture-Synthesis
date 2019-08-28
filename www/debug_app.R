practice <- 'Tillage'
filter_1 <- 'Conventional tillage'
filter_2 <- c("Conservation tillage")
rv <- 'Climate Mitigation'
depth <- c('0-30 cm', '0-60 cm', '0-150 cm', '0-100 cm', NA)
yrs <- c('Year 1-5', 'Years 1-10','Years 1-20', 'Years 1-30', 'Years 1-40', 'Years 1-50')
#yrs <- ''

#df_practice
df_practice <- function(MgmtPractice){
  filtered_by_practice <- summary_data %>%
    filter(Review %in% MgmtPractice)
  
  if(MgmtPractice == 'Cover crop'){
    filtered_by_practice %>%
      mutate(filter1 = Trt_1name,
             filter2 = Trt_2name,
             filter1_name = 'Cover Crop Mixture',
             filter2_name = 'Cover Crop Species')
  } else if(MgmtPractice == 'Tillage'){
    filtered_by_practice %>%
      mutate(filter1 = factor(Trt_1name, levels = c('Conventional tillage', 'Conservation tillage', 'Zonal tillage')),
             filter2 = factor(Trt_2name, levels = c('Conservation tillage', 'Zonal tillage', 'No tillage')),
             filter1_name = 'Tillage Type #1',
             filter2_name = 'Tillage Type #2')
  } else if(MgmtPractice %in% c('Nutrient Management')){
    filtered_by_practice %>%
      mutate(filter1 = nutrient_groups, 
             filter2 = Trt_2name,
             filter1_name = 'Management Details',
             filter2_name = 'Application Specifics')
  } else if(MgmtPractice %in% c('Early Season Pest Management')){
    filtered_by_practice %>% 
      mutate(filter1 = pm_group1,
             filter2 = pm_group2,
             filter1_name = 'Pesticide Type',
             filter2_name = 'Pesticide Application Site')
  }
  
}


df_filter1 <- function(Filter1){
  
  if((practice == 'Cover crop') & (Filter1 %in% df_practice(practice)$cc_group1)){
    new_df <- df_practice(practice) %>%
      mutate(filter1 = cc_group1)
  } else if ((practice == 'Cover crop') & (Filter1 %in% df_practice(practice)$cc_group2)){
    new_df <- df_practice(practice) %>%
      mutate(filter1 = cc_group2)
  } else {
    new_df <- df_practice(practice)
  }
  
  new_df <- new_df %>% 
    filter(filter1 == Filter1)
  
  #need to write special case to make sure filter1 = zonal tillage => fitler2 = no tillage only 
  #note that we could've also done this in df_filter2 assignment, but putting it here updates the choices as well
  if(Filter1 == 'Zonal tillage'){
    new_df <- new_df %>% 
      filter(filter2 == 'No tillage') 
  }
  
  new_df
}


df_filter2 <- function(Filter2){
  df_filter1(filter_1) %>%
    filter(filter2 %in% Filter2)
}


df_outcome <- function(RV){
  df_filter2(filter_2) %>%
    filter(group_level1 %in% RV) 
}

df_depth <- function(SoilDepth){
  df <- df_outcome(rv)
  
  if(rv %in% c('Soil Nutrients', 'Other Soil Properties', 'Climate Mitigation') & !all(is.na(df$sample_depth))){
    #sort the sample depths options NUMERICALLY
    # sample_depth_options <- df_outcome()$sample_depth %>% 
    #   unique %>% 
    #   stringr::str_sort(numeric = T)
    # 
    # #this will pull out all the previous choices (eg if SoilDepth = '0-60 cm', we want to pull out c('0-25 cm', '0-30 cm', '0-60 cm'))
    # cumulative_sample_depth_choices <- sample_depth_options[1:which(sample_depth_options == SoilDepth)]
    # 
    df %>%
      filter(sample_depth %in% SoilDepth | (is.na(sample_depth) & "" %in% SoilDepth))
    #filter(sample_depth %in% cumulative_sample_depth_choices)
    
  }
  else{
    df
  }
}

df_years <- function(years){
  df <- df_depth(depth)
  if(practice == 'Tillage'){
    
    # filter dataset to display selected review and response variables
    df %>%
      filter(sample_year %in% years | (is.na(sample_year) && years %in% "")) %>%
      group_by(sample_year) %>%
      mutate(group_facet_level32 = fct_reorder(group_facet_level32, mean_per_change)) %>%
      ungroup()
  }
  else{
    df
  }
}

df_map <- function() {
  
  #pick out all the paper ids in the filtered dataset df_outcome() is prefiltered for practice/outcome
  #the filter command in the line below filters the "grouping" seleciton
  #paper_id_list is a string column, with comma delim lists of ints
  #so on each element/list in the column, we split the list on commas, then turn the list into a vector, convert the vector to a numeric one.
  #So now we have a list of numeric vectors. We turn this list into one long vector, and then pull out unique values
  filtered_paper_id <- df_years(yrs)$paper_id_list %>%
    lapply(function(x) strsplit(x, split = ";") %>%
             unlist %>%
             as.integer) %>% 
    unlist %>% 
    unique
  
  #now we filter map.data where paper_id matches any of the numbers inside filtered_paper_id
  map.data %>%
    #state corresponds to the map selection
    #filter((State %in% input$State) & (Paper_id %in% filtered_paper_id))
    #region corresponds to the region selection in the sidebar
    #filter((Region %in% input$Region) & (Paper_id %in% filtered_paper_id))
    filter(Paper_id %in% filtered_paper_id)
}


df_plot <- function(){
  df <- df_years(yrs)
  if(df$Review[1] == 'Cover crop'){
    df %>% 
      group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year) %>% 
      summarize(mean_per_change = mean(mean_per_change), 
                sem_per_change = mean(sem_per_change), 
                paper_id_list = paste(paper_id_list, collapse = ";"), 
                Trt_1name = Trt_1name[1], 
                group_facet_level32 = group_facet_level32[1], 
                Trt_2name = paste(Trt_2name, collapse = ","),
                filter1 = paste(unique(filter1), collapse = ','),
                filter2 = paste(unique(filter2), collapse = ","))
  } else if (df$Review[1] == "Early Season Pest Management"){
    df %>% 
      group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year, filter1) %>% 
      summarize(mean_per_change = mean(mean_per_change), 
                sem_per_change = mean(sem_per_change), 
                paper_id_list = paste(paper_id_list, collapse = ";"), 
                Trt_1name = Trt_1name[1], 
                group_facet_level32 = group_facet_level32[1], 
                Trt_2name = paste(Trt_2name, collapse = ","),
                filter2 = paste(unique(filter2), collapse = ","))
  } else {
    df
  }
  
}

#### Run all the functions above =================================
dfp <- df_practice(practice)
dff1 <- df_filter1(filter_1)
dff2<- df_filter2(filter_2)
dfo <- df_outcome(rv)
dfd <- df_depth(depth)
dfy <- df_years(yrs)
dfm <- df_map()
dfplot <- df_plot()



labs <- lapply(seq(nrow(dfm)), function(i) {
  paste0(
    "State: ", dfm[i, "State"], "<p>", #"County: ", df_map()[i, "NAME"],
    "<p>", "Treatment: ", dfm[i, "Review"], "<p>", "DOI: ", dfm[i, "DOI"]
  )
})



plot_filtered_paper_id <- dfplot$paper_id_list %>%
  lapply(function(x) strsplit(x, split = ";") %>%
           unlist %>%
           as.integer) %>%
  unlist %>%
  unique

references %>%
  select(-citation_short) %>%
  filter(Paper_id %in% plot_filtered_paper_id)

#### Quick check that cum_depth_avg() works =======================


# .x will be the new updated values, .y represents old values (for mean_per_change, sem_per_change, mean_actual_diff, sem_actual_diff)
cumulative_compare <- left_join(summary_data, summary_base, by = joinnames)  %>% 
  # group_by the original groupby summary names except the means/SEs to compare
  group_by(Review, group_level1, group_level2, group_level3, sample_year, Trt_compare, Trt_1name, Trt_2name, trt_specifics, nutrient_groups) %>%
  # make it a little easier to compare
  select(Review, contains('group_level'), 'sample_depth', contains('mean'), contains('sem')) %>% 
  # look at each df separately
  group_split(keep = FALSE)

# We can only really compare if the group has more than 1 row (there's nothing cumulative about one row)
compare_elements <- sapply(a, function(x) nrow(x) > 1)
cumulative_compare_filtered <- cumulative_compare[compare_elements]

# now you manually inspect cumulative_compare_filtered






#### is sample_depth missingness determined by group_level3?
summary_data %>% 
  group_by(Review, group_level3) %>% 
  summarise(pct_depthNA = sum(is.na(sample_depth))/length(sample_depth)) %>% 
  filter(pct_depthNA > 0 & pct_depthNA < 1)




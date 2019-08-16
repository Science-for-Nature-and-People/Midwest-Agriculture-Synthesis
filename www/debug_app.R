practice <- 'Tillage'
filter_1 <- 'Conventional tillage'
filter_2 <- 'No tillage'
rv <- 'Climate Mitigation'
depth <- c('0-30 cm', '0-60 cm', '0-150 cm', '0-100 cm', NA)
#yrs <- c('Year 1-5', 'Years 1-10','Years 1-20', 'Years 1-30', 'Years 1-40', 'Years 1-50')
yrs <- ''

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
      mutate(filter1 = Trt_2name,
             filter2 = trt_specifics,
             filter1_name = 'Pesticide Type',
             filter2_name = 'Pesticide Application Site')
  }
  
}


df_filter1 <- function(Filter1){
  new_df <- df_practice(practice) %>% 
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
    filter(filter2 == Filter2)
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
  if(practice == 'Tillage'){
    df <- df_depth(depth)
    
    # filter dataset to display selected review and response variables
    df %>%
      filter(sample_year %in% years| (is.na(sample_year) & years %in% "")) %>%
      group_by(sample_year) %>%
      mutate(group_facet_level32 = fct_reorder(group_facet_level32, mean_per_change)) %>%
      ungroup()
  }
  else{
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




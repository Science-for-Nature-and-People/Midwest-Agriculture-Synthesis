practice <- 'Tillage'
filter_1 <- 'Conventional tillage'
filter_2 <- 'No tillage'
rv <- 'Climate Mitigation'
depth <- '0-30 cm'
yrs <- 'Year 1-5'

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
  if(RV %in% c('Soil Nutrients', 'Other Soil Properties', 'Climate Mitigation') & !all(is.na(df_outcome(rv)$sample_depth))){
    #sort the sample depths options NUMERICALLY
    # sample_depth_options <- df_outcome()$sample_depth %>% 
    #   unique %>% 
    #   stringr::str_sort(numeric = T)
    # 
    # #this will pull out all the previous choices (eg if SoilDepth = '0-60 cm', we want to pull out c('0-25 cm', '0-30 cm', '0-60 cm'))
    # cumulative_sample_depth_choices <- sample_depth_options[1:which(sample_depth_options == SoilDepth)]
    # 
    df_outcome(rv) %>%
      filter(sample_depth %in% SoilDepth)
    #filter(sample_depth %in% cumulative_sample_depth_choices)
    
  }
  else{
    df_outcome(rv)
  }
}

df_years <- function(years){
  if(MgmtPractice == 'Tillage'){
    # filter dataset to display selected review and response variables
    df_depth(depth) %>%
      filter(sample_year %in% years) %>%
      group_by(sample_year) %>%
      mutate(group_facet_level32 = fct_reorder(group_facet_level32, mean_per_change)) %>%
      ungroup()
  }
  else{
    df_depth(depth)
  }
}

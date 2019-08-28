#### SERVER INSTRUCTIONS ####

# create the 'then" operator to chain tests together
`%then%` <- shiny:::`%OR%`

server <- function(input, output, session) {
  
  
  ########################################
  
  ### Reactive Data Filtering ###
  
  ########################################
  
  # Reactive selection by management practice
    #we want this to update if user click a new practice
    
    #we also add new columns filter1, filter2, filter1_name, filter2_name to the data frame 
    # This helps us filter the app using consistent column names
  df_practice <- eventReactive(c(input$MgmtPractice, input$summaryPractice), {
    # # Cat statements are helpful debugging tools
    #cat(file = stderr(), 'df_practice is updated \n ')
    # filter dataset to display selected review and response variables
    filtered_by_practice <- summary_data %>%
      filter(Review %in% input$MgmtPractice)
    
    if(input$MgmtPractice == 'Cover crop'){
      filtered_by_practice %>%
        #we have to change filter1 based on whether the choice was in cc_group1 or cc_group2, so this filter1 is a dummy
        mutate(filter1 = cc_group1,
               filter2 = Trt_2name,
               filter1_name = 'Cover Crop Mixture',
               filter2_name = 'Cover Crop Species')
    } else if(input$MgmtPractice == 'Tillage'){
      filtered_by_practice %>%
        mutate(filter1 = factor(Trt_1name, levels = c('Conventional tillage', 'Conservation tillage', 'Zonal tillage')),
               filter2 = factor(Trt_2name, levels = c('Conservation tillage', 'Zonal tillage', 'No tillage')),
               filter1_name = 'Tillage Type #1',
               filter2_name = 'Tillage Type #2')
    } else if(input$MgmtPractice %in% c('Nutrient Management')){
      filtered_by_practice %>%
        mutate(filter1 = nutrient_groups, 
               filter2 = Trt_2name,
               filter1_name = 'Management Details',
               filter2_name = 'Application Specifics')
    } else if(input$MgmtPractice %in% c('Early Season Pest Management')){
      filtered_by_practice %>% 
        mutate(filter1 = pm_group1,
               filter2 = pm_group2,
               filter1_name = 'Pesticide Type',
               filter2_name = 'Pesticide Application Site')
    }
       
  })
  
  
  
  
  # React based on changes to the Practice or Filter1
  df_filter1 <- eventReactive(c(df_practice(), input$Filter1), {
    req(input$Filter1)
    # # Cat statements are helpful debugging tools
    # cat(file = stderr(), 'df_filter1 is updated\n')
    # write special case for cover crop, since filter1 can belong to either cc_group1 or cc_group2
    if((input$MgmtPractice == 'Cover crop') & (input$Filter1 %in% df_practice()$cc_group1)){
      new_df <- df_practice() %>%
        mutate(filter1 = cc_group1)
    } else if ((input$MgmtPractice == 'Cover crop') & (input$Filter1 %in% df_practice()$cc_group2)){
      new_df <- df_practice() %>%
        mutate(filter1 = cc_group2)
    } else {
      new_df <- df_practice()
    }
    
    new_df <- new_df %>% 
      filter(filter1 %in% input$Filter1)
    
    #need to write special case to make sure filter1 = zonal tillage => fitler2 = no tillage only 
      #note that we could've also done this in df_filter2 assignment, but putting it here updates the choices as well
    if('Zonal tillage' %in% input$Filter1){
      new_df <- new_df %>% 
        filter(filter2 == 'No tillage') 
    }
    
    new_df
  })
  
  
  #do the next filter
  df_filter2 <- eventReactive(c(df_filter1(), input$Filter2), {
    # # Cat statements are helpful debugging tools
    #cat(file = stderr(), input$Filter2, 'dftillage2 is updated\n')
    
    df_filter1() %>%
        filter(filter2 %in% input$Filter2)
    
  })
  

  
  # Next tier selection of reactive selection of outcome grouping
    # we want this to update if the user clicks the update button, if they click a new outcome, or if they click a new practice (since outcome depends on practice)
  df_outcome <- eventReactive(c(df_filter2(),input$RV), {
    # # Cat statements are helpful debugging tools
    #cat(file = stderr(), input$RV,'df_outcome is updated \n')
    df_filter2() %>%
      filter(group_level1 %in% input$RV) 
    
    
  })
  
  # filter by soil sampling depth (if outcome is Soil Nutrients or Other Soil Properties)
  df_depth <- eventReactive(c(df_outcome(), input$SoilDepth),{
    if(input$RV %in% c('Soil Nutrients', 'Other Soil Properties', 'Climate Mitigation') & !all(is.na(df_outcome()$sample_depth))){
        df_outcome() %>%
          filter(sample_depth %in% input$SoilDepth)
    }
    # else basically says "if there isn't any information on soil, skip this filter"
    else{ 
      df_outcome()
    }
  })
  

  # filter by year of implementation
  df_years <- eventReactive(c(df_depth(), input$years), {
    
    # # Cat statements are helpful debugging tools
    #cat(file = stderr(), is.null(input$years), 'df_years is updated \n \n')
    
    # validate(
    #   need(input$years, 'no year selected'),
    #   need(nrow(df_depth())>0, 'df_depth is empty' )
    # )
    
    # Year is tillage specific, so ignore it if management practice isn't tillage
    if(input$MgmtPractice == 'Tillage'){
      if(is.null(input$years)){
        #return an empty tibble, which will disable the update button
        return(tibble())
      }
      # Check if there are selected years that don't exist in our data (include empty string for NA)
      if(length(setdiff(input$years, c('', unique(df_depth()$sample_year)))) > 0){
        return(tibble())
      }
      # filter dataset to display selected review and response variables
      df_depth() %>%
        #if NA is a selected choice, it shows up as a empty string in input$year, so we gotta check explicitly
        filter(sample_year %in% input$years | (is.na(sample_year) & input$years %in% "")) %>%
        group_by(sample_year) %>%
        mutate(group_facet_level32 = fct_reorder(group_facet_level32, mean_per_change)) %>%
        ungroup()
    }
    else{
      df_depth()
    }
    
      
  })
  
  
  # Filter by geography (separate dataset for the map, filtered by above)
  df_map <- eventReactive(input$update,  {
    
    
    #pick out all the paper ids in the filtered dataset df_outcome() is prefiltered for practice/outcome
      #the filter command in the line below filters the "grouping" seleciton
    #paper_id_list is a string column, with ; delim lists of ints
      #so on each element/list in the column, we split the list on commas, then turn the list into a vector, convert the vector to a numeric one.
        #So now we have a list of numeric vectors. We turn this list into one long vector, and then pull out unique values
    filtered_paper_id <- df_years()$paper_id_list %>%
      lapply(function(x) strsplit(x, split = ";") %>%
               unlist %>%
               as.integer) %>% 
      unlist %>%
      unique
    
    #now we filter map.data where paper_id matches any of the numbers inside filtered_paper_id
    # The comments refer to use ditching region for now
    map.data %>%
      #state corresponds to the map selection
      #filter((State %in% input$State) & (Paper_id %in% filtered_paper_id))
      #region corresponds to the region selection in the sidebar
      #filter((Region %in% input$Region) & (Paper_id %in% filtered_paper_id))
      filter(Paper_id %in% filtered_paper_id)
  })

  
  
  
  
  
  
  
  
  ############################################
  
  ### Reactive (Hierarchical) Input/UI Updating ###
  
  ############################################
  ## It's hierarchical because there is an ordering to the updates.
    # Filter1 changes whenever MgmtPractice changes, Filter2 changes when Filter1 changes, etc.
    # The full chain in the Data tab is MgmtPractice -> Filter1 -> Filter2 -> Outcome -> soilDepth -> Years
    # Caveats: The summary tab messes with this chain a bit by allowing the user to select MgmtPractice and Outcome.
                  # whenever possible, we tried to set the Filter1/Filter2 defaults for each MgmtPractice such that all Outcomes are possible, to preserve the chain
              # renderUI (which was necessary for Filter1/Filter2) also disturbs this chain, 
                  # since I couldn't find a way to time the rendering, or make it react on an event (so it's more like an "observe" than an "observeEvent")
                  # The workaround behind the scenes is pretty much to make everything run twice. Once to get the renderedUI up to speed, and another to actually process the data.
          
  
  
  
  #update practice on summary choice
    #this changes MgmtPractice, triggering change in df_practice -> df_outcome -> df_years
  observeEvent(input$summaryPractice, {
    updateSelectInput(session, "MgmtPractice", "Practice",
                             choices = unique(summary_data$Review) %>% sort(),
                             selected = input$summaryPractice
    )
  })
  
  #update Outcome on summary choice
    #this changes RV, which triggers change in df_outcome -> df_years
  observeEvent(input$summaryRV, {
    # validate(
    #   need(df_filter2(), 'no df_filter2'),
    #   need(input$summaryRV, "no summary output?"),
    #   need(df_filter2()$group_level1, "no grouplevel1")
    # )
    updateRadioButtons(session, "RV", "Outcome",
      #choices = unique(df_filter2()$group_level1) %>% sort(),
      #selected = unique(df_practice()$group_level1)
      selected = input$summaryRV
    )
    
    # # The below chunk of code is used if we need the Summary Outcome to update Filter1 or Filter2 options
    # # this requires using the filter ordering: review -> outcome -> filter1 -> filter2
    #   #which is different from the regular order: review -> filter1 -> filter2 -> outcome
    # if(input$MgmtPractice %in% c('Tillage', 'Cover crop')){
    #   new_filter1 <- (df_practice() %>% filter(group_level1 == input$summaryRV))$Trt_1name %>% unique %>% sort
    # }
    # else if(input$MgmtPractice == 'Early Season Pest Management'){
    #   new_filter1 <- (df_practice() %>% filter(group_level1 == input$summaryRV))$Trt_2name %>% unique %>% sort
    # }
    # else if(input$MgmtPractice == 'Nutrient Management'){
    #   new_filter1 <- (df_practice() %>% filter(group_level1 == input$summaryRV))$nutrient_groups %>% unique %>% sort
    # }
    # 
    # updateRadioButtons(session, 'Filter1', 
    #                    choices = new_filter1,
    #                    selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1[1])
    #                    )
  })
  
  
  # Filter1 changes between a checkboxGroupInput (multiple inputs) or a radio button (single input) depending on practice, requiring renderUI
  # We also make Year of Impelementation input show/hide here.
  output$filter_one <- renderUI({
    validate(
      need(input$MgmtPractice, "no management practice")
    )
   # First, (this is kinda unrelated to filter_one), we want to show/hide the year filter for tillage
    if(input$MgmtPractice == 'Tillage'){
      shinyjs::show('years')
    } else{
      shinyjs::hide('years')
    }
    
    
    # write special case for cover crop, where the first selection can filter on two different columns
    if(input$MgmtPractice == 'Cover crop'){
      new_filter1 <- c(unique(df_practice()$cc_group1), unique(df_practice()$cc_group2))
    } else {
      new_filter1 <- unique(df_practice()$filter1) %>% sort
    }
    
    # write special case for early season pest management, where we allow multiple inputs for filter1
    if(input$MgmtPractice == 'Early Season Pest Management'){
      list(
        checkboxGroupInput('Filter1', unique(df_practice()$filter1_name),
                           choices = new_filter1,
                           #as.character is needed when we have the filter1 is a factor (tillage)
                           selected = new_filter1),
        checkboxInput('AllPesticideTypes', "All Types", value = TRUE)
      )
      
    } else {
      radioButtons('Filter1', unique(df_practice()$filter1_name),
                   choices = new_filter1,
                   #as.character is needed when we have the filter1 is a factor (tillage)
                   selected = new_filter1[1])
    }
  })
  
  
  
  
  
  # Update summary outcome choices and tillage type based on practice
  observeEvent(input$MgmtPractice,{
    
    # Update the summary page outcome based on the chosen practice
    new_summaryrv <- unique(df_practice()$group_level1) %>% sort
    updateSelectInput(session, "summaryRV", "",
                      #choices = unique(df_outcome()$sample_year),
                      choices = new_summaryrv,
                      selected = ifelse(input$summaryRV %in% new_summaryrv, input$summaryRV, new_summaryrv[1])
    )
    
    # Cat statements are helpful debugging tools
    #cat(file = stderr(), 'tillage type should update\n\n')
    
    validate(
      need(df_practice()$filter1, 'no filter1'),
      need(input$Filter1, "no filter1")
    )
    # write special case for cover crop, since the first filter looks at 2 columns
    if(input$MgmtPractice == 'Cover crop'){
      new_filter1 <- c(unique(df_practice()$cc_group1), unique(df_practice()$cc_group2))
    } else {
      new_filter1 <- unique(df_practice()$filter1) %>% sort
    }
    
    # write special case for early season pest management, since we want filter1 to allow multiple selections
    if(input$MgmtPractice == 'Early Season Pest Management'){
      updateCheckboxGroupInput(session, 'Filter1', unique(df_practice()$filter1_name),
                               choices = new_filter1,
                               #as.character is needed when we have the filter1 is a factor (tillage)
                               selected = new_filter1#ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1)
                               )
    } else {
    
      #df_practice()$filter1_name is the same for all rows in a practice, so doing unique is just a way to grab one of them
      updateRadioButtons(session, 'Filter1', unique(df_practice()$filter1_name),
                         choices = new_filter1,
                         #as.character is needed when we have the filter1 is a factor (tillage)
                         selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, as.character(new_filter1[1])))
    }
    
    

  })
  
  
  
  # Filter2 changes between a checkboxGroupInput (multiple inputs) or a radioButton (single input) depending on the practice, requiring renderUI
  output$filter_two <- renderUI({
    new_filter2 <- sort(unique(as.character(df_filter1()$filter2)))
    # # Cat statements are helpful debugging tools
    # cat(file = stderr(), 'filter2 init selected is ', new_filter2[1], ". \n")
    if(input$MgmtPractice == 'Cover crop'){
      list(
        #pickerInput is a special input that's like a selectInput, but with the option to select multiple
          # We use it instead of a checkboxGroupInput since there are so many different Cover Crop Species, we didn't want it to crowd the sidebarPanel.
        shinyWidgets::pickerInput('Filter2', label = 'Cover Crop Species',
                           choices = new_filter2,
                           #as.character is needed when we have the filter2 is a factor (tillage)
                           selected = new_filter2,
                           multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             `select-all-text` = 'All Species',
                             `deselect-all-text` = "No Species",
                             size = 5
                           )
        )
        
      )
      
    } else if(input$MgmtPractice == 'Early Season Pest Management'){
      list(
        checkboxGroupInput('Filter2', unique(df_filter1()$filter2_name),
                   choices = new_filter2,
                   selected = new_filter2),
        
        # Separate Checkbox needed for the "All" option
       checkboxInput('AllPesticideSites', 'All Sites',
                     value = TRUE) 
      )
    } else {
      radioButtons('Filter2', unique(df_filter1()$filter2_name),
                   choices = new_filter2,
                   selected = new_filter2[1]
      )
    }
  })

  # Update 2nd tillage type based on first
  observeEvent(c(input$MgmtPractice, input$Filter1),{

    new_filter2 <- sort(unique(as.character(df_filter1()$filter2)))
    # Only change the selected inputs if we have to
    #&& is a hack to make sure null is evaluated correctly
     if(input$Filter2 %in% new_filter2 && !is.null(input$Filter2)){
       new_filter2_selected <- input$Filter2
     } else {
      new_filter2_selected <- new_filter2
    }

    # We account for the different Input styles of Filter1/Filter2 based on Practice
    if(input$MgmtPractice == 'Cover crop'){
      updatePickerInput(session, 'Filter2', unique(df_filter1()$filter2_name),
                               choices =  new_filter2,
                               selected = new_filter2_selected)
    } else if(input$MgmtPractice == "Early Season Pest Management"){
      updateCheckboxGroupInput(session, 'Filter2', unique(df_filter1()$filter2_name),
                               choices = new_filter2,
                               #as.character is needed when we have the filter2 is a factor (tillage)
                               selected = new_filter2_selected)
    } else {
      updateRadioButtons(session, 'Filter2', unique(df_filter1()$filter2_name),
                         choices = new_filter2,
                         #&& is a hack to make sure null is evaluated correctly
                         selected = new_filter2_selected[1]
                         )
    }

    # debugging tool
    #cat(file = stderr(), 'newfilter2 is ', paste(new_filter2, collapse = ','), '. \n')


  })


  # Update outcome on practices
    #if the old RV isn't an option in the new practice, this changes RV, which triggers change in df_outcome -> df_years
  observeEvent(c(input$MgmtPractice, input$Filter1, input$Filter2),{
      # cat statement is a debugging tool
      #cat(file = stderr(), 'outcome should be updated \n')
    new_outcomes <- unique(df_filter2()$group_level1) %>% sort
    updateRadioButtons(session, "RV", "Outcome",
                             choices = new_outcomes,
                              # ifelse below tries to keep the same inputs in the app wherever possible
                             selected = ifelse(input$RV %in% new_outcomes, input$RV, new_outcomes[1])
    )
  })



# Update year legend based on previous buttons
  #if  the old selected legend isn't an option in the new practice/outcomes, then this changes sample_year, triggeirng change in df_years
  observeEvent({
    df_depth()
    }, {

    # Cat statementes are debugging tools
    #cat(file = stderr(), input$RV, '\n')
    #cat(file = stderr(), unique(input$years), ': legend \n')

    #new_choices are groupings, which depend on the selected practice and depths (eg df_depth)
    new_choices <- unique(df_depth()$sample_year) %>% stringr::str_sort(na_last = TRUE, numeric = TRUE)
    validate(
      need(new_choices, 'There are no available years of implementation')
    )
    updateCheckboxGroupInput(session, "years", "Years of Implementation",
      choices = new_choices,
      #if groupings are the same as last groupings (old groupings are input$years + empty string for NA)
        #then keep the old groupings. if the groupings are new, just pick the first one
      selected = ifelse(input$years %in% c(new_choices, ""), input$years, new_choices[1])
    )
      #cat(file = stderr(), new_choices, ': df_practice$legend \n')
      #cat(file = stderr(), unique(df_depth()$sample_year), ':df_depth$legend \n')
  })

  # We decided to drop this Region selection on the map for now, because we're in the Midwest always.
  # observeEvent(df_map(), {
  #   updateSelectInput(session, "Region", "Location",
  #                            choices = unique(df_map()$Region),
  #                            selected = input$Region
  #   )
  # })


  # df_years() is what shows up in the forest plot
  observeEvent(c(input$Filter2,df_years()),{

    #cat(file = stderr(), 'nrow df_years = ', nrow(df_years()), '\n')
    
    
    # If there's no data in this final dataframe, then there's some kind of error in the selections.
      # We disable the update button and show an error message in this case.
    if(nrow(df_years()) == 0 | is.null(input$Filter2)){
      shinyjs::disable('update')
      shinyjs::show('no_data')
  
    }else{
      shinyjs::enable('update')
      shinyjs::hide('no_data')
    }

  })

  #show soil depth only if soil nutrients or other soil properties is selected
    #see this link for an idea of how to do bidirectional filtering
  # https://groups.google.com/forum/#!topic/shiny-discuss/Q1ZvnjDCzUM
  observeEvent(df_outcome(),{
    
    if(input$RV %in% c('Soil Nutrients', 'Other Soil Properties', 'Climate Mitigation')){
      validate(
        need(df_outcome()$sample_depth, 'no sample depth')
      )
      new_depths <- df_outcome()$sample_depth %>% 
        unique %>% 
        str_sort(numeric = TRUE) 
      # If soil surface is there, it will show up last in the sort, but we want it to be first (since it's 0 cm)
      if("Soil Surface" %in% new_depths){
        new_depths <- new_depths %>%
          setdiff("Soil Surface") %>%
          c("Soil Surface", .)
      }
       
      
      # # Cat statements are debugging messages
      # cat(file = stderr(), paste(new_depths, collapse = ','), '\n')
      
      updateCheckboxGroupInput(session, inputId = 'SoilDepth',
                               choices = c(new_depths),
                               selected = ifelse(input$SoilDepth %in% new_depths, input$SoilDepth, new_depths))

      shinyjs::show('SoilDepth')
      shinyjs::show('AllDepths')
    }
    else{
      shinyjs::hide('SoilDepth')
      shinyjs::hide('AllDepths')
    }
  })

  ### add "All/None" checkbox for the soil depths to select all/none
  observeEvent(input$AllDepths,{
    new_depths <- df_outcome()$sample_depth %>% unique %>% sort(na.last = TRUE) 
    updateCheckboxGroupInput(session, inputId = 'SoilDepth',
                        selected = if(input$AllDepths) new_depths else character(0)
                        )
    
  })
  
  ### add "All/None" checkbox for the early season pest management filter1 to select all/none
  observeEvent(input$AllPesticideTypes,{
    new_types <- df_practice()$filter1 %>% unique %>% sort(na.last = TRUE)
    updateCheckboxGroupInput(session, inputId = 'Filter1',
                             selected = if(input$AllPesticideTypes) new_types else character(0)
    )

  })
  
  ### add "All/None" checkbox for the early season pest management filter2 to select all/none
  observeEvent(input$AllPesticideSites,{
    new_sites <- df_filter1()$filter2 %>% unique %>% sort(na.last = TRUE)
    updateCheckboxGroupInput(session, inputId = 'Filter2',
                             selected = if(input$AllPesticideSites) new_sites else character(0)
    )
    
  })
  

  
  
  # Adds labels to the map
  observe({

    # Function to define multi-line labels
    labs <- lapply(seq(nrow(df_map())), function(i) {
      paste0(
        "State: ", df_map()[i, "State"], "<p>", #"County: ", df_map()[i, "NAME"],
        "<p>", "Treatment: ", df_map()[i, "Review"], "<p>", "DOI: ", df_map()[i, "DOI"]
      )
    })

    # Print map with points
    leafletProxy("map", data = df_map()) %>%
      clearShapes() %>%
      addMarkers(
        label = lapply(labs, HTML),
        clusterOptions = markerClusterOptions()
      )
  })



  # Build figure and map based on selected data

  # Add base map and set scale
  output$map <- renderLeaflet({
    leaflet(df_map()) %>%
      addTiles() %>%
      fitBounds(~ min(Longitude), ~ min(Latitude), ~ max(Longitude), ~ max(Latitude))
  })

  #this is a dataframe that WILL NOT change based on direct inputs, but only change when the update button is pressed
  #this shows all the data in the current plot, and is NOT sensitive to changes in the radio/select inputs
    #ie df_years() changes whenever an input changes (eg input$RV). But the plot doesn't change when input$RV changes.
    #df_plot will reflect the data in the plot, not the current df_years()
  #maybe isolate() is a more elegant way to do this, but I couldn't figure out how to use it on a reactive.
  df_plot <- eventReactive(input$update,{
    # # Cat statements are helpful debugging tools
    # cat(file = stderr(), "years has dim", dim(df_years()))
    # cat(file = stderr(), 'filter2 looks like ', unique(df_years()$filter2))
    
    # #can pick any element of Review, since it was already filtered by input$MgmtPratice.
      # I do it this way just to keep the dependency only on df_years
    practice <- df_years()$Review[1]
    filter1_selections <- df_years()$filter1 %>% unique
    pm1_all_choices <- summary_data %>%
      filter(Review == practice) %>%
      select(pm_group1) %>%
      unique %>%
      deframe
    
    # for practices where the user can select multiple inputs in filter1/filter2, we want to create a grouped mean for the different selections the user makes 
    # We want to group up Pesticide Type (eg filter1/pm_group1 for Early Season Pest Mgmt) ONLY if all pesticide types are selected
    if((input$MgmtPractice == 'Cover crop') | ((input$MgmtPractice == 'Early Season Pest Management') & (length(pm1_all_choices) == length(filter1_selections)))){
      df_years() %>%
        group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year) %>% 
        summarize(mean_per_change = mean(mean_per_change), 
                  sem_per_change = mean(sem_per_change), 
                  paper_id_list = paste(unique(paper_id_list), collapse = ";"), 
                  Trt_1name = Trt_1name[1], 
                  group_facet_level32 = group_facet_level32[1], 
                  Trt_2name = paste(Trt_2name, collapse = ","),
                  filter1 = paste(unique(filter1), collapse = ","),
                  filter2 = paste(unique(filter2), collapse = ","))
      # Early Season Pest Management is slightly different from Cover Crop because we don't want to group by filter1 (unless all filter1 is selected)
    } else if(input$MgmtPractice == "Early Season Pest Management") {
      df_years() %>%
        group_by(Review, group_level1, group_level2, group_level3, sample_depth, sample_year, filter1) %>%
          summarize(mean_per_change = mean(mean_per_change), 
                    sem_per_change = mean(sem_per_change), 
                    paper_id_list = paste(unique(paper_id_list), collapse = ";"), 
                    Trt_1name = Trt_1name[1], 
                    group_facet_level32 = group_facet_level32[1], 
                    Trt_2name = paste(Trt_2name, collapse = ","),
                    filter2 = paste(unique(filter2), collapse = ","))
        
    } else { # Cover Crop and Early season pest management are weird. If it isn't one of those two, just keep df_years the same.
      df_years()
    }
    
    
  })
  
  ## let's do the same "isolate" on a reactive with filter1. need this to get the choices for the title text in plot
  df_filter1_for_plot <- eventReactive(input$update, {
    df_filter1()
  })
  
  
  ##########################
  
  ### Forest Plot ###
  
  ##########################

  #forestplot will change whenever df_plot changes (so whenever the update button is pressed)
  # to achieve this, we need no calls to input$* in this function
  output$forestplot <- renderPlot({
    
    # Control text replaces the 0 point in our plot.
    control_text <- paste(unique(df_plot()$Trt_1name), collapse = 'and')

    #we use this dataframe to make sure that we only plot the control text on the bottom facet (not all the facets)
    control_labels <- data.frame(group_level2 = factor(tail(sort(df_plot()$group_level2),1),       #the facets are sorted alphabetically, so this pulls out the bottom one
                                                     levels = df_plot()$group_level2 %>% unique), #we set the levels just to get rid of some warnings
                                 group_facet_level32 = 0,                                    #group_facet_level32 is the x axis on the plot, so this will put the control text at x = 0
                                 mean_per_change = 0,                                      #mean_per_change is the y axis on the plot, so this plots the control text at y = 0
                                 sem_per_change = 0)                                       #this is just needed because the ggplot below uses this to calculate the error bars. number doesn't matter I think

    
    # we only want to make the plot react on the update button, so we gotta isolate all the inputs to kill reactivity
    practice <- isolate(input$MgmtPractice)
    filter1 <- isolate(input$Filter1)
    filter2_selected <- isolate(input$Filter2)
    # we change some text if ALL the possible pm1_choices are selected. 
      # filter2_selected gives us the ones that are selected. pm1_all_choices gives us all the options. Note we need to start all the way back with summary_data for this..
    pm1_all_choices <- summary_data %>%
      filter(Review == practice) %>%
      select(pm_group1) %>%
      unique %>%
      deframe
    filter2_all_choices <- unique(df_filter1_for_plot()$Trt_2name) %>% sort 
    
    #cat(stderr(), "\n pm1_all_chocies:", pm1_all_choices, "\n filter1:", isolate(input$Filter1))
    
    # This ugly control sequence write special cases for cover crop and Early Season Pest Management, where the user can select multiple Trt_2names
      # Maybe it could look a little better in a case_when, but I thought this was clearer
    if(practice %in% 'Cover crop'){
      if(length(filter2_all_choices) == length(filter2_selected)){
        title_text <- paste0('Effects of All Species compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and'))
      } else if (length(filter2_selected) > 1){
        title_text <- paste0('Effects of Multiple "', filter1 ,'" compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and'))
      } else{
        title_text <- paste0('Effects of ', paste(unique(df_plot()$Trt_2name), collapse = 'and'), ' compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and'))
      }
    } else if (practice %in% "Early Season Pest Management"){
      if(length(pm1_all_choices) == length(isolate(input$Filter1))){
        title_text <- paste0('Effects of All Pesticide Types compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and'))
      } else if (length(filter1) > 1) {
        title_text <- paste0('Effects of Multiple Pesticide Types compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and'))
      } else {
        title_text <- paste0("Effects of ", paste(unique(df_plot()$filter1), collapse = ","), " compared to ", paste(unique(df_plot()$Trt_1name), collapse = 'and'))
      }

    } else {
      title_text <- paste0('Effects of ', paste(unique(df_plot()$Trt_2name), collapse = 'and'), ' compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and'))
    }
    
    
    # Write special case for plot color variable (should be sample year for tillage, and pesticide type for early season pest management)
      # The others don't have color, but I'll make em sample_year to check that there aren't any rogue years in the rest of the data
         # this exploits the fact that if sample_year is totally NA, the all the points will be black.
      # We also don't want to color if all pesticide types are selected (since in this case, we consolidate all the different types into 1 mean)
    if(practice == "Early Season Pest Management" & length(pm1_all_choices) != length(isolate(input$Filter1))){
      color_var <- sym('filter1')
    } else if (practice == "Cover crop") {
      color_var <- sym("sample_depth")
    } else {
      color_var <- sym('sample_year')
    }
    
    

    ggplot(df_plot(), aes(group_facet_level32, mean_per_change, # remember that group_facet_level32 is the column ordered by group_level3 and group_level2
                      ymin = mean_per_change - sem_per_change,
                      ymax = mean_per_change + sem_per_change
    )) +
      scale_x_discrete("", breaks = df_plot()$group_facet_level32, label = df_plot()$group_level3) + # this line relabels the x from "group_facet_level32" to just "group_level2"
      geom_pointrange() +
      geom_errorbar(aes(
        ymin = mean_per_change - sem_per_change,
        ymax = mean_per_change + sem_per_change,
        width = .5
      )) +
      geom_hline(yintercept = 0, lty = 2) + # add a dotted line at x=0 after flip
      coord_flip(clip = "off") + # flip coordinates (puts labels on y axis)
      # clip = "off" allows for plot annotations outside the plot area (used for the control annotations below the x axis)
      labs(
        #adding the extra paste(unique(....., collapse = ..)) to catch cases where there are multiple trt_2name or trt_1name
          # this is only a problem when the user doesn't get to select Trt_1name (like Early Season pest management)
          # by default, if you have a vector in paste0, it will only pull out the first element.
        title = title_text,
        #  subtitle = df_plot()$group_level1[1],
        x = "",
        y = "Percent difference between treatment and control (%)"
      ) +
      # scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +
      theme_bw() +
      geom_point(aes(colour = !!color_var), size = 3) + # color labeling of fine level groupings
      scale_color_brewer(palette = "Set2") +          # change colors using RColorBrewer package to be ok for red-green colorblind
      # see all options using RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
      facet_grid(group_level2 ~ ., scales = "free", space = "free") +
      theme(
        legend.title = element_blank(), legend.position = "top",
        strip.text.y = element_text(angle = 0), text = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20)) #moves the x.axis down to make room for the control annotation
      ) +
      geom_text(data = control_labels, label = control_text, vjust = 2.75)    #vjust puts the text underneath the x axis
    #the control_labels dataframe specifies which facet to put the text, puts the text at the x axis (y=0), so vjust moves it down a little
    })

  
  
  # Text Description right below the forest plot. 
  # We aren't using this now (it's commented in ui.R)
  output$text_description <- renderText({
    if (input$MgmtPractice == "Cover crop") {
      "Cover crops in all areas, on average, are positively related to soil properties, but insignificantly related to crop yield."
      # if(df()$group_level1[1] == "Crop Production"){
      #   "This is the text we show for cover crop & crop production"
      # }
      # else if(df()$group_level1[1] =="Soil"){
      #   "This is the text we show for cover crop and soil"
      # }
      # else if ... ect ect
    }
    else {
      "Early season pest management is related to pests."
    }
  })

  #############################
  
  ### Tables (Reference tab / debugging tool) ###
  
  ##############################

  output$reference_table <- renderTable({
    # Get all the paper id's from the dataframe used to generate the plot
    plot_filtered_paper_id <- df_plot()$paper_id_list %>%
      lapply(function(x) strsplit(x, split = ";") %>%
               unlist %>%
               as.integer) %>%
      unlist %>%
      unique

    # Filter the references dataframe (defined in global.R) by the Paper id's in the plot,
      # and replace the paper_id's with an enumeration.
    references %>%
      select(-citation_short) %>%
      filter(Paper_id %in% plot_filtered_paper_id) %>%
      mutate(Paper_id = 1:nrow(.))
    # sanatize.text lets us render HTML, so that the doi's show up as a link.
  },sanitize.text.function = function(x) x)
  

  # Debugging tool to see current table used to generate plot. Might be commented out in ui.R
  output$current_table <- renderTable({
    df_plot()
  })
  
  
  ########################################
  
  ### Download Buttons ###
  
  ########################################

  # picks out the filtered data for download as a csv
  output$downloadData <- downloadHandler(
    filename = "filtered_app_data.csv",
    content = function(file) {
      #remove the filtering columns i made to make the filtering easier
      write.csv(select(df_plot(), -c(filter1, filter2, filter1_name, filter2_name)), file, row.names = FALSE)
    }
  )

  # picks out all data for download as a csv
    # this is pre-grouping
  output$downloadAllData <- downloadHandler(
    filename = "all_app_data.csv",
    content = function(file) {
      write.csv(raw_data, file, row.names = FALSE)
    }
  )

  # prints static figure
  output$downloadFigure <- downloadHandler(
    filename = 'figure.pdf',
    content = function(file){
      ggsave(file, width = 10, height = 15)
    }
  )

  
  #######################
  
  ### Update Button ###
  
  #######################
  # Updates when the user clicks the go button in the landing page
  observeEvent(input$go,{
    updateNavbarPage(session, 'navbar', select = 'Data')
    # delay makes it so that the renderUI statements have time to adjust
    delay(500,click('update'))
  })

  # This originally made sure that the app started with a plot. This is no longer necessary witht he addition of the landing page
  # observe({
  #   delay(500, click("update"))
  # 
  # })
  # 
  

}

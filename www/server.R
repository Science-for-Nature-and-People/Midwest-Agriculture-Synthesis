#### SERVER INSTRUCTIONS ####

# create the 'then" operator to chain tests together
`%then%` <- shiny:::`%OR%`

server <- function(input, output, session) {
  
  #lookup table for control descriptions
  # control_lookup <- data.frame(review_name = summary_data$Review %>% as.factor() %>% levels(), 
  #                              control_descrip = c("No Cover Crops", "No Pesticides", "Single Application", "Uniform Application", "Broadcast Application", "Applied to Surface", "Applied in Fall", "Applied Preplant"))
  # 
  
  
  
  
  # Reactive selection by management practice
    #we want this to update if user click a new practice
    
    #we also add new columns to the data frame to help us filter the app
    #need to deselect these columns before letting the user download the filtered data
  df_practice <- eventReactive(input$MgmtPractice, {
    cat(file = stderr(), 'df_practice is updated \n ')
    # filter dataset to display selected review and response variables
    
    filtered_by_practice <- summary_data %>%
      filter(Review %in% input$MgmtPractice)
    
    if(input$MgmtPractice == 'Cover crop'){
      filtered_by_practice %>%
        mutate(filter1 = Trt_1name,
               filter2 = Trt_2name,
               filter1_name = 'Cover Crop Mixture',
               filter2_name = 'Cover Crop Species')
    } else if(input$MgmtPractice == 'Tillage'){
      filtered_by_practice %>%
        mutate(filter1 = Trt_1name,
               filter2 = Trt_2name,
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
        mutate(filter1 = Trt_2name,
               filter2 = trt_specifics,
               filter1_name = 'Pesticide Type',
               filter2_name = 'Pesticide Application Site')
    }
       
  })
  
  
  #
  df_filter1 <- eventReactive(c(df_practice(), input$Filter1), {
    cat(file = stderr(), 'df_filter1 is updated\n')
    # if(input$MgmtPractice %in% c('Tillage', 'Cover crop')){
    #   df_practice() %>%
    #     filter(Trt_1name == input$Filter1)
    # }
    # else if(input$MgmtPractice == 'Nutrient Management'){
    #   df_practice() %>%
    #     filter(nutrient_groups == input$Filter1)
    # }
    # else if(input$MgmtPractice == 'Early Season Pest Management'){
    #   df_practice() %>%
    #     filter(Trt_2name == input$Filter1)
    # }
    df_practice() %>% 
      filter(filter1 == input$Filter1)
  })
  
  df_filter2 <- eventReactive(c(df_filter1(), input$Filter2), {
    cat(file = stderr(), 'dftillage2 is updated\n')
    # if(input$MgmtPractice %in% c('Tillage', 'Cover crop', 'Nutrient Management')){
    #   df_filter1() %>%
    #     filter(Trt_2name == input$Filter2)
    # }
    # else if(input$MgmtPractice == 'Early Season Pest Management'){
    #   df_filter1() %>%
    #     filter(trt_specifics == input$Filter2)
    # }
    df_filter1() %>%
      filter(filter2 == input$Filter2)
  })
  

  
  # Next tier selection of reactive selection of outcome grouping
    # we want this to update if the user clicks the update button, if they click a new outcome, or if they click a new practice (since outcome depends on practice)
  df_outcome <- eventReactive(c(df_filter2(),input$RV), {
    cat(file = stderr(), input$RV,'df_outcome is updated \n')
    df_filter2() %>%
      filter(group_level1 %in% input$RV) 
    
    
  })
  
  # filter by soil sampling depth (if outcome is Soil Nutrients or Other Soil Properties)
    # also check that there's at least one non-NA value in sample_depth
  df_depth <- eventReactive(c(df_outcome(), input$SoilDepth),{
    if(input$RV %in% c('Soil Nutrients', 'Other Soil Properties', 'Climate Mitigation') & !all(is.na(df_outcome()$sample_depth))){
        df_outcome() %>%
          filter(sample_depth %in% input$SoilDepth)
    }
    else{
      df_outcome()
    }
  })
  

  # filter by year of implementation
  df_years <- eventReactive(c(df_depth(), input$years), {
    
    
    cat(file = stderr(), 'df_years is updated \n \n')
    
    #year is tillage specific, so ignore it if management practice isn't tillage
    if(input$MgmtPractice == 'Tillage'){
      # filter dataset to display selected review and response variables
      df_depth() %>%
        filter(sample_year %in% input$years) %>%
        group_by(sample_year) %>%
        mutate(group_facet_level32 = fct_reorder(group_facet_level32, mean_per_change)) %>%
        ungroup()
    }
    else{
      df_depth()
    }
    
      
  })
  
  

  
  # Filter by geography (separate dataset for the map, filtered by above)
  df_map <- eventReactive(input$State,  {
    
    #pick out all the paper ids in the filtered dataset df_outcome() is prefiltered for practice/outcome
      #the filter command in the line below filters the "grouping" seleciton
    #paper_id_list is a string column, with comma delim lists of ints
      #so on each element/list in the column, we split the list on commas, then turn the list into a vector, convert the vector to a numeric one.
        #So now we have a list of numeric vectors. We turn this list into one long vector, and then pull out unique values
    filtered_paper_id <- (df_depth() %>% 
                            filter(sample_year %in% input$years))$paper_id_list %>%
      lapply(function(x) strsplit(x, split = ",") %>%
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
  })

  
  
  
  
  
  
  
  
  
  
  
  
  
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
    updateRadioButtons(session, "RV", "Outcome",
      choices = unique(df_filter2()$group_level1) %>% sort(),
      #selected = unique(df_practice()$group_level1)
      selected = input$summaryRV
    )
    
    # #change the filter1/2 options as well. this requires using the filter ordering review -> outcome -> filter1 -> filter2
    #   #which is different from the regular order review -> filter1 -> filter2 -> outcome
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
  
  #update summary outcome choices and tillage type based on practice
  observeEvent(input$MgmtPractice,{
    
    # Update the summary page outcome based on the chosen practice
    new_summaryrv <- unique(df_practice()$group_level1) %>% sort
    updateSelectInput(session, "summaryRV", "",
                      #choices = unique(df_outcome()$sample_year),
                      choices = new_summaryrv,
                      selected = ifelse(input$summaryRV %in% new_summaryrv, input$summaryRV, new_summaryrv[1])
    )
    
    cat(file = stderr(), 'tillage type should update\n\n')
    
    # validate(
    #   need(df_practice()$Trt_1name, 'no trt_1name')
    # )
    # if(input$MgmtPractice == 'Tillage'){
    #   new_filter1 <- unique(df_practice()$Trt_1name) %>% sort()
    #   updateRadioButtons(session, 'Filter1', 'Tillage Type #1',
    #                      choices = new_filter1,
    #                      selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1[1]))
    #   shinyjs::show('years')
    # }
    # else if(input$MgmtPractice == 'Cover crop'){
    #   new_filter1 <- unique(df_practice()$Trt_1name) %>% sort()
    #   updateRadioButtons(session, 'Filter1', 'Cover Crop Mixture',
    #                      choices = new_filter1,
    #                      selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1[1]))
    #   shinyjs::hide('years')
    # }
    # else if(input$MgmtPractice == 'Nutrient Management'){
    #   new_filter1 <- unique(df_practice()$nutrient_groups) %>% sort()
    #   updateRadioButtons(session, 'Filter1', 'Management Details',
    #                      choices = new_filter1,
    #                      selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1[1]))
    #   shinyjs::hide('years')
    # }
    # else if(input$MgmtPractice == 'Early Season Pest Management'){
    #   new_filter1 <- unique(df_practice()$Trt_2name) %>% sort()
    #   updateRadioButtons(session, 'Filter1', 'Pesticide Type',
    #                      choices = new_filter1,
    #                      selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1[1]))
    #   shinyjs::hide('years')
    # }
    
    validate(
      need(df_practice()$filter1, 'no filter1')
    )
    new_filter1 <- unique(df_practice()$filter1) %>% sort
    #df_practice()$filter1_name is the same for all rows in a practice, so doing unique is just a way to grab one of them
    updateRadioButtons(session, 'Filter1', unique(df_practice()$filter1_name),
                       choices = new_filter1,
                       selected = ifelse(input$Filter1 %in% new_filter1, input$Filter1, new_filter1[1]))
    
    #tillage only has the year filter
    if(input$MgmtPractice == 'Tillage'){
      shinyjs::show('years')
    } else{
      shinyjs::hide('years')
    }
    
    

  })
  

  #update 2nd tillage type based on first
  observeEvent(c(input$MgmtPractice, input$Filter1),{
    # validate(
    #   need(df_filter1()$Trt_2name, 'no filter2')
    # )
    # if(input$MgmtPractice == 'Tillage'){
    #   new_filter2 <- unique(df_filter1()$Trt_2name) %>% sort()
    #   updateRadioButtons(session, 'Filter2', 'Tillage Type #2',
    #                      choices = new_filter2,
    #                      selected = ifelse(input$Filter2 %in% new_filter2, input$Filter2, new_filter2[1]))
    # }
    # else if(input$MgmtPractice == 'Cover crop'){
    #   new_filter2 <- unique(df_filter1()$Trt_2name) %>% sort()
    #   updateRadioButtons(session, 'Filter2', 'Cover Crop Species',
    #                      choices = new_filter2,
    #                      selected = ifelse(input$Filter2 %in% new_filter2, input$Filter2, new_filter2[1])
    #                      )
    # }
    # else if(input$MgmtPractice == 'Nutrient Management'){
    #   new_filter2 <- unique(df_filter1()$Trt_2name) %>% sort()
    #   updateRadioButtons(session, 'Filter2', 'Application Specifics',
    #                      choices = new_filter2,
    #                      selected = ifelse(input$Filter2 %in% new_filter2, input$Filter2, new_filter2[1]))
    # }
    # else if(input$MgmtPractice == 'Early Season Pest Management'){
    #   new_filter2 <- unique(df_filter1()$trt_specifics) %>% sort()
    #   updateRadioButtons(session, 'Filter2', 'Pesticide Application Site',
    #                      choices = new_filter2,
    #                      selected = ifelse(input$Filter2 %in% new_filter2, input$Filter2, new_filter2[1]))
    # }
    
    #new_filter2 <- ifelse(input$MgmtPractice %in% 'Cover crop', c(sort(unique(df_filter1()$filter2)), 'All'), sort(unique(df_filter1()$filter2)))
    new_filter2 <- sort(unique(df_filter1()$filter2))
    updateRadioButtons(session, 'Filter2', unique(df_filter1()$filter2_name),
                       choices = new_filter2,
                       selected = ifelse(input$Filter2 %in% new_filter2, input$Filter2, new_filter2[1])
                       )
    
    cat(file = stderr(), 'newfilter2 is ', paste(new_filter2, collapse = ','), '\n')
    
    
  })

  #update outcome on practices
    #if the old RV isn't an option in the new practice, this changes RV, which triggers change in df_outcome -> df_years
  observeEvent(#df_practice(),
    c(input$MgmtPractice, input$Filter1, input$Filter2),{
      #cat(file = stderr(), 'outcome should be updated \n')
    new_outcomes <- unique(df_filter2()$group_level1) %>% sort
    updateRadioButtons(session, "RV", "Outcome",
                             choices = new_outcomes,
                             #selected = unique(df_practice()$group_level1)
                             selected = ifelse(input$RV %in% new_outcomes, input$RV, new_outcomes[1])
    )
  })



#update year legend based on previous buttons
  #if  the old selected legend isn't an option in thew new practice/outcomes, then this changes sample_year, triggeirng change in df_years
  observeEvent({
    df_outcome()
    #input$update
    }, {

    #cat(file = stderr(), input$RV, '\n')
    #cat(file = stderr(), unique(input$years), ': legend \n')

    #new_choices are groupings, which depend on the selected practice and outcomes (eg df_outcome)
    new_choices <- unique(df_outcome()$sample_year) %>% sort
    validate(
      need(new_choices, 'There are no available years of implementation')
    )
    updateCheckboxGroupInput(session, "years", "Years of Implementation",
      #choices = unique(df_outcome()$sample_year),
      choices = new_choices,
      #if groupings are the same as last groupings (old groupings are input$years)
        #then keep the old groupings. if the groupings are new, just pick the first one
      selected = ifelse(input$years %in% new_choices, input$years, new_choices[1])
    )
      #cat(file = stderr(), new_choices, ': df_practice$legend \n')
      #cat(file = stderr(), unique(df_outcome()$sample_year), ':df_outcome$legend \n')
  })

  # observeEvent(df_map(), {
  #   updateSelectInput(session, "Region", "Location",
  #                            choices = unique(df_map()$Region),
  #                            selected = input$Region
  #   )
  # })


  #df_years() is what shows up in the forest plot. We choose not to plot if there are less than 5 rows
  observeEvent(df_years(),{

    # otherwise we hide the error message
    #cat(file = stderr(), 'nrow df_years = ', nrow(df_years()), '\n')
    if(nrow(df_years()) == 0){
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
      new_depths <- df_outcome()$sample_depth %>% unique %>% sort
      cat(file = stderr(), paste(new_depths, collapse = ','), '\n')
      updateCheckboxGroupInput(session, inputId = 'SoilDepth',
                               choices = new_depths,
                               selected = ifelse(input$SoilDepth %in% new_depths, input$SoilDepth, new_depths))
      shinyjs::show('SoilDepth')
    }
    else{
      shinyjs::hide('SoilDepth')
    }
  })

  observe({

    # Function to define multi-line labels
    labs <- lapply(seq(nrow(df_map())), function(i) {
      paste0(
        "State: ", df_map()[i, "State"], "<p>", "County: ", df_map()[i, "NAME"],
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
  #maybe isolate() is a more elegant way to do this?
  df_plot <- eventReactive(input$update,{
    df_years()
  })

  #forestplot will change whenever makeplot changes (so whenever the update button is pressed)
  output$forestplot <- renderPlot({
    #control_text <- control_lookup[which(control_lookup$review_name == df_plot()$Review[1]),2]
    control_text <- paste(unique(df_plot()$Trt_1name), collapse = 'and')

    #we use this dataframe to make sure that we only plot the control text on the bottom facet (not all the facets)
    control_labels <- data.frame(group_level2 = factor(tail(sort(df_plot()$group_level2),1),       #the facets are sorted alphabetically, so this pulls out the bottom one
                                                     levels = df_plot()$group_level2 %>% unique), #we set the levels just to get rid of some warnings
                                 group_facet_level32 = 0,                                    #group_facet_level32 is the x axis on the plot, so this will put the control text at x = 0
                                 mean_per_change = 0,                                      #mean_per_change is the y axis on the plot, so this plots the control text at y = 0
                                 sem_per_change = 0)                                       #this is just needed because the ggplot below uses this to calculate the error bars. number doesn't matter I think


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
        title = paste0('Effects of ', paste(unique(df_plot()$Trt_2name), collapse = 'and'), ' compared to ', paste(unique(df_plot()$Trt_1name), collapse = 'and')),
        #  subtitle = df_plot()$group_level1[1],
        x = "",
        y = "Percent difference between treatment and control (%)"
      ) +
      # scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +
      theme_bw() +
      geom_point(aes(colour = sample_depth), size = 3) + # color labeling of fine level groupings
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

  #the text description at the bottom only depends on managementPractice
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



  output$reference_table <- renderTable({
    plot_filtered_paper_id <- df_plot()$paper_id_list %>%
      lapply(function(x) strsplit(x, split = ";") %>%
               unlist %>%
               as.integer) %>%
      unlist %>%
      unique

    references %>%
      select(-citation_short) %>%
      filter(Paper_id %in% plot_filtered_paper_id)
  },sanitize.text.function = function(x) x)
  
  
  output$current_table <- renderTable({
    df_plot()
  })
  
  
  

  # picks out the filtered data for download as a csv
  output$downloadData <- downloadHandler(
    filename = "filtered_app_data.csv",
    content = function(file) {
      #remove the filtering columns i made to make the filtering easier
      write.csv(select(df_plot(), -c(filter1, filter2, filter1_name, filter2_name)), file, row.names = FALSE)
    }
  )

  # picks out all data for download as a csv
  output$downloadAllData <- downloadHandler(
    filename = "all_app_data.csv",
    content = function(file) {
      write.csv(summary_data, file, row.names = FALSE)
    }
  )

  # prints static figure
  output$downloadFigure <- downloadHandler(
    filename = 'figure.pdf',
    content = function(file){
      ggsave(file, width = 10, height = 15)
    }
  )

  observeEvent(input$go,{
    updateNavbarPage(session, 'navbar', select = 'Data')
    click('update')
  })

  observe({
    # Will click the update button at the start so the app starts with a plot.
    click("update")


  })
}

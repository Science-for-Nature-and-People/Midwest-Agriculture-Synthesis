#### SERVER INSTRUCTIONS ####

# create the 'then" operator to chain tests together
`%then%` <- shiny:::`%OR%`

server <- function(input, output, session) {
  
  #lookup table for control descriptions
  control_lookup <- data.frame(review_name = tillage_results$Review %>% as.factor() %>% levels(), 
                               control_descrip = c("No Cover Crops", "No Pesticides", "Single Application", "Uniform Application", "Broadcast Application", "Applied to Surface", "Applied in Fall", "Applied Preplant"))
  
  
  # Reactive selection by management practice
    #we want this to update if the user clicks the update button, or if they click a new practice
  df_practice <- eventReactive(input$MgmtPractice, {
    #cat(file = stderr(), 'df_practice is updated \n ')
    # filter dataset to display selected review and response variables
    tillage_results %>%
      filter(Review %in% input$MgmtPractice)
    
    
  })
  
  #
  df_tillage1 <- eventReactive(c(df_practice(), input$TillageType1), {
    df_practice() %>%
      filter(Trt_1name == input$TillageType1)
  })
  
  df_tillage2 <- eventReactive(c(df_tillage1(), input$TillageType2), {
    df_tillage1() %>%
      filter(Trt_2name == input$TillageType2)
  })
  

  
  # Next tier selection of reactive selection of outcome grouping
    # we want this to update if the user clicks the update button, if they click a new outcome, or if they click a new practice (since outcome depends on practice)
  df_outcome <- eventReactive(c(df_tillage2(),input$RV), {
    #cat(file = stderr(), input$RV,'df_outcome is updated \n')
    df_tillage2() %>%
      filter(group_level1 %in% input$RV) 
    
    
  })
  
  # filter by soil sampling depth (if outcome is Soil Nutrients or Other Soil Properties)
  df_depth <- eventReactive(c(df_outcome(), input$SoilDepth),{
    if(input$RV %in% c('Soil Nutrients', 'Other Soil Properties')){
      df_outcome() %>%
        filter(sample_depth %in% input$SoilDepth)
    }
    else{
      df_outcome()
    }
  })
  

  # filter by year of implementation
  df_years <- eventReactive(c(df_depth(), input$years), {
    
    
    #cat(file = stderr(), 'df_years is updated \n \n')
    # filter dataset to display selected review and response variables
    df_depth() %>%
      filter(sample_year %in% input$years) %>%
      group_by(sample_year) %>%
      mutate(group_facet_level32 = fct_reorder(group_facet_level32, mean_per_change)) %>%
      ungroup() %>%
      filter(num_comparisons > 4 & sem_per_change != 0 & sem_actual_diff != 0)
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
                             choices = unique(tillage_results$Review) %>% sort(),
                             selected = input$summaryPractice
    )
  })
  
  #update Outcome on summary choice
    #this changes RV, which triggers change in df_outcome -> df_years
  observeEvent(input$summaryRV, {
    updateRadioButtons(session, "RV", "Outcome",
      choices = unique(df_practice()$group_level1) %>% sort(),
      #selected = unique(df_practice()$group_level1)
      selected = input$summaryRV
    )
  })
  
  #update outcome on practices
    #if the old RV isn't an option in the new practice, this changes RV, which triggers change in df_outcome -> df_years
  observeEvent(#df_practice(),
    input$MgmtPractice,{
    new_outcomes <- unique(df_practice()$group_level1) %>% sort
    updateRadioButtons(session, "RV", "Outcome",
                             choices = new_outcomes,
                             #selected = unique(df_practice()$group_level1)
                             selected = ifelse(input$RV %in% new_outcomes, input$RV, new_outcomes[1]) 
    )
  })
  
  #update 2nd tillage type based on first
  observeEvent(input$TillageType1,{
    new_tillage_types <- unique(df_tillage1()$Trt_2name) %>% sort
    updateRadioButtons(session, inputId = 'TillageType2',
                       choices = new_tillage_types,
                       selected = ifelse(input$TillageType2 %in% new_tillage_types, input$TillageType2, new_tillage_types[1]))
    
  })

#update year legend based on the other two choices
  #if  the old selected legend isn't an option in thew new practice/outcomes, then this changes sample_year, triggeirng change in df_years
  observeEvent({
    c(input$MgmtPractice, input$RV)
    #input$update
    }, {
    
    #cat(file = stderr(), input$RV, '\n')    
    #cat(file = stderr(), unique(input$years), ': legend \n')  
      
    #new_choices are groupings, which depend on the selected practice and outcomes (eg df_outcome)
    new_choices <- unique(df_outcome()$sample_year)
    updateCheckboxGroupInput(session, "sample_year", "Grouping",
      #choices = unique(df_outcome()$sample_year),
      choices = new_choices,
      #if groupings are the same as last groupings (old groupings are input$years)
        #then keep the old groupings. if the groupings are new, just pick the first one
      selected = ifelse(input$years %in% new_choices, input$years, new_choices[1]) 
    )
      #cat(file = stderr(), new_choices, ': df_practice$legend \n')
      #cat(file = stderr(), unique(df_outcome()$sample_year), ':df_outcome$legend \n')
      
      
    # Update the summary page outcome based on the chosen practice
      updateSelectInput(session, "summaryRV", "",
                               #choices = unique(df_outcome()$sample_year),
                               choices = unique(df_practice()$group_level1) %>% sort,
                               selected = input$summaryRV # add [1] to select option in list, remove (as is) for Default is select all options
      ) 
  })
  
  # observeEvent(df_map(), {
  #   updateSelectInput(session, "Region", "Location",
  #                            choices = unique(df_map()$Region),
  #                            selected = input$Region
  #   )
  # })
  
  
  # #df_years() is what shows up in the forest plot. We choose not to plot if there are less than 5 rows
  # observeEvent(df_years(),{
  #   #count the number of observations in our final dataset.
  # #if there are less than 5 rows, then we disable the update button and show an error message
  #   # otherwise we hide the error message
  #   #cat(file = stderr(), 'nrow df_years = ', nrow(df_years()), '\n')
  #   if(nrow(df_years()) < 5){
  #     shinyjs::disable('update')
  #     shinyjs::show('no_data')
  #   }else{
  #     shinyjs::enable('update')
  #     shinyjs::hide('no_data')
  #   }
  #   
  # })
  
  #show soil depth only if soil nutrients or other soil properties is selected
    #see this link for an idea of how to do bidirectional filtering
  # https://groups.google.com/forum/#!topic/shiny-discuss/Q1ZvnjDCzUM
  observeEvent(df_outcome(),{
    if(input$RV %in% c('Soil Nutrients', 'Other Soil Properties')){
      new_depths <- df_outcome()$sample_depth %>% unique %>% sort
      updateCheckboxGroupInput(session, inputId = 'SoilDepth',
                               choices = new_depths,
                               selected = ifelse(input$SoilDepth %in% new_depths, input$SoilDepth, new_depths[1]))
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
  df_plot <- eventReactive(input$update,{
    df_years()
  })
  
  #forestplot will change whenever makeplot changes (so whenever the update button is pressed)
  output$forestplot <- renderPlot({
    #control_text <- control_lookup[which(control_lookup$review_name == df_plot()$Review[1]),2]
    control_text <- input$TillageType1
    
    #we use this dataframe to make sure that we only plot the control text on the bottom facet (not all the facets)
    control_labels <- data.frame(group_level2 = factor(tail(sort(df_plot()$group_level2),1),       #the facets are sorted alphabetically, so this pulls out the bottom one
                                                     levels = df_plot()$group_level2 %>% unique), #we set the levels just to get rid of some warnings
                                 group_facet_level32 = 0,                                    #group_facet_level32 is the x axis on the plot, so this will put the control text at x = 0
                                 mean_per_change = 0,                                      #mean_per_change is the y axis on the plot, so this plots the control text at y = 0
                                 sem_per_change = 0)                                       #this is just needed because the ggplot below uses this to calculate the error bars. number doesn't matter I think
    
    
    ggplot(df_plot(), aes(group_facet_level32, mean_per_change, # remember that group_facet_level32 is the column ordered by group_level2 and group_metric
                      ymin = mean_per_change - sem_per_change,
                      ymax = mean_per_change + sem_per_change
    )) +
      scale_x_discrete("", breaks = df_plot()$group_facet_level32, label = df_plot()$group_level3) + # this line relabels the x from "group_metric_group_level2" to just "group_metric"
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
        title = df_plot()$Review[1], # since we are filtering tillage_results to only have 1 value for review/group_level1, we can take any element as the label (they should all be the same)
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
    #the control_lables dataframe specifies which facet to put the text, puts the text at the x axis (y=0), so vjust moves it down a little
    })

  #the text description at the bottom only depends on managementPractice
  output$text_description <- renderText({
    if (input$MgmtPractice == "Cover Crops") {
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
    
  # picks out the filtered data for download as a csv
  output$downloadData <- downloadHandler(
    filename = "filtered_app_data.csv",
    content = function(file) {
      write.csv(df_plot(), file, row.names = FALSE)
    }
  )
  
  # picks out all data for download as a csv
  output$downloadAllData <- downloadHandler(
    filename = "all_app_data.csv",
    content = function(file) {
      write.csv(tillage_results, file, row.names = FALSE)
    }
  )
  
  # prints static figure
  output$downloadFigure <- downloadHandler(
    filename = 'figure.pdf',
    content = function(file){
      ggsave(file, width = 10, height = 15)
    }
  )
  
  output$intro <- renderText({
    'I want to know the impact of '
    })

  observeEvent(input$go,{
    updateNavbarPage(session, 'navbar', select = 'Data')
    click('update')
  })
  
  observe({
    # Will click the update button at the start so the app starts with a plot.
    click("update")
  
    
  })
}
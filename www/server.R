#### SERVER INSTRUCTIONS ####

# create the 'then" operator to chain tests together
`%then%` <- shiny:::`%OR%`

server <- function(input, output, session) {
  
  #lookup table for control descriptions
  control_lookup <- data.frame(review_name = summary_all$Review %>% as.factor() %>% levels(), 
                               control_descrip = c("No Cover Crops", "No Pesticides", "Single Application", "Uniform Application", "Broadcast Application", "Applied to Surface", "Applied in Fall", "Applied Preplant"))
  
  
  # Reactive selection by management practice
    #we want this to update if the user clicks the update button, or if they click a new practice
  df0 <- eventReactive(input$MgmtPractice, {
    #cat(file = stderr(), 'df0 is updated \n ')
    # filter dataset to display selected review and response variables
    summary_all %>%
      filter(Review %in% input$MgmtPractice)
    
    
  })

  # Next tier selection of reactive selection of outcome grouping
    # we want this to update if the user clicks the update button, if they click a new outcome, or if they click a new practice (since outcome depends on practice)
  df1 <- eventReactive(c(df0(),input$RV), {
    #cat(file = stderr(), 'df1 is updated \n')
    df0() %>%
      filter(Group_RV %in% input$RV) 
    
    
  })

  # Merge by cover crop type
  df2 <- eventReactive(c(df1(), input$Legend_1), {
    
    
    #cat(file = stderr(), 'df2 is updated \n \n')
    # filter dataset to display selected review and response variables
    df1() %>%
      filter(Legend_1 %in% input$Legend_1) %>%
      group_by(Legend_1) %>%
      mutate(group_metric_facet = fct_reorder(group_metric_facet, mean_per_change1)) %>%
      ungroup()
    
  })

  
  # Filter by geography
  df3 <- eventReactive(input$State,  {
    
    #pick out all the paper ids in the filtered dataset df1() is prefiltered for practice/outcome
      #the filter command in the line below filters the "grouping" seleciton
    #paper_id_list1 is a string column, with comma delim lists of ints
      #so on each element/list in the column, we split the list on commas, then turn the list into a vector, convert the vector to a numeric one.
        #So now we have a list of numeric vectors. We turn this list into one long vector, and then pull out unique values
    filtered_paper_id <- (df1() %>% 
                            filter(Legend_1 %in% input$Legend_1))$paper_id_list1 %>%
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
    #this changes MgmtPractice, triggering change in df0 -> df1 -> df2
  observeEvent(input$summaryPractice, {
    updateRadioButtons(session, "MgmtPractice", "Practice",
                             choices = unique(summary_all$Review) %>% sort(),
                             selected = input$summaryPractice
    )
  })
  
  #update Outcome on summary choice
    #this changes RV, which triggers change in df1 -> df2
  observeEvent(input$summaryRV, {
    updateCheckboxGroupInput(session, "RV", "Outcome",
      choices = unique(df0()$Group_RV) %>% sort(),
      #selected = unique(df0()$Group_RV)
      selected = input$summaryRV
    )
  })
  
  #update outcome on practices
    #if the old RV isn't an option in the new practice, this changes RV, which triggers change in df1 -> df2
  observeEvent(#df0(),
    input$MgmtPractice,{
    new_outcomes <- unique(df0()$Group_RV) %>% sort
    updateCheckboxGroupInput(session, "RV", "Outcome",
                             choices = new_outcomes,
                             #selected = unique(df0()$Group_RV)
                             selected = ifelse(input$RV %in% new_outcomes, input$RV, new_outcomes[1]) 
    )
  })

#update lagend based on the other two choices
  #if  the old selected legend isn't an option in thew new practice/outcomes, then this changes legend_1, triggeirng change in df2
  observeEvent({
    c(input$MgmtPractice, input$RV)
    #input$update
    }, {
    
    #cat(file = stderr(), input$RV, '\n')    
    #cat(file = stderr(), unique(input$Legend_1), ': legend \n')  
      
    #new_choices are groupings, which depend on the selected practice and outcomes (eg df1)
    new_choices <- unique(df1()$Legend_1)
    updateCheckboxGroupInput(session, "Legend_1", "Grouping",
      #choices = unique(df1()$Legend_1),
      choices = new_choices,
      #if groupings are the same as last groupings (old groupings are input$Legend_1)
        #then keep the old groupings. if the groupings are new, just pick the first one
      selected = ifelse(input$Legend_1 %in% new_choices, input$Legend_1, new_choices[1]) 
    )
      #cat(file = stderr(), new_choices, ': df0$legend \n')
      #cat(file = stderr(), unique(df1()$Legend_1), ':df1$legend \n')
      
      
    # Update the summary page outcome based on the chosen practice
      updateSelectInput(session, "summaryRV", "",
                               #choices = unique(df1()$Legend_1),
                               choices = unique(df0()$Group_RV) %>% sort,
                               selected = input$summaryRV # add [1] to select option in list, remove (as is) for Default is select all options
      ) 
  })
  
  # observeEvent(df3(), {
  #   updateSelectInput(session, "Region", "Location",
  #                            choices = unique(df3()$Region),
  #                            selected = input$Region
  #   )
  # })
  
  
  #df2() is what shows up in the forest plot. We choose not to plot if there are less than 5 rows
  observeEvent(df2(),{
    #count the number of observations in our final dataset.
  #if there are less than 5 rows, then we disable the update button and show an error message
    # otherwise we hide the error message
    #cat(file = stderr(), 'nrow df2 = ', nrow(df2()), '\n')
    if(nrow(df2()) < 5){
      shinyjs::disable('update')
      shinyjs::show('no_data')
    }else{
      shinyjs::enable('update')
      shinyjs::hide('no_data')
    }
    
  })

  observe({

    # Function to define multi-line labels
    labs <- lapply(seq(nrow(df3())), function(i) {
      paste0(
        "State: ", df3()[i, "State"], "<p>", "County: ", df3()[i, "NAME"],
        "<p>", "Treatment: ", df3()[i, "Review"], "<p>", "DOI: ", df3()[i, "DOI"]
      )
    })

    # Print map with points
    leafletProxy("map", data = df3()) %>%
      clearShapes() %>%
      addMarkers(
        label = lapply(labs, HTML),
        clusterOptions = markerClusterOptions()
      )
  })



  # Build figure and map based on selected data

  # Add base map and set scale
  output$map <- renderLeaflet({
    leaflet(df3()) %>%
      addTiles() %>%
      fitBounds(~ min(Longitude), ~ min(Latitude), ~ max(Longitude), ~ max(Latitude))
  })
  
  #this is a dataframe that WILL NOT change based on direct inputs, but only change when the update button is pressed
  #this shows all the data in the current plot, and is NOT sensitive to changes in the radio/select inputs
    #ie df2() changes whenever an input changes (eg input$RV). But the plot doesn't change when input$RV changes. 
    #df_plot will reflect the data in the plot, not the current df2()
  df_plot <- eventReactive(input$update,{
    df2()
  })
  
  #forestplot will change whenever makeplot changes (so whenever the update button is pressed)
  output$forestplot <- renderPlot({
    control_text <- control_lookup[which(control_lookup$review_name == df_plot()$Review[1]),2]
    
    #we use this dataframe to make sure that we only plot the control text on the bottom facet (not all the facets)
    control_labels <- data.frame(main_group = factor(tail(sort(df_plot()$main_group),1),       #the facets are sorted alphabetically, so this pulls out the bottom one
                                                     levels = df_plot()$main_group %>% unique), #we set the levels just to get rid of some warnings
                                 group_metric_facet = 0,                                    #group_metric_facet is the x axis on the plot, so this will put the control text at x = 0
                                 mean_per_change1 = 0,                                      #mean_per_change1 is the y axis on the plot, so this plots the control text at y = 0
                                 sem_per_change1 = 0)                                       #this is just needed because the ggplot below uses this to calculate the error bars. number doesn't matter I think
    
    
    ggplot(df_plot(), aes(group_metric_facet, mean_per_change1, # remember that group_metric_facet is the column ordered by main_group and group_metric
                      ymin = mean_per_change1 - sem_per_change1,
                      ymax = mean_per_change1 + sem_per_change1
    )) +
      scale_x_discrete("", breaks = df_plot()$group_metric_facet, label = df_plot()$group_metric) + # this line relabels the x from "group_metric_main_group" to just "group_metric"
      geom_pointrange() +
      geom_errorbar(aes(
        ymin = mean_per_change1 - sem_per_change1,
        ymax = mean_per_change1 + sem_per_change1,
        width = .5
      )) +
      geom_hline(yintercept = 0, lty = 2) + # add a dotted line at x=0 after flip
      coord_flip(clip = "off") + # flip coordinates (puts labels on y axis)
      # clip = "off" allows for plot annotations outside the plot area (used for the control annotations below the x axis)
      labs(
        title = df_plot()$Review[1], # since we are filtering summary_all to only have 1 value for review/group_rv, we can take any element as the label (they should all be the same)
        #  subtitle = df_plot()$Group_RV[1],
        x = "",
        y = "Percent difference between treatment and control (%)"
      ) +
      # scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +
      theme_bw() +
      geom_point(aes(colour = Legend_1), size = 3) + # color labeling of fine level groupings
      scale_color_brewer(palette = "Set2") +          # change colors using RColorBrewer package to be ok for red-green colorblind
      # see all options using RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
      facet_grid(main_group ~ ., scales = "free", space = "free") +
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
      # if(df()$Group_RV[1] == "Crop Production"){
      #   "This is the text we show for cover crop & crop production"
      # }
      # else if(df()$Group_RV[1] =="Soil"){
      #   "This is the text we show for cover crop and soil"
      # }
      # else if ... ect ect
    }
    else {
      "Early season pest management is related to pests."
    }
  })
  
  
  
  output$reference_table <- renderTable({
    plot_filtered_paper_id <- df_plot()$paper_id_list1 %>% 
      lapply(function(x) strsplit(x, split = ",") %>%
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
      write.csv(summary_all, file, row.names = FALSE)
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
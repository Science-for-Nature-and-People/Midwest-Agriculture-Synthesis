#### SERVER INSTRUCTIONS ####
server <- function(input, output, session) {
  
  #lookup table for control descriptions
  control_lookup <- data.frame(review_name = summary_all$Review %>% as.factor() %>% levels(), 
                               control_descrip = c("No Cover Crops", "No Pesticides", "Single Application", "Uniform Application", "Broadcast Application", "Applied to Surface", "Applied in Fall", "Applied Preplant"))
  
  
  # Reactive selection by management practice
  df0 <- eventReactive(c(input$update, input$MgmtPractice), {

    # filter dataset to display selected review and response variables
    summary_all %>%
      filter(Review %in% input$MgmtPractice)
    
  })

  # Next tier selection of reactive selection of outcome grouping
  df1 <- eventReactive(c(input$update, input$RV), {
    df0() %>%
      filter(Group_RV %in% input$RV) 
      
  })

  # Merge by cover crop type
  df2 <- eventReactive(input$update, {

    # filter dataset to display selected review and response variables
    df1() %>%
      filter(Legend_1 %in% input$Legend_1) %>%
      group_by(Legend_1) %>%
      mutate(group_metric_facet = fct_reorder(group_metric_facet, mean_per_change1)) %>%
      ungroup()
  })

  
  # Filter by geography
  df3 <- eventReactive(c(input$update, input$State),  {
    
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
      #filter((State %in% input$State) & (Paper_id %in% filtered_paper_id))
      filter((Region %in% input$Region) & (Paper_id %in% filtered_paper_id))
  })

  #update practice on summary choice
  observeEvent(input$summaryPractice, {
    updateRadioButtons(session, "MgmtPractice", "Practice",
                             choices = unique(summary_all$Review) %>% sort(),
                             selected = input$summaryPractice
    )
  })
  
  #update Outcome on summary choice
  observeEvent(input$summaryRV, {
    updateCheckboxGroupInput(session, "RV", "Outcome",
      choices = unique(df0()$Group_RV) %>% sort(),
      #selected = unique(df0()$Group_RV)
      selected = input$summaryRV
    )
  })
  
  #update outcome on practices
  observeEvent(df0(), {
    updateCheckboxGroupInput(session, "RV", "Outcome",
                             choices = unique(df0()$Group_RV) %>% sort(),
                             #selected = unique(df0()$Group_RV)
                             selected = input$RV
    )
  })

  observeEvent({
    df0()
    #df1()
    #input$update
    }, {
    
    #cat(file = stderr(), unique(input$Legend_1), '\n')    
      
    #new_choices is based on the selected practice (eg df0)
    new_choices <- unique(df0()$Legend_1)
    updateCheckboxGroupInput(session, "Legend_1", "Grouping",
      #choices = unique(df1()$Legend_1),
      choices = new_choices,
      #if groupings are the same as last groupings (old groupings are input$Legend_1)
        #then keep the old groupings. if the groupings are new, just pick the first one
      selected = ifelse(input$Legend_1 %in% new_choices, input$Legend_1, new_choices[1]) 
    )
      #cat(file = stderr(), unique(input$Legend_1), ': legend \n')  
      
      
      updateSelectInput(session, "summaryRV", "",
                               #choices = unique(df1()$Legend_1),
                               choices = unique(df0()$Group_RV) %>% sort,
                               selected = input$summaryRV # add [1] to select option in list, remove (as is) for Default is select all options
      ) 
  })
  
  observeEvent(df3(), {
    updateSelectInput(session, "Region", "Location",
                             choices = unique(df3()$Region),
                             selected = unique(df3()$Region)[1]
    )
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
  

  output$forestplot <- renderPlot({
    #control_text should hold the control name
    control_text <- control_lookup[which(control_lookup$review_name == df2()$Review[1]),2]
    
    #we use this dataframe to make sure that we only plot the control text on the bottom facet (not all the facets)
    control_labels <- data.frame(main_group = factor(tail(sort(df2()$main_group),1),       #the facets are sorted alphabetically, so this pulls out the bottom one
                                                     levels = df2()$main_group %>% unique), #we set the levels just to get rid of some warnings
                                 group_metric_facet = 0,                                    #group_metric_facet is the x axis on the plot, so this will put the control text at x = 0
                                 mean_per_change1 = 0,                                      #mean_per_change1 is the y axis on the plot, so this plots the control text at y = 0
                                 sem_per_change1 = 0)                                       #this is just needed because the ggplot below uses this to calculate the error bars. number doesn't matter I think

    
    ggplot(df2(), aes(group_metric_facet, mean_per_change1, # remember that group_metric_facet is the column ordered by main_group and group_metric
      ymin = mean_per_change1 - sem_per_change1,
      ymax = mean_per_change1 + sem_per_change1
    )) +
      scale_x_discrete("", breaks = df2()$group_metric_facet, label = df2()$group_metric) + # this line relabels the x from "group_metric_main_group" to just "group_metric"
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
        title = df2()$Review[1], # since we are filtering summary_all to only have 1 value for review/group_rv, we can take any element as the label (they should all be the same)
        #  subtitle = df2()$Group_RV[1],
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

  output$text_description <- renderText({
    if (df2()$Review[1] == "Cover Crop") {
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
  
  #add reference table, filtered by the plot filters.
  output$reference_table <- renderTable({
    
    plot_filtered_paper_id <- df2()$paper_id_list1 %>% 
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
      write.csv(df2(), file, row.names = FALSE)
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
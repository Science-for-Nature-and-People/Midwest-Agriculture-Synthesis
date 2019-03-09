#### SERVER INSTRUCTIONS ####
server <- function(input, output, session) {
  
  cdata <- session$clientData
  
  # Reactive selection by management practice
  df0 <- eventReactive(input$MgmtPractice, {

    # filter dataset to display selected review and response variables
    summary_all %>%
      filter(Review %in% input$MgmtPractice)
  })

  # Next tier selection of reactive selection of outcome grouping
  df1 <- eventReactive(input$RV, {
    df0() %>%
      filter(Group_RV %in% input$RV)
  })

  # Merge by cover crop type
  df2 <- eventReactive(input$update, {

    # filter dataset to display selected review and response variables
    df1() %>%
      filter(Legend_1 %in% input$Legend_1) %>%
      group_by(Legend_1) %>%
      mutate(group_metric_facet = fct_reorder(group_metric_facet, mean_per_change1))
  })

  # Filter by geography
  df3 <- eventReactive(input$State, {
    map.data %>%
      filter(State %in% input$State)
  })

  observeEvent(df0(), {
    updateSelectInput(session, "RV", "Outcome",
      choices = unique(df0()$Group_RV),
      selected = unique(df0()$Group_RV)[1]
    )
  })

  observeEvent(df1(), {
    updateSelectInput(session, "Legend_1", "Grouping",
      choices = unique(df1()$Legend_1),
      selected = unique(df1()$Legend_1) # add [1] to select option in list, remove (as is) for Default is select all options
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
    leaflet(map.data) %>%
      addTiles() %>%
      fitBounds(~ min(Longitude), ~ min(Latitude), ~ max(Longitude), ~ max(Latitude))
  })

  output$forestplot <- renderPlotly({
    p <- ggplot(df2(), aes(group_metric_facet, mean_per_change1, # remember that group_metric_facet is the column ordered by main_group and group_metric
      ymin = mean_per_change1 - sem_per_change1,
      ymax = mean_per_change1 + sem_per_change1,
      text = sprintf("Number of Papers: %s<br>Number of Comparisons: %s<br>Mean: %s<br>SE: %s", num_papers1, num_comparisons1, round(mean_per_change1,2), round(sem_per_change1,2))  #text will be hoverlabel
    )) +
      scale_x_discrete("", breaks = df2()$group_metric_facet, label = df2()$group_metric) + # this line relabels the x from "group_metric_main_group" to just "group_metric"
      geom_pointrange() +
      geom_errorbar(aes(
        ymin = mean_per_change1 - sem_per_change1,
        ymax = mean_per_change1 + sem_per_change1,
        width = 2, #controls whisker length
        size = 1   #controls line width
      )) +
      geom_hline(yintercept = 0, lty = 2) + # add a dotted line at x=0 after flip
      coord_flip() + # flip coordinates (puts labels on y axis)
      labs(
        title = df2()$Review[1], # since we are filtering summary_all to only have 1 value for review/group_rv, we can take any element as the label (they should all be the same)
        #  subtitle = df2()$Group_RV[1],
        x = "",
        y = "Percent difference between treatment and control (%)"
      ) +
      # scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +
      theme_bw() +
      geom_point(aes(colour = Legend_1),
                 size = 0.5) + # color labeling of fine level groupings
      facet_grid(main_group ~ ., scales = "free", space = "free") +
      theme(
        plot.title = element_text(size = 13),                     #changes plot title size
        legend.title = element_blank(), legend.position = "top",  # moves the legend
        strip.text.y = element_text(angle = 0, size = rel(1.5)),  # turns the facet names horizontal
        text = element_text(size = 7),
        strip.background = element_blank(), #element_rect(fill = "white", color = NULL, size = 50),
        strip.placement = "outside"         #meant to add spacing between facet names and plot, but not working in plotly.
      )
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(b = 0, l = 110, r= 100), #bottom, left, and right respectively
             legend = list(y = 1.11, font = list(size=10), orientation = "h", xanchor = "left", linetype = "solid"),
             hoverlabel = list(font = list(size = 10)), #size will change font of text inside
             yaxis = list(scaleanchor = "x")) #this is an attempt to imiate "scales = free" in the ggplot, but it's not working
                
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
    ggsave("figure.pdf")
  )

  # Will click the update button at the start so the app starts with a plot.
  observe({
    click("update")
  })
}
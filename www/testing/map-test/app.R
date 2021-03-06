###############################
# Mapping test script         #
###############################

#### LOAD PACKAGES ####
library(shiny)
library(leaflet)
library(tidyverse) # For reading and manipulating data
library(htmltools) # For HTML function in Shiny


#### LOAD DATA ####
setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data/mapping")
map.data <- read_csv("site-data_with_counties.csv")


#### USER INTERFACE ####
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

  leafletOutput("map", width = "100%", height = "100%"),

  # Filter by state
  absolutePanel(
    top = 10, right = 10,
    selectInput(
      inputId = "State", label = "State", multiple = T, selected = unique(map.data$State),
      choices = unique(map.data$State) %>% sort()
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {
  df0 <- eventReactive(input$State, {
    map.data %>%
      filter(State %in% input$State)
  })

  # Add base map and set scale
  output$map <- renderLeaflet({
    leaflet(map.data) %>%
      addTiles() %>%
      fitBounds(~ min(Longitude), ~ min(Latitude), ~ max(Longitude), ~ max(Latitude))
  })

  observe({

    # Function to define multi-line labels
    labs <- lapply(seq(nrow(df0())), function(i) {
      paste0("State: ", df0()[i, "State"], "<p>", "County: ", df0()[i, "NAME"], 
             "<p>", "Treatment: ", df0()[i, "Trtmt_main"], "<p>", "DOI: ", df0()[i, "DOI"])
    })

    # Print map with points
    leafletProxy("map", data = df0()) %>%
      clearShapes() %>%
      addMarkers(
        label = lapply(labs, HTML),
        clusterOptions = markerClusterOptions()
      )
  })
}


#### CALL APP ####
shinyApp(ui, server)
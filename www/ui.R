# -------------------------------------------
# Midwestern agriculture synthesis Shiny app
# Managing Soil Carbon - SNAPP Working Group
# -------------------------------------------

#### Load libraries ####
library(readr)
library(tidyverse) # For reading and manipulating data
library(shiny) # for making Shiny app
library(ggplot2) # for plotting data
library(leaflet) # for mapping
library(shinyjs) # for interactive clicking
library(gdata) # reorder legends
library(shinydashboard)
library(here) # to deal with tehfact we are using a sub directory (www)
library(crosstalk) # to get the map and figure to communicate
library(plotly) #to get interactive plots

#### User Interface ####
# user interface
ui <- navbarPage(
  "Midwest Soil Health Evidence",

  tabPanel(
    "Data",

    # shinyjs required for the update button to initialize a plot
    useShinyjs(),

    tags$style(type = "text/css", ".selectize-input { font-size: 11px; line-height: 11px;}"),

    # Set up header columns
    fluidRow(
      column(
        4,
        align = "center",
        selectInput(
          inputId = "MgmtPractice", label = "Practice",
          choices = unique(summary_all$Review) %>% sort(),
          selected = "Cover Crops"
        )
      ),
      column(
        4,
        align = "center",
        selectInput(
          inputId = "RV", label = "Outcome",
          choices = unique(summary_all$Group_RV) %>% sort(), multiple = T,
          selected = "Soil"
        )
      ),
      column(
        4,
        align = "center",
        selectInput(
          inputId = "Legend_1", label = "Grouping",
          choices = unique(summary_all$Legend_1) %>% sort(), multiple = T,
          selected = "Single species"
        )
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        actionButton(inputId = "update", label = "Update data", style = "padding:4px; font-size:80%")
      )
    ),
    
    hr(),

    # Set up row with plots of map and forest plot
    fluidRow(
      column(
        4,
        leafletOutput("map"),

        # Filter by state
        absolutePanel(
          top = 10, right = 10,
          selectInput(
            inputId = "State", label = "State", multiple = T, selected = unique(map.data$State),
            choices = unique(map.data$State) %>% sort()
          )
        )
      ),
      column(
        8,
        plotlyOutput(outputId = "forestplot")
      )
    ),

    hr(),

    # Set up row for text entry
    fluidRow(
      column(
        12,
        align = "center",
        textOutput(outputId = "text_description")
      )
    ),

    hr(),
    
    fluidRow(
      column(
        12,
        align = "center",
        actionButton(inputId = "downloadData", label = "Download filtered data", style = "padding:4px; font-size:80%"),
        actionButton(inputId = "downloadAllData", label = "Download all data", style = "padding:4px; font-size:80%"),
        actionButton(inputId = "downloadFigure", label = "Download figure", style = "padding:4px; font-size:80%")
      )
    ),
    
    hr()
  ),

  tabPanel("References"),
  tabPanel("Methods")
)

# #### RUN THE APP ####
# shinyApp(ui = ui, server = server)

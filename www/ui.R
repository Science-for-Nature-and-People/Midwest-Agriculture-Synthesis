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





#### Load data ####

summary_all <- read_csv(here("www", "data", "data-for-app.csv"))
map.data <- read_csv(here("www","data", "mapping/site-data_with_counties.csv"))

# setwd(".")
# setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data")
# summary_all <- read_csv("data-for-app.csv")
# map.data <- read_csv("mapping/site-data_with_counties.csv")

# #for nathan's system
# datapath <- "/Users/nathan/Desktop/Midwest-Agriculture-Synthesis/www/data"
# summary_all <- read_csv(file.path(datapath, "data-for-app.csv"))

# #for Lesley's system
# datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data"
# summary_all <- read_csv(file.path(datapath, "data-for-app.csv"))


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
        actionButton(inputId = "update", label = "Update", style = "padding:4px; font-size:80%")
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
        plotOutput(outputId = "forestplot")
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

    hr()
  ),

  tabPanel("References"),
  tabPanel("Methods"),
  downloadButton(outputId = "downloadData", label = "Download") # the download button goes to the bottom
)

# #### RUN THE APP ####
# shinyApp(ui = ui, server = server)

# -------------------------------------------
# Midwestern agriculture synthesis Shiny app
# Managing Soil Carbon - SNAPP Working Group
# -------------------------------------------

#note: we can run this code outside of the ui/server scripts by using the command runApp('www/appScripts/') (assuming your working directory is up to Midwest-Agriculture-Synthesis)

#### Load libraries ####
library(readr)
library(tidyverse)
library(shiny)    # for making Shiny app
library(ggplot2)  # for plotting data
library(leaflet)  # for mapping
library(shinyjs)  # for interactive clicking
library(gdata)    # reorder legends
library(shinydashboard)

#### Load data ####
#setwd(".")
# setwd("~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data")
# summary_all <- read_csv("data-for-app.csv")

# #for nathan's system
# datapath <- "/Users/nathan/Desktop/Midwest-Agriculture-Synthesis/www/data"
# summary_all <- read_csv(file.path(datapath, "data-for-app.csv"))

# #for Lesley's system
 # datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data"
 # summary_all <- read_csv(file.path(datapath, "data-for-app.csv"))


#collist <- c("Review_id", "main_group", "group_metric", "Legend_1", "Legend_2", "Legend_3", "Group_RV", "Review")
#summary_all[collist] <- lapply(summary_all[collist], factor)

#### User Interface ####
# user interface
ui <- navbarPage(
  "Midwest Soil Health Evidence",
  
  tabPanel(
    "Data",
    
    # shinyjs required for the update button to initialize a plot
    useShinyjs(),
    
    tags$style(type='text/css', ".selectize-input { font-size: 11px; line-height: 11px;}"),
    
    # Set up header columns
    fluidRow(
      column(
        3,
        align = "center",
        selectInput(
          inputId = "MgmtPractice", label = "Practice",
          choices = unique(summary_all$Review) %>% sort(),# multiple = T,
          selected = "Cover Crops"
        )
      ),
      column(
        3,
        align = "center",
        selectInput(
          inputId = "RV", label = "Outcome",
          choices = unique(summary_all$Group_RV) %>% sort(), multiple = T, 
          selected = "Soil"
        )
      ),
      column(
        3,
        align = "center",
        selectInput(
          inputId = "Legend_1", label = "Grouping",
          choices = unique(summary_all$Legend_1) %>% sort(), multiple = T, 
          selected = "Monoculture"
        )
      ),
      column(
        3,
        align = "center",
        selectInput(
          inputId = "Geography", label = "State",
          choices = unique(summary_all$Legend_1) %>% sort(), multiple = T, 
          selected = "Monoculture"
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
        leafletOutput("Map")
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
  downloadButton(outputId = "downloadData", label = "Download") #the download button goes to the bottom
)

# #### RUN THE APP ####
# shinyApp(ui = ui, server = server)

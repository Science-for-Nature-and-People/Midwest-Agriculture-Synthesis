# -------------------------------------------
# Midwestern agriculture synthesis Shiny app
# Managing Soil Carbon - SNAPP Working Group
# -------------------------------------------

#### Load libraries ####
library(shiny)    # for making Shiny app
library(dplyr)    # for sorting and summarizing data
library(readxl)   # for importing dataframe
library(ggplot2)  # for plotting data
library(leaflet)  # for mapping
library(shinyjs)  # for interactive clicking
library(gdata)    # reorder legends
library(shinydashboard)
library(forcats)  # reorder data display from greatest to least


#### Load data ####
setwd(".")
datapath <- "~/Box Sync/Work/Code/Midwest-Agriculture-Synthesis/www/data"

# import data -> summary files
covercrop <- read.csv(file.path(datapath, "/CC_FULL_Summary.csv"), stringsAsFactors = FALSE)
pestmgmt <- read.csv(file.path(datapath, "/PestMgmt_FULL_Summary2.csv"), stringsAsFactors = FALSE)


#### Manipulate data ####
summary_all <- full_join(covercrop, pestmgmt)

# change columns to factors
collist <- c("Review_id", "main_group", "group_metric", "Legend_1", "Legend_2", "Legend_3", "Group_RV", "Review")
summary_all[collist] <- lapply(summary_all[collist], factor)
levels(summary_all$Legend_1)

#reorder the data for the legend
summary_all$Legend_1 <- reorder.factor(summary_all$Legend_1, new.order = c("Single species", "Two species", "Three or more species", "Soil", "Foliage","Seed", "Seed & Foliage" ))  
#rearrange the data according to the new ordering defined above
summary_all <- summary_all %>% arrange(Legend_1)

# make a temporary new column that just combines the group_metric and the main group.
# This new column has all rows then.
# then we reorder this column, so that it will be organized by both facet(main group)
# and by group_metric
summary_all$group_metric_facet <- with(summary_all, paste(group_metric, main_group, sep = "_"))
# summary_all$group_metric_facet <- reorder.factor(summary_all$group_metric_facet, new.order = sort(unique(summary_all$group_metric_facet), decreasing = TRUE))

summary_all %>%
  group_by(Legend_1, Group_RV, Review) %>%
  mutate(group_metric_facet = fct_reorder(group_metric_facet, mean_per_change1)) -> summary_all

#### User Interface ####
# user interface
ui <- navbarPage(
  "Midwest Soil Health Evidence",

  tabPanel(
    "Data",

    # shinyjs required for the update button to initialize a plot
    useShinyjs(),

    # Set up header columns
    fluidRow(
      column(
        3,
        align = "center",
        radioButtons(
          inputId = "MgmtPractice", label = "Practice",
          choices = unique(summary_all$Review),
          selected = "Cover Crop"
        )
      ),
      column(
        4,
        align = "center",
        selectInput(
          inputId = "RV", label = "Outcome",
          choices = unique(summary_all$Group_RV) %>% sort(), multiple = T, # Now able to select multiple options from list
          selected = "Soil"
        )
      ),
      column(
        4,
        align = "center",
        selectInput(
          inputId = "Legend_1", label = "Grouping",
          choices = unique(summary_all$Legend_1) %>% sort(), multiple = T, # Now able to select multiple options from list
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
        tabsetPanel(
          tabPanel(
            "Data",
            plotOutput(outputId = "forestplot")
          ),
          tabPanel(
            "References"
          )
        )
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
  
  tabPanel("Methods")
)

#### SERVER INSTRUCTIONS ####
server <- function(input, output, session) {
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

  # Reactive selection by management practice
  df0 <- eventReactive(input$MgmtPractice, {

    # filter dataset to display selected review and response variables
    summary_all %>%
      filter(Review == input$MgmtPractice)
  })

  # Next tier selection of reactive selection of outcome grouping
  df1 <- eventReactive(input$RV, {
    df0() %>%
      filter(Group_RV %in% input$RV)
  })

  # Merge by cover crop type
  df2 <- eventReactive(input$update, {

    # filter dataset to display selected review and response variables
          ##Here we need to figure out how to sort data when asked to display all Legend_1 variables
          ##as is it only reorders when Legend_1 has one selection

    df1() %>%
      filter(Legend_1 %in% input$Legend_1) %>%
      group_by(Legend_1) %>% #needs to group by the greatest option within Legend_1 
      mutate(group_metric_facet = fct_reorder(group_metric_facet, mean_per_change1))
  })


  # Build figure and map based on selected data

  output$Map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat = 40, lng = -91, zoom = 5)
  })

  output$forestplot <- renderPlot({
    ggplot(df2(), aes(group_metric_facet, mean_per_change1, # remember that group_metric_facet is the column ordered by main_group and group_metric
      ymin = mean_per_change1 - sem_per_change1,
      ymax = mean_per_change1 + sem_per_change1
    )) +
      scale_x_discrete("", breaks = df2()$group_metric_facet, label = df2()$group_metric) + # this line relabels the x from "group_metric_main_group" to just "group_metric"
      geom_pointrange() +
      geom_errorbar(aes(
        ymin = mean_per_change1 - sem_per_change1,
        ymax = mean_per_change1 + sem_per_change1,
        width = .1
      )) +
      geom_hline(yintercept = 0, lty = 2) + # add a dotted line at x=0 after flip
      coord_flip() + # flip coordinates (puts labels on y axis)
      labs(
        title = df2()$Review[1], # since we are filtering summary_all to only have 1 value for review/group_rv, we can take any element as the label (they should all be the same)
        subtitle = df2()$Group_RV[1],
        x = "",
        y = "percent difference between control and treatment (%)"
      ) +
      # scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +
      theme_bw() +
      geom_point(aes(colour = Legend_1)) + # color labeling of fine level groupings
      facet_grid(main_group ~ ., scales = "free", space = "free") +
      theme(strip.text.y = element_text(angle = 0))
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

  # Will click the update button at the start so the app starts with a plot.
  observe({
    click("update") 
  })
}


#### RUN THE APP ####
shinyApp(ui = ui, server = server)

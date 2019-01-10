#Website: Midwestern Ag Synthesis
#Shiny app

library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(stringi)
library(forcats)
library(ggplot2)
library(colorRamps)
library(colorspace)
library(shiny)

#import data -> summary files
covercrops <- read.csv("C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_data.csv", header=TRUE, row.names = "X")
    cc_yield_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Yield_Summary.csv", header=TRUE, row.names = "X")
    cc_soil_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Soil_Summary.csv", header=TRUE, row.names = "X")
    cc_pest_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Pest_Summary.csv", header=TRUE, row.names = "X")
    cc_water_summary <- read.csv(file = "C:/Users/LWA/github/midwesternag_synthesis/CoverCrop_Water_Summary.csv", header=TRUE, row.names = "X")
 
   
    ###Figure for Cover Crop Review: Soils####
    df <- 
      cc_soil_summary

#Input()
#Output()

ui <-  fluidPage(sidebarLayout(#add elements to the app here - html code whin here, design app from here
      #see tags....names(tags)
  h2('Synthesis of the trade-offs associated with Best Management Practices (BMPs) in the US Midwest'),
  tags$h3 ("knit sentences together", tags$a(href = "http://www.nceas.ucsb.edu", "NCEAS")),
      
      #radioButtons()
      #selectInput()
 sidebarPanel( checkboxGroupInput(inputId = "MgmtPractice", label = "Management Practice", 
        choices = (c('Cover Crops', 'Nutrient Management', 'Pest Management', 'Tillage' )),
        selected = 'Cover Crops')), actionButton(inputId = "go", label = "Update")),
  mainPanel(fluidRow(column(4, "test"), column(4, "test2"), column(4, "test3")), #adds new row to the page, adds below. add more to create more. 12 units wide, divide these up however you want
  fluidRow(column(8, offset = 4, plotOutput(outputId = "forest")),
  #sidebarPanel(
   # selectInput('Review', 'X Variable', names(df)), ### Use this for subgroups after main group is selected
  
    #selectInput('percent_diff', 'Percent Difference (%)', names(df)),
  

  textOutput("group")
  #plotOutput()#
      #imageOutput()
      #?textOutput() or ?verbatimTextOutput()
      #tableOutput()

  )))


server <- function(input, output) { #build plot in server function####
 
  #input$MgmtPractice #use as reactive value, need to integrate into output code. sets how the data are queried
      #establishes summary that will be set to df and used for figure out
  
  #data <- reactive({#write querying code here based on input$MgmtPractice selected from checkboxes
   #         })
          #need to add a few other drop down bars to account for other selection options.!!!Creates a function!
          
  #data <- eventReactive(input$go, {checkboxGroupInput$MgmtPractice})
   #triggers code to run on the server!, allows you to precisely specify which reactive values to invalidate
  
  #reactive tookit <- functions
        #renderPlot({}) <- build something that will be displayed
        #reactive({}) <- use this to create reactive expressions, these are technically functions.
        #isolate({}) <- prevent app from responding before all choices are selected. Hit go? Maybe useful as user selects which groups to look at
        # actionButton(inputId = "go", label = "Selection Complete") createbutton in ui section that will download specific file for use, circumvent the slow process of querying within the app?
          #observeEvent() #Trigger code***** place code outside of server function if it needs to be run once per session (querying here)
              #code inside server function is run once connection 
              #(filtering to see which query file to grab goes wihin reactive fnction (render function))
        #eventReactive(input$go, {checkboxGroupInput$MgmtPractice})  #delay reaction
        #
  
  
    #need to set each grouping name to each new datafile
    #I think you link each list option to the different data sets
    
   output$forest <- #code for figure must include a render function....
                    #renderImage(), renderDataTable(), renderText, renderTable, renderPrint
              renderPlot({
                  ggplot(df, aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
                  geom_pointrange() +
                  geom_errorbar(aes(ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change, width=.1)) +
                  geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
                  coord_flip() + # flip coordinates (puts labels on y axis)
                  labs(title ="Cover Crop Review", subtitle = "Soils", x = "", y = "percent difference (%)") + 
                  theme_bw() +
                  geom_point( aes(colour = Cover_crop_diversity)) + #color labeling of fine level groupings
                  facet_grid(main_group ~ .,scales = "free")})
      #output$group <- renderPrint({
       #           checkboxGroupInput
      #})
  
} 

shinyApp(ui = ui, server = server)


#Share the app
#replace my computer with a web server
#Create directory with every file the app needs...datasets, images, css, helper scripts, etc.
#app.R #name of your script which ends witha call to shinyApp()

#shinyapps.io <- server maintained by RStudy to upload apps as they are developed

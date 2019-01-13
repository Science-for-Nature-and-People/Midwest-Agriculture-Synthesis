# -----------------------------------------
#Midwestern agriculture synthesis Shiny app
# -----------------------------------------
# Managing Soil Carbon - SNAPP Working Group


library("shiny")      # for making Shiny app
library("dplyr")      # for sorting and summarizing data
library("readxl")     # for importing dataframe
library("ggplot2")    # for plotting data

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis/www/data")

#import data -> summary files
summary_all <- read.csv("./CoverCrop_Summary.csv", header=TRUE, row.names = "X")
 
   


  #user interface
ui <-  fluidPage( 
  
titlePanel('Synthesis of the trade-offs associated with Best Management Practices (BMPs) in the US Midwest'),
      sidebarPanel( 
        radioButtons(inputId = "MgmtPractice", label = "Management Practice", 
          choices = unique(CC_summary_all$Review), #will be expanded as review dataframes are populated
          selected = "Cover Cropping")),

        radioButtons(inputId = "RV", label = "Infield Agro-Environmental Response",
          choices = unique(CC_summary_all$Group_RV),
          selected = "Crop Production"),
    

        actionButton(inputId = "update", label = "Update the Figure")
          
)
  
  mainPanel(
        #textOutput(outputId ="description_of_selection"),
        plotOutput(outputId = "forestplot")
        
        )



####server instructions####
#build plot in server function

server <- function(input, output) { 
      
  df <- eventReactive(input$update,{ #set action button to initiate changes in the figures displayed
          
    #filter dataset to display selected review and response variables
          summary_all %>%
            filter(Review == input$MgmtPractice) %>%
            filter(Group_RV == input$RV)
          }) 
   
           #build figure based on selected data
        output$forestplot <- renderPlot({
            
            
          ggplot(df(), aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
            geom_pointrange() +
            geom_errorbar(aes(ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change, width=.1)) +
            geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
            coord_flip() + # flip coordinates (puts labels on y axis)
            labs(title =input$MgmtPractice, subtitle = input$RV, x = "", y = "percent difference between control and treatment (%)") + 
            #scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +        
            theme_bw() +
            geom_point( aes(colour = Cover_crop_diversity2)) + #color labeling of fine level groupings
            facet_grid(main_group ~., scales = "free", space = "free") +
            theme(strip.text.y = element_text(angle = 0))
          
        })
}
      

shinyApp(ui = ui, server = server)


#Share the app
#replace my computer with a web server
#Create directory with every file the app needs...datasets, images, css, helper scripts, etc.
#app.R #name of your script which ends witha call to shinyApp()

#shinyapps.io <- server maintained by RStudy to upload apps as they are developed


#server notes
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

#df <- eventReactive(input$update,{
#  if_else(input$RV  == "Crop Production",cc_yield_summary,
#if_else(input$RV  == "Pest Regulation",cc_pest_summary,
#if_else(input$RV  == "Soils", cc_soil_summary,
#if_else(input$RV  == "Water Movement", cc_water_summary, NULL)
#)))})

#df <- eventReactive(input$update,{ #merge datasets and then filter based on RVs
#    if (input$RV %in% "Crop Production") {
#       dataset1 <- cc_yield_summary
#  if (input$RV %in% "Pest Regulation") {
#   dataset1 <- cc_pest_summary
#if (input$RV %in% "Soils") {
# dataset1 <- cc_soil_summary
#}
#  return(dataset1)
#}
#}})

#ggplot(df(), aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
 # geom_pointrange() +
  #geom_errorbar(aes(ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change, width=.2)) +
  #geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
  #coord_flip() + # flip coordinates (puts labels on y axis)
  #theme_bw() +
  #geom_point( aes(colour = Cover_crop_diversity2)) + #color labeling of fine level groupings
  #facet_grid(main_group ~ .,scales = "free", space = "free") +
  #theme(strip.text.y = element_text(angle = 0))


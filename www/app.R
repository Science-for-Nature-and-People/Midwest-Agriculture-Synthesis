# -----------------------------------------
#Midwestern agriculture synthesis Shiny app
# -----------------------------------------

setwd("C:/Users/LWA/Desktop/github/midwesternag_synthesis")
getwd()

library("shiny", lib.loc="~/R/win-library/3.5")      # for making Shiny app
library("dplyr", lib.loc="~/R/win-library/3.5")      # for sorting and summarizing data
library("readxl",lib.loc="~/R/win-library/3.5")     # for importing dataframe
library("ggplot2", lib.loc="~/R/win-library/3.5")    # for plotting data
library("colorRamps", lib.loc="~/R/win-library/3.5") # for colorschemes in plots
library("colorspace", lib.loc="~/R/win-library/3.5") # for colorschemes in plots


#import data -> summary files
cc_yield_summary <- read.csv(file = "CoverCrop_Yield_Summary.csv", header=TRUE, row.names = "X")
cc_soil_summary <- read.csv(file = "CoverCrop_Soil_Summary.csv", header=TRUE, row.names = "X")
cc_pest_summary <- read.csv(file = "CoverCrop_Pest_Summary.csv", header=TRUE, row.names = "X")
cc_water_summary <- read.csv(file = "CoverCrop_Water_Summary.csv", header=TRUE, row.names = "X")
 
   
    ###Figure for Cover Crop Review: Soils####
    #df <-   cc_soil_summary


  #user interface
ui <-  fluidPage( 
  
titlePanel('Synthesis of the trade-offs associated with Best Management Practices (BMPs) in the US Midwest'),
      sidebarPanel( 
        radioButtons(inputId = "MgmtPractice", label = "Select 1 Management Practice", 
          choices = list("Cover Cropping" ,
                         "Soil Fertility", 
                         "Pest Contro", 
                         "Tillage"),
          selected = "Cover Cropping")),

        radioButtons(inputId = "RV", label = "Select 1 Agro-Environmental Response (Infield)",
          choices = c( "Crop Production",
                       "Pest Regulation", 
                       "Soils", 
                       "Water Movement"), 
          selected = "Crop Production"),
    

        actionButton(inputId = "update", label = "Update the Figure")
          
)
  
  mainPanel(
        plotOutput(outputId = "forestplot")
        #textOutput(outputId ="Mgmt_RV")
        )



  
  #df <- case_when(
   # input$MgmtPractice == "Cover Cropping" & input$RV  == "Crop Production" ~ cc_yield_summary,
  #  input$MgmtPractice == "Cover Cropping" & input$RV  == "Pest Regulation"~ cc_pest_summary,
   # input$MgmtPractice == "Cover Cropping" & input$RV  == "Soils" ~ cc_soil_summary,
    #input$MgmtPractice == "Cover Cropping" & input$RV  == "Water Movement" ~ cc_water_summary
  #)
  
    
  

####server instructions####
#build plot in server function

server <- function(input, output) { 
       #df <- eventReactive(input$update,{
        #  if_else(input$RV  == "Crop Production",cc_yield_summary,
        #if_else(input$RV  == "Pest Regulation",cc_pest_summary,
        #if_else(input$RV  == "Soils", cc_soil_summary,
        #if_else(input$RV  == "Water Movement", cc_water_summary, NULL)
        #)))})
           
       df <- eventReactive(input$update,{ #merge datasets and then filter based on RVs
            if (input$RV %in% "Crop Production") {
                dataset1 <- cc_yield_summary
            if (input$RV %in% "Pest Regulation") {
              dataset1 <- cc_pest_summary
            if (input$RV %in% "Soils") {
              dataset1 <- cc_soil_summary
            }
              return(dataset1)
            }
            }})
                
       
           #data files to access from RV choices with cover cropping mangement practice
        output$forestplot <- renderPlot({
          
          
           ggplot(df(), aes(group_metric, mean_per_change, ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change)) +
                  geom_pointrange() +
                  geom_errorbar(aes(ymin = mean_per_change-sem_per_change, ymax = mean_per_change +sem_per_change, width=.2)) +
                  geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
                  coord_flip() + # flip coordinates (puts labels on y axis)
                  #labs(title ="Cover Crop Review", subtitle = "Soils", x = "", y = "percent difference (%)") + 
                  theme_bw() +
                  geom_point( aes(colour = Cover_crop_diversity2)) + #color labeling of fine level groupings
                  facet_grid(main_group ~ .,scales = "free", space = "free") +
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
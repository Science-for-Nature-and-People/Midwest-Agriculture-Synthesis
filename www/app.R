# -----------------------------------------
#Midwestern agriculture synthesis Shiny app
# -----------------------------------------
# Managing Soil Carbon - SNAPP Working Group


library("shiny")      # for making Shiny app
library("dplyr")      # for sorting and summarizing data
library("readxl")     # for importing dataframe
library("ggplot2")    # for plotting data
library(shinyjs)
library(gdata)        #reorder legends
library(shinydashboard)

setwd(".")
#datapath <- "/Users/LWA/Desktop/github/midwesternag_synthesis/" 
datapath <- "/Users/nathan/Desktop/Midwest-Agriculture-Synthesis/"

#import data -> summary files
covercrop <-  read.csv(file.path(datapath, "Cover Crop Review/CC_FULL_Summary2.csv"), stringsAsFactors = FALSE)
pestmgmt <-  read.csv(file.path(datapath, "PestMgmt Review/PestMgmt_FULL_Summary2.csv"), stringsAsFactors = FALSE)

summary_all <- full_join(covercrop, pestmgmt)

#change columns to factors
collist <- c("Review_id", "main_group", "group_metric", "Legend_1", "Legend_2", "Legend_3", "Group_RV", "Review")
summary_all[collist] <- lapply(summary_all[collist], factor)

levels(summary_all$Legend_1)

#reorder the data for the legend
summary_all$Legend_1 <- reorder.factor(summary_all$Legend_1, new.order = c("Monoculture", "Mixture (2 Spp.)", "Mixture (3+ Spp.)", "Soil", "Foliage","Seed", "Seed & Foliage" ))  
#rearrange the data according to the new ordering defined above
summary_all <- summary_all %>% arrange(Legend_1)

#make a temporary new column that just combines the group_metric and the main group. This new column has all rows then.
#then we reorder this column, so that it will be organized by both facet(main group) and by group_metric
summary_all$group_metric_facet <- with(summary_all, paste(group_metric, main_group, sep = "_"))
summary_all$group_metric_facet <- reorder.factor(summary_all$group_metric_facet, new.order = sort(unique(summary_all$group_metric_facet), decreasing = TRUE))

###start of management button test (safe to delete this chunk)###
  #all of our data has cover cropping in this column. I make half of the data (randomly chosen) a different entry to see if the button works.
#levels(summary_all$Review) <- c(levels(summary_all$Review), "test")
#summary_all$Review[sample(x = nrow(summary_all), size = nrow(summary_all)/2)] <- "test"
###end of test###

  #user interface
ui <-  fluidPage( 
useShinyjs(), #this lets us use the shinyjs package. This is required just for the "click" function below, which "clicks" the update button to initialize a plot at the start
titlePanel('Synthesis of the trade-offs associated with Best Management Practices (BMPs) in the US Midwest'),

sidebarLayout(
  sidebarPanel( 
        radioButtons(inputId = "MgmtPractice", label = "Management Practice", 
          choices = unique(summary_all$Review), #will be expanded as review dataframes are populated
          selected = "Cover Crop"),

        radioButtons(inputId = "RV", label = "Infield Agro-Environmental Response",
          choices = unique(summary_all$Group_RV) %>% sort(),
          selected = "Crop Production"),
    
          actionButton(inputId = "update", label = "Update the Figure")
        ),
          

  mainPanel(
            plotOutput(outputId = "forestplot"),
            br(),
            fluidRow(
              box(
                textOutput(outputId = "text_description"),
                title = "Plot Description",
                width = 10 #change width depending on how big you want the textbox to be. can go from 1-12
                           #to use other features of box (like color, collapsable, ect..) we need to change fluidPage to dashboardPage
                
              )
            )
        )

  )    
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
            
          ggplot(df(), aes(group_metric_facet, mean_per_change1, #remember that group_metric_facet is the column ordered by main_group and group_metric
                           ymin = mean_per_change1-sem_per_change1, 
                           ymax = mean_per_change1 +sem_per_change1)) +
            scale_x_discrete("", breaks = summary_all$group_metric_facet, label = summary_all$group_metric) + #this line relabels the x from "group_metric_main_group" to just "group_metric"
            geom_pointrange() +
            geom_errorbar(aes(ymin = mean_per_change1-sem_per_change1, 
                              ymax = mean_per_change1 +sem_per_change1, 
                              width=.1)) +
            geom_hline(yintercept=0, lty=2) +# add a dotted line at x=0 after flip
            coord_flip() + # flip coordinates (puts labels on y axis)
            labs(title =df()$Review[1], #since we are filtering summary_all to only have 1 value for review/group_rv, we can take any element as the label (they should all be the same)
                 subtitle = df()$Group_RV[1], 
                 x = "",
                 y = "percent difference between control and treatment (%)") + 
            #scale_fill_discrete(breaks=c("Monoculture","Mixture (2 Spp.)","Mixture (3+ Spp.)")) +        
            theme_bw() +
            geom_point( aes(colour = Legend_1)) + #color labeling of fine level groupings
            facet_grid(main_group ~., scales = "free", space = "free") +
            theme(strip.text.y = element_text(angle = 0)) 
        })
        
        output$text_description <- renderText({
          if(df()$Review[1] == "Cover Crop"){
            "this is the text we can show for Cover Crop options. We can add another if statement within this one if we want a separate description for each response. See commented code for an example"
            # if(df()$Group_RV[1] == "Crop Production"){
            #   "This is the text we show for cover crop & crop production"
            # }
            # else if(df()$Group_RV[1] =="Soil"){
            #   "This is the text we show for cover crop and soil"
            # }
            # else if ... ect ect
          }
          else{
            "this is the text we can show for Early Season Pest Management. The width of these text boxes can be adjusted in the ui, in the 'box' function"
          }
          
          })
        
        observe({
          click("update")  #this chunk will click the update button at the very start, so that our app starts with a plot.
                           # if you want to change the default plot, then change the default "selected" values above
          #invalidateLater(1000) #invalidateLater would click the update button every '1000' milliseconds
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


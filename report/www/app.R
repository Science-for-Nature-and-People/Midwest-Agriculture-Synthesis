# -----------------------------------------
#Midwestern agriculture synthesis Shiny app
# -----------------------------------------
# Managing Soil Carbon - SNAPP Working Group
# Evidence Report


library("shiny")      # for making Shiny app
library("dplyr")      # for sorting and summarizing data
library("readxl")     # for importing dataframe
library("DT")         # for data table manipulations

setwd(".")
datapath <- "C:/Users/LWA/Desktop/github/midwesternag_synthesis/report/www/data/" 


#import data -> summary files
CC_report <- read.csv(file.path(datapath, "CC_report.csv"), header=TRUE, row.names = "X")
names(CC_report)[names(CC_report) == 'summary_statement2'] <- 'Synopsis'
names(CC_report)[names(CC_report) == 'Group_RV'] <- 'Response_Group'
names(CC_report)[names(CC_report) == 'main_group'] <- 'Measured_Responses'
names(CC_report)[names(CC_report) == 'Group_finelevel'] <- 'Cover_Crop_Composition'
names(CC_report)[names(CC_report) == 'Paper_id'] <- 'Original_Paper_ID'




CC_report$Review <- paste("Cover Cropping")
CC_report <- CC_report[c('Original_Paper_ID', "Review", 'Response_Group', 'Measured_Responses', 'Cover_Crop_Composition', 'Synopsis')]
    #add citation to dataframe and include here


###start of management button test (safe to delete this chunk)###
  #all of our data has cover cropping in this column. I make half of the data (randomly chosen) a different entry to see if the button works.
levels(CC_report$Review) <- c(levels(CC_report$Review), "test")
CC_report$Review[sample(x = nrow(CC_report), size = nrow(CC_report)/2)] <- "test"
###end of test###

  #user interface
ui <-  fluidPage( 
  
titlePanel('Synthesis of the trade-offs associated with Best Management Practices (BMPs) in the US Midwest'),

sidebarLayout(
  sidebarPanel( 
        radioButtons(inputId = "MgmtPractice", label = "Management Practices", 
          choices = unique(CC_report$Review), #will be expanded as review dataframes are populated
          selected = "Cover Cropping"),

        radioButtons(inputId = "RVGroup", label = "Agro-Environmental Response Groups",
          choices = unique(CC_report$Response_Group),
          selected = "Crop Production"),
        
        
        #This needs to automatically update depending on above
        selectInput(inputId = "RV", label = "Measured Responses",
                     choices = unique(CC_report$Measured_Responses) # plus add 'All' to this list
        ),
                     #selected = "Crop Production"), how do you select when these values change depending on above?
    
          actionButton(inputId = "update", label = "Update")
        ),
          

  mainPanel(
        textOutput(outputId ="review_description"),
        dataTableOutput(outputId = "summaries")
        
            )

  )    
)

####server instructions####
#build plot in server function

server <- function(input, output) { 
      
  df <- eventReactive(input$update,{ #set action button to initiate changes in the figures displayed
   
    
    #filter dataset to display selected review and response variables
          CC_report %>%
            filter(Review == input$MgmtPractice) %>%
            filter(Response_Group == input$RVGroup) %>%
            filter(Measured_Responses == input$RV) %>%
          #add specific response group variable
            select(Original_Paper_ID, Synopsis)
          }) 
  
  
 
  
   
           #build summary table based on selected data
  #Maybe datatable isn't the best output type
  
        output$review_description <- renderText({
                paste(input$MgmtPractice, ": ", input$RVGroup, ": ", input$RV)
                  })
        
                  #break
        
        #output$summaries <- renderDataTable(
         #     df(), 
      # choose columns to display    
  
  output$summaries <- renderDataTable({
    
    df()
  })
  
  
        #output$summaries <- DT::renderDataTable({
           #     DT::datatable(df())#, selected = Synopsis)
          #selectColumns()
            #  })
        #datatable(
         # head(iris),
        #  caption = 'Table 1: This is a simple caption for the table.'
        #)
        
}
      

shinyApp(ui = ui, server = server)



# -------------------------------------------
# Midwestern agriculture synthesis Shiny app
# Managing Soil Carbon - SNAPP Working Group
# -------------------------------------------



#### User Interface ####
# user interface

# how to try to get download button to the navbar: 
    # https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
    # https://github.com/daattali/advanced-shiny/tree/master/navbar-add-text

ui <- navbarPage(
  "Midwest Soil Health Evidence",
  id = 'navbar',
  footer = a(href = 'https://snappartnership.net/teams/managing-soil-organic-carbon/',
             img(src='snapp-logo.png', align = 'right', height = 50)),
  tabPanel(
    'Summary',
    style = 'background-color:#F5FAFE;',
    fluidRow(
      column(12, offset = 0,
             p('I want to know the impact of', style = 'font-size:30px; padding: 5px; text-align:center;')
             )
    ),
    fluidRow(
      column(4, offset = 1, align = 'center',
             div(style="display:inline-block; margin-left:130px; text-align:center; font-size:25px; line-height:10px;",
                 selectInput(inputId = "summaryPractice", label = "",
                             choices = unique(summary_all$Review) %>% sort(),
                             selected =  'Cover Crops'
                             ))),
      column(2, offset = 0, align = 'center', 
             p("on", style = "font-size:30px; margin-top:15px;")),
      column(4, offset = 0, align = 'center', 
             div(style="display:inline-block; margin-left:-200px; text-align:center; font-size:25px; line-height:10px",
                 selectInput(inputId = "summaryRV", label = "",
                                 choices = unique(summary_all$Group_RV), 
                                 selected = "Soil"
                             ))
      )
    ),

    fluidRow(
      column(6,offset = 3, align = 'center',
             actionButton(inputId = "go", label = "Go", 
                          style = 'padding:5px; font-size: 200%; width: 500px; color: white; background-color: green; margin-top:100px' ))
    ),
    #use the line below if you want to change ALL selectInputs. 
      # I don't do much with it because there is a selectInput in two tabs, and they need different formatting
    tags$head(tags$style(HTML(".selectize-input { width: 300px; padding: 15px} 
                              .selectize-dropdown { font-size: 15px; line-height: 20px;}")))
    
  ),

  tabPanel(
    "Data",

    # shinyjs required for the update button to initialize a plot
    useShinyjs(),


    # Set up header columns
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "MgmtPractice", label = "Practice",
          choices = unique(summary_all$Review) %>% sort(),
          selected = "Cover Crops"
        ),
        checkboxGroupInput(
          inputId = "RV", label = "Outcome",
          choices = unique(summary_all$Group_RV), #multiple = T,
          selected = "Soil"
        ),
        checkboxGroupInput(
          inputId = "Legend_1", label = "Grouping",
          choices = unique(summary_all$Legend_1) %>% sort(),# multiple = T,
          selected = "Single species"
          #selected = unique(summary_all$Legend_1)[1]
        ),
        
        div(style = 'font-size:15px ',
            selectInput(
              inputId = "Region", label = "Location",
              choices = unique(map.data$Region) %>% sort(),# multiple = T,
              selected = "Midwest"
            )
        ),
        
        actionButton(inputId = "update", label = "Update data", style = "padding:4px; font-size:80%")
      ),
    # fluidRow(
    #   column(
    #     4,
    #     align = "center",
    #     selectInput(
    #       inputId = "MgmtPractice", label = "Practice",
    #       choices = unique(summary_all$Review) %>% sort(),
    #       selected = "Cover Crops"
    #     )
    #   ),
    #   column(
    #     4,
    #     align = "center",
    #     selectInput(
    #       inputId = "RV", label = "Outcome",
    #       choices = unique(summary_all$Group_RV) %>% sort(), multiple = T,
    #       selected = "Soil"
    #     )
    #   ),
    #   column(
    #     4,
    #     align = "center",
    #     selectInput(
    #       inputId = "Legend_1", label = "Grouping",
    #       choices = unique(summary_all$Legend_1) %>% sort(), multiple = T,
    #       selected = "Single species"
    #     )
    #   )
    # ),
    # fluidRow(
    #   column(
    #     12,
    #     align = "center",
    #     actionButton(inputId = "update", label = "Update data", style = "padding:4px; font-size:80%")
    #   )
    # ),
    # 
    # hr(),
    mainPanel(
      # Set up row with plots of map and forest plot
      fluidRow(
        # column(
        #   4,
        #   leafletOutput("map"),
        #   
        #   # Filter by state
        #   absolutePanel(
        #     top = 10, right = 10,
        #     selectInput(
        #       inputId = "State", label = "State", multiple = T, selected = unique(map.data$State),
        #       choices = unique(map.data$State) %>% sort()
        #     )
        #   )
        # ),
        column(
          #8,
          12,
          #height of 750 to accomodate largest possible plot (cover crops, all outcomes selected.)
            # ideally, it'll be dynamic, but height = 'auto' doesn't work.
          plotOutput(outputId = "forestplot", height = '750px')
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
          downloadButton(outputId = "downloadData", label = "Download filtered data", style = "padding:4px; font-size:80%"),
          downloadButton(outputId = "downloadAllData", label = "Download all data", style = "padding:4px; font-size:80%"),
          downloadButton(outputId = "downloadFigure", label = "Download figure", style = "padding:4px; font-size:80%")
        )
      ),
      
      hr()      
    )

  )
  ),

  tabPanel("References", tableOutput(outputId = "reference_table")),
  tabPanel("Methods")
  
)

# #### RUN THE APP ####
# shinyApp(ui = ui, server = server)

#runApp('www', display.mode = 'showcase')

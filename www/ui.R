# -------------------------------------------
# Midwestern agriculture synthesis Shiny app
# Managing Soil Carbon - SNAPP Working Group
# -------------------------------------------



#### User Interface ####
# user interface

# how to try to get download button to the navbar: 
    # https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
    # https://github.com/daattali/advanced-shiny/tree/master/navbar-add-text

# add path for picture (prefix is just the id for the path, which we reference in the footer)
  #we need this because the app is in the www directory, so the paths are all mixed up
addResourcePath(prefix = 'pics', directoryPath = here('www'))

ui <- navbarPage(
  "Midwest Soil Health Evidence",
  id = 'navbar',
  footer = a(href = 'https://snappartnership.net/teams/managing-soil-organic-carbon/',
             img(src='pics/snapp-logo.png', align = 'right', height = 50)),
  tabPanel(
    'Summary',
    style = 'background-color:#F5FAFE;',
    fluidRow(
      column(12, offset = 0,
             p('I want to know the impact of', style = 'font-size:30px; padding: 5px; text-align:center;')
             )
    ),
    fluidRow(
      column(4, offset = 0, align = 'center',
             #div(style="display:inline-block; margin-left:130px; text-align:center; font-size:25px; line-height:10px;",
             div(style="display:inline-block;  text-align:center; font-size:25px; line-height:10px;width: 300px; ",    
                 selectInput(inputId = "summaryPractice", label = "",
                             choices = unique(summary_data$Review) %>% sort(),
                             selected =  'Tillage'
                             ))),
      column(1, br()),
      column(2, offset = 0, align = 'center',
            p("on", style = "font-size:30px; margin-top:15px;")),
      column(1, br()),
      column(4, offset = 0, align = 'center',
             #div(style="display:inline-block; margin-left:-200px; text-align:center; font-size:25px; line-height:10px",
             div(style="display:inline-block; text-align:center; font-size:25px; line-height:10px;width: 300px;",
                 selectInput(inputId = "summaryRV", label = "",
                                 choices = unique(summary_data$group_level1),
                                 selected = "Climate Mitigation"
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
    tags$head(tags$style(HTML(".selectize-input {padding: 15px} 
                              .selectize-dropdown { font-size: 15px; line-height: 20px;}")))
    
  ),

  tabPanel(
    "Data",

    # shinyjs required for the update button to initialize a plot
    useShinyjs(),


    # Set up header columns
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "MgmtPractice", label = "Practice",
          choices = unique(summary_data$Review) %>% sort(),
          selected = "Tillage"
        ),
        fluidRow(
          column(6,
          radioButtons(
            inputId = 'Filter1', label = 'Tillage Type #1',
            choices = unique(summary_data$Trt_1name) %>% sort(),
            selected = 'Conventional tillage'
          )),
          column(6,
          radioButtons(
            inputId = 'Filter2', label = 'Tillage Type #2', 
            choices = unique(summary_data$Trt_2name) %>% sort(),
            selected = 'No tillage'
          ))
          ),
        fluidRow(
          column(6,
            radioButtons(
              inputId = "RV", label = "Outcome",
              choices = unique(summary_data$group_level1), #multiple = T,
              selected = "Climate Mitigation"
            )
          ),
          column(6,
                 shinyjs::hidden(
                    radioButtons(
                      inputId = "SoilDepth", label = "Soil Sampling Depth",
                      choices = unique(summary_data$sample_depth) %>% sort(),# multiple = T,
                      selected = "0-25 cm"
                      #selected = unique(summary_data$Legend_1)[1]
                    )
                 )
            )
          ),
        shinyjs::hidden(radioButtons(
          inputId = "years", label = "Years of Implementation",
          choices = unique(summary_data$sample_year) %>% sort(),# multiple = T,
          selected = "Year 1-5"
          #selected = unique(summary_data$Legend_1)[1]
        )
        ),

        
        # div(style = 'font-size:15px ',
        #     selectInput(
        #       inputId = "Region", label = "Location",
        #       choices = unique(map.data$Region) %>% sort(),# multiple = T,
        #       selected = "Midwest"
        #     )
        # ),
        
        actionButton(inputId = "update", label = "Update data", style = "padding:4px; font-size:80%"),
        #create hidden text that only shows up if there isn't enough data. should be used in conjunction with disabled update button
        shinyjs::hidden(p(id = 'no_data', 'No Data! Please change your selection',
                          style = 'color: gray'))
      ),
    # fluidRow(
    #   column(
    #     4,
    #     align = "center",
    #     selectInput(
    #       inputId = "MgmtPractice", label = "Practice",
    #       choices = unique(summary_data$Review) %>% sort(),
    #       selected = "Cover Crops"
    #     )
    #   ),
    #   column(
    #     4,
    #     align = "center",
    #     selectInput(
    #       inputId = "RV", label = "Outcome",
    #       choices = unique(summary_data$group_level1) %>% sort(), multiple = T,
    #       selected = "Soil"
    #     )
    #   ),
    #   column(
    #     4,
    #     align = "center",
    #     selectInput(
    #       inputId = "Legend_1", label = "Grouping",
    #       choices = unique(summary_data$Legend_1) %>% sort(), multiple = T,
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
        column(12,
               tableOutput(outputId = 'current_table'))
      ),
      
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

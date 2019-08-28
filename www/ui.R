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


#' Cool method for getting something (the object "out") to show up underneath a sidebarPanel
#' It basically makes a sidebar panel from scratch using HTML. See link below for details
#' https://stackoverflow.com/questions/52544228/r-shiny-display-static-text-outside-sidebar-panel
#' class = 'well' is what forms the gray box. 
#' a form is the overarching object that contains all the inputs
#' @param ... are whatever we want inside the sidebarPanel. same as the regular `sidebarPanel` function
#' @param out is whatever you want underneat the sidebarPanel
#' @param width is the width of the sidebarPanel, using fluidRow metrics (max 12)
sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}

# navbarPage is what allows those tabs at the top
ui <- navbarPage(
  "Midwest Soil Health Evidence",
  id = 'navbar',
  
  # snapp logo in the bottom right
  footer = a(href = 'https://snappartnership.net/teams/managing-soil-organic-carbon/',
             img(src='pics/snapp-logo.png', align = 'right', height = 50)),
  
  ### First tab: Summary tab / landing page =======================================================================
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
    
    # Change the formatting of all select inputs using HTML
    tags$head(tags$style(HTML(".selectize-input {padding: 15px} 
                              .selectize-dropdown { font-size: 15px; line-height: 20px;}")))
    
  ),

  
  ### Second tab: Main tab with all the inputs and plots =======================================================================
  tabPanel(
    "Data",

    # shinyjs required for the update button / hide / show functionality
    useShinyjs(),


    # Set up header columns
    sidebarLayout(
      sidebarPanel2(
        selectInput(
          inputId = "MgmtPractice", label = "Practice",
          choices = unique(summary_data$Review) %>% sort(),
          selected = "Tillage"
        ),
        fluidRow(
          column(6,
                 # This is reactive on the practice, so see the generating code in server.R
                 uiOutput("filter_one")
          ),
          column(6,
                 # This is reactive on the practice, so see the generating code in server.R
                 uiOutput('filter_two')
          )
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
                    checkboxGroupInput(
                      inputId = "SoilDepth", label = "Soil Sampling Depth",
                      choices = c(sort(unique(summary_data$sample_depth)), 'Soil Surface'),# multiple = T,
                      selected = c(sort(unique(summary_data$sample_depth)), 'Soil Surface')
                      #selected = unique(summary_data$Legend_1)[1]
                    ),
                    checkboxInput(inputId = 'AllDepths', label = 'All depths & Aboveground', value = TRUE)
                 )
            )
          ),
        shinyjs::hidden(checkboxGroupInput(
          inputId = "years", label = "Years of Implementation",
          #choices = c(sort(unique(summary_data$sample_year)), NA),# multiple = T,
          choices = c(stringr::str_sort(unique(summary_data$sample_year), na_last = T, numeric = TRUE)),
          selected = "Year 1-5"
          #selected = unique(summary_data$Legend_1)[1]
        )
        ),

        # We decided to remove the Region select input, since everything's in the Midwest.
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
                          style = 'color: gray')),
        # map shows up underneath the sidebarPanel.
        out = leafletOutput("map")
      ),
    
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
      
      # # Debugging tool to see the data used to generated current plot
      # fluidRow(
      #   column(12,
      #          tableOutput(outputId = 'current_table'))
      # ),
      
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

  ### Third tab: References based on selected data in second tab =======================================================================
  
  tabPanel("References", tableOutput(outputId = "reference_table"))#,
  
  ### Optional additional tabs: Methods and the leaflet map ====================================================================
  
  #tabPanel("Methods")#,
  #tabPanel("Map", leafletOutput('map'))
  
)

# #### RUN THE APP ####
# shinyApp(ui = ui, server = server)

# # Using this display mode is a cool little debugging tool.
#runApp('www', display.mode = 'showcase')

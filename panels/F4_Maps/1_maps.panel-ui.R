###----------------------------------------###
###  User Interface for the "Maps" Module  ###
###----------------------------------------###
###
###  Date Created  : Feb 22, 2017.
###  Last Modified : Mar 19, 2017.




###-----------------###
###  Sidebar Panel  ###
###-----------------###

##  set up the "help" functionality for this module
maps.help = function() {
  help.display(
    title = "Maps Module",
    id = "Maps_Module",
    file = "panels/F4_Maps/3_maps.panel-help.md")
}


## set up the sidebar panel with "maps.sidebarPanel()"
maps.sidebarPanel = function(data.set) {
  
  ## Perform a routine data check
  if (is.null(data.set)) {
    stop("Please select a data set!")
  }
  
  sidebarPanelUI = list(
    tabsetPanel(
      id = "maps_sidebar_tabs",
      type = "pills",
      
      tabPanel(
        
        title = "Select Variables",
        
        ## type of map data
        h4("Type of Map Data"),
        
        radioButtons(
          inputId = "map_type",
          label = NULL,
          choices =
            c("Coordinate (latitude, longitude)" = 1,
              "Regions (country, state, county, etc.)" = 2),
          selected = 1
        ),
        
        hr(),
        
        h4("Mapping Variables"),
        
        conditionalPanel(
          condition = "input.map_type == 1",
          
          ## mapping variables (latitude and longitude)
          uiOutput("latitude_panel"),  
          uiOutput("longitude_panel"),
          
          hr(),
          
          h4("Code Variables"),
          
          uiOutput("colourby_panel"),
          uiOutput("sizeby_panel"),
          uiOutput("opacifyby_panel")
        ),
        
        conditionalPanel(
          condition = "input.map_type == 2",
          
          ## mapping variables (country, state and county...)
          uiOutput("maplocation_panel"),         
          uiOutput("locationvariable_panel"),
          
          hr(),
          
          ## plotting variable
          h4("Plotting Variable"),
          
          uiOutput("plottingvariable_panel")
        ),
        
#        hr(),
        
        ## subset variables
#        uiOutput("mapssubset1_panel"),
#        uiOutput("mapssubset2_panel"),
        
        hr(),
        
        maps.help()
        
      ),
      
      tabPanel(
        
        title = "More Options",
        
        conditionalPanel(
          condition = "input.map_type == 1",
          
          ## code variables
#          h4("Code Variables"),
          
#          uiOutput("colourby_panel"),
#          uiOutput("sizeby_panel"),
#          uiOutput("opacifyby_panel"),
          
#          hr(),
          
          ## plot options
#          h4("Plot Options"),
          
          uiOutput("plot_maptype_panel"),
          uiOutput("plot_colour_panel"),
          
          sliderInput(inputId = "pointsize", 
                      label = "Point size", 
                      min = 0.05, max = 3.5, 
                      value = 0.6, step = 0.05),
          
          sliderInput(inputId = "transparency", 
                      label = "Transparency", 
                      min = 0, max = 100, 
                      value = 0, step = 1),
          
          checkboxInput(inputId = "connectpoints",
                        label = strong("Connect points by lines"),
                        value = FALSE),
          
          conditionalPanel(
            condition = "input.connectpoints",
            uiOutput("linescolour_panel")
          )
        ),
        
        conditionalPanel(
          condition = "input.map_type == 2",
          
          ## plot options
#          h4("Plot Options"),
          
          uiOutput("plot_region_colour_panel"),
          uiOutput("missingvaluecolour_panel"),
          uiOutput("plotlabels_panel")
        )

      )
    )
    
  )
  
}



###--------------###
###  Main Panel  ###
###--------------###
###
###  We now set up the main panel with "maps.mainpanel()":
maps.mainPanel = function() {
  fluidPage(
    plotOutput("maps_plot"),
    
    conditionalPanel(
      condition = "input.map_type == 1 &
                   input.select_latitude != 'Select Latitude Information' &
                   input.select_longitude != 'Select Longitude Information' ||
                   input.map_type == 2 &
                   input.maplocation != 'Select Map Location' &
                   input.locationvariable != 'Select Location Variable' &
                   input.plottingvariable != 'Select Variable'",
  
      
#      downloadButton(outputId = "savemaps", label = "Save Maps"),
#      radioButtons(inputId = "savemapstype", 
#                   label = "Select the file type", 
#                   choices = list("jpg", "png", "pdf"), inline = TRUE),
      fixedRow(
        column(
          width = 5, offset = 1,
          uiOutput("mapssubset1_panel"),
          conditionalPanel(
            condition = "input.mapssubset1 != 'None'",
            ##  Slider input GUI for the first subset variable
            br(),
            uiOutput("mapssubset1_slider_panel")
          )
        ),
        column(
          width = 5, offset = 1,
          uiOutput("mapssubset2_panel"),
          ##  Slider input GUI for the second subset variable.
          conditionalPanel(
            condition = "input.mapssubset2 != 'None'",
            br(),
            uiOutput("mapssubset2_slider_panel")
          )
        )
      ),

      fixedRow(column(width = 2, offset = 1,
                      downloadButton(outputId = "savemaps", label = "Save Maps")),
               column(width = 5,
                      radioButtons(inputId = "savemapstype", 
                                   label = "Select the file type", 
                                   choices = list("jpg", "png", "pdf"), inline = TRUE)))
    )
  )
}



###------------------###
###  Mpas UI  ###
###------------------###
###
###  We combine the sidebarPanel and mainPanel functions to
###  complete the UI for the maps module.

maps.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/F4_Maps/4_maps.panel-null.md")
      )
    } else {
      fluidRow(
        column(4, maps.sidebarPanel(data.set)),
        column(8, maps.mainPanel())
      )
    }
  )
}



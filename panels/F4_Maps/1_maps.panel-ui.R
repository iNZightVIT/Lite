###----------------------------------------###
###  User Interface for the "Maps" Module  ###
###----------------------------------------###
###
###  Date Created  : Feb 22, 2017.
###  Last Modified : May 20, 2018.




###-----------------###
###  Sidebar Panel  ###
###-----------------###

##  set up the "help" functionality for this module


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
        
        title = "Select Map",
        
        ## type of map data
        h4("Type of Map Data"),
        
        radioButtons(
          inputId = "map_type",
          label = NULL,
          choices =
            c("Coordinate (latitude, longitude)" = 1,
              "Regions (country, state, county, etc.)" = 2),
          selected = ifelse(("latitude" %in% tolower(colnames(data.set))) & ("longitude" %in% tolower(colnames(data.set))), 
                            1, 2)
        ),
        
        hr(),
        
#        h4("Mapping Variables"),
        
        conditionalPanel(
          condition = "input.map_type == 1",

          h4("Mapping Variables"),
          
          ## mapping variables (latitude and longitude)
          uiOutput("latitude_panel"),  
          uiOutput("longitude_panel"),
          
          hr(),
          
          h4("Extra Plot Options"),
          
          uiOutput("plot_maptype_panel"),
          uiOutput("plot_plottitle_panel"),
          hr(),
          uiOutput("plot_overallsize_panel")
          
          
          #h4("Code Variables"),
          
          #uiOutput("colourby_panel")
          ## uiOutput("sizeby_panel"),
          ## uiOutput("opacifyby_panel")
        ),
        
        conditionalPanel(
          condition = "input.map_type == 2",
          
          h4("Select Map"),
          
          ## select map
          uiOutput("selectmap_panel"),
          
          conditionalPanel(
            condition = "input.selectshapefile == 1",
            uiOutput("inbuiltmap_panel")
          ),
          
          conditionalPanel(
            condition = "input.selectshapefile == 1 & input.selectinbuiltmap == 1",
            uiOutput("continentsoptions_panel")
          ),
          
          conditionalPanel(
            condition = "input.selectshapefile == 1 & input.selectinbuiltmap == 2",
            uiOutput("countriesoptions_panel")
          ),
          
          conditionalPanel(
            condition = "input.selectshapefile == 1 & input.selectinbuiltmap == 3",
            uiOutput("worldoptions_panel")
          ),
          
          conditionalPanel(
            condition = "input.selectshapefile == 2",
            fileInput("loadshapefiles", label = "",  multiple = F)
          ),
          
          conditionalPanel(
            condition = "input.selectshapefile == 1",
            actionButton(inputId = "importmap",
                         label = "Import Map",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          
          hr(),
          
          ## select variables
          
          h4("Select matching Name Variables"),
          
          uiOutput("datavariable_panel"),
          uiOutput("mapvairable_panel"),
          uiOutput("sequencevariable_panel"),
          
          hr(),
          
          ## unmatched data oanel
          uiOutput("unmatched_panel"),
          verbatimTextOutput("unmatchedcounts_panel")
          
#          hr(),
          
          ## mapping variables (country, state and county...)
#          uiOutput("maplocation_panel"),         
#          uiOutput("locationvariable_panel")
          
#          hr(),
          
          ## plotting variable
#          h4("Plotting Variable"),
          
#          uiOutput("plottingvariable_panel")
        ),
        
#        hr(),
        
        ## subset variables
#        uiOutput("mapssubset1_panel"),
#        uiOutput("mapssubset2_panel"),
        
        hr(),
        
        tags$a(href = "https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=maps&ver=lite", 
                      "Help", class = 'btn btn-xs btn-success',
                      target="_blank")
        
        
      ),
      
      tabPanel(
        
        title = "Select Variables",
        
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
          
          uiOutput("plot_colourcheck_panel"),

#          h4("Colour"),
          uiOutput("plot_colour_panel"),
          uiOutput("plot_colpalette_panel"),

          hr(),
          
#          h4("Size"),
          uiOutput("plot_sizecheck_panel"),
          uiOutput("plot_size_panel"),

          hr(),
          
#          h4("Transparency"),
          uiOutput("plot_transcheck_panel"),
          uiOutput("plot_trans_panel"),

          hr(),

#          h4("Point Symbol"),
          uiOutput("plot_symbolcheck_panel"),
          uiOutput("plot_pointsymbol_panel"),

          hr(),

#          h4("Connect Points"),
          
          checkboxInput(inputId = "connectpoints",
                        label = "Connect points with lines",
                        value = FALSE),
          
          conditionalPanel(
            condition = "input.connectpoints",
            uiOutput("linescolour_panel")
          ),

          hr()
        ),
        
        conditionalPanel(
          condition = "input.map_type == 2",
          
          ## plot options
#          h4("Plot Options"),

          uiOutput("advancedmapoptions_panel"),
          hr(),
          uiOutput("advancedplotoptions_panel"),
          hr(),
          uiOutput("variabletodisplay_panel"),
          hr(),
          uiOutput("multipleobsoption_panel"),
          hr(),
          uiOutput("plotas_panel"),
          uiOutput("sizeandtransparency_panel"),
          hr()
          
#          uiOutput("plot_region_colour_panel"),
#          uiOutput("missingvaluecolour_panel"),
#          uiOutput("plotlabels_panel")
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
    
    ## add a tabset for interactive maps
    tabsetPanel(
      id = "maps_selector",
      type = "pills",
      ##  Plot Panel
      tabPanel(
        title = "Maps",
        
        fixedRow(column(2, fixedRow(column(5, NULL),
                                    column(7, 
                                           actionButton("refreshmap", 
                                                        icon("refresh", "fa-2x"),
                                                        style = "color: #337ab7; 
                                                        background-color: #ffffff; 
                                                        border-color: #ffffff;
                                                        padding:4px; 
                                                        font-size:90%")))),
                 column(10, hr())),
        plotOutput("maps_plot", height = "600px"),
        
        ## to remove flickerings when plot parameters change
        tags$style(type="text/css", "#maps_plot.recalculating { opacity: 1.0; }"),
        
        conditionalPanel(
          condition = "input.map_type == 1 &
          input.select_latitude != 'Select Latitude Information' &
          input.select_longitude != 'Select Longitude Information'",
          
          fixedRow(
            column(
              width = 5, offset = 1,
              uiOutput("mapssubset1_panel"),
              conditionalPanel(
                condition = "input.mapssubset1 != 'None'",
                ##  Slider input GUI for the first subset variable
                br(),
                uiOutput("mapssubset1_slider_panel"),
                uiOutput("speed_delay1")
              )
            ),
            column(
              width = 5, offset = 1,
              uiOutput("mapssubset2_panel"),
              ##  Slider input GUI for the second subset variable.
              conditionalPanel(
                condition = "input.mapssubset2 != 'None'",
                br(),
                uiOutput("mapssubset2_slider_panel"),
                uiOutput("speed_delay2")
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
        ),

      tabPanel(
        title = "Interactive Maps",

        uiOutput("interactive.plot.download"), 
        br(),
        htmlOutput("interactive.maps")
      )
    )
  )
}



###------------------###
###  Mpas UI  ###
###------------------###
###
###  We combine the sidebarPanel and mainPanel functions to
###  complete the UI for the maps module.

newmaps.panel.ui = function(data.set) {
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



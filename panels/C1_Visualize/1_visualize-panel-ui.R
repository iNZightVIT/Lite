###  Last Modified : March 12, 2017.
###---------------------------------------------###
###  User Interface for the "Visualize" Module  ###
###---------------------------------------------###
###
###  Date Created  : January 25, 2015.
###  Last Modified : April 1, 2017.
###
###  The UI is divided into two panels:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: Thie file is to be sourced locally within "server.R" *


###-----------------###
###  Sidebar Panel  ###
###-----------------###
###
###  First, we set up the "help" functionality for this module.
visualize.help = function() {
    help.display("Visualize Module",
                 "Visualize",
                 "panels/C1_Visualize/3_visualize-panel-help.md")
}


###  Next, we set up the sidebar panel with "vis.sidebarPanel()".
vis.sidebarPanel = function() {
  
  sidebarPanelUI = list(
    
    tabsetPanel(
      id = "visualize_sidebar_tabs",
      type = "pills",
      
      tabPanel(
        title = "Select Variables",
        
        hr(),
        
        fixedRow(column(10,h4("Variable selection")),
                 column(2,checkboxInput("change_var_selection",
                                        value=F,
                                        label=""))),
        
        ##  Select the first variable.
        uiOutput("vari1_panel"),
        
        ##  Select the second variable.
        uiOutput("vari2_panel"),
        hr(),
        ## Select desired subset for the first variable.
        uiOutput("subs1_panel"),
        
        ##  Select desired subset for the second variable.
        uiOutput("subs2_panel"),
        
        
        ##########
        
        hr(),
        actionButton(inputId = "reset.graphics",
                     label = "Reset To Default"),
        br(),
        br(),
        visualize.help()
      ),
      
      tabPanel(
        title = "Add To Plot",
        
        hr(),

        
#        fixedRow(column(8,h4("Inference")),
#                 column(2,
#                        checkboxInput("toggle_inference",
#                                      label="",
#                                      value=input$toggle_inference))),
#        fixedRow(column(width = 8,
#                        uiOutput("add_inference"))),
        
#        hr(),
        
#        h4("Select Additions"),
        
        uiOutput("select_additions_panel"),
        
        conditionalPanel(
          condition = "input.select_additions=='Customise Plot Appearance'",
          
          uiOutput("plot.appearance.panel"),
          uiOutput("code.variables.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Trend Lines and Curves'",
          
          uiOutput("trend.curve.panel"),
          uiOutput("join.points.panel"),
          uiOutput("xy.line.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Axes and Labels'",
          
          uiOutput("customize.labels.panel"),
          uiOutput("add.jitter.panel"),
          uiOutput("add.rugs.panel"),
          uiOutput("adjust.axis.panel"),
          uiOutput("adjust.number.bars.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Identify Points'",
          
          uiOutput("points.identify.panel")
        ),

        conditionalPanel(
          condition = "input.select_additions=='Add Inference Information'",
  
#          fixedRow(column(8,h4("Inference")),
#                   column(2,
#                          checkboxInput("toggle_inference",
#                                        label="",
#                                        value=TRUE))),
          fixedRow(column(width = 8,
                          uiOutput("add_inference")))
        )

      )
    )
  )
}



###  We set up the main panel.
vis.mainPanel = function() {
  toggle_advanced_options = T
  if(!is.null(input$toggle_advanced_options)){
    toggle_advanced_options = input$toggle_advanced_options
  }
  panel = list(
    br(),
    tabsetPanel(
      id = "plot_selector",
      type = "pills",
      ##  Plot Panel
      tabPanel(
        title = "Plot",
        br(),
        fixedRow(
          column(width = 12,
                 helpText("Plots for visualizing data."),
                 plotOutput("visualize.plot"),
                 
                 fixedRow(column(width = 2, offset = 1,
                                 downloadButton(outputId = "saveplot", label = "Save Plot")),
                          column(width = 6,
                                 radioButtons(inputId = "saveplottype", 
                                              label = "Select the file type", 
                                              choices = list("jpg", "png", "pdf", "interactive html"), inline = TRUE)),
                          column(width = 2,
                                 actionButton(inputId = "gotointeractivehtml", 
                                              label = "Interactive HTML"))),
                 
#                 downloadButton(outputId = "saveplot", label = "Save Plot"),
#                 radioButtons(inputId = "saveplottype", 
#                              label = "Select the file type", 
#                              choices = list("jpg", "png", "pdf"), inline = TRUE),
                 fixedRow(
                   column(
                     width = 5, offset = 1,
                     conditionalPanel(
                       condition = "input.subs1 != 'none'",
                       ##  Slider input GUI for the first variable.
                       br(),
                       uiOutput("subs1_conditional")
                     )
                   ),
                   column(
                     width = 5, offset = 1,
                     ##  Slider input GUI for the second variable.
                     conditionalPanel(
                       condition = "input.subs2 != 'none'",
                       br(),
                       uiOutput("subs2_conditional")
                     )
                   )
                 )           
                )
          )
        ),
      ##  Summary Panel
      tabPanel(
        title = "Summary",
        br(),
        helpText("Statistical Sumary for the data."),
        verbatimTextOutput("visualize.summary")
        ),
      ##  Inference Panel
      tabPanel(
        title = "Inference",
        br(),
        selectInput("type.inference.select",
                    choices = c("normal",
                                "bootstrap"),
                    label = "Select type of inference"),
        br(),
        helpText("Statistical Inference for the data."),
        verbatimTextOutput("visualize.inference")
        )
      )
    )
  panel
}

###----------------------###
###  Visualize Panel UI  ###
###----------------------###
###
###  We combine the vis.sidebarPanel() and vis.mainPanel() functions to
###  complete the UI for the Time Series module. If no data set has been
###  selected, we display a helpful warning message.

visualize.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md")
      )
    } else {
      fixedRow(
        column(4, vis.sidebarPanel()),
        column(8, vis.mainPanel())
      )
    }
  )
}









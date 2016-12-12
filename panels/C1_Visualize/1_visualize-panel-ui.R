###  Last Modified : March 22, 2015.
###---------------------------------------------###
###  User Interface for the "Visualize" Module  ###
###---------------------------------------------###
###
###  Date Created  : January 25, 2015.
###  Last Modified : March 22, 2015.
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
vis.sidebarPanel1 = function() {
    ##  Note the user of "hr()" (= horizontal rule) to separate
    ##  sections.
    sidebarPanelUI = list(
        ##########
        hr(),
        actionButton(inputId = "go.visualisation",
                   label = "Go To Visualisation"),
        
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
    )
}





#############################
## set up another side bar ##
#############################

vis.sidebarPanel2 = function() {
  ##  Note the user of "hr()" (= horizontal rule) to separate
  ##  sections.
  sidebarPanelUI = list(
    hr(),
    actionButton(inputId = "backto.variableselection",
                 label = "Back To Variable Selection"),
    hr(),
    fixedRow(column(8,h4("Inference")),
             column(2,
                    checkboxInput("toggle_inference",
                                  label="",
                                  value=input$toggle_inference))),
    fixedRow(column(width = 8,
                    uiOutput("add_inference"))),
    fixedRow(column(8,h4("Advanced options")),column(2,
                                                     checkboxInput("toggle_advanced_options",
                                                                   label="",
                                                                   value=TRUE))),
    conditionalPanel("input.toggle_advanced_options",
                     fixedRow(column(width = 12,
                                     uiOutput("advanced_options_panel"))),
                     fixedRow(column(width=12,
                                     conditionalPanel("input.advanced_options=='Code more variables'",
                                                      uiOutput("code.variables.panel")),
                                     conditionalPanel("input.advanced_options=='Add trend curves'",
                                                      uiOutput("trend.curve.panel")),
                                     conditionalPanel("input.advanced_options=='Add x=y line'",
                                                      uiOutput("xy.line.panel")),
                                     conditionalPanel("input.advanced_options=='Add a jitter'",
                                                      uiOutput("add.jitter.panel")),
                                     conditionalPanel("input.advanced_options=='Add rugs'",
                                                      uiOutput("add.rugs.panel")),
                                     conditionalPanel("input.advanced_options=='Join points by line'",
                                                      uiOutput("join.points.panel")),
                                     conditionalPanel("input.advanced_options=='Change plot appearance'",
                                                      uiOutput("plot.appearance.panel")),
                                     conditionalPanel("input.advanced_options=='Identify points'",
                                                      uiOutput("points.identify.panel")),
                                     conditionalPanel("input.advanced_options=='Customize labels'",
                                                      uiOutput("customize.labels.panel")),
                                     conditionalPanel("input.advanced_options=='Adjust axis limits'",
                                                      uiOutput("adjust.axis.panel")),
                                     conditionalPanel("input.advanced_options=='Adjust number of Bars'",
                                                      uiOutput("adjust.number.bars.panel"))))),
    hr(),
    actionButton(inputId = "reset.graphics",
                 label = "Reset To Default"),
    br(),
    br(),
    visualize.help()
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

visualize.panel1.ui = function(data.set) {
    fluidPage(  
        if (is.null(data.set)) {
            fixedRow(
                includeMarkdown(
                    "panels/C1_Visualize/4_visualize-panel-null.md")
            )
        } else {
          fixedRow(
            column(4, vis.sidebarPanel1()),
            column(8, vis.mainPanel())
          )
        }
    )
}

visualize.panel2.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md")
      )
    } else {
      fixedRow(
        column(4, vis.sidebarPanel2()),
        column(8, vis.mainPanel())
      )
    }
  )
}



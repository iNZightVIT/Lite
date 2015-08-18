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
vis.sidebarPanel = function() {
    ##  Note the user of "hr()" (= horizontal rule) to separate
    ##  sections.
    sidebarPanelUI = list(
        hr(),
        ##  Select the first variable.
        uiOutput("vari1_panel"),

        ##  Select the second variable.
        uiOutput("vari2_panel"),
        hr(),
        ## Select desired subset for the first variable.
        uiOutput("subs1_panel"),

        ##  Select desired subset for the second variable.
        uiOutput("subs2_panel"),
        hr(),
        ##  Reset graphical parameters.
        radioButtons(inputId = "customize_plot",
                     label =  "Advanced Options: ",
                     choices =
                         c("Hide" = 1,
                           "Show" = 2),
                     selected = 1),
        hr(),
        actionButton(inputId = "reset.graphics",
                     label = "Reset All"),
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
                 conditionalPanel(condition = "input.customize_plot == 1",
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
                                  ),
                 conditionalPanel(condition = "input.customize_plot == 2",
                                  fixedRow(
                                    column(
                                      width = 8,
                                      helpText("Plots for visualizing the data."),
                                      plotOutput("mini.plot"),
                                      fixedRow(column(width = 6,
                                                      conditionalPanel(
                                                        condition = "input.subs1 != 'none'",
                                                        uiOutput("subs1_conditional_mini")
                                                        )
                                                      ),
                                               column(width = 6,
                                                      conditionalPanel(condition = "input.subs2 != 'none'",
                                                                       uiOutput("subs2_conditional_mini")
                                                                       )
                                                      )
                                               )
                                      ),
                                    column(
                                      width = 4,
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
                                                                                                     value=toggle_advanced_options))),
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
                                                                                        uiOutput("adjust.number.bars.panel"))))))
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
                column(2, vis.sidebarPanel()),
                column(10, vis.mainPanel())
            )
        }
    )
}

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
                 "panels/5_Visualize/3_visualize-panel-help.md")
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
        conditionalPanel(
            condition = "input.subs1 != 'none'",
            uiOutput("subs2_panel")
        ),
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
    panel = list(
        br(),
        tabsetPanel(
            id = "plot_selector",
            type = "pills",
            ##  Plot Panel
            tabPanel(
                title = "Plot",
                br(),
                fluidRow(
                    column(
                        width = 12,                            
                        fluidRow(
                            conditionalPanel(
                                condition = "input.customize_plot == 1",
                                column(
                                    width = 12,
                                    helpText("Plots for visualizing data."),  
                                    plotOutput("visualize.plot"),
                                    fluidRow(
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
                        conditionalPanel(
                            condition = "input.customize_plot == 2",
                            column(
                                width = 8,
                                helpText("Plots for visualizing the data."),
                                plotOutput("mini.plot"),
                                fluidRow(
                                    column(
                                        width = 6,                                       
                                        conditionalPanel(
                                            condition = "input.subs1 != 'none'",
                                            uiOutput("subs1_conditional_mini")
                                        )
                                    ),
                                    column(
                                        width = 6,
                                        conditionalPanel(
                                            condition = "input.subs2 != 'none'",
                                            uiOutput("subs2_conditional_mini")
                                        )
                                    )
                                )
                            ),
                            column(
                                width = 4,
                                helpText("Advanced Options"),
                                br(),
                                fluidRow(
                                    column(
                                        width = 12,
                                        selectInput(inputId = "advanced-options",
                                                    label = "Options",
                                                    choices =
                                                        c("Plot Appearance",
                                                          "Inferential Markups"),
                                                    selected = "Plot Appearance")
                                    )
                                ),                                            
                                fluidRow(
                                    column(
                                        width = 6,
                                        textInput(inputId = "title", label = "Title:")
                                    ),
                                    column(
                                        width = 6,
                                        textInput(inputId = "xlab", label = "Axis Label:")
                                    )
                                ),
                                fluidRow(
                                    column(
                                        width = 6,
                                        uiOutput("resize.by")
                                    ),
                                    column(
                                        width = 6,
                                        uiOutput("colour.by")
                                    )
                                ),
                                fluidRow(
                                    column(
                                        width = 6,
                                        uiOutput("background")
                                    ),
                                    column(
                                        width = 6,
                                        uiOutput("plot_type")
                                    )
                                ),
                                fluidRow(
                                    column(
                                        width = 6,
                                        ##  Choose Object Colour
                                        conditionalPanel(
                                            condition = "input.choose_plot == 1",
                                            uiOutput("bar_colour")
                                        ),
                                        conditionalPanel(
                                            condition = "input.choose_plot == 2",
                                            uiOutput("symbol_colour")
                                        ),
                                        conditionalPanel(
                                            condition = "input.choose_plot == 3",
                                            uiOutput("box_colour")
                                        )
                                    ),
                                    column(
                                        width = 6,
                                        conditionalPanel(
                                            condition = "input.choose_plot == 1",
                                            ## uiOutput("bar_width"),
                                            uiOutput("bar_border")
                                        ),
                                        conditionalPanel(
                                            condition = "input.choose_plot == 2",
                                            ## uiOutput("symbol_transparency"),
                                            uiOutput("symbol_size")
                                        ),
                                        conditionalPanel(
                                            condition = "input.choose_plot == 3",
                                            ## uiOutput("box_width"),
                                            uiOutput("box_border")
                                        )
                                    )                     
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
            fluidRow(
                includeMarkdown(
                    "panels/5_Visualize/4_visualize-panel-null.md")
            )
        } else {
            fluidRow(
                column(2, vis.sidebarPanel()),
                column(10, vis.mainPanel())
            )
        }
    )
}

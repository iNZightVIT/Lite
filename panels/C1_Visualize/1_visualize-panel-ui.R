###  Last Modified : March 12, 2017.
### ---------------------------------------------###
###  User Interface for the "Visualize" Module  ###
### ---------------------------------------------###
###
###  Date Created  : January 25, 2015.
###  Last Modified : Feb 15, 2019.
###
###  The UI is divided into two panels:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: Thie file is to be sourced locally within "server.R" *


### -----------------###
###  Sidebar Panel  ###
### -----------------###
###
###  First, we set up the "help" functionality for this module.
visualize.help <- function() {
  help.display(
    "Visualize Module",
    "Visualize",
    "panels/C1_Visualize/3_visualize-panel-help.md"
  )
}


###  Next, we set up the sidebar panel with "vis.sidebarPanel()".
vis.sidebarPanel <- function() {
  #  conditionalPanel(condition = "output.showsidebar == 1",
  sidebarPanelUI <- list(
    tabsetPanel(
      id = "visualize_sidebar_tabs",
      type = "pills",
      tabPanel(
        title = "Select Variables",
        fixedRow(
          column(9, hr()),
          column(3,
            align = "left",
            conditionalPanel(
              condition = "output.showsidebar == 1",
              actionButton("hideSidebar",
                icon("arrow-circle-left", "fa-2x"),
                style = paste(
                  "color: #337ab7;",
                  "background-color: #ffffff;",
                  "border-color: #ffffff;",
                  "padding:4px;",
                  "font-size:100%"
                )
              )
            )
          )
        ),
        h5(strong("Variable selection")),

        ##  Select the first variable.
        h5("Select first variable:"),
        fixedRow(
          column(8, uiOutput("vari1_panel")),
          column(2, actionButton("switch1", "",
            icon = icon("arrow-down", "fa-2x"),
            style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:60%"
          ))
        ),

        ##  Select the second variable.
        h5("Select second variable:"),
        fixedRow(
          column(8, uiOutput("vari2_panel")),
          column(2, actionButton("switch2", "",
            icon = icon("arrow-down", "fa-2x"),
            style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:60%"
          ))
        ),
        fixedRow(column(10, hr())),

        ## Select desired subset for the first variable.
        h5("Subset by:"),
        fixedRow(
          column(8, uiOutput("subs1_panel")),
          column(2, actionButton("switch3", "",
            icon = icon("arrow-down", "fa-2x"),
            style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:60%"
          ))
        ),

        ##  Select desired subset for the second variable.
        h5("Subset by:"),
        fixedRow(column(8, uiOutput("subs2_panel"))),
        fixedRow(column(10, hr())),
        actionButton(
          inputId = "reset.graphics",
          label = "Reset To Default",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        br(),
        br(),
        visualize.help()
      ),
      tabPanel(
        title = "Add To Plot",
        id = "add.to.plot",
        class = "plausible-event-name=Plot+Panel",
        fixedRow(
          column(9, hr()),
          column(3, conditionalPanel(
            condition = "output.showsidebar == 1",
            actionButton("hideSidebar2",
              icon("arrow-circle-left", "fa-2x"),
              style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:100%"
            )
          ))
        ),
        uiOutput("select_additions_panel"),
        conditionalPanel(
          condition = "input.select_additions=='Customise Plot Appearance'",
          uiOutput("plot.appearance.panel.title"),
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
          fixedRow(column(
            width = 8,
            uiOutput("add_inference")
          ))
        )
      )
    )
  )
  #                   )
}



###  We set up the main panel.
vis.mainPanel <- function() {
  toggle_advanced_options <- T
  if (!is.null(input$toggle_advanced_options)) {
    toggle_advanced_options <- input$toggle_advanced_options
  }
  panel <- list(
    strong(textOutput("data_info")),
    br(),
    tabsetPanel(
      id = "plot_selector",
      type = "pills",
      ##  Plot Panel
      tabPanel(
        title = "Plot",
        # br(),
        fixedRow(
          column(
            width = 12,
            fixedRow(
              column(2, fixedRow(
                column(
                  5,
                  conditionalPanel(
                    condition = "output.showsidebar == 0",
                    actionButton("showSidebar",
                      icon("arrow-circle-right", "fa-2x"),
                      style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:100%"
                    )
                  )
                ),
                column(
                  7,
                  actionButton("refreshplot",
                    icon("refresh", "fa-2x"),
                    style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:90%"
                  )
                )
              )),
              column(10, hr())
            ),
            helpText("Plots for visualizing data."),
            plotOutput("visualize.plot"),
            br(),
            fixedRow(
              column(
                width = 4,
                downloadButton(outputId = "saveplot", label = "Download Plot")
              ),
              column(
                width = 5,
                radioButtons(
                  inputId = "saveplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf", "svg"), inline = TRUE
                )
              ),
              column(
                width = 3,
                uiOutput("add.fitted.residuals.panel")
              )
            ),
            fixedRow(
              column(
                width = 5, offset = 1,
                conditionalPanel(
                  condition = "input.subs1 != 'none'",
                  ##  Slider input GUI for the first variable.
                  br(),
                  uiOutput("subs1_conditional"),
                  uiOutput("speed_value1")
                )
              ),
              column(
                width = 5, offset = 1,
                ##  Slider input GUI for the second variable.
                conditionalPanel(
                  condition = "input.subs2 != 'none'",
                  br(),
                  uiOutput("subs2_conditional"),
                  uiOutput("speed_value2")
                )
              )
            ),
            br(),
            includeMarkdown("panels/C1_Visualize/5_visualize-panel-note.md")
          )
        )
      ),
      ##  Summary Panel
      tabPanel(
        title = "Summary",
        class = "plausible-event-name=Summary+Panel",
        fixedRow(
          column(2, conditionalPanel(
            condition = "output.showsidebar == 0",
            actionButton("showSidebar2",
              icon("arrow-circle-right", "fa-2x"),
              style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:100%"
            )
          )),
          column(10, hr())
        ),
        helpText("Statistical Summary for the data."),
        verbatimTextOutput("visualize.summary"),
        helpText("Formatting options"),
        fixedRow(
          column(
            3,
            numericInput("global.sig.level",
              label = "Round values (significant figures)",
              value = graphical.par$signif,
              min = 1, step = 1
            )
          ),
          column(
            3,
            numericInput("global.round.pct",
              label = "Round percentages (decimal places)",
              value = graphical.par$round_percent,
              min = 0, step = 1
            )
          )
        )
      ),
      ##  Inference Panel
      tabPanel(
        title = "Inference",
        class = "plausible-event-name=Inference+Panel",
        fixedRow(
          column(2, conditionalPanel(
            condition = "output.showsidebar == 0",
            actionButton("showSidebar3",
              icon("arrow-circle-right", "fa-2x"),
              style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:100%"
            )
          )),
          column(10, hr())
        ),
        fixedRow(
          column(3, uiOutput("inference_type")),
          column(3, uiOutput("inference_test")),
          column(6, uiOutput("inference_out")),
          column(9, uiOutput("inference_opts")),
          column(3, uiOutput("inference_epi"))
        ),
        br(),
        helpText("Statistical Inference for the data."),
        verbatimTextOutput("visualize.inference")
      ),
      tabPanel(
        value = 1,
        title = "Interactive Plot",
        fixedRow(
          column(2, conditionalPanel(
            condition = "output.showsidebar == 0",
            actionButton("showSidebar4",
              icon("arrow-circle-right", "fa-2x"),
              style = "color: #337ab7; background-color: #ffffff; border-color: #ffffff; padding:4px; font-size:100%"
            )
          )),
          column(10, hr())
        ),
        uiOutput("interactive.plot.select"),
        br(),
        htmlOutput("interactive.plot")
      ),
      tabPanel(
        value = 2,
        title = "Interactive Plot (via plotly)",
        uiOutput("plotly_nw"),
        plotlyOutput("plotly_inter", height = "500px") %>% withSpinner()
      ),
      tabPanel(
        title = "VIT",
        class = "plausible-event-name=VIT+Panel",
        radioButtons(
          "vit-type", "VIT Test",
          c("Bootstrap" = "bootstrap", "Randomisation Test" = "random")
        ),
        tags$iframe(name = "vit-frame", title = "VIT Frame", class = "fill-frame"),
        value = "vit"
      )
    )
  )
  panel
}



### ----------------------###
###  Visualize Panel UI  ###
### ----------------------###
###
###  We combine the vis.sidebarPanel() and vis.mainPanel() functions to
###  complete the UI for the Time Series module. If no data set has been
###  selected, we display a helpful warning message.

visualize.panel.ui <- function(data.set) {
  fluidPage(
    useShinyjs(),
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md"
        )
      )
    } else {
      fluidRow(
        column(width = 4, id = "Sidebar", vis.sidebarPanel()),
        column(width = 8, id = "Main", vis.mainPanel())
      )
    }
  )
}


hidesidebar.visualize.panel.ui <- function(data.set) {
  fluidPage(
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md"
        )
      )
    } else {
      fluidRow(
        column(12, vis.mainPanel())
      )
    }
  )
}

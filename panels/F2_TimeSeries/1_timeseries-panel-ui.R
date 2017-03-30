###-----------------------------------------------###
###  User Interface for the "Time Series" Module  ###
###-----------------------------------------------###
###
###  Date Created  : January 16, 2015.
###  Last Modified : March 30, 2017.
###
###  The UI is divided into two panels:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: This is to be sourced within "server.R" *

###-----------------###
###  Sidebar Panel  ###
###-----------------###
###
###  First, we set up the "help" functionality for this module.
ts.help = function() {
    help.display(
        title = "Time Series Module",
        id = "Time_Series",
        file = "panels/F2_TimeSeries/3_timeseries-panel-help.md")
}



###  Next, we set up the sidebar panel with "ts.sidebarPanel()".
ts.sidebarPanel = function(data.set) {
  ##  Perform a routine data check (to be replaced).
  if (is.null(data.set)) {
        stop("Please select a data set!")
  }
  ##  We set up the sidebar panel UI. The code is in 4 sections:
  ##
  ##    -  Section 1: Time Information
  ##    -  Section 2: Seasonal Pattern
  ##    -  Section 3: Select Variables
  ##    -  Section 4: Select Labels
  ##
  ##  Note the use of "hr()" (= horizontal rule) to separate sections.
  sidebarPanelUI = list(
      ##  Section 1: Time Information
      ##
      ##  First, we ask the user to provide time information.
      ##  This can be done by either selecting a variable from a list
      ##  of variables extracted from the dataset, or providing one
      ##  manually.
      hr(),
      #uiOutput("time_info"),
       radioButtons(
           inputId = "time_info",
           label = "Time Information: ",
           choices =
               c("Select time variable" = 1,
                 "Provide time manually" = 2)
       ),
      ##  If the user decides to select a variable, then load a panel
      ##  which contains the list of variables extracted from the
      ##  dataset.
      conditionalPanel(
          condition = "input.time_info == 1",
          uiOutput("time.select")
      ),
      ##  If the user chooses to provide time information manually,
      ##  then load a panel that allows the user to do so.
      conditionalPanel(
          condition = "input.time_info == 2",
          helpText("Choose a start date and frequency. Choose any 
                   day of the starting month. This will add an 
                   additional column to the data called \"time\"."),
          dateInput("provide_startdate",label="",format="yyyy-mm-dd"),
          selectInput(inputId = "provide_frequency",
                    label = "Frequency: ",
#                       choices=c("","Day","Month","Quarter"),# to be added back when dayly data can be used
                    choices=c("","Month","Quarter"),
                    selected=""),
          actionButton(inputId = "provide_actionButton",
                       label = "Provide time information")
      ),
      hr(),
      ##  Section 2: Seasonal Pattern
      ##
      ##  Next, we ask the user to specify a seasonal pattern.
      ##  This can be either additive or multiplicative.
      radioButtons(inputId = "choose_season",
                   label = "Seasonal Pattern: ",
                   choices =
                       c("Additive" = FALSE,
                         "Multiplicative" = TRUE)
                   ),
      ##  A slider bar for smoothing parameter.
      sliderInput(inputId = "slidersmoothing", 
                  label = "Smoothness:", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.01),
      ##  Section 3: Select Variables
      ##
      ##  We then ask the user to select the variables to plot in the
      ##  main panel. This amounts to selecting the points to be plotted
      ##  on the y-axis. "rev()" is used so that a non-time variable is
      ##  selected (since time is on the x-axis), since the time
      ##  variable is often the first element of "colnames(data)".
      uiOutput("time.plot.select"),

      ##  Section 4: Select Labels
      ##
      ##  We give the user the option of providing her own set of x- and
      ##  y- axes labels. This is set to "No" by default to save screen
      ##  real estate.
      hr(),
      radioButtons(inputId = "customize_labels",
                   label =  "Customize Labels: ",
                   choices =
                       c("No" = 1,
                         "Yes" = 2),
                   selected = 1),
      ## hr(),
      conditionalPanel(
          condition = "input.customize_labels == 2",
          textInput(inputId = "provide_xlab",
                    label = "Label for the x-axis:",
                    value = ""),
          textInput(inputId = "provide_ylab",
                    label = "Label for the y-axis:",
                    value = "")
      ),
      hr(),
      ts.help()
  )
}



###--------------###
###  Main Panel  ###
###--------------###
###
###  We now set up the main panel with "ts.mainpanel()":
ts.mainPanel = function() {
    ##  We set up the main panel UI. The code is organised in 3 sections:
    ##
    ##    -  Section 1: Data Validation
    ##    -  Section 2: Single Series Plots
    ##    -  Section 3: Multiple Series Plots
    ##
    ##  Note the use of "br()" (= line break) for vertical spacing.

    mainPanelUI = list(
        ##  Section 1: Data Validation
        ##
        ##  First, we add a data validation mechanism.
        ## h3(textOutput(outputId = "validate"), style = "color:red"),
        ## h3(textOutput(outputId = "variable_message"), style = "color:red"),

        ##  Section 2: Single Series Plots
        ##
        ##  Next, we create a tabset panel for Single Series Plots.
        ##  This panel appears if the user selects only ONE variable to
        ##  plot. Note that each tab (within a panel) has some help text
        ##  preceding any output produced.
        conditionalPanel(
            condition = "input.select_variables.length == 1",
            tabsetPanel(
                id = "singleSeriesTabs",
                type = "pills", # Try type = "tabs" is you wish...
                ##  Tab 1: Time Series Plot
                tabPanel(
                    title = "Time",
                    helpText(
                        br(),
                        "A",
                        strong("Time Plot"),
                        "for",
                        strong("single"),
                        "series."),
                    plotOutput("timeseries_plot")
                ),
                ## uiOutput("timeseries_layout")),
                ## actionButton(inputId = "start_animate",
                ##              label = "Start Animation"),
                ## actionButton(inputId = "stop_animate",
                ##              label = "Stop Animation")
                ##  Tab 2: Seasonal Plot
                tabPanel(
                    title = "Seasonal",
                    helpText(
                        br(),
                        "A",
                        strong("Seasonal Plot"),
                        "for",
                        strong("single"),
                        "series.",
                        br()),
                    plotOutput(outputId = "seasonal_plot")
                ),

                ##  Tab 3: Decomposed Plot
                tabPanel(
                    title = "Decomposed",
                    helpText(
                        br(),
                        "A",
                        strong("Decomposed Plot"),
                        "for",
                        strong("single"),
                        "series.",
                        br(),
                        br()),
                    plotOutput(outputId = "decomposed_plot")
                ),

                ##  Tab 4: Trend + Seasonal Plot
                tabPanel(
                    title = "Recomposed",
                    helpText(
                        br(),
                        "A",
                        strong("Recomposed Plot"),
                        "for",
                        strong("single"),
                        "series.",
                        br(),
                        br()),
                    plotOutput(outputId = "trSeasonal_plot")
                ),

                ##  Tab 5: Forecast Plot
                tabPanel(
                    title = "Forecast",
                    helpText(
                        br(),
                        "A",
                        strong("Forecast Plot"),
                        "for",
                        strong("single"),
                        "series.",
                        br(),
                        br()
                    ),
                    plotOutput(outputId = "forecast_plot")
                ),

                ##  Tab 6: Forecast Summary
                tabPanel(
                    title = "Summary",
                    helpText(
                        br(),
                        "A",
                        strong("Forecast Summary"),
                        "for",
                        strong("single"),
                        "series.",
                        br(),
                        br()
                    ),
                    verbatimTextOutput(outputId = "forecast_summary")
                )
            )
        ),
        ##  Section 3: Multiple Series Plots
        conditionalPanel(
            condition = "input.select_variables.length > 1",
            tabsetPanel(
                id = "multipleSeriesTabs",
                type = "pills", # Try type = "tabs" if you wish...
                ##  Tab 1:  Single Plot Layout
                tabPanel(
                    title = "Single Plot",
                    helpText(
                        br(),
                        "A",
                        strong("Single-Plot"),
                        "for",
                        strong("several"),
                        "series.",
                        br(),
                        br()
                    ),
                    uiOutput("multipleSeries_single_layout")
                ),
                ##  Tab 2:  Multiple Plot Layout
                tabPanel(
                    title = "Multiple Plot",
                    helpText(
                        br(),
                        "A",
                        strong("Multi-Plot"),
                        "for",
                        strong("several"),
                        "series.",
                        br(),
                        br()
                    ),
                    uiOutput("multipleSeries_multi_layout")
                )
            )
        )
    )
}



###------------------###
###  Time Series UI  ###
###------------------###
###
###  We combine the ts.sidebarPanel() and ts.mainPanel() functions to
###  complete the UI for the Time Series module.

timeseries.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/F2_TimeSeries/4_timeseries-panel-null.md")
      )
    } else {
      fluidRow(
        column(2, ts.sidebarPanel(data.set)),
        column(10, ts.mainPanel())
      )
    }
  )
}

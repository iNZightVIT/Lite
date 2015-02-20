###-----------------------------------------------###
###  User Interface for the "Time Series" Module  ###
###-----------------------------------------------###
###
###  Date Created  : January 16, 2015.
###  Last Modified : January 20, 2015.
###
###  The UI is divided into two panels:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the documentation for the Time Series module in
###  "/documentation/time-series-module" before modifying any code.
###
###  If you have any questions and/or suggestions, drop me an e-mail:
###  Chris Park <cpar137@aucklanduni.ac.nz>
###
###  Note: This file is to be sourced locally within "server.R".

###-----------------###
###  Sidebar Panel  ###
###-----------------###
###
###  First, we set up the sidebar panel with "ts.sidebarPanel()".
ts.sidebarPanel <- function() {
    ##  Perform a routine data check.
    if (is.null(data)) {
        stop("Please load a suitable dataset!")
    }
    ##  We set up the sidebar panel UI. The code is organised in 4 sections:
    ##
    ##    -  Section 1: Time Information
    ##    -  Section 2: Seasonal Pattern
    ##    -  Section 3: Select Variables
    ##    -  Section 4: Select Labels
    ##
    ##  Note the use of "hr()" (= horizontal rule) to separate sections.
    sidebarPanelUI <- list(
        ##  Section 1: Time Information
        ##
        ##  First, we ask the user to provide time information.
        ##  This can be done by either selecting a variable from a list
        ##  of variables extracted from the dataset, or providing one
        ##  manually.
        hr(),
        radioButtons(inputId = "time_info",
                     label = "Time Information: ",
                     choices =
                         c("Select time variable" = 1,
                           "Provide time manually" = 2),
                     selected = 1),
        ##  If the user decides to select a variable, then load a panel
        ##  which contains the list of variables extracted from the dataset.
        conditionalPanel(
            condition = "input.time_info == 1",
            selectInput(inputId = "select_timevars",
                        label = "Select time variable: ",
                        choices = colnames(data))
        ),
        ##  If the user chooses to provide time information manually, then
        ##  load a panel that allows the user to do so.
        conditionalPanel(
            condition = "input.time_info == 2",
            textInput(inputId = "provide_startdate",
                      label = "Specify start date: "),
            textInput(inputId = "provide_season",
                      label = "Specify season: "),
            textInput(inputId = "provide_frequency",
                      label = "Specify frequency: "),
            actionButton(inputId = "provide_actionButton",
                         label = "Provide time information")
        ),
        ##  Section 2: Seasonal Pattern
        ##
        ##  Next, we ask the user to specify a seasonal pattern. This can be
        ##  either additive or multiplicative.
        radioButtons(inputId = "choose_season",
                     label = "Seasonal Pattern: ",
                     choices =
                         c("Multiplicative" = TRUE,
                           "Additive" = FALSE)),
        ##  Section 3: Select Variables
        ##
        ##  We then ask the user to select the variables to plot in the
        ##  main panel. This amounts to selecting the points to be plotted
        ##  on the y-axis. "rev()" is used so that a non-time variable is
        ##  selected (since time is on the x-axis), since the time variable
        ##  is often the first element of the vector "colnames(data)".
        ##  If this sounds confusing, e-mail me: <cpar137@aucklanduni.ac.nz>.
        selectInput(inputId = "select_variables",
                    label = "Series Variables: ",
                    choices =  rev(colnames(data)),
                    selected = rev(colnames(data))[1],
                     multiple = TRUE),
        helpText("(Remove variables with the",
                 strong('Delete'),
                 "key)"),

        ##  Section 4: Select Labels
        ##
        ##  We give the user the option of providing her own set of x- and
        ##  y- axes labels. This is set to "No" by default to save screen
        ##  real estate.
        radioButtons(inputId = "customize_labels",
                     label =  "Customize Labels: ",
                     choices =
                         c("No" = 1,
                           "Yes" = 2),
                     selected = 1),
        hr(),
        conditionalPanel(
            condition = "input.customize_labels == 2",
            textInput(inputId = "provide_xlab",
                      label = "Label for the x-axis",
                      value = ""),
            textInput(inputId = "provide_ylab",
                      label = "Label for the y-axis",
                      value = "")
        ),
        hr()
    )
    
}


###--------------###
###  Main Panel  ###
###--------------###
###
###  We now set up the main panel with "ts.mainpanel()":
ts.mainPanel <- function() {
    ##  Perform a routine data check.
    if (is.null(data)) {
        stop("Please load a non-empty dataset!")
    }
    ##  We set up the main panel UI. The code is organised in 3 sections:
    ##
    ##    -  Section 1: Data Validation
    ##    -  Section 2: Single Series Plots
    ##    -  Section 3: Multiple Series Plots
    ##
    ##  Note the use of "br()" (= line break) for vertical spacing.

    mainPanelUI <- list(
        ##  Section 1: Data Validation
        ##
        ##  First, we add a data validation mechanism.
        h3(textOutput(outputId = "validate"), style = "color:red"),

        ##  Section 2: Single Series Plots
        ##
        ##  Next, we create a tabset panel for Single Series Plots.
        ##  This panel appears if the user selects only ONE variable to plot.
        ##  Note that each tab (within a panel) has some help text preceding
        ##  any output produced.
        conditionalPanel(
            condition = "input.select_variables.length == 1",
            tabsetPanel(id = "singleSeriesTabs",
                        type = "pills", # Try type = "tabs" is you wish...
                        ##  Tab 1: Time Series Plot
                        tabPanel(title = "Time Series",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Time Series Plot"),
                                     "for",
                                     strong("single"),
                                     "series."),
                                 plotOutput("timeseries_plot")),
                                 ## uiOutput("timeseries_layout")),
                                 ## actionButton(inputId = "start_animate",
                                 ##              label = "Start Animation"),
                                 ## actionButton(inputId = "stop_animate",
                                 ##              label = "Stop Animation")
                        ##  Tab 2: Seasonal Plot
                        tabPanel(title = "Seasonal",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Seasonal Plot"),
                                     "for",
                                     strong("single"),
                                     "series.",
                                     br()),
                                 plotOutput(outputId = "seasonal_plot")),

                        ##  Tab 3: Decomposed Plot
                        tabPanel(title = "Decomposed",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Decomposed Plot"),
                                     "for",
                                     strong("single"),
                                     "series.",
                                     br(),
                                     br()),
                                 plotOutput(outputId = "decomposed_plot")),

                        ##  Tab 4: Trend + Seasonal Plot
                        tabPanel(title = "Recomposed",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Recomposed Plot"),
                                     "for",
                                     strong("single"),
                                     "series.",
                                     br(),
                                     br()),
                                 plotOutput(outputId = "trSeasonal_plot")),

                        ##  Tab 5: Forecast Plot
                        tabPanel(title = "Forecast",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Forecast Plot"),
                                     "for",
                                     strong("single"),
                                     "series.",
                                     br(),
                                     br()),
                                 plotOutput(outputId = "forecast_plot")),
                        ##  Tab 6: Forecast Summary
                        tabPanel(title = "Summary",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Forecast Summary"),
                                     "for",
                                     strong("single"),
                                     "series.",
                                     br(),
                                     br()),
                                 verbatimTextOutput(outputId = "forecast_summary")))

        ),
        ##  Section 3: Multiple Series Plots
        conditionalPanel(
            condition = "input.select_variables.length > 1",
            tabsetPanel(id = "multipleSeriesTabs",
                        type = "pills", # Try type = "tabs" if you wish...
                        ##  Tab 1:  Single Plot Layout
                        tabPanel(title = "Single Plot",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Single-Plot"),
                                     "for",
                                     strong("several"),
                                     "series.",
                                     br(),
                                     br()),
                                 uiOutput("multipleSeries_single_layout")),

                        ##  Tab 2:  Multiple Plot Layout
                        tabPanel(title = "Multiple Plot",
                                 helpText(
                                     br(),
                                     "A",
                                     strong("Multi-Plot"),
                                     "for",
                                     strong("several"),
                                     "series.",
                                     br(),
                                     br()),
                                 uiOutput("multipleSeries_multi_layout"))
                        ))
    )
}

###------------------###
###  Time Series UI  ###
###------------------###
###
###  We combine the ts.sidebarPanel() and ts.mainPanel() functions to complete
###  the UI for the Time Series module.
time.series.panel <- function() {
    fluidPage(
        column(2, ts.sidebarPanel()),
        column(10, ts.mainPanel())
    )
}


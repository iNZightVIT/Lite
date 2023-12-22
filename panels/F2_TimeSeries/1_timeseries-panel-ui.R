### -----------------------------------------------###
###  User Interface for the "Time Series" Module  ###
### -----------------------------------------------###
###
###  Date Created  : January 16, 2015.
###  Last Modified : May 27, 2018.
###
###  The UI is divided into two panels:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: This is to be sourced within "server.R" *

### -----------------###
###  Sidebar Panel  ###
### -----------------###
###
###  First, we set up the "help" functionality for this module.
ts.help <- function() {
  help.display(
    title = "Time Series Module",
    id = "Time_Series",
    file = "panels/F2_TimeSeries/3_timeseries-panel-help.md"
  )
}



###  Next, we set up the sidebar panel with "ts.sidebarPanel()".
ts.sidebarPanel <- function(data.set) {
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
  sidebarPanelUI <- list(
    ##  Section 1: Time Information
    ##
    ##  First, we ask the user to provide time information.
    ##  This can be done by either selecting a variable from a list
    ##  of variables extracted from the dataset, or providing one
    ##  manually.
    hr(),
    h5(strong("Time Information: ")),
    # uiOutput("time_info"),
    div(
      style = "padding: 0px 0px; margin-top:-1.5em",
      radioButtons(
        inputId = "time_info",
        label = "",
        choices =
          c(
            "Select time variable" = 1,
            "Provide time manually" = 2
          )
      )
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
      fixedRow(column(6, selectInput(
        inputId = "TS.period",
        label = "Period :",
        choices = c("", "Year", "Week", "Day"),
        selected = NULL
      ))),
      fixedRow(
        column(6, uiOutput("TS.manual")),
        column(6, numericInput("TS.timeFreqNum", label = "", value = NULL))
      ),
      p("*How many observations per period?"),
      fixedRow(
        column(6, numericInput("TS.timeStartPeriod",
          label = "Start date : ", value = 1
        )),
        column(6, div(
          style = "margin-top: 4px;",
          numericInput("TS.timeStartSeason", label = " ", value = 1)
        ))
      ),
      fixedRow(
        column(6, div(style = "margin-top: -8px;", textOutput("TS.startlbl1"))),
        column(6, div(style = "margin-top: -8px;", textOutput("TS.startlbl2")))
      )
    ),
    hr(),
    h5(strong("Model Settings:")),
    ##  Section 2: Seasonal Pattern
    ##
    ##  Next, we ask the user to specify a seasonal pattern.
    ##  This can be either additive or multiplicative.
    checkboxInput("timeseries_smoother", label = "Show smoother"),
    radioButtons(
      inputId = "choose_season",
      label = "Seasonal Pattern: ",
      choices =
        c(
          "Additive" = FALSE,
          "Multiplicative" = TRUE
        ),
      inline = T
    ),

    ##  A slider bar for smoothing parameter.
    sliderInput(
      inputId = "slidersmoothing",
      label = "Smoothness:",
      min = 0,
      max = 1,
      value = 0.1,
      step = 0.01
    ),
    hr(),
    ##  Section 3: Select Variables
    ##
    ##  We then ask the user to select the variables to plot in the
    ##  main panel. This amounts to selecting the points to be plotted
    ##  on the y-axis. "rev()" is used so that a non-time variable is
    ##  selected (since time is on the x-axis), since the time
    ##  variable is often the first element of "colnames(data)".

    fixedRow(
      column(width = 6, uiOutput("time.plot.select")),
      column(
        width = 6,
        h5(strong("Plot Type Options:")),
        div(
          style = "padding: 0px 0px; margin-top:-1.5em",
          uiOutput("ts_plot_type")
        )
      )
    ),

    ##  Section 4: Select Labels
    ##
    ##  We give the user the option of providing her own set of x- and
    ##  y- axes labels. This is set to "No" by default to save screen
    ##  real estate.
    hr(),
    radioButtons(
      inputId = "customize_labels",
      label = "Customize Labels: ",
      choices =
        c(
          "No" = 1,
          "Yes" = 2
        ),
      selected = 1,
      inline = T
    ),
    conditionalPanel(
      condition = "input.customize_labels == 2",
      uiOutput("provide_xlab_ts"),
      textInput(
        inputId = "provide_ylab",
        label = "Label for the y-axis:",
        value = ""
      )
    ),
    hr(),
    radioButtons(
      inputId = "customize_adjust_limits",
      label = "Adjust Limits: ",
      choices = c("No" = 1, "Yes" = 2),
      selected = 2,
      inline = T
    ),
    conditionalPanel(
      condition = "input.customize_adjust_limits == 2",
      uiOutput("time.range.var")
    ),
    hr(),
    ts.help(),
    br(),
    br()
  )
}



### --------------###
###  Main Panel  ###
### --------------###
###
###  We now set up the main panel with "ts.mainpanel()":
ts.mainPanel <- function() {
  ##  We set up the main panel UI. The code is organised in 3 sections:
  ##
  ##    -  Section 1: Data Validation
  ##    -  Section 2: Single Series Plots
  ##    -  Section 3: Multiple Series Plots
  ##
  ##  Note the use of "br()" (= line break) for vertical spacing.

  mainPanelUI <- list(
    uiOutput("ts.main.ui")
  )
}



### ------------------###
###  Time Series UI  ###
### ------------------###
###
###  We combine the ts.sidebarPanel() and ts.mainPanel() functions to
###  complete the UI for the Time Series module.

timeseries.panel.ui <- function(data.set) {
  fluidPage(
    shinyjs::useShinyjs(),
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/F2_TimeSeries/4_timeseries-panel-null.md"
        )
      )
    } else {
      fluidRow(
        column(3, ts.sidebarPanel(data.set)),
        column(9, ts.mainPanel())
      )
    }
  )
}

ts.help <- function() {
  help.display(
    title = "Time Series Module",
    id = "Time_Series",
    file = "panels/F2_TimeSeries/3_timeseries-panel-help.md"
  )
}



ts.sidebarPanel <- function(data.set) {
  if (is.null(data.set)) {
    stop("Please select a data set!")
  }

  sidebarPanelUI <- list(
    hr(),
    
    h5(strong("Time Information: ")),
    uiOutput("time.select"),
    uiOutput("key.select"),
    hr(),
    
    h5(strong("Choose variables:")),
    radioButtons(
      inputId = "choose_var_type",
      label = "",
      choices =
        c(
          "Numeric Variables" = "num",
          "Categorical Variables" = "cat"
        ),
      selected = "num",
      inline = T
    ),
    uiOutput("time.plot.select"),
    radioButtons(
      inputId = "choose_season",
      label = "",
      choices =
        c(
          "Additive" = "add",
          "Multiplicative" = "multi"
        ),
      selected = "add",
      inline = T
    ),
    hr(),
    
    h5(strong("Plot type:")),
    # radioButtons(
    #   inputId = "time_plot_info", label = "",
    #   choices = c(
    #     "Default" = "default",
    #     "Decomposed" = "decomposed",
    #     "Seasonal" = "seasonal",
    #     "Forecast" = "forecast"
    #   ),
    #   selected = "default",
    #   inline = T
    # ),
    uiOutput("time_plot_info"),
    hr(),
    
    h5(strong("Plot settings:")),
    checkboxInput("timeseries_smoother", label = "Show smoother"),
    checkboxInput("timeseries_seasonally_adjusted", label = "Seasonally adjust series"),
    sliderInput(
      inputId = "slidersmoothing",
      label = "Smoothness:",
      min = 0,
      max = 100,
      value = 15,
      step = 0.1
    ),
    hr(),
    h5(strong("Range settings:")),
    uiOutput("time.range.var")
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

###  Time information panel - conditional on the type of dataset.
# output$time_info <- renderUI({
#   radioButtons(
#     inputId = "time_info",
#     label = "Time Information: ",
#     choices = c(
#       "Select time variable" = 1,
#       "Provide time manually" = 2
#     )
#   )
# })


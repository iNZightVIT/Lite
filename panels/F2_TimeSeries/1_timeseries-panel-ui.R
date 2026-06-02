ts.help <- function() {
  help.display(
    title = "Time Series Module",
    id = "Time_Series",
    file = "panels/F2_TimeSeries/3_timeseries-panel-help.md"
  )
}

x = 1

TS.sidebarPanel <- function(data.set) {
  if (is.null(data.set)) {
    stop("Please select a data set!")
  }

  sidebarPanelUI <- list(
    hr(),
    
    h5(strong("Time Information: ")),
    tabsetPanel(
      id = "tsui_time_info_mode",
      tabPanel(
        "Select time variable",
        uiOutput("tsui_time_select"),
        uiOutput("tsui_key_select")
      ),
      tabPanel(
        "Provide time manually",
        div(
          class = "tsui-manual-time",
          style = "padding-top: 14px;",
          div(
            style = "margin-bottom: 16px;",
            fixedRow(
              column(
                4,
                tags$label(class = "control-label", `for` = "tsui_period", "Period :")
              ),
              column(
                8,
                selectInput(
                  inputId = "tsui_period",
                  label = NULL,
                  choices = c("Year", "Week", "Day"),
                  selected = "Year"
                )
              )
            )
          ),
          div(
            style = "margin-bottom: 16px;",
            fixedRow(
              column(
                4,
                tags$label(class = "control-label", "Frequency* :")
              ),
              column(
                8,
                div(
                  style = "margin-bottom: -10px;",
                  fixedRow(
                    column(6, uiOutput("tsui_manual_freq")),
                    column(
                      6,
                      numericInput(
                        inputId = "tsui_time_freq_num",
                        label = NULL,
                        value = 1,
                        min = 1,
                        step = 1
                      )
                    )
                  )
                ),
                tags$p(
                  style = "margin: 2px 0 0 0; font-size: 12px;",
                  "*How many observations per period?"
                )
              )
            )
          ),
          div(
            style = "margin-bottom: 22px;",
            fixedRow(
              column(
                4,
                tags$label(
                  class = "control-label",
                  `for` = "tsui_time_start_period",
                  "Start date :"
                )
              ),
              column(
                4,
                div(
                  style = "margin-bottom: -10px;",
                  numericInput(
                    inputId = "tsui_time_start_period",
                    label = NULL,
                    value = 1,
                    min = 1,
                    step = 1
                  )
                )
              ),
              column(
                4,
                div(
                  style = "margin-bottom: -10px;",
                  numericInput(
                    inputId = "tsui_time_start_season",
                    label = NULL,
                    value = 1,
                    min = 1,
                    step = 1
                  )
                )
              )
            ),
            div(
              style = "margin-top: 2px;",
              fixedRow(
                column(4),
                column(
                  4,
                  div(
                    style = "font-size: 12px; line-height: 1.2;",
                    textOutput("tsui_start_lbl1")
                  )
                ),
                column(
                  4,
                  div(
                    style = "font-size: 12px; line-height: 1.2;",
                    textOutput("tsui_start_lbl2")
                  )
                )
              )
            )
          ),
          div(
            style = "width: 100%; margin-top: 6px; text-align: right;",
            div(
              style = "display: inline-block;",
              actionButton(
                inputId = "tsui_manual_submit",
                label = "Apply Settings"
              )
            )
          )
        )
      )
    ),
    hr(),
    
    h5(strong("Choose variables:")),
    radioButtons(
      inputId = "tsui_choose_var_type",
      label = "",
      choices =
        c(
          "Numeric Variables" = "num",
          "Categorical Variables" = "cat"
        ),
      selected = "num",
      inline = T
    ),
    uiOutput("tsui_time_plot_select"),
    radioButtons(
      inputId = "tsui_choose_season",
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
    uiOutput("tsui_time_plot_info"),
    hr(),
    
    h5(strong("Plot settings:")),
    checkboxInput("tsui_smoother", label = "Show smoother"),
    checkboxInput("tsui_seasonally_adjusted", label = "Seasonally adjust series"),
    sliderInput(
      inputId = "tsui_smoothing",
      label = "Smoothness:",
      min = 0,
      max = 100,
      value = 15,
      step = 0.1
    ),
    hr(),
    uiOutput("tsui_ranges")
  )
}



### --------------###
###  Main Panel  ###
### --------------###
###
###  We now set up the main panel with "ts.mainpanel()":
TS.mainPanel <- function() {
  ##  We set up the main panel UI. The code is organised in 3 sections:
  ##
  ##    -  Section 1: Data Validation
  ##    -  Section 2: Single Series Plots
  ##    -  Section 3: Multiple Series Plots
  ##
  ##  Note the use of "br()" (= line break) for vertical spacing.
  
  mainPanelUI <- list(
    uiOutput("tsui_main")
  )
}



### ------------------###
###  Time Series UI  ###
### ------------------###
###
###  We combine the ts.sidebarPanel() and ts.mainPanel() functions to
###  complete the UI for the Time Series module.

TS.panel.ui <- function(data.set) {
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
        column(3, TS.sidebarPanel(data.set)),
        column(9, TS.mainPanel())
      )
    }
  )
}


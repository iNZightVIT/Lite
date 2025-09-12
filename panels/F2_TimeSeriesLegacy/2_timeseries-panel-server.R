### -------------------------------------------------###
###  Server Functions for the "Time Series" Module  ###
### -------------------------------------------------###
###
###  Date Created   :   January 16, 2015
###  Last Modified  :   May 27, 2018
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *

# initialize gui
output$timeseries.legacy.panel <- renderUI({
  timeseries.panel.ui(get.data.set())
})

getTime <- function(data, index = TRUE) {
  ## look for time or date
  time_re <- "([Tt][Ii][Mm][Ee])|([Dd][Aa][Tt][Ee])"
  ind <- grep(time_re, names(data))
  if (length(ind) == 0) ind <- 1 else ind <- ind[1]
  if (index) {
    return(ind)
  }
  return(names(data)[ind])
}

freqOpts <- list(
  "Year" = c(
    "Yearly (1)" = 1,
    "Quarterly (4)" = 4,
    "Monthly (12)" = 12,
    "Weekly (52)" = 52,
    "Daily (365/366)" = 365
  ),
  "Week" = c(
    "Daily (7)" = 7,
    "Daily - work week (5)" = 5
  ),
  "Day" = c(
    "Hourly (24)" = 24
  )
)

## specify time manually
output$TS.manual <- renderUI({
  input$TS.period
  isolate({
    if (input$TS.period == "") {
      choice <- ""
    } else {
      choice <- c("", names(freqOpts[[input$TS.period]]), "Custom")
    }
    selectInput(
      inputId = "TS.timeFreqList",
      label = "Frequency* :",
      choices = choice,
      selected = NULL
    )
  })
})



output$TS.startlbl1 <- renderText({
  if (input$TS.period == "") {
    "Period"
  } else {
    input$TS.period
  }
})


output$TS.startlbl2 <- renderText({
  if (!is.null(input$TS.timeFreqList)) {
    if (input$TS.timeFreqList == "Custom" || input$TS.timeFreqList == "") {
      "Season"
    } else {
      season.name <- gsub(
        "ly$", "",
        strsplit(input$TS.timeFreqList, " ")[[1]][1]
      )
      if (season.name == "Dai") season.name <- "Day"
      if (season.name == "Year") season.name <- " "
      season.name
    }
  }
})


observe({
  input$input$TS.period
  input$TS.timeFreqList
  isolate({
    if (!is.null(input$TS.timeFreqList) && !is.null(input$TS.period)) {
      if (input$TS.timeFreqList == "Custom" || input$TS.timeFreqList == "") {
        shinyjs::enable("TS.timeFreqNum")
      } else {
        updateNumericInput(session,
                           inputId = "TS.timeFreqNum",
                           label = "",
                           value = freqOpts[[input$TS.period]][[input$TS.timeFreqList]]
        )
        shinyjs::disable("TS.timeFreqNum")
      }
    }
  })
})


## create sliderInput
output$time.range.var <- renderUI({
  if ((date_check(get.data.set(), input$select_timevars) ||
       input$time_info == 2) && !is.null(ts.para$tsObj$tsObj)) {
    xr <- range(time(ts.para$tsObj$tsObj))
    xby <- 1 / ts.para$tsObj$freq
    xx <- seq(xr[1], xr[2], by = xby)
    timeVar <- getTime(ts.para$tsObj$data, index = FALSE)
    xd <- as.character(ts.para$tsObj$data[[timeVar]])
    list(
      fixedRow(
        column(
          width = 6,
          sliderTextInput(
            inputId = "adjust_limit_from",
            label = "Display data from...",
            choices = xd,
            selected = xd[1]
          )
        ),
        column(
          width = 6,
          sliderTextInput(
            inputId = "adjust_limit_until",
            label = "until...",
            choices = xd,
            selected = xd[length(xd)]
          )
        )
      ),
      checkboxInput("check_lim_fit",
                    label = "Use above limits for fitting model", value = TRUE
      ),
      conditionalPanel(
        condition = "input.check_lim_fit == false",
        fixedRow(
          column(
            width = 6,
            sliderTextInput(
              inputId = "mod_limit_from",
              label = "Fit model to data from ... ",
              choices = xd,
              selected = xd[1]
            )
          ),
          column(
            width = 6,
            sliderTextInput(
              inputId = "mod_limit_until",
              label = "until...",
              choices = xd,
              selected = xd[length(xd)]
            )
          )
        )
      )
    )
  }
})

## update sliders
observe({
  get.data.set()
  input$adjust_limit_from
  isolate({
    if (!is.null(ts.para$tsObj) && length(input$adjust_limit_from) > 0) {
      tryCatch(
        {
          xr <- range(time(ts.para$tsObj$tsObj))
          xby <- 1 / ts.para$tsObj$freq
          xx <- seq(xr[1], xr[2], by = xby)
          timeVar <- getTime(ts.para$tsObj$data, index = FALSE)
          xd <- as.character(ts.para$tsObj$data[[timeVar]])
          lim1 <- xd[
            which(xx == (xx[xd == input$adjust_limit_from] + 2)):length(xd)
          ]
          updateSliderTextInput(session,
                                inputId = "adjust_limit_until",
                                label = "until...",
                                choices = lim1,
                                selected = input$adjust_limit_until
          )
          if (input$adjust_limit_from == xd[
            which(xx == (xx[xd == input$adjust_limit_until] - 2))
          ]) {
            shinyjs::disable("adjust_limit_until")
          } else {
            shinyjs::enable("adjust_limit_until")
          }
        },
        warning = function(w) {
          print(w)
        },
        error = function(e) {
          print(e)
        }
      )
    }
  })
})

observe({
  get.data.set()
  input$mod_limit_from
  isolate({
    if (!is.null(ts.para$tsObj$tsObj) && length(ts.para$tsObj$tsObj) > 0) {
      tryCatch(
        {
          xr <- range(time(ts.para$tsObj$tsObj))
          xby <- 1 / ts.para$tsObj$freq
          xx <- seq(xr[1], xr[2], by = xby)
          timeVar <- getTime(ts.para$tsObj$data, index = FALSE)
          xd <- as.character(ts.para$tsObj$data[[timeVar]])
          lim1 <- xd[
            which(xx == (xx[xd == input$mod_limit_from] + 2)):length(xd)
          ]
          updateSliderTextInput(session,
                                inputId = "mod_limit_until",
                                label = "until...",
                                choices = lim1,
                                selected = input$mod_limit_until
          )
          if (input$mod_limit_from == xd[
            which(xx == (xx[xd == input$mod_limit_until] - 2))
          ]) {
            shinyjs::disable("mod_limit_until")
          } else {
            shinyjs::enable("mod_limit_until")
          }
        },
        warning = function(w) {
          print(w)
        },
        error = function(e) {
          print(e)
        }
      )
    }
  })
})



observe({
  get.data.set()
  input$adjust_limit_until
  isolate({
    if (!is.null(ts.para$tsObj$tsObj)) {
      tryCatch(
        {
          xr <- range(time(ts.para$tsObj$tsObj))
          xby <- 1 / ts.para$tsObj$freq
          xx <- seq(xr[1], xr[2], by = xby)
          timeVar <- getTime(ts.para$tsObj$data, index = FALSE)
          xd <- as.character(ts.para$tsObj$data[[timeVar]])
          lim2 <- xd[1:which(xx == (xx[xd == input$adjust_limit_until] - 2))]
          updateSliderTextInput(session,
                                inputId = "adjust_limit_from",
                                label = "Display data from...",
                                choices = lim2,
                                selected = input$adjust_limit_from
          )
          if (input$adjust_limit_until == xd[
            which(xx == (xx[xd == input$adjust_limit_from] + 2))
          ]) {
            shinyjs::disable("adjust_limit_from")
          } else {
            shinyjs::enable("adjust_limit_from")
          }
        },
        warning = function(w) {
          print(w)
        },
        error = function(e) {
          print(e)
        }
      )
    }
  })
})

tryCatch(
  {
  },
  warning = function(w) {
    print(w)
  },
  error = function(e) {
    print(e)
  }
)

observe({
  get.data.set()
  input$mod_limit_until
  isolate({
    if (!is.null(ts.para$tsObj$tsObj) && length(ts.para$tsObj$tsObj) != 0) {
      tryCatch(
        {
          xr <- range(time(ts.para$tsObj$tsObj))
          xby <- 1 / ts.para$tsObj$freq
          xx <- seq(xr[1], xr[2], by = xby)
          timeVar <- getTime(ts.para$tsObj$data, index = FALSE)
          xd <- as.character(ts.para$tsObj$data[[timeVar]])
          lim2 <- xd[1:which(xx == (xx[xd == input$mod_limit_until] - 2))]
          updateSliderTextInput(session,
                                inputId = "mod_limit_from",
                                label = "Fit model to data from ... ",
                                choices = lim2,
                                selected = input$mod_limit_from
          )
          if (input$mod_limit_until == xd[
            which(xx == (xx[xd == input$mod_limit_from] + 2))
          ]) {
            shinyjs::disable("mod_limit_from")
          } else {
            shinyjs::enable("mod_limit_from")
          }
        },
        warning = function(w) {
          print(w)
        },
        error = function(e) {
          print(e)
        }
      )
    }
  })
})



output$provide_xlab_ts <- renderUI({
  textInput(
    inputId = "provide_xlab",
    label = "Label for the x-axis:",
    value = getTime(get.data.set(), index = FALSE)
  )
})

season_select_ts <- reactiveValues()
season_select_ts$re <- as.logical()



observe({
  get.data.set()
  variable.names()
  input$select_variables
  input$choose_season
  isolate({
    tryCatch({
      can_multiply <- all(
        sapply(
          variable.names(),
          function(i) all(get.data.set()[[i]] > 0)
        )
      )
      if (!is.na(can_multiply)) {
        if (can_multiply == T) {
          shinyjs::enable("choose_season")
          season_select_ts$re <- input$choose_season
        } else {
          season_select_ts$re <- F
          shinyjs::reset("choose_season")
          shinyjs::disable("choose_season")
        }
      }
    }, error = function(e) {
      print(e)
    }, finally = {})
  })
})



## create xlim and modlim
ts.para <- reactiveValues()
ts.para$tsObj <- NULL
ts.para$mod.lim <- vector()
ts.para$xlim <- vector()

observe({
  get.data.set()
  ## create tsObj
  if ((!is.null(input$time_info) &&
       input$time_info == 1 && !is.null(input$select_timevars)) ||
      (!is.null(input$time_info) && input$time_info == 2 &&
       !is.null(input$TS.period) && !is.null(input$TS.timeFreqNum) &&
       !is.na(input$TS.timeFreqNum))) {
    temp <- get.data.set()
    if (input$time_info == 1) {
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        if (!input$select_timevars %in% "time") {
          colnames(temp)[
            which(colnames(temp) %in% input$select_timevars)
          ] <- "time"
        }
        tryCatch(
          {
            ts.para$tsObj <- iNZightTSLegacy::iNZightTS(temp,
                                       var = variable.names(),
                                       time.col =
                                         which(colnames(temp) == "time")
            )
          },
          warning = function(w) {
            print(w)
          },
          error = function(e) {
            print(e)
          }
        )
      }
    } else {
      tryCatch(
        {
          ts.para$tsObj <- iNZightTSLegacy::iNZightTS(temp,
                                     var = variable.names(),
                                     start = c(input$TS.timeStartPeriod, input$TS.timeStartSeason),
                                     freq = input$TS.timeFreqNum
          )
        },
        warning = function(w) {
          print(w)
        },
        error = function(e) {
          print(e)
        }
      )
    }
    
    ## modlim and xlim
    if (!is.null(input$adjust_limit_from) &&
        !is.null(input$adjust_limit_until) &&
        length(input$adjust_limit_until) > 0 &&
        length(input$adjust_limit_from) > 0 &&
        !is.null(input$mod_limit_from) && !is.null(input$mod_limit_until) &&
        length(input$mod_limit_until) > 0 && length(input$mod_limit_from) > 0) {
      xr <- range(time(ts.para$tsObj$tsObj))
      xby <- 1 / ts.para$tsObj$freq
      xx <- seq(xr[1], xr[2], by = xby)
      timeVar <- getTime(ts.para$tsObj$data, index = FALSE)
      xd <- as.character(ts.para$tsObj$data[[timeVar]])
      tryCatch(
        {
          ts.para$xlim[1] <- xx[xd == input$adjust_limit_from]
          ts.para$xlim[2] <- xx[xd == input$adjust_limit_until]
        },
        warning = function(w) {
          print(w)
        },
        error = function(e) {
          print(e)
        }
      )
      
      if (input$check_lim_fit == TRUE) {
        ts.para$mod.lim <- ts.para$xlim
      } else if (input$check_lim_fit == FALSE) {
        ts.para$mod.lim[1] <- xx[xd == input$mod_limit_from]
        ts.para$mod.lim[2] <- xx[xd == input$mod_limit_until]
      }
    }
  }
})



## main UI
output$ts.main.ui <- renderUI({
  get.data.set()
  input$select_variables
  input$time_plot_info1
  input$time_plot_info
  ret <- NULL
  isolate({
    if (!is.null(input$select_variables) &&
        length(input$select_variables) == 1 &&
        input$time_plot_info1 == 1) {
      ret <- list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" is you wish...
          ##  Tab 1: Time Series Plot
          tabPanel(
            title = "Plot",
            helpText(
              br(),
              "A",
              strong("Time Plot"),
              "for",
              strong("single"),
              "series."
            ),
            plotOutput("timeseries_plot"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveTimeplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveTimeplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            )
          ),
          tabPanel(
            title = "Interactive Plot (via plotly)",
            uiOutput("plotly_tsmainnw"),
            plotlyOutput("plotly_tsmain", height = "500px") %>% withSpinner()
          )
        )
      )
    } else if (!is.null(input$select_variables) &&
               length(input$select_variables) == 1 &&
               input$time_plot_info1 == 2) {
      ret <- list(
        tabsetPanel(
          type = "pills",
          ##  Tab 2: Decomposed Plot
          tabPanel(
            title = "Plot",
            helpText(
              br(),
              "A",
              strong("Decomposed Plot"),
              "for",
              strong("single"),
              "series.",
              br(),
              br()
            ),
            plotOutput(outputId = "decomposed_plot", height = "600px"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveDecomposedplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveDecomposedplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            )
          )
        )
      )
    } else if (!is.null(input$select_variables) &&
               length(input$select_variables) == 1 &&
               input$time_plot_info1 == 3) {
      ret <- list(
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Plot",
            helpText(
              br(),
              "A",
              strong("Recomposed Plot"),
              "for",
              strong("single"),
              "series.",
              br(),
              br()
            ),
            plotOutput(outputId = "trSeasonal_plot", height = "600px"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveRecomposedplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveRecomposedplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            )
          )
        )
      )
    } else if (!is.null(input$select_variables) &&
               length(input$select_variables) == 1 &&
               input$time_plot_info1 == 4) {
      ret <- list(
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Plot",
            helpText(
              br(),
              "A",
              strong("Seasonal Plot"),
              "for",
              strong("single"),
              "series.",
              br()
            ),
            plotOutput(outputId = "seasonal_plot"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveSeasonalplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveSeasonalplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            )
          )
        )
      )
    } else if (!is.null(input$select_variables) &&
               length(input$select_variables) == 1 &&
               input$time_plot_info1 == 5) {
      ret <- list(
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Plot",
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
            plotOutput(outputId = "forecast_plot"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveForecastplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveForecastplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            ),
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
          ),
          tabPanel(
            title = "Interactive Plot (via plotly)",
            uiOutput("plotly_tsforecastnw"),
            plotlyOutput("plotly_tsforecast", height = "500px") |>
              withSpinner()
          )
        )
      )
    } else if (!is.null(input$select_variables) &&
               length(input$select_variables) > 1 &&
               input$time_plot_info == 2) {
      ret <- list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" if you wish...
          ##  Tab 2:  Multiple Plot Layout
          tabPanel(
            title = "Plot",
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
            uiOutput("multipleSeries_multi_layout"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveMultiplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveMultiplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            )
          )
        )
      )
    } else if (!is.null(input$select_variables) &&
               length(input$select_variables) > 1 &&
               input$time_plot_info == 1) {
      ret <- list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" if you wish...
          tabPanel(
            title = "Plot",
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
            uiOutput("multipleSeries_single_layout"),
            br(),
            fixedRow(
              column(
                width = 3,
                NULL
              ),
              column(
                width = 3,
                downloadButton(
                  outputId = "saveSingleplot",
                  label = "Download Plot"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "saveSingleplottype",
                  label = strong("Select the file type"),
                  choices = list("jpg", "png", "pdf"), inline = TRUE
                )
              )
            )
          )
        )
      )
    }
    ret
  })
})



### --------------###
###  Data check  ###
### --------------###

###  We write a function that handles data.
date_check <- function(dafr, time.vars) {
  ##  Set of error checks.
  if (is.null(dafr)) {
    return(FALSE)
  }
  if (is.null(time.vars) || time.vars == "") {
    return(FALSE)
  }
  if (!(time.vars %in% names(dafr))) {
    return(FALSE)
  }
  ##  We obtain a subset of the data.
  subdata <- dafr[, time.vars]
  ##  If any of the time series structures return an NA,
  ##  return TRUE. Else, return FALSE.
  # function iNZightTSLegacy:::get.ts.structure is not stable
  # it throws an error if a normal factor variable is
  # passed in.
  tryCatch({
    if (any(is.na(suppressWarnings(iNZightTSLegacy:::get.ts.structure(subdata))))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }, error = function(e) {
    cat("Handled error in date_check\n")
    print(e)
    return(F)
  }, finally = {})
}

###  Time information panel - conditional on the type of dataset.
output$time_info <- renderUI({
  radioButtons(
    inputId = "time_info",
    label = "Time Information: ",
    choices = c(
      "Select time variable" = 1,
      "Provide time manually" = 2
    )
  )
})

# observes the Add time variable button.
# Add an additional column "time" to the data
# set. This column is in the format of:
# year = yyyy (i.e 2004)
# frequency = Q or M (Quarter or Month)
# start = 1-4 for Quarters or 1-12 for Month
# example 2004Q2 (quarter 2 of the year 2004)
#         2002M7 (July 2002)

###  Variable Names
variable.names <- reactive({
  input$select_variables
  variable.names <- which(names(get.data.set()) %in% input$select_variables)
})

### -----------------------###
###  Single Series Plots  ###
### -----------------------###
###
###  Time Series Plot
output$timeseries_plot <- renderPlot({
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        g <- iNZightTSLegacy:::plot.iNZightTS(ts.para$tsObj,
                  ## start = start),
                  xlab = input$provide_xlab,
                  ylab = input$provide_ylab,
                  multiplicative = as.logical(season_select_ts$re),
                  t = 100 * input$slidersmoothing,
                  smoother = input$timeseries_smoother,
                  model.lim = ts.para$mod.lim,
                  xlim = ts.para$xlim
        )
        dev.off()
        g
      },
      #        warning = function(w) {
      #          cat("Warning produced in timseries plot\n")
      #          print(w)
      #        },
      error = function(e) {
        cat("Handled error in timseries plot\n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})


### download Time Series Plot
output$saveTimeplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveTimeplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveTimeplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveTimeplottype == "jpg") {
        jpeg(file)
      } else if (input$saveTimeplottype == "png") {
        png(file)
      } else if (input$saveTimeplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy:::plot.iNZightTS(
              ts.para$tsObj,
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              multiplicative = as.logical(season_select_ts$re),
              t = 100 * input$slidersmoothing,
              smoother = input$timeseries_smoother,
              model.lim = ts.para$mod.lim,
              xlim = ts.para$xlim
            )
          },
          error = function(e) {
            cat("Handled error in timseries plot\n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)


## interactive plot
output$plotly_tsmain <- renderPlotly({
  get.data.set()
  input$select_variables
  input$time_info
  input$provide_actionButton
  input$timeseries_smoother
  season_select_ts$re
  input$slidersmoothing
  input$provide_xlab
  input$provide_ylab
  input$time_plot_info1
  input$adjust_limit_from
  input$adjust_limit_until
  input$mod_limit_from
  input$mod_limit_until
  isolate({
    if (date_check(get.data.set(), input$select_timevars) ||
        input$time_info == 2) {
      pdf(NULL)
      suppressWarnings(tryCatch(
        {
          iNZightTSLegacy:::plot.iNZightTS(
            ts.para$tsObj,
            ## start = start),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = as.logical(season_select_ts$re),
            t = 100 * input$slidersmoothing,
            smoother = input$timeseries_smoother,
            model.lim = ts.para$mod.lim,
            xlim = ts.para$xlim
          )
          g <- plotly::ggplotly()
          g
        },
        error = function(e) {
          cat("Handled error in timseries plot\n")
        }, finally = {}
      ))
    }
  })
})

# open in new window
output$plotly_tsmainnw <- renderUI({
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    curdir <- getwd()
    on.exit(setwd(curdir))
    # set to temp directory
    tdir <- tempdir()
    setwd(tdir)
    cdev <- dev.cur()
    on.exit(dev.off(cdev), add = TRUE)
    suppressWarnings(tryCatch(
      {
        iNZightTSLegacy:::plot.iNZightTS(
          ts.para$tsObj,
          xlab = input$provide_xlab,
          ylab = input$provide_ylab,
          multiplicative = as.logical(season_select_ts$re),
          t = 100 * input$slidersmoothing,
          smoother = input$timeseries_smoother,
          model.lim = ts.para$mod.lim,
          xlim = ts.para$xlim
        )
      },
      error = function(e) {
        cat("Handled error in timseries plot\n")
        print(e)
      }, finally = {}
    ))
    htmlwidgets::saveWidget(as_widget(plotly::ggplotly()), "index.html")
    addResourcePath("path", normalizePath(tdir))
    list(
      br(),
      br(),
      tags$a(
        href = "path/index.html",
        "Open in a new window",
        target = "_blank"
      ),
      br(),
      br()
    )
  }
})


###  Seasonal Plot
output$seasonal_plot <- renderPlot({
  #     input$selector
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        g <- iNZightTSLegacy::seasonplot(
          ts.para$tsObj,
          ylab = input$provide_ylab,
          xlab = input$provide_xlab,
          multiplicative = as.logical(season_select_ts$re),
          t = 100 * input$slidersmoothing,
          model.lim = ts.para$mod.lim
        )
        # dev.off()
        g
      },
      error = function(e) {
        cat("Handled error in seasonplot\n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})


###  download Seasonal Plot
output$saveSeasonalplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveSeasonalplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveSeasonalplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveSeasonalplottype == "jpg") {
        jpeg(file)
      } else if (input$saveSeasonalplottype == "png") {
        png(file)
      } else if (input$saveSeasonalplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy::seasonplot(
              ts.para$tsObj,
              ylab = input$provide_ylab,
              xlab = input$provide_xlab,
              multiplicative = as.logical(season_select_ts$re),
              t = 100 * input$slidersmoothing,
              model.lim = ts.para$mod.lim
            )
          },
          error = function(e) {
            cat("Handled error in seasonplot\n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)








###  Decomposed Plot
output$decomposed_plot <- renderPlot({
  #     input$selector
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        
        g <- iNZightTSLegacy:::plot.inzdecomp(
          iNZightTSLegacy::decompose(
            ts.para$tsObj,
            multiplicative = as.logical(season_select_ts$re),
            t = 100 * input$slidersmoothing,
            model.lim = ts.para$mod.lim
          ),
          xlab = input$provide_xlab,
          ylab = input$provide_ylab,
          xlim = ts.para$xlim
        )
        # dev.off()
        g
      },
      error = function(e) {
        cat("Handled error in decompositionplot \n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})



### download Decomposed Plot
output$saveDecomposedplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveDecomposedplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveDecomposedplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveDecomposedplottype == "jpg") {
        jpeg(file)
      } else if (input$saveDecomposedplottype == "png") {
        png(file)
      } else if (input$saveDecomposedplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy:::plot.inzdecomp(
              iNZightTSLegacy::decompose(
                ts.para$tsObj,
                multiplicative = as.logical(season_select_ts$re),
                t = 100 * input$slidersmoothing,
                model.lim = ts.para$mod.lim
              ),
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              xlim = ts.para$xlim
            )
          },
          error = function(e) {
            cat("Handled error in decompositionplot \n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)



###  Trend + Seasonal Plot
output$trSeasonal_plot <- renderPlot({
  #     input$selector
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        g <- iNZightTSLegacy:::plot.inzdecomp(
          iNZightTSLegacy::decompose(
            ts.para$tsObj,
            multiplicative = as.logical(season_select_ts$re),
            t = 100 * input$slidersmoothing,
            model.lim = ts.para$mod.lim
          ),
          xlab = input$provide_xlab,
          ylab = input$provide_ylab,
          xlim = ts.para$xlim,
          recompose.progress = c(1, nrow(get.data.set()))
        )
        # dev.off()
        g
      },
      error = function(e) {
        cat("Handled error in recompose plot \n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})


### download Trend + Seasonal Plot
output$saveRecomposedplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveRecomposedplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveRecomposedplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveRecomposedplottype == "jpg") {
        jpeg(file)
      } else if (input$saveRecomposedplottype == "png") {
        png(file)
      } else if (input$saveRecomposedplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy:::plot.inzdecomp(
              iNZightTSLegacy::decompose(
                ts.para$tsObj,
                multiplicative = as.logical(season_select_ts$re),
                t = 100 * input$slidersmoothing,
                model.lim = ts.para$mod.lim
              ),
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              xlim = ts.para$xlim,
              recompose.progress = c(1, nrow(get.data.set()))
            )
          },
          error = function(e) {
            cat("Handled error in recompose plot \n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)







###  Forecast Plot
output$forecast_plot <- renderPlot({
  #     input$selector
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        pl <- try(
          iNZightTSLegacy:::plot.iNZightTS(
            ts.para$tsObj,
            multiplicative = as.logical(season_select_ts$re),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            forecast = ts.para$tsObj$freq * 2,
            model.lim = ts.para$mod.lim,
            xlim = ts.para$xlim
        ), silent = TRUE)
        if (inherits(pl, "try-error")) {
          return()
        }
        dev.off()
        ts.para$forecasts <- iNZightTSLegacy::pred(pl)
        pl
      },
      error = function(e) {
        cat("Handled error in forecastplot \n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})


## plotly
output$plotly_tsforecast <- renderPlotly({
  get.data.set()
  input$select_variables
  input$time_info
  input$provide_actionButton
  input$timeseries_smoother
  season_select_ts$re
  input$slidersmoothing
  input$provide_xlab
  input$provide_ylab
  input$time_plot_info1
  input$adjust_limit_from
  input$adjust_limit_until
  input$mod_limit_from
  input$mod_limit_until
  isolate({
    if (date_check(get.data.set(), input$select_timevars) ||
        input$time_info == 2) {
      pdf(NULL)
      suppressWarnings(tryCatch(
        {
          iNZightTSLegacy:::plot.iNZightTS(
            ts.para$tsObj,
            multiplicative = as.logical(season_select_ts$re),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            forecast = ts.para$tsObj$freq * 2,
            model.lim = ts.para$mod.lim,
            xlim = ts.para$xlim
          )
        },
        error = function(e) {
          cat("Handled error in forecastplot \n")
          print(e)
        }, finally = {}
      ))
      plotly::ggplotly()
    }
  })
})


## open in new window
output$plotly_tsforecastnw <- renderUI({
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    curdir <- getwd()
    on.exit(setwd(curdir))
    # set to temp directory
    tdir <- tempdir()
    setwd(tdir)
    cdev <- dev.cur()
    on.exit(dev.off(cdev), add = TRUE)
    suppressWarnings(tryCatch(
      {
        iNZightTSLegacy:::plot.iNZightTS(
          ts.para$tsObj,
          multiplicative = as.logical(season_select_ts$re),
          xlab = input$provide_xlab,
          ylab = input$provide_ylab,
          forecast = ts.para$tsObj$freq * 2,
          model.lim = ts.para$mod.lim,
          xlim = ts.para$xlim
        )
      },
      error = function(e) {
        cat("Handled error in forecastplot \n")
        print(e)
      }, finally = {}
    ))
    htmlwidgets::saveWidget(as_widget(plotly::ggplotly()), "index.html")
    addResourcePath("path", normalizePath(tdir))
    list(
      br(),
      br(),
      tags$a(
        href = "path/index.html",
        "Open in a new window",
        target = "_blank"
      ),
      br(),
      br()
    )
  }
})


###  download Forecast Plot
output$saveForecastplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveForecastplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveForecastplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveForecastplottype == "jpg") {
        jpeg(file)
      } else if (input$saveForecastplottype == "png") {
        png(file)
      } else if (input$saveForecastplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        temp <- get.data.set()
        if (!input$select_timevars %in% "time") {
          colnames(temp)[
            which(colnames(temp) %in% input$select_timevars)
          ] <- "time"
        }
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy:::plot.iNZightTS(
              ts.para$tsObj,
              multiplicative = as.logical(season_select_ts$re),
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              forecast = ts.para$tsObj$freq * 2,
              model.lim = ts.para$mod.lim,
              xlim = ts.para$xlim
            )
          },
          error = function(e) {
            cat("Handled error in forecastplot \n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)





output$forecast_summary <- renderPrint({
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch({
      ts.para$forecasts
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      print(e)
    }, finally = {}))
  } else {
    cat("No time variable found.")
  }
})


### -------------------------###
###  Multiple Series Plots  ###
### -------------------------###
###
output$multiple_single_plot <- renderPlot({
  #     input$selctor
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        iNZightTSLegacy:::plot.iNZightTS(
          ts.para$tsObj,
          multiplicative = as.logical(season_select_ts$re),
          t = 100 * input$slidersmoothing,
          xlab = input$provide_xlab,
          ylab = input$provide_ylab,
          smoother = input$timeseries_smoother,
          model.lim = ts.para$mod.lim,
          xlim = ts.para$xlim
        )
      },
      error = function(e) {
        cat("Handled error in compareplot \n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})

output$saveSingleplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveSingleplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveSingleplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveSingleplottype == "jpg") {
        jpeg(file)
      } else if (input$saveSingleplottype == "png") {
        png(file)
      } else if (input$saveSingleplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy:::plot.iNZightTS(
              ts.para$tsObj,
              multiplicative = as.logical(season_select_ts$re),
              t = 100 * input$slidersmoothing,
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              smoother = input$timeseries_smoother,
              model.lim = ts.para$mod.lim,
              xlim = ts.para$xlim
            )
          },
          error = function(e) {
            cat("Handled error in compareplot \n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)


output$multipleSeries_single_layout <- renderUI({
  #     input$selctor
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    columns <- length(input$multi_series_vars)
    if (columns <= 6) {
      plotOutput("multiple_single_plot", height = "500px")
    } else {
      plotOutput("multiple_single_plot", height = "800px")
    }
  }
})


output$multiple_multi_plot <- renderPlot({
  #     input$selector
  if (date_check(get.data.set(), input$select_timevars) ||
      input$time_info == 2) {
    suppressWarnings(tryCatch(
      {
        iNZightTSLegacy:::plot.iNZightTS(
          ts.para$tsObj,
          multiplicative = as.logical(season_select_ts$re),
          t = 100 * input$slidersmoothing,
          xlab = input$provide_xlab,
          ylab = input$provide_ylab,
          smoother = input$timeseries_smoother,
          model.lim = ts.para$mod.lim,
          xlim = ts.para$xlim,
          compare = FALSE
        )
      },
      error = function(e) {
        cat("Handled error in multiseries plot \n")
        print(e)
      }, finally = {}
    ))
  } else {
    plot.new()
    text(0.5, 0.5,
         "No time variable found.\nPlease generate a time variable.",
         cex = 2
    )
  }
})


output$saveMultiplot <- downloadHandler(
  filename = function() {
    paste("TimeSeriesPlot",
          switch(input$saveMultiplottype,
                 "jpg" = "jpg",
                 "png" = "png",
                 "pdf" = "pdf"
          ),
          sep = "."
    )
  },
  content = function(file) {
    if (input$saveMultiplottype %in% c("jpg", "png", "pdf")) {
      if (input$saveMultiplottype == "jpg") {
        jpeg(file)
      } else if (input$saveMultiplottype == "png") {
        png(file)
      } else if (input$saveMultiplottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      
      if (date_check(get.data.set(), input$select_timevars) ||
          input$time_info == 2) {
        suppressWarnings(tryCatch(
          {
            iNZightTSLegacy:::plot.iNZightTS(
              ts.para$tsObj,
              multiplicative = as.logical(season_select_ts$re),
              t = 100 * input$slidersmoothing,
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              smoother = input$timeseries_smoother,
              model.lim = ts.para$mod.lim,
              xlim = ts.para$xlim,
              compare = FALSE
            )
          },
          error = function(e) {
            cat("Handled error in multiseries plot \n")
            print(e)
          }, finally = {}
        ))
      } else {
        plot.new()
        text(0.5, 0.5,
             "No time variable found.\nPlease generate a time variable.",
             cex = 2
        )
      }
      dev.off()
    }
  }
)


output$multipleSeries_multi_layout <- renderUI({
  columns <- length(input$multi_series_vars)
  if (columns <= 5) {
    plotOutput("multiple_multi_plot", height = "500px")
  } else {
    plotOutput("multiple_multi_plot", height = "800px")
  }
})



output$time.select <- renderUI({
  sel <- ""
  if ("time" %in% colnames(get.data.set())) {
    sel <- "time"
  } else {
    sel <- colnames(get.data.set())[1]
  }
  
  get.vars <- parseQueryString(session$clientData$url_search)
  if (!is.null(get.vars$url)) {
    temp <- session$clientData$url_search
    get.vars$url <- sub(".*?url=(.*?)&.*", "\\1", temp)
  }
  if (length(get.vars) > 0 &&
      (any(names(get.vars) %in% "url") ||
       any(names(get.vars) %in% "example")) &&
      (any(names(get.vars) %in% "time") &&
       !get.vars$time %in% "")) {
    sel <- get.vars$time
  }
  
  selectInput(
    inputId = "select_timevars",
    label = "Select time variable: ",
    choices = colnames(get.data.set()),
    selected = sel,
    selectize = FALSE
  )
})

output$time.plot.select <- renderUI({
  sel <- get.numeric.column.names(get.data.set())[1]
  
  get.vars <- parseQueryString(session$clientData$url_search)
  if (!is.null(get.vars$url)) {
    temp <- session$clientData$url_search
    get.vars$url <- sub(".*?url=(.*?)&.*", "\\1", temp)
  }
  if (length(get.vars) > 0 &&
      (any(names(get.vars) %in% "url") ||
       any(names(get.vars) %in% "example")) &&
      (any(names(get.vars) %in% "seriesVars") &&
       !get.vars$seriesVars %in% "")) {
    sel <- strsplit(get.vars$seriesVars, ",")[[1]]
  }
  list(
    h5(strong("Series Variables:")),
    div(
      style = "padding: 0px 0px; margin-top:-1.5em",
      selectInput(
        inputId = "select_variables",
        label = "",
        choices = get.numeric.column.names(get.data.set()),
        selected = sel,
        multiple = TRUE,
        selectize = FALSE,
        size = 7
      )
    )
  )
})


output$ts_plot_type <- renderUI({
  input$select_variables
  list(
    conditionalPanel(
      condition = "input.select_variables.length == 1",
      radioButtons(
        inputId = "time_plot_info1", label = "",
        choices = c(
          "Standard" = 1,
          "Decomposed" = 2,
          "Recomposed" = 3,
          "Seasonal" = 4,
          "Forecast" = 5
        ),
        selected = 1
      )
    ),
    conditionalPanel(
      condition = "input.select_variables.length > 1",
      radioButtons(
        inputId = "time_plot_info", label = "",
        choices = c(
          "Single graph" = 1,
          "Separate graphs" = 2
        ),
        selected = 1
      )
    )
  )
})

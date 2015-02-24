##  Server Functions for the "Time Series" Module
##
##  Date Created: January 16, 2015.
##
##  Please consult the documentation for the Time Series module before
##  editing any code. If you have any questions and/or suggestions,
##  drop me an e-mail: Chris Park <cpar137@aucklanduni.ac.nz>
##
##  * Note: This is to be sourced within "server.R" *

##--------------##
##  Data check  ##
##--------------##

library(iNZightTS)

##  Reactive data
ts.data <- reactive({
    input$selector
    data
    ts.data <- data
})

##  Initial Data Handling.
date_check <- function() {
    ##  Set of error checks.
    if (is.null(ts.data())) {
        return(FALSE)
        ## stop("Please load a dataset.")
    }
    if (is.null(input$select_timevars) || input$select_timevars == "") {
        return(FALSE)
        ## stop("Your dataset must have suitable time variables.")
    }
    if (!(input$select_timevars %in% names(ts.data()))) {
        ## stop("Please enter a valid variable name.")
        return(FALSE)
    }
    ##  We obtain a subset of the data.
    subdata <- ts.data()[, input$select_timevars]
    ##  If any of the time series structures return an NA,
    ##  return TRUE. Else, return FALSE.
    if (any(is.na(iNZightTS:::get.ts.structure(subdata)))) {
        ## stop("Please load a valid dataset.")
        return(FALSE)
    } else {
        return(TRUE)
    }
}


##---------------------##
##  Validation Output  ##
##---------------------##
output$validate <- renderText({
    if (is.null(ts.data())) {
        return()
    }
    if (is.null(input$select_variables)) {
        return()
    }
    if (date_check()) {
        ""
    } else {
        "This data is NOT valid for Time Series Analysis.\n
         Please load an alternative dataset that has a well-defined
         TIME variable."
    }
})



##  Variable Names
variable.names <- reactive({
    input$select_variables
    variable.names <- which(names(ts.data()) %in% input$select_variables)
})

##-------------------------------------##
##  Plot Outputs: Single Series Plots  ##
##-------------------------------------##
##
##  Time Series Plot
output$timeseries_plot <- renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        rawplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season
        )
    }
})


## output$timeseries_stop_animation <- renderPlot({
##     input$stop_animate
##     if (length(input$singleSeriesTabs) > 0) {
##         rawplot(
##             iNZightTS(ts.data(),
##                       var = variable.names()),
##             xlab = input$provide_xlab,
##             ylab = input$provide_ylab,
##             multiplicative = input$choose_season)
##     }
## })

## output$timeseries_start_animation <- renderImage({
##     input$start_animate
##     if (length(input$singleSeriesTabs) > 0) {
##         return(list(
##             src = "~/Desktop/ts/Time_Series.gif",
##             contentType = "image/gif",
##             alt = "GIF"
##             ## width = 800,
##             ## height = 350

##         ))
##     }
## }, deleteFile = FALSE)

## output$timeseries_start_animation <- renderImage({
##     input$start_animate
##     if (length(input$singleSeriesTabs) > 0) {
##         rawplot(
##             iNZightTS(ts.data(),
##                       var = variable.names()),
##             xlab = input$provide_xlab,
##             ylab = input$provide_ylab,
##             multiplicative = input$choose_season,
##             animate = TRUE)
##         currDir <- getwd()
##         return(list(
##             src = paste0(currDir, "/timeseries.gif"),
##             contentType = "image/gif",
##             alt = "Animation"))
##     }
## }, deleteFile = TRUE)

## output$timeseries_layout <- renderUI({
##     if (length(input$singleSeriesTabs) > 0) {
##         if (input$start_animate[1] > input$stop_animate[1]) {
##             imageOutput("timeseries_start_animation")
##         } else {
##             plotOutput("timeseries_stop_animation")
##         }
##     }
## })

##  Seasonal Plot
output$seasonal_plot <- renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        seasonplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season
        )
    }
})

##  Decomposed Plot
output$decomposed_plot <- renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        decompositionplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season
        )
    }
})

##  Trend + Seasonal Plot
output$trSeasonal_plot <- renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        iNZightTS:::recompose(
            iNZightTS:::decompositionplot(
                iNZightTS(ts.data(),
                          var = variable.names()),
                ## xlab = input$provide_xlab,
                ## ylab = input$provide_ylab,
                multiplicative = input$choose_season
            ),
            animate = FALSE
        )
    }
})

##  Forecast Plot
output$forecast_plot <- renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        forecastplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season
        )
    }
})

output$forecast_summary <- renderPrint({
    input$selctor
    if (length(input$singleSeriesTabs) > 0) {
        forecastplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            multiplicative = input$choose_season,
            show = FALSE
        )
    }
})


##---------------------------------------##
##  plot Outputs: Multiple Series Plots  ##
##---------------------------------------##
##
output$multiple_single_plot <- renderPlot({
    input$selctor
    if (length(input$singleSeriesTabs) > 0) {
        iNZightTS:::compareplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            multiplicative = input$choose_season
        )
    }
})

output$multipleSeries_single_layout <- renderUI({
    input$selctor
    if (length(input$multipleSeriesTabs) > 0) {
        columns <- length(input$multi_series_vars)
        if (columns <= 6) {
            plotOutput("multiple_single_plot", height = "500px")
        } else {
            plotOutput("multiple_single_plot", height = "800px")
        }
    }
})


output$multiple_multi_plot <- renderPlot({
    input$selector
    if (length(input$multipleSeriesTabs) > 0) {
        multiseries(
            iNZightTS(ts.data(),
                      var = variable.names()),
            multiplicative = input$choose_season
        )
    }
})



output$multipleSeries_multi_layout <- renderUI({
    input$selector
    if (length(input$multipleSeriesTabs) > 0) {
        columns <- length(input$multi_series_vars)
        if (columns <= 5) {
            plotOutput("multiple_multi_plot", height = "500px")
        } else {
            plotOutput("multiple_multi_plot", height = "800px")
        }
    }
})

output$variable_message <- renderPrint({
    print("Please choose one or more variables to plot")
})


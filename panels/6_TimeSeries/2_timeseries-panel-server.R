###-------------------------------------------------###
###  Server Functions for the "Time Series" Module  ###
###-------------------------------------------------###
###
###  Date Created   :   January 16, 2015
###  Last Modified  :   February 26, 2015
###
###  Please consult the comments before editing any code.
###
###  If you have any questions and/or suggestions, drop me
###  an e-mail: Chris Park <cpar137@aucklanduni.ac.nz>
###
###  * Note: This is to be sourced within "server.R" *

###--------------###
###  Data check  ###
###--------------###

###  Let data be reactive.
###  If the dataset has no time variable defined, then print an error message.
###  We need to replace this with a conditional panel that only offers the
###  option to define one's own time variable.

###  If no variables are selected, print an error message.
## output$variable_message = renderPrint({
##     if (length(input$select_variables) < 1) {
##         "Please choose one or more variables to plot"
##     }
## })

ts.data = reactive({
    validate(
        need(!is.null(data), "Please select a data set!"),
        need(length(input$select_variables) >= 1,
             "Please choose one or more variables!"),
        errorClass = "myClass")
    ts.data = data
})


###  We write a function that handles data.
date_check = function() {
    ##  Set of error checks.
    if (is.null(ts.data())) {
        return(FALSE)
    }
    if (is.null(input$select_timevars) || input$select_timevars == "") {
        return(FALSE)
    }
    if (!(input$select_timevars %in% names(ts.data()))) {
        return(FALSE)
    }
    ##  We obtain a subset of the data.
    subdata = ts.data()[, input$select_timevars]
    ##  If any of the time series structures return an NA,
    ##  return TRUE. Else, return FALSE.
    if (any(is.na(iNZightTS:::get.ts.structure(subdata)))) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

###  Time information panel - conditional on the type of dataset.
output$time_info = renderUI({
    if (date_check()) {
        radioButtons(
            inputId = "time_info",
            label = "Time Information: ",
            choices =
                c("Select time variable" = 1,
                  "Provide time manually" = 2)
        )
    } else {
        radioButtons(
            inputId = "time_info",
            label = "Time Information: ",
            choices = c("Provide time manually" = 2)
        )
    }                
})

## reactive({
##     if (!is.null(input$provide_actionButton)) {
##         season = input$provide_season
##         start = c(input$provide_startdate, season)
##         freq = input$provide_frequency
##     } else {
##         season = 1
##         start = c(1, season)
##         freq = 1
##     }
## })

        
###  Variable Names
variable.names = reactive({
    input$select_variables
    variable.names = which(names(ts.data()) %in% input$select_variables)
})

###-----------------------###
###  Single Series Plots  ###
###-----------------------###
###
###  Time Series Plot
output$timeseries_plot = renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        rawplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
                      ## start = start),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season
        )
    }
})

###  Seasonal Plot
output$seasonal_plot = renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        seasonplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            ylab = input$provide_ylab,
            xlab = input$provide_xlab,
            multiplicative = input$choose_season
        )
    }
})

###  Decomposed Plot
output$decomposed_plot = renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        decompositionplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season
        )
    }
})

###  Trend + Seasonal Plot
output$trSeasonal_plot = renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        iNZightTS:::recompose(
            iNZightTS:::decompositionplot(
                iNZightTS(ts.data(),
                          var = variable.names()),
                xlab = input$provide_xlab,
                ylab = input$provide_ylab,
                multiplicative = input$choose_season
            ),
            animate = FALSE
        )
    }
})

###  Forecast Plot
output$forecast_plot = renderPlot({
    input$selector
    if (length(input$singleSeriesTabs) > 0) {
        forecastplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            multiplicative = input$choose_season
        )
    }
})

output$forecast_summary = renderPrint({
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


###-------------------------###
###  Multiple Series Plots  ###
###-------------------------###
###
output$multiple_single_plot = renderPlot({
    input$selctor
    if (length(input$singleSeriesTabs) > 0) {
        iNZightTS:::compareplot(
            iNZightTS(ts.data(),
                      var = variable.names()),
            multiplicative = input$choose_season
        )
    }
})

output$multipleSeries_single_layout = renderUI({
    input$selctor
    if (length(input$multipleSeriesTabs) > 0) {
        columns = length(input$multi_series_vars)
        if (columns <= 6) {
            plotOutput("multiple_single_plot", height = "500px")
        } else {
            plotOutput("multiple_single_plot", height = "800px")
        }
    }
})


output$multiple_multi_plot = renderPlot({
    input$selector
    if (length(input$multipleSeriesTabs) > 0) {
        multiseries(
            iNZightTS(ts.data(),
                      var = variable.names()),
            multiplicative = input$choose_season
        )
    }
})



output$multipleSeries_multi_layout = renderUI({
    input$selector
    if (length(input$multipleSeriesTabs) > 0) {
        columns = length(input$multi_series_vars)
        if (columns <= 5) {
            plotOutput("multiple_multi_plot", height = "500px")
        } else {
            plotOutput("multiple_multi_plot", height = "800px")
        }
    }
})

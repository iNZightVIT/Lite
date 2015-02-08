
##  Server Functions for the Time Series Module
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
   
##  Load all panels into memory.
## tsfilepaths <- list.files(pattern = "[.]R$",
##                         path = "iNZightTS/R",
##                         full.names = TRUE)
## sapply(tsfilepaths, source)


library(iNZightTS)


##  Initial Data Handling.
## observe({        
date_check <- function() {
    ##  Set of error checks.
    if (is.null(data)) {
        return(FALSE)
        ## stop("Please load a dataset.")
    }        
    if (is.null(input$select_timevars) || input$select_timevars == "") {
        return(FALSE)
        ## stop("Your dataset must have suitable time variables.")   
    }
    if (!(input$select_timevars %in% names(data))) {
        ## stop("Please enter a valid variable name.")
        return(FALSE)
    }
    ##  We obtain a subset of the data.
    subdata <- data[, input$select_timevars]
    ##  If any of the time series structures return an NA,
    ##  return TRUE. Else, return FALSE.
    if (any(is.na(iNZightTS:::get.ts.structure(subdata)))) {
        ## stop("Please load a valid dataset.")
        return(FALSE)
    } else {
        return(TRUE)            
    }
}
## })


##  Data and Input Handling for *Single Series*.
## observe({
##     if (!is.null(input$time_info)) {
##         ##  Initialise dateSeason and frequency information.
##         single_dateSeason_info <- single_freq_info <- NULL
##         ##  Provide initial information (if specified).
##         if(!is.null(input$provide_actionButton)) {
##             isolate({
##                 single_dateSeason_info <- c(input$provide_startdate,
##                                             input$provide_season)
##             })
##         }
##         ##  Try using only one instance of input$provide_actionButton
##         ##  once things work out. For now, leave as is.
##         if (!is.null(input$provide_actionButton)) {
##             isolate({
##                 single_freq_info <- input$provide_frequency
##             })
##         }
##         ##  Define logical markers for conditioning.
##         single_dateSeason_check <- single_frequency_check <- FALSE
##         ##  If the initial dateSeason information is EMPTY, set
##         ##  dateSeason_check to equal TRUE. Do the same for frequency.
##         if (!(all(single_dateSeason_info == c("", "")))){
##             single_dateSeason_check <- TRUE
##         }
##         if (single_freq_info != "") {
##             single_frequency_check <- TRUE
##         }    
##         ##  Create a vector of variable names
##         single_series_varnames <- which(names(data) %in% input$single_series_vars)
##         ##  If vector is NULL, return NULL.
##         if (is.null(single_series_varnames)) {
##             return(NULL)
##         }
##         ##  If one of either dateSeason or freq (or both) is NON-EMPTY, then set
##         ##  the data for Single Series using the "iNZightTS()" function.
##         if ((single_dateSeason_check & !single_frequency_check) ||
##             (!single_dateSeason_check & single_frequency_check) ||
##             (!single_dateSeason_check & !single_frequency_check)) {
##             single_ts_data <- iNZightTS:::iNZightTS(data,
##                                                     var = single_series_varnames)
##         }
##         ##  If both dateSeason and freq are EMPTY, then specify the start and freq
##         ##  arguments manually.
##         if (single_dateSeason_check & single_frequency_check) {
##             single_ts_data <- iNZightTS:::iNZightTS(data,
##                                                     start = as.numeric(single_dateSeason_info),
##                                                     freq = as.numeric(single_freq_info),
##                                                     var = single_series_varnames)
##         }
##     }
## })

##  Data and Input Handling for *Several Series*.
## observe({            
##     if (!is.null(input$choose_series)) {
##         ##  Initialise dateSeason and frequency information.
##         several_dateSeason_info <- several_freq_info <- NULL
##         ##  Provide initial information (if specified).
##         if (!is.null(input$provide_actionButton)) {
##             isolate({
##                 several_dateSeason_info <- c(input$provide_startdate,
##                                              input$provide_season)
##             })
##         }
##         ##  Try using only one instance of input$provide_actionButton
##         ##  once things work out. For now, leave as is.
##         if (!is.null(input$provide_actionButton)) {
##             isolate({
##                 several_freq_info <- input$provide_frequency
##             })
##         }
##         ##  Define logical markers for conditioning.
##         several_dateSeason_check <- several_frequency_check <- FALSE
##         ##  Check if multiple variables have been selected.
##         if (length(input$multiple_series_vars) <= 1) {
##             return(NULL)
##         }
##         ##  If the initial dateSeason information is EMPTY, set
##         ##  dateSeason_check to equal TRUE. Do the same for frequency.
##         if (!(all(several_dateSeason_info == c("", "")))) {
##             several_dateSeason_check <- TRUE
##         }
##         if (freq_info != "") {
##             several_frequency_check <- TRUE
##         }    
##         ##  Create a vector of variable names
##         multiple_series_varnames <- which(names(data) %in% input$multiple_series_vars)
##         ##  If vector is NULL, return NULL.
##         if (is.null(multiple_series_varnames)) {
##             return(NULL)
##         }
##         ##  If one of either dateSeason or freq (or both) is NON-EMPTY, then set
##         ##  the data for Single Series using the "iNZightTS()" function.
##         if ((several_dateSeason_check & !several_freq_check) ||
##             (!several_dateSeason_check & several_freq_check) ||
##             (!several_dateSeason_check & !several_freq_check)) {
##             multiple_ts_data <- iNZightTS:::iNZightTS(data,
##                                                       var = multiple_series_varnames)
##         }
##         ##  If both dateSeason and freq are EMPTY, then specify the start and freq
##         ##  arguments manually.
##         if (several_dateSeason_check & several_freq_check) {
##             multiple_ts_data <- iNZightTS:::iNZightTS(data,
##                                                       start = as.numeric(several_dateSeason_info),
##                                                       freq = as.numeric(several_freq_info),
##                                                       var = multiple_series_varnames)
##         }
##     }
## })


##---------------------##
##  Validation Output  ##
##---------------------##
output$validate <- renderText({
    if (is.null(data)) {
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


##-------------------------------------##
##  Plot Outputs: Single Series Plots  ##
##-------------------------------------##
##
##  Time Series Plot


output$timeseries_plot <- renderPlot({
    if (length(input$singleSeriesTabs) > 0) {
        rawplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }
})


output$timeseries_stop_animation <- renderPlot({
    input$stop_animate
    if (length(input$singleSeriesTabs) > 0) {
        rawplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }
})

output$timeseries_start_animation <- renderImage({
    input$start_animate
    if (length(input$singleSeriesTabs) > 0) {
        return(list(
            src = "~/Desktop/ts/Time_Series.gif",
            contentType = "image/gif",
            alt = "GIF"
            ## width = 800,
            ## height = 350  

        ))
    }
}, deleteFile = FALSE)

## output$timeseries_start_animation <- renderImage({
##     input$start_animate
##     if (length(input$singleSeriesTabs) > 0) {
##         rawplot(
##             iNZightTS(data,
##                       var = which(names(data) %in% input$select_variables)),
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
      
output$timeseries_layout <- renderUI({
    if (length(input$singleSeriesTabs) > 0) {
        if (input$start_animate[1] > input$stop_animate[1]) {
            imageOutput("timeseries_start_animation")
        } else {
            plotOutput("timeseries_stop_animation")
        } 
    }    
})

##  Seasonal Plot
output$seasonal_plot <- renderPlot({
    ## if (!is.null(single_ts_data)) {
    ##     iNZightTS:::seasonplot(single_ts_data,
    if (length(input$singleSeriesTabs) > 0) {
        seasonplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }                 
})

##  Decomposed Plot
output$decomposed_plot <- renderPlot({
    ## if (!is.null(single_ts_data)) {
    ##     iNZight:::decompositionplot(single_ts_data,
    if (length(input$singleSeriesTabs) > 0) {
        decompositionplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }
})

##  Trend + Seasonal Plot
output$trSeasonal_plot <- renderPlot({
    ## if (!is.null(single_ts_data)) {
    if (length(input$singleSeriesTabs) > 0) {
        iNZightTS:::recompose(
            iNZightTS:::decompositionplot(
                iNZightTS(data,
                          var = which(names(data) %in% input$select_variables)),
                xlab = input$provide_xlab,
                ylab = input$provide_ylab,
                multiplicative = input$choose_season),
            animate = FALSE)      
    }
})

##  Forecast Plot
output$forecast_plot <- renderPlot({
    ## if (!is.null(single_ts_data)) {
    ##     iNZightTS:::forecastplot(single_ts_data,
    if (length(input$singleSeriesTabs) > 0) {
        forecastplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }     
})

output$forecast_summary <- renderPrint({
    if (length(input$singleSeriesTabs) > 0) {
        forecastplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season,
            show = FALSE)
    }     
})


##---------------------------------------##
##  plot Outputs: Multiple Series Plots  ##
##---------------------------------------##
##
output$multiple_single_plot <- renderPlot({
    ## if (!is.null(multiple_ts_data)) {
    ##     iNZightTS:::compareplot(multiple_ts_data,
    if (length(input$singleSeriesTabs) > 0) {
        iNZightTS:::compareplot(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }
})


output$multipleSeries_single_layout <- renderUI({
    if (length(input$multipleSeriesTabs) > 0) {
        plotOutput("multiple_single_plot", height = "700px")            
        ## columns <- length(input$multi_series_vars)
        ## if (columns <= 8) {
        ##     plotOutput("multiple_single_plot", height = "600px")
        ## } else {
        ##     plotOutput("multiple_single_plot", height = "900px")
        ## }            
    }
})


output$multiple_multi_plot <- renderPlot({
    ## if (!is.null(multiple_ts_data)) {
    ##     iNZightTS:::multiseries(multiple_ts_data,
    if (length(input$multipleSeriesTabs) > 0) {
        multiseries(
            iNZightTS(data,
                      var = which(names(data) %in% input$select_variables)),
            ## xlab = input$provide_xlab,
            ## ylab = input$provide_ylab,
            multiplicative = input$choose_season)
    }        
})



output$multipleSeries_multi_layout <- renderUI({
    if (length(input$multipleSeriesTabs) > 0) {
        plotOutput("multiple_multi_plot", height = "700px")
        ## columns <- length(input$multi_series_vars)
        ## if (columns <= 8) {
        ##     plotOutput("multiple_multi_plot", height = "600px")
        ## } else {
        ##     plotOutput("multiple_multi_plot", height = "900px")
        ## }            
    }
})

output$variable_message <- renderPrint({
    print("Please choose one or more variables to plot")
})


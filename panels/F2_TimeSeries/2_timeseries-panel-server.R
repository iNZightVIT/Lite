###-------------------------------------------------###
###  Server Functions for the "Time Series" Module  ###
###-------------------------------------------------###
###
###  Date Created   :   January 16, 2015
###  Last Modified  :   February 26, 2015
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *

# initialize gui
output$timeseries.panel <- renderUI({
  timeseries.panel.ui(get.data.set())
})

###--------------###
###  Data check  ###
###--------------###

###  We write a function that handles data.
date_check = function(dafr,time.vars) {
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
    subdata = dafr[,time.vars]
    ##  If any of the time series structures return an NA,
    ##  return TRUE. Else, return FALSE.
    # function iNZightTS:::get.ts.structure is not stable 
    # it throws an error if a normal factor variable is 
    # passed in.
    tryCatch({
      if (any(is.na(suppressWarnings(iNZightTS:::get.ts.structure(subdata))))) {
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
output$time_info = renderUI({
  radioButtons(inputId = "time_info",
               label = "Time Information: ",
               choices = c("Select time variable" = 1,
                           "Provide time manually" = 2)
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
observe({
  input$provide_actionButton
  isolate({
    if(!is.null(input$provide_actionButton)&&
         input$provide_actionButton>0){
      time = NULL
      if(!is.null(input$provide_frequency)&&
           input$provide_frequency%in%"Day"){
        time = seq(as.Date(input$provide_startdate), 
                   by='day', 
                   length=nrow(get.data.set()))
        time = unlist(lapply(strsplit(as.character(time),"-"),
                             function(x){
                               paste0(x[1],"D",
                                      strftime(as.Date(paste(x,collapse="-")),
                                               format="%j"))
                               }))
      }else if (!is.null(input$provide_frequency)&&
                  input$provide_frequency%in%"Month"){
        time = seq(as.Date(input$provide_startdate), 
                   by='month', 
                   length=nrow(get.data.set()))
        time = unlist(lapply(strsplit(as.character(time),"-"),
                             function(x){
                               paste0(x[1],"M",x[2])
                             }))
      }else if(!is.null(input$provide_frequency)&&
                 input$provide_frequency%in%"Quarter"){
        time = seq(as.Date(input$provide_startdate), 
                   by='month', 
                   length=nrow(get.data.set())*3)[seq(from=1,by=3,
                                                      length.out=nrow(get.data.set()))]
        time = unlist(lapply(strsplit(as.character(time),"-"),
                             function(x){
                               paste0(x[1],"Q",ceiling(as.numeric(x[2])/3))
                             }))
      }
      values$data.set = cbind(time,get.data.set())
    }
  })
})

        
###  Variable Names
variable.names = reactive({
  input$select_variables
  variable.names = which(names(get.data.set()) %in% input$select_variables)
})

###-----------------------###
###  Single Series Plots  ###
###-----------------------###
###
###  Time Series Plot
output$timeseries_plot = renderPlot({
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          rawplot(
            iNZightTS(temp,
                      var = variable.names()),
            ## start = start),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season
          )
        }, warning = function(w) {
          cat("Warning produced in timseries plot\n")
          print(w)
        }, error = function(e) {
          cat("Handled error in timseries plot\n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

###  Seasonal Plot
output$seasonal_plot = renderPlot({
#     input$selector
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          seasonplot(
            iNZightTS(temp,
                      var = variable.names()),
            ylab = input$provide_ylab,
            xlab = input$provide_xlab,
            multiplicative = input$choose_season
          )
        }, warning = function(w) {
          cat("Warning produced in seasonplot\n")
          print(w)
        }, error = function(e) {
          cat("Handled error in seasonplot\n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

###  Decomposed Plot
output$decomposed_plot = renderPlot({
#     input$selector
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          decompositionplot(
            iNZightTS(temp,
                      var = variable.names()),
            xlab = input$provide_xlab,
            ylab = input$provide_ylab,
            multiplicative = input$choose_season,
            t = 10
          )
        }, warning = function(w) {
          cat("Warning produced in decompositionplot \n")
          print(w)
        }, error = function(e) {
          cat("Handled error in decompositionplot \n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

###  Trend + Seasonal Plot
output$trSeasonal_plot = renderPlot({
#     input$selector
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          iNZightTS:::recompose(
            iNZightTS:::decompositionplot(
              iNZightTS(temp,
                        var = variable.names()),
              xlab = input$provide_xlab,
              ylab = input$provide_ylab,
              multiplicative = input$choose_season,
              t = 10
            ),
            animate = FALSE
          )
        }, warning = function(w) {
          cat("Warning produced in recompose plot \n")
          print(w)
        }, error = function(e) {
          cat("Handled error in recompose plot \n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

###  Forecast Plot
output$forecast_plot = renderPlot({
#     input$selector
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          forecastplot(
            iNZightTS(temp,
                      var = variable.names()),
            multiplicative = input$choose_season
          )
        }, warning = function(w) {
          cat("Warning produced in forecastplot \n")
          print(w)
        }, error = function(e) {
          cat("Handled error in forecastplot \n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

output$forecast_summary = renderPrint({
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          forecastplot(
            iNZightTS(temp,
                      var = variable.names()),
            multiplicative = input$choose_season,
            show = FALSE
          )
        }, warning = function(w) {
          cat("")
        }, error = function(e) {
          cat("")
        }, finally = {})
      }
    }else{
      cat("No time variable found.")
#       plot.new()
#       text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})


###-------------------------###
###  Multiple Series Plots  ###
###-------------------------###
###
output$multiple_single_plot = renderPlot({
#     input$selctor
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$singleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          iNZightTS:::compareplot(
            iNZightTS(temp,
                      var = variable.names()),
            multiplicative = input$choose_season
          )
        }, warning = function(w) {
          cat("Warning produced in compareplot \n")
          print(w)
        }, error = function(e) {
          cat("Handled error in compareplot \n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

output$multipleSeries_single_layout = renderUI({
#     input$selctor
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$multipleSeriesTabs) > 0) {
          columns = length(input$multi_series_vars)
          if (columns <= 6) {
              plotOutput("multiple_single_plot", height = "500px")
          } else {
              plotOutput("multiple_single_plot", height = "800px")
          }
      }
    }
})


output$multiple_multi_plot = renderPlot({
#     input$selector
    if(date_check(get.data.set(),input$select_timevars)){
      if (length(input$multipleSeriesTabs) > 0) {
        temp = get.data.set()
        if(!input$select_timevars%in%"time"){
          colnames(temp)[which(colnames(temp)%in%input$select_timevars)] = "time"
        }
        tryCatch({
          multiseries(
            iNZightTS(temp,
                      var = variable.names()),
            multiplicative = input$choose_season
          )
        }, warning = function(w) {
          cat("Warning produced in multiseries plot \n")
          print(w)
        }, error = function(e) {
          cat("Handled error in multiseries plot \n")
          print(e)
        }, finally = {})
      }
    }else{
      plot.new()
      text(0.5,0.5,"No time variable found.\nPlease generate a time variable.",cex=2)
    }
})

output$multipleSeries_multi_layout = renderUI({
#     input$selector
    if (length(input$multipleSeriesTabs) > 0) {
        columns = length(input$multi_series_vars)
        if (columns <= 5) {
            plotOutput("multiple_multi_plot", height = "500px")
        } else {
            plotOutput("multiple_multi_plot", height = "800px")
        }
    }
})

output$time.select = renderUI({
  sel = ""
  if("time"%in%colnames(get.data.set())){
    sel="time"
  }else{
    sel = colnames(get.data.set())[1]
  }
  get.vars = parseQueryString(session$clientData$url_search)
  if(!is.null(get.vars$url)) {
    temp = session$clientData$url_search
    get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
  }
  if(length(get.vars)>0&&
       (any(names(get.vars)%in%"url")||
          any(names(get.vars)%in%"example"))&&
       (any(names(get.vars)%in%"time")&&
          !get.vars$time%in%"")){
    sel=get.vars$time
  }
  selectInput(
    inputId = "select_timevars",
    label = "Select time variable: ",
    choices = colnames(get.data.set()),
    selected = sel,
    selectize = FALSE
  )
})

output$time.plot.select = renderUI({
  sel = rev(get.numeric.column.names(get.data.set()))[1]
  get.vars = parseQueryString(session$clientData$url_search)
  if(!is.null(get.vars$url)) {
    temp = session$clientData$url_search
    get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
  }
  if(length(get.vars)>0&&
       (any(names(get.vars)%in%"url")||
          any(names(get.vars)%in%"example"))&&
       (any(names(get.vars)%in%"seriesVars")&&
          !get.vars$seriesVars%in%"")){
    sel = strsplit(get.vars$seriesVars,",")[[1]]
  }
  selectInput(inputId = "select_variables",
              label = "Series Variables: ",
              choices =  rev(get.numeric.column.names(get.data.set())),
              selected = sel,
              multiple = TRUE,
              selectize = FALSE)
})


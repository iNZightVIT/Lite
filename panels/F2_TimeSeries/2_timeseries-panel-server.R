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

guess_key_ts_object = function() {
  temp <- get.data.set()
  cols <- names(temp)
  
  time_var_index = which(colnames(temp) == ts_rvals$sel_time)
  cat_cols <- cols[!sapply(temp, is.numeric)][-time_var_index]
  t_var <- names(temp)[[time_var_index]]
  
  maybe_key <- NULL
  ts_test <- try(
    iNZightTS::inzightts(temp, index = t_var, key = maybe_key),
    silent = TRUE
  )
  if (!inherits(ts_test, "inz_ts")) {
    for (key_cand in cat_cols) {
      maybe_key <- c(maybe_key, key_cand)
      ts_test <- try(
        iNZightTS::inzightts(temp, index = t_var, key = maybe_key),
        silent = TRUE
      )
      if (inherits(ts_test, "inz_ts")) {
        key_msg <- paste0(
          ifelse(length(maybe_key) > 1, "c(", ""),
          paste(maybe_key, collapse = ", "),
          ifelse(length(maybe_key) > 1, ")", "")
        )
        message("Guessing key = ", key_msg)
        # ts_rvals$sel_key = maybe_key
        return(ts_test)
      }
    }
  }
}

# new
create_ts_object = function() {
  ts_rvals$obj = NULL
  temp = get.data.set()
  ts_rvals$available_vars = colnames(temp)
  
  ri <- ti <- which(colnames(temp) == input$select_timevars)
  # key_col <- ifelse(is.null(ts_rvals$sel_key), NULL, which(colnames(temp) == ts_rvals$sel_key))
  key_col <- which(colnames(temp) == input$select_keys)
  if (length(key_col)) {
    ki <- ts_rvals$available_vars[key_col]
    ri <- c(ri, ki)
  } else {
    ki <- NULL
    key_col <- NULL
  }

  t <- try(
    iNZightTS::inzightts(
      temp,
      index = ti,
      key = ki
    ),
    silent = TRUE
  )
  if (inherits(t, "try-error")) {
    message("Unable to create temporal object. Will guess key.")
    ts_rvals$obj = guess_key_ts_object()
    if(is.null(ts_rvals$obj)) {
      message("Unable to guess key.")
      ts_rvals$sel_key = NULL
      return()
    }
    ts_rvals$sel_key = tsibble::key_vars(ts_rvals$obj)
  } else {
    ts_rvals$obj = t
    ts_rvals$sel_key = input$select_keys
  }
  
  # ts_rvals$sel_index = index_var(ts_rvals$obj)
  ts_rvals$sel_time = input$select_timevars
  ts_rvals$sel_var = input$select_variables # measured_vars(t)
}

# initialize gui

ts_rvals = reactiveValues()
ts_rvals$obj = NULL
ts_rvals$sel_index = NULL
ts_rvals$sel_time = NULL
ts_rvals$sel_key = NULL
ts_rvals$sel_var = NULL
ts_rvals$num_vars = NULL
ts_rvals$cat_vars = NULL
ts_rvals$choose_season = NULL
ts_rvals$available_vars = NULL

# season_select_ts <- reactiveValues()
# season_select_ts$re <- as.logical()


output$timeseries.panel <- renderUI({
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

# init ts object
observe({
  get.data.set()
  input$select_timevars
  input$select_keys
  input$select_variables
  
  # print(input$select_timevars)
  # print(input$select_keys)
  # print(input$select_variables)
 
  if (
      !is.null(input$select_timevars) &&
      !is.null(input$select_variables) &&
      # first ui render, the first measure var might be the same as time var
      !(input$select_timevars %in% input$select_variables)
    ) {
      create_ts_object()
      print("-----")
      print(ts_rvals$sel_time)
      print(ts_rvals$sel_key )
      print(ts_rvals$sel_var)
      print("-----\n")
  } else {
    ts_rvals$obj = NULL
  }
})

output$timeseries_plot <- renderPlot({
  check = list(
    ts_rvals$obj,
    ts_rvals$sel_var,
    input$choose_season,
    input$time_plot_info,
    input$slidersmoothing,
    input$timeseries_smoother,
    input$timeseries_seasonally_adjusted,
    input$slidersmoothing,
    input$adjust_limit_from,
    input$adjust_limit_until
  )
  if (is.null(ts_rvals$obj)) {
    plot.new()
    text(
      0.5, 
      0.5,
      paste("Unable to create temporal object. Maybe you forgot to specify the keys?"),
      cex = 2
    )
  } 
  if (is.null(ts_rvals$sel_var)) {
      plot.new()
      text(0.5, 0.5, "No variables selected.", cex = 2)
  } else if(all(!sapply(check, is.null))){ #  
    
    
    t = try({
      ts_p = ts_rvals$obj
      
      as_range <- function(x) {
        if (is.numeric(x)) {
          x 
        }
        else {
          distinct(ts_p, !!tsibble::index(ts_p)) %>% 
            filter(!!tsibble::index(ts_p) %in% x) %>% 
            pull() %>% as.Date()
        }
      }
      plot_range <- as_range(c(input$adjust_limit_from, input$adjust_limit_until))
      if (tsibble::n_keys(ts_p) > 1) { #  && svalue(key_filter) != "(Show all)"
        key_i <- which(colnames(ts_p) == ts_rvals$sel_key) - # key_filter$get_index() -
          (tsibble::n_keys(ts_p) < 20) +
          (input$time_plot_info == "Decomposition")
        ts_p <- tsibble::key_data(ts_p)[key_i, ] |>
          dplyr::left_join(ts_p, by = tsibble::key_vars(ts_p), multiple = "all") |>
          tsibble::as_tsibble(index = !!tsibble::index(ts_p), key = NULL) |>
          inzightts()
      }
      key_to_hl <- NULL
      if (length(ts_rvals$sel_key) && which(colnames(ts_p) == ts_rvals$sel_key) != 1L) {
        key_to_hl <- which(colnames(ts_p) == ts_rvals$sel_key) - 1L
      }
      
      smooth_value = ifelse(input$timeseries_smoother, input$slidersmoothing, 0)
      if(input$time_plot_info == "default") {
        plot(
          x = ts_p,
          var = ts_rvals$sel_var,
          # emphasise = ts_rvals$sel_key,
          emphasise = key_to_hl,
          t = smooth_value,
          xlim = plot_range,
          mult_fit = input$choose_season == "multi",
          seasonal_adjustment = input$timeseries_seasonally_adjusted
        )
      } else if(input$time_plot_info == "decomposed") {
        plot(
          decomp(
            x = ts_p,
            var = ts_rvals$sel_var,
            t = smooth_value,
            model_range = plot_range,
            mult_fit = input$choose_season == "multi"
          )
        )
      } else if(input$time_plot_info == "seasonal") {
        seasonplot(
          x = ts_p,
          var = ts_rvals$sel_var,
          t = smooth_value,
          model_range = plot_range,
          mult_fit = input$choose_season == "multi"
        )
      } else if(input$time_plot_info == "forecast") {
        forecasts = predict(
          object = ts_p,
          var = ts_rvals$sel_var,
          model_range = plot_range,
          mult_fit = input$choose_season == "multi"
        )
        plot(x = forecasts, t_range = plot_range)
      } else {
        plot.new()
        text(
          0.5, 
          0.5,
          paste("Plot type '", input$time_plot_info, "' not suppored"),
          cex = 2
        )
      }
    }, silent = TRUE)
    
    if (inherits(t, "try-error")) {
      plot.new()
      text(
        0.5, 
        0.5,
        paste("Unable to create temporal object. Maybe you forgot to specify the keys?"),
        cex = 2
      )
    } else {
      t
    }
    
  }
})

## main UI
output$ts.main.ui <- renderUI({
  get.data.set()
  input$select_variables
  # input$time_plot_info1
  input$time_plot_info
  ret <- NULL
  isolate({
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
        )
        # ,
        # tabPanel(
        #   title = "Interactive Plot (via plotly)",
        #   uiOutput("plotly_tsmainnw"),
        #   plotlyOutput("plotly_tsmain", height = "500px") %>% withSpinner()
        # )
      )
    )
    ret
  })
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
  ts_rvals$sel_time = sel
  
  list(
    h5("Time:"),
    div(
      style = "padding: 0px 0px; margin-top:-1.5em",
      selectInput(
        inputId = "select_timevars",
        label = "",
        choices = colnames(get.data.set()),
        selected = sel,
        selectize = FALSE
      )
    )
  )

})

output$key.select <- renderUI({
  ts_rvals$obj
  # sel <- get.numeric.column.names(get.data.set())[1]
  
  # get.vars <- parseQueryString(session$clientData$url_search)
  # if (!is.null(get.vars$url)) {
  #   temp <- session$clientData$url_search
  #   get.vars$url <- sub(".*?url=(.*?)&.*", "\\1", temp)
  # }
  # if (length(get.vars) > 0 &&
  #     (any(names(get.vars) %in% "url") ||
  #      any(names(get.vars) %in% "example")) &&
  #     (any(names(get.vars) %in% "seriesVars") &&
  #      !get.vars$seriesVars %in% "")) {
  #   sel <- strsplit(get.vars$seriesVars, ",")[[1]]
  # }
  # ts_rvals$sel_key = input$select_keys
  
  # available_keys = colnames(get.data.set())
  # available_keys = available_keys[-which(available_keys == ts_rvals$obj)]
  list(
    h5("Key:"),
    div(
      style = "padding: 0px 0px; margin-top:-1.5em",
      selectInput(
        inputId = "select_keys",
        label = "",
        choices = colnames(get.data.set()),
        selected = ts_rvals$sel_key,
        multiple = TRUE,
        selectize = FALSE,
        size = 7
      )
    )
  )
})
# time.plot.select
output$time.plot.select <- renderUI({
  input$choose_var_type
  temp = get.data.set()
  available_vars = colnames(temp)

  # remove time and key
  if(!is.null(ts_rvals$sel_time)) {
    available_vars = available_vars[available_vars != ts_rvals$sel_time]
  }
  if(!is.null(ts_rvals$sel_key)) {
    available_vars = available_vars[available_vars != ts_rvals$sel_key]
  }
  
  vartypes = iNZightTools::vartypes(temp[, available_vars])
  num_vars = names(which(vartypes == "num"))
  cat_vars = names(which(vartypes == "cat"))
  
  # get.vars <- parseQueryString(session$clientData$url_search)
  # if (!is.null(get.vars$url)) {
  #   temp <- session$clientData$url_search
  #   get.vars$url <- sub(".*?url=(.*?)&.*", "\\1", temp)
  # }
  # if (length(get.vars) > 0 &&
  #     (any(names(get.vars) %in% "url") ||
  #      any(names(get.vars) %in% "example")) &&
  #     (any(names(get.vars) %in% "seriesVars") &&
  #      !get.vars$seriesVars %in% "")) {
  #   sel <- strsplit(get.vars$seriesVars, ",")[[1]]
  # }
  sel = NULL
  choices = c()
  if (input$choose_var_type == "num") {
    if (length(num_vars) > 0) {
      sel = num_vars[1]
      choices = num_vars
      ts_rvals$num_vars = num_vars
    }
    shinyjs::enable(id = "choose_season")
  } else {
    if (length(cat_vars) > 0) {
      sel = cat_vars[1]
      choices = cat_vars
      ts_rvals$cat_vars = cat_vars
    }
    shinyjs::disable(id = "choose_season")
  }
  ts_rvals$sel_var = sel
  
  list(
    div(
      style = "padding: 0px 0px; margin-top:-1.5em",
      selectInput(
        inputId = "select_variables",
        label = "",
        choices = choices,
        selected = sel,
        multiple = TRUE,
        selectize = FALSE,
        size = 7
      )
    )
  )
})

# create sliderInput
output$time.range.var <- renderUI({
  if(!is.null(ts_rvals$obj)) {
    idx = sort(unique(ts_rvals$obj[[tsibble::index(ts_rvals$obj)]]))
    list(
      h5("Plot data from/to:"),
      fixedRow(
        column(
          width = 6,
          sliderTextInput(
            inputId = "adjust_limit_from",
            label = "",
            choices = idx,
            selected = idx[1]
          )
        ),
        column(
          width = 6,
          sliderTextInput(
            inputId = "adjust_limit_until",
            label = "",
            choices = idx,
            selected = idx[length(idx)]
          )
        )
      )
    )
  }
})

output$time_plot_info = renderUI({
  input$choose_var_type
  if(!is.null(ts_rvals$obj) && input$choose_var_type %in% c("num", "cat")) {
    choices = c(
      "Default" = "default",
      "Decomposed" = "decomposed",
      "Seasonal" = "seasonal",
      "Forecast" = "forecast"
    )
    if(input$choose_var_type == "cat") {
      choices = choices[1]
    }
    radioButtons(
      inputId = "time_plot_info", label = "",
      choices = choices,
      selected = "default",
      inline = T
    )
  }
})

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
      
      if (!is.null(ts_rvals$obj)) {
        suppressWarnings(tryCatch(
          {
            plot(ggplot2::last_plot())
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

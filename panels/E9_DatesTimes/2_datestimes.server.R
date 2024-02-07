output$convert_datestimes_panel <- renderUI({
  get.data.set()

  ret <- NULL
  isolate({
    ret <- list(
      selectInput(
        inputId = "convert_datestimes_selectvars",
        label = "Select variables to convert from",
        choices = colnames(get.data.set()),
        selectize = FALSE,
        multiple = TRUE,
        size = 7
      ),
      textInput("convert_datestimes_newname",
        label = "Name for the new variable", value = ""
      ),
      selectInput(
        inputId = "convert_datestimes_selectorder",
        label = "Select the order format of your data",
        choices = c(
          "",
          "year month date",
          "year month date Hour Minute Second",
          "year month date Hour Minute Second pm/am",
          "day month year",
          "day month year Hour Minute Second",
          "day month year Hour Minute Second pm/am",
          "Unix timestamp (secs from 1970)"
        ),
        selectize = FALSE,
        multiple = FALSE
      ),
      fixedRow(
        column(6, DTOutput("convert_datestimes_original_table")),
        column(6, DTOutput("convert_datestimes_converted_table"))
      ),
      fixedRow(
        column(3, actionButton("preview_convert_datestimes", "Preview",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(3, actionButton("convert_datestimes_button", "Convert",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      )
    )
  })
  ret
})


observe({
  input$convert_datestimes_selectvars
  isolate({
    if (!is.null(input$convert_datestimes_selectvars) &&
      length(input$convert_datestimes_selectvars) > 0) {
      temp.data <- get.data.set()
      factorname <- input$convert_datestimes_selectvars
      varx <- ""
      new_name <- ""
      for (num in 1:length(factorname)) {
        name <- factorname[num]
        varx <- paste(varx, temp.data[[name]])
        new_name <- paste(new_name, name, ".", sep = "")
      }

      updateTextInput(session, "convert_datestimes_newname",
        value = paste0(new_name, ".dt", sep = "")
      )

      output$convert_datestimes_original_table <- renderDT(
        {
          if (!is.null(session$userData$LITE_VERSION) &&
            session$userData$LITE_VERSION == "CAS") {
            data <- data.frame(
              Original = data.frame(
                Original = varx,
                stringsAsFactors = TRUE
              )[values$sample.row, ]
            )
            row.names(data) <- 1:length(values$sample.row)
          } else {
            data <- data.frame(Original = varx, stringsAsFactors = TRUE)
          }
          data
        },
        options = list(
          sDom = '<"top">lrt<"bottom">ip',
          lengthMenu = c(5, 30, 50), pageLength = 5,
          columns.defaultContent = "NA", scrollX = T
        )
      )
    }
  })
})


observe({
  input$preview_convert_datestimes
  isolate({
    if (!is.null(input$convert_datestimes_selectorder) &&
      input$convert_datestimes_selectorder != "") {
      convname <- input$convert_datestimes_selectorder
      if (!is.null(input$convert_datestimes_selectvars) &&
        length(input$convert_datestimes_selectvars) > 0) {
        factorname <- input$convert_datestimes_selectvars
        if (!is.null(input$convert_datestimes_newname) &&
          !grepl("^\\s*$", input$convert_datestimes_newname)) {
          name <- input$convert_datestimes_newname
          # TODO:
          data <- tryCatch(
            data.frame(
              Converted = iNZightTools::convert_to_datetime(
                get.data.set(), factorname, convname, name
              )[[name]],
              stringsAsFactors = TRUE
            ),
            warning = function(w) {
              if (w$message == "Failed to parse") {
                data.frame(
                  Converted = "Invalid format",
                  stringsAsFactors = TRUE
                )
              } else {
                data.frame(Converted = w$message, stringsAsFactors = TRUE)
              }
            }
          )

          output$convert_datestimes_converted_table <- renderDT(
            {
              if (!is.null(session$userData$LITE_VERSION) &&
                session$userData$LITE_VERSION == "CAS") {
                data <- data.frame(Converted = data[values$sample.row, ])
                row.names(data) <- 1:length(values$sample.row)
              }
              data
            },
            options = list(
              sDom = '<"top">lrt<"bottom">ip',
              lengthMenu = c(5, 30, 50), pageLength = 5,
              columns.defaultContent = "NA", scrollX = T
            )
          )
        }
      }
    }
  })
})


observe({
  input$convert_datestimes_button
  isolate({
    if (!is.null(input$convert_datestimes_selectorder) &&
      input$convert_datestimes_selectorder != "") {
      convname <- input$convert_datestimes_selectorder
      if (!is.null(input$convert_datestimes_selectvars) &&
        length(input$convert_datestimes_selectvars) > 0) {
        factorname <- input$convert_datestimes_selectvars
        if (!is.null(input$convert_datestimes_newname) &&
          !grepl("^\\s*$", input$convert_datestimes_newname)) {
          name <- input$convert_datestimes_newname
          # TODO: 
          data <- iNZightTools::convert_to_datetime(
            get.data.set(), factorname, convname, name
          )
          updatePanel$datachanged <- updatePanel$datachanged + 1
          values$data.set <- data
          values <- sample_if_cas(
            rvalues = values, d = data,
            new_sample = FALSE
          )

          output$convert_datestimes_original_table <- renderDT(
            {
              NULL
            },
            options = list(
              sDom = '<"top">lrt<"bottom">ip',
              lengthMenu = c(5, 30, 50), pageLength = 5,
              columns.defaultContent = "NA", scrollX = T
            )
          )

          output$convert_datestimes_converted_table <- renderDT(
            {
              NULL
            },
            options = list(
              sDom = '<"top">lrt<"bottom">ip',
              lengthMenu = c(5, 30, 50), pageLength = 5,
              columns.defaultContent = "NA", scrollX = T
            )
          )
        }
      }
    }
  })
})



output$aggregate_datestimes_panel <- renderUI({
  get.data.set()

  ret <- NULL
  isolate({
    ret <- list(
      selectInput(
        inputId = "aggregate_datestimes_selectvars",
        label = "Select a column",
        choices = c("", colnames(get.data.set())),
        selectize = FALSE,
        multiple = FALSE
      ),
      selectInput(
        inputId = "aggregate_datestimes_format",
        label = "Choose a format",
        choices = c(
          "",
          "Weekly", "Monthly", "Quarterly", "Yearly"
        ),
        selectize = FALSE,
        multiple = FALSE
      ),
      selectInput(
        inputId = "aggregate_datestimes_how",
        label = "How to aggregate",
        choices = c("Sum", "Mean", "Median"),
        selectize = FALSE,
        multiple = FALSE
      ),
      fixedRow(
        column(3, actionButton("preview_aggregate_datestimes", "Preview",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(3, actionButton("aggregate_datestimes_button", "Aggregate",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      )
    )
  })
  ret
})

output$convert.datestimes.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)

output$dates.times <- renderUI({
  dates.times.panel()
})

output$aggregate_datestimes.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)

output$convert.datestimes.data.sample.info <- renderText({
  sample_info_cas()
})

output$aggregate_datestimes.data.sample.info <- renderText({
  sample_info_cas()
})

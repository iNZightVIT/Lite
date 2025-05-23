##  Row operations (Perform row operations) --> Filter Dataset

code.data.modify <- function(data.name, temp) {
  code <- tidy_assign_pipe(gsub(
    "get.data.set\\()",
    data.name,
    iNZightTools::code(temp)
  ))
  code <- do.call(paste, c(as.list(code), sep = ""))
  code <- do.call(c, lapply(code, function(x) {
    y <- try(
      {
        formatR::tidy_source(
          text = x,
          width.cutoff = 80,
          output = F,
          indent = 4
        )$text.tidy
      },
      silent = TRUE
    )
    if (inherits(y, "try-error")) {
      x
    } else {
      c(y, "\n")
    }
  }))
  code <- c(paste0(data.name, " <- \n"), code)
}



observe({
  input$filter_data_perform
  isolate({
    if (!is.null(input$filter_data_perform) &&
      input$filter_data_perform > 0) {
      if (input$select_filter %in% "levels of categorical variable") {
        if (!is.null(input$select_categorical1) &&
          !input$select_categorical1 %in% "") {
          temp <- iNZightTools::filter_cat(
            get.data.set(),
            var = input$select_categorical1,
            levels = input$levels1
          )
          if (!is.null(temp)) {
            ## save code
            code.save$dataname <- paste(code.save$name, "filtered", sep = ".")
            code <- code.data.modify(code.save$dataname, temp)
            code.save$variable <- c(code.save$variable, list(c("\n", code)))
            ## save data
            updatePanel$datachanged <- updatePanel$datachanged + 1
            values$data.set <- as.data.frame(temp)

            values <- sample_if_cas(rvalues = values, d = values$data.set)

            code.save$name <- code.save$dataname
            values$data.name <- code.save$dataname
            if (class(values$data.set[, which(colnames(get.data.set()) %in%
              input$select_categorical1)]) == "factor") {
              values$data.set[, which(colnames(get.data.set()) %in%
                input$select_categorical1)] <-
                droplevels(get.data.set()[, which(colnames(get.data.set()) %in%
                  input$select_categorical1)])
            }

            updateSelectInput(
              session = session,
              inputId = "select_categorical1",
              choices = c("", get.categorical.column.names(get.data.set())),
              selected = 1
            )
            updateSelectInput(
              session = session,
              inputId = "levels1",
              choices = "",
              selected = 1
            )
          }
        }
      } else if (input$select_filter %in% "numeric condition") {
        if (!input$select_numeric1 %in% "" &
          !input$select_operation1 %in% "" &
          all(is.convertable.numeric(input$numeric_input1))) {
          indexes.keep <- 1:nrow(get.data.set())

          temp <- iNZightTools::filter_num(
            get.data.set(),
            var = input$select_numeric1,
            op = input$select_operation1,
            num = input$numeric_input1
          )
          if (!is.null(temp)) {
            ## save code
            code.save$dataname <- paste(code.save$name, "filtered", sep = ".")
            code <- code.data.modify(code.save$dataname, temp)
            code.save$variable <- c(code.save$variable, list(c("\n", code)))
            ## save data
            updatePanel$datachanged <- updatePanel$datachanged + 1
            values$data.set <- as.data.frame(temp)

            values <- sample_if_cas(rvalues = values, d = values$data.set)

            code.save$name <- code.save$dataname
            values$data.name <- code.save$dataname
          }
        }
      } else if (input$select_filter %in% "row indices") {
        if (all(is.convertable.numeric(strsplit(
          input$row_op_indexes, ",",
          fixed = T
        )[[1]]))) {
          indices <- as.numeric(strsplit(input$row_op_indexes, ",",
            fixed = T
          )[[1]])
          indices <- indices[which(indices %in% (1:nrow(get.data.set())))]
          temp <- iNZightTools::remove_rows(get.data.set(), indices)
          if (!is.null(temp)) {
            ## save code
            code.save$dataname <- paste(code.save$name, "filtered", sep = ".")
            code <- code.data.modify(code.save$dataname, temp)
            code.save$variable <- c(code.save$variable, list(c("\n", code)))
            ## save data
            updatePanel$datachanged <- updatePanel$datachanged + 1
            values$data.set <- as.data.frame(temp)

            values <- sample_if_cas(rvalues = values, d = values$data.set)

            code.save$name <- code.save$dataname
            values$data.name <- code.save$dataname
          }
        }
      } else if (input$select_filter %in% "randomly") {
        if (all(is.convertable.numeric(input$numeric_input2)) &&
          all(is.convertable.numeric(input$numeric_input3)) &&
          as.numeric(input$numeric_input2) <= nrow(get.data.set()) &&
          ((
            as.numeric(input$numeric_input2) * as.numeric(input$numeric_input3)
          ) <= nrow(get.data.set()))) {
          temp <- iNZightTools::random_sample(
            get.data.set(),
            n = as.numeric(input$numeric_input3),
            sample_size = as.numeric(input$numeric_input2)
          )
          if (!is.null(temp)) {
            ## save code
            code.save$dataname <- paste(code.save$name, "filtered", sep = ".")
            code <- code.data.modify(code.save$dataname, temp)
            code.save$variable <- c(code.save$variable, list(c("\n", code)))
            ## save data
            updatePanel$datachanged <- updatePanel$datachanged + 1
            values$data.set <- as.data.frame(temp)

            values <- sample_if_cas(rvalues = values, d = values$data.set)

            code.save$name <- code.save$dataname
            values$data.name <- code.save$dataname
          }
        }
      }
    }
  })
})


observe({
  input$select_categorical1
  isolate({
    if (!is.null(input$select_categorical1) && nchar(input$select_categorical1) > 0) {
      if (is.null(levels(get.data.set()[, which(colnames(get.data.set()) %in%
        input$select_categorical1)]))) {
        updateSelectInput(
          session = session,
          inputId = "levels1",
          choices = sort(unique(
            get.data.set()[
              ,
              which(colnames(get.data.set()) %in%
                input$select_categorical1)
            ]
          ))
        )
      }
      updateSelectInput(
        session = session,
        inputId = "levels1",
        choices = levels(get.data.set()[
          ,
          which(colnames(get.data.set()) %in%
            input$select_categorical1)
        ])
      )
    }
  })
})

output$message2 <- renderPrint({
  valid <- all(is.convertable.numeric(
    strsplit(input$row_op_indexes, ",", fixed = TRUE)[[1]]
  ))
  isolate({
    if (!valid) {
      cat("Please provide a comma seperated list of indices.")
    } else {
      cat("")
    }
  })
})

output$message1 <- renderPrint({
  input$select_numeric1
  input$select_operation1
  input$numeric_input1
  isolate({
    if (!all(is.convertable.numeric(input$numeric_input1))) {
      cat("Please provide a numeric variable.")
    } else {
      cat(
        input$select_numeric1,
        input$select_operation1,
        input$numeric_input1
      )
    }
  })
})

output$filter.data.summary <- renderPrint({
  get.data.set()
  input$filter_data_perform
  isolate({
    data.summary(get.data.set())
  })
})

output$filter.dataset <- renderUI({
  filter.data.panel(get.data.set())
})


observe({
  input$filter_data_perform
  isolate({
    updateSelectInput(session, "subs2", selected = "none")
    updateSelectInput(session, "subs1", selected = "none")
    updateSelectInput(session, "vari2", selected = "none")
    updateSelectInput(session, "vari1", selected = "none")
    if (!is.null(values$data.set) && !is.null(plot.par$locate)) {
      plot.par$locate <- NULL
    }
  })
})

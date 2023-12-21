separate_colns <- reactiveValues(
  n.colnames = 0
)

## when click "preview" button
observe({
  input$preview_separatecolumns_button
  isolate({
    col <- ""
    sep <- ""
    left <- "col1"
    right <- "col2"
    num <- 2
    if (!is.null(input$select_separate_mode) && input$select_separate_mode == "Separate a column into several columns") {
      check <- "Column"
      if (!is.null(input$select_column_to_separate) && input$select_column_to_separate != "") {
        col <- input$select_column_to_separate
        varx <- get.data.set()[[col]]
        if (!is.null(input$separator) && !grepl("^\\s*$", input$separator)) {
          sep <- input$separator
          data <- get.data.set() %>% dplyr::select(col, dplyr::everything())
          while (TRUE %in% grepl(sep, varx)) {
            data <- iNZightTools::separate(data, col, left, right, sep, check)
            col <- paste0("col", num)
            varx <- eval(parse(text = paste0("data$", col)))
            left <- paste0("col", num)
            right <- paste0("col", num + 1)
            num <- num + 1
          }

          data.set <- as.data.frame(data)
          sample.num <- ifelse(nrow(data.set) > 2000, 500, round(nrow(data.set) / 4))
          sample.row <- sort(sample(1:nrow(data.set), sample.num))
          output$previewseparatecolumns.table <- renderDT(
            {
              temp.d <- as.data.frame(data.set[sample.row, ])
              row.names(temp.d) <- 1:nrow(temp.d)
              colnames(temp.d) <- colnames(data.set)
              temp.d
            },
            options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA", scrollX = T)
          )
          numcol <- sum(grepl("^col[1-9]+$", names(temp)))
          separate_colns$n.colnames <- numcol

          output$separate_change_column_names <- renderUI({
            ret <- NULL
            isolate({
              if (numcol > 0) {
                print(numcol)
                ret <- vector(mode = "list", length = numcol + 1)
                ret[[1]] <- h5("Change column names (Click SEPARATE to apply)")
                for (i in 1:numcol) {
                  ret[[i + 1]] <- fixedRow(
                    column(3, h5(paste("Column", i, sep = " "))),
                    column(9, textInput(paste("changecolname", i, sep = ""),
                      label = NULL,
                      value = ""
                    ))
                  )
                }
              }
            })
            ret
          })
        }
      }
    } else if (!is.null(input$select_separate_mode) && input$select_separate_mode == "Separate a column to make several rows") {
      check <- "Row"
      if (!is.null(input$select_column_to_separate) && input$select_column_to_separate != "") {
        col <- input$select_column_to_separate
        if (!is.null(input$separator) && !grepl("^\\s*$", input$separator)) {
          sep <- input$separator
          data <- get.data.set() %>% dplyr::select(col, dplyr::everything())
          temp <- iNZightTools::separate(data, col, left, right, sep, check)

          data.set <- as.data.frame(temp)
          sample.num <- ifelse(nrow(data.set) > 2000, 500, round(nrow(data.set) / 4))
          sample.row <- sort(sample(1:nrow(data.set), sample.num))
          output$previewseparatecolumns.table <- renderDT(
            {
              temp.d <- as.data.frame(data.set[sample.row, ])
              row.names(temp.d) <- 1:nrow(temp.d)
              colnames(temp.d) <- colnames(data.set)
              temp.d
            },
            options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA", scrollX = T)
          )
        }
      }
    }
  })
})


## when click "separate" button
observe({
  input$separatecolumns_dataset_button
  isolate({
    col <- ""
    sep <- ""
    left <- "col1"
    right <- "col2"
    num <- 2
    if (!is.null(input$select_separate_mode) && input$select_separate_mode == "Separate a column into several columns") {
      check <- "Column"
      if (!is.null(input$select_column_to_separate) && input$select_column_to_separate != "") {
        col <- input$select_column_to_separate
        varx <- get.data.set()[[col]]
        if (!is.null(input$separator) && !grepl("^\\s*$", input$separator)) {
          sep <- input$separator
          data <- get.data.set() %>% dplyr::select(col, dplyr::everything())
          while (TRUE %in% grepl(sep, varx)) {
            data <- iNZightTools::separate(data, col, left, right, sep, check)
            col <- paste0("col", num)
            varx <- eval(parse(text = paste0("data$", col)))
            left <- paste0("col", num)
            right <- paste0("col", num + 1)
            num <- num + 1
          }
          temp <- data

          if (separate_colns$n.colnames > 0) {
            vec.index <- NULL
            vec.colnames <- NULL
            for (i in 1:separate_colns$n.colnames) {
              if (!is.null(eval(parse(text = paste0("input$changecolname", i)))) &&
                length(eval(parse(text = paste0("input$changecolname", i)))) > 0 &&
                !grepl("^\\s*$", eval(parse(text = paste0("input$changecolname", i))))) {
                vec.index <- c(vec.index, i)
                vec.colnames <- c(vec.colnames, eval(parse(text = paste0("input$changecolname", i))))
              }
            }
            if (length(vec.index) > 0) {
              colnames(temp)[vec.index] <- vec.colnames
            }
          }

          output$previewseparatecolumns.table <- renderDT(
            {
              NULL
            },
            options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA", scrollX = T)
          )

          output$separate_change_column_names <- renderUI({
            ret <- NULL
            isolate({

            })
            ret
          })
          updatePanel$datachanged <- updatePanel$datachanged + 1

          values$data.set <- as.data.frame(temp)
          values <- sample_if_cas(rvalues = values, d = values$data.set)
        }
      }
    } else if (!is.null(input$select_separate_mode) && input$select_separate_mode == "Separate a column to make several rows") {
      check <- "Row"
      if (!is.null(input$select_column_to_separate) && input$select_column_to_separate != "") {
        col <- input$select_column_to_separate
        if (!is.null(input$separator) && !grepl("^\\s*$", input$separator)) {
          sep <- input$separator
          data <- get.data.set() %>% dplyr::select(col, dplyr::everything())
          temp <- iNZightTools::separate(data, col, left, right, sep, check)
          output$previewseparatecolumns.table <- renderDT(
            {
              NULL
            },
            options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA", scrollX = T)
          )
          updatePanel$datachanged <- updatePanel$datachanged + 1

          values$data.set <- as.data.frame(temp)
          values <- sample_if_cas(rvalues = values, d = values$data.set)
        }
      }
    }
  })
})

output$separatecolumns.table <- renderDT(
  {
    values$data.sample
  },
  options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA", scrollX = T)
)

output$separate.columns <- renderUI({
  separate.columns.panel()
})

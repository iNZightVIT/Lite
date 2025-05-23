mergejoin <- reactiveValues(
  data.to.join = NULL,
  data.to.append = NULL
)

output$join_data_panel <- renderUI({
  get.data.set()

  ret <- NULL
  isolate({
    ret <- list(
      fileInput("import_to_join", label = "Import data", multiple = F),
      selectInput(
        inputId = "select_join_methods",
        label = "Select join methods",
        choices = c(
          "Inner Join", "Left Join", "Full Join",
          "Semi Join", "Anti Join"
        ),
        selected = "Left Join",
        selectize = FALSE,
        multiple = FALSE
      ),
      textInput("dup_original",
        label = "Duplicated cols: suffix for Original", value = "Orig"
      ),
      textInput("dup_new",
        label = "Duplicated cols: suffix for New",
        value = "New"
      ),
      uiOutput("match_columns_panel"),
      verbatimTextOutput("join_true_false"),
      fixedRow(
        column(3, actionButton("preview_join_button", "Preview",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(3, actionButton("join_data_button", "Join",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      )
    )
  })
  ret
})

output$match_columns_panel <- renderUI({
  get.data.set()
  input$import_to_join
  ret <- NULL
  isolate({
    if (!is.null(mergejoin$data.to.join)) {
      ret <- list(
        h5("Please specify columns to match on from two datasets"),
        fixedRow(
          column(6, selectInput(
            inputId = "select_matchcolumn1",
            label = NULL,
            choices = c("", colnames(get.data.set())),
            selectize = FALSE,
            multiple = FALSE
          )),
          column(6, selectInput(
            inputId = "select_matchcolumn2",
            label = NULL,
            choices = c("", colnames(mergejoin$data.to.join)),
            selectize = FALSE,
            multiple = FALSE
          ))
        )
      )
    }
  })
  ret
})

observeEvent(input$import_to_join, {
  if (file.exists(input$import_to_join[1, "datapath"])) {
    isolate({
      temp.join.data <- load.data(get.data.dir.imported(),
        fileID = input$import_to_join[1, "name"],
        path = input$import_to_join[1, "datapath"]
      )[[2]]

      if (!is.null(temp.join.data)) {
        mergejoin$data.to.join <- temp.join.data
      }

      output$previewimport.table <- renderDT(
        {
          temp.join.data
        },
        options = list(
          lengthMenu = c(5, 30, 50), pageLength = 5,
          columns.defaultContent = "NA", scrollX = T
        )
      )
    })
  }
})


observe({
  input$preview_join_button
  # isolate({
    join_method <- "left_join"
    left_col <- ""
    right_col <- ""
    left_name <- "Orig"
    right_name <- "New"
    d1 <- tryCatch(
      join_data(),
      error = function(e) {
        if (e$message ==
          "`by` required, because the data sources have no common variables") {
          a <- tibble::tibble()
          attr(a, "join_cols") <- ""
        }
      }
    )
    attr <- attr(d1, "join_cols")
    left_col <- as.character(attr)
    right_col <- left_col
    if (!is.null(mergejoin$data.to.join)) {
      if (!is.null(input$select_join_methods) &&
        length(input$select_join_methods) > 0) {
        browser()
        
        join_method <- switch(input$select_join_methods,
          "Inner Join" = "inner",
          "Left Join" = "left",
          "Full Join" = "full",
          "Semi Join" = "semi",
          "Anti Join" = "anti"
        )
      }
      if (!is.null(input$dup_original) && length(input$dup_original) > 0) {
        left_name <- input$dup_original
      }
      if (!is.null(input$dup_new) && length(input$dup_new) > 0) {
        right_name <- input$dup_new
      }
      if (!is.null(input$select_matchcolumn1) &&
        input$select_matchcolumn1 != "") {
        left_col <- input$select_matchcolumn1
      }
      if (!is.null(input$select_matchcolumn2) &&
        input$select_matchcolumn2 != "") {
        right_col <- input$select_matchcolumn2
      }
      data <- get.data.set()
      newdata <- mergejoin$data.to.join

      orig_type <- class(data[[left_col]])
      new_type <- class(newdata[[right_col]])

      if (orig_type == new_type | orig_type == "character" &
        new_type == "factor" | orig_type == "factor" &
        new_type == "character") {
            # TODO:
        
        temp.join <- iNZightTools::join_data(
          data_l = data, 
          data_r = newdata, 
          left_col, 
        #   right_col, 
          how = join_method,
          suffix_l = left_name, 
          suffix_r = right_name
        )

        data.set <- as.data.frame(temp.join)

        sample.num <- ifelse(nrow(data.set) > 2000, 500,
          round(nrow(data.set) / 4)
        )
        sample.row <- sort(sample(1:nrow(data.set), sample.num))
        output$previewjoin.table <- renderDT(
          {
            temp.d <- as.data.frame(data.set[sample.row, ])
            row.names(temp.d) <- 1:nrow(temp.d)
            colnames(temp.d) <- colnames(data.set)
            temp.d
          },
          options = list(
            lengthMenu = c(5, 30, 50), pageLength = 5,
            columns.defaultContent = "NA", scrollX = T
          )
        )
        output$join_true_false <- renderPrint({
        })
      } else {
        output$join_true_false <- renderPrint({
          cat("Selected columns are of different types")
        })
      }
    }
  # })
})



observe({
  input$join_data_button
  # isolate({
    join_method <- "left_join"
    left_col <- ""
    right_col <- ""
    left_name <- "Orig"
    right_name <- "New"
    d1 <- tryCatch(
      join_data(),
      error = function(e) {
        if (e$message ==
          "`by` required, because the data sources have no common variables") {
          a <- tibble::tibble()
          attr(a, "join_cols") <- ""
        }
      }
    )
    attr <- attr(d1, "join_cols")
    left_col <- as.character(attr)
    right_col <- left_col
    if (!is.null(mergejoin$data.to.join)) {
      if (!is.null(input$select_join_methods) &&
        length(input$select_join_methods) > 0) {
        browser()
        join_method <- switch(input$select_join_methods,
          "Inner Join" = "inner",
          "Left Join" = "left",
          "Full Join" = "full",
          "Semi Join" = "semi",
          "Anti Join" = "anti"
        )
      }
      if (!is.null(input$dup_original) && length(input$dup_original) > 0) {
        left_name <- input$dup_original
      }
      if (!is.null(input$dup_new) && length(input$dup_new) > 0) {
        right_name <- input$dup_new
      }
      if (!is.null(input$select_matchcolumn1) &&
        input$select_matchcolumn1 != "") {
        left_col <- input$select_matchcolumn1
      }
      if (!is.null(input$select_matchcolumn2) &&
        input$select_matchcolumn2 != "") {
        right_col <- input$select_matchcolumn2
      }
      data <- get.data.set()
      newdata <- mergejoin$data.to.join

      orig_type <- class(data[[left_col]])
      new_type <- class(newdata[[right_col]])

      if (orig_type == new_type | orig_type == "character" &
        new_type == "factor" | orig_type == "factor" &
        new_type == "character") {
        # temp.join <- iNZightTools::joindata(
        #   data, newdata, left_col,
        #   right_col, join_method, left_name, right_name
        # )
        # TODO:
        
        temp.join <- iNZightTools::join_data(
          data_l = data, 
          data_r = newdata, 
          left_col, 
        #   right_col, 
          how = join_method,
          suffix_l = left_name, 
          suffix_r = right_name
        )
        output$previewimport.table <- renderDT(
          {
            NULL
          },
          options = list(
            lengthMenu = c(5, 30, 50), pageLength = 5,
            columns.defaultContent = "NA", scrollX = T
          )
        )
        mergejoin$data.to.join <- NULL
        output$previewjoin.table <- renderDT(
          {
            NULL
          },
          options = list(
            lengthMenu = c(5, 30, 50), pageLength = 5,
            columns.defaultContent = "NA", scrollX = T
          )
        )
        ## save code
        code.save$dataname <- paste(code.save$name, "joined", sep = ".")
        code <- code.data.modify(code.save$dataname, temp)
        code.save$variable <- c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged <- updatePanel$datachanged + 1

        values$data.set <- as.data.frame(temp.join)
        values <- sample_if_cas(rvalues = values, d = values$data.set)

        code.save$name <- code.save$dataname
        values$data.name <- code.save$dataname
        output$join_true_false <- renderPrint({
        })
      } else {
        output$join_true_false <- renderPrint({
          cat("Selected columns are of different types")
        })
      }
    }
  # })
})






output$append_rows_panel <- renderUI({
  get.data.set()

  ret <- NULL
  isolate({
    ret <- list(
      fileInput("import_to_append", label = "Import data", multiple = F),
      checkboxInput(
        inputId = "attach_timestamp",
        label = strong(
          "Tick if you want to attach a timestamp to the appended rows"
        ),
        value = FALSE
      ),
      fixedRow(
        column(3, actionButton("preview_appendrows_button", "Preview",
          style =
            "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(3, actionButton("append_rows_button", "Append",
          style =
            "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      )
    )
  })
  ret
})


observeEvent(input$import_to_append, {
  if (file.exists(input$import_to_append[1, "datapath"])) {
    isolate({
      temp.append.data <- load.data(get.data.dir.imported(),
        fileID = input$import_to_append[1, "name"],
        path = input$import_to_append[1, "datapath"]
      )[[2]]

      if (!is.null(temp.append.data)) {
        mergejoin$data.to.append <- temp.append.data
      }
    })
  }
})


observe({
  input$preview_appendrows_button
  isolate({
    data <- FALSE
    if (!is.null(mergejoin$data.to.append)) {
      if (!is.null(input$attach_timestamp) &&
        length(input$attach_timestamp) > 0) {
        date <- input$attach_timestamp
      }
      data <- get.data.set()
      newdata <- mergejoin$data.to.append
      oldcols <- names(data)
      newcols <- names(newdata)
      common <- intersect(oldcols, newcols)
      if (length(common) != 0) {
        for (i in 1:length(common)) {
          colname <- common[i]
          if (class(data[[colname]]) != class(newdata[[colname]])) {
            colnames(data)[which(names(data) == colname)] <-
              paste0(colname, class(data[[colname]]))
            colnames(newdata)[which(names(newdata) == colname)] <-
              paste0(colname, class(newdata[[colname]]))
          }
        }
      }
      temp.append <- iNZightTools::append_rows(data, newdata, date)

      data.set <- as.data.frame(temp.append)

      sample.num <- ifelse(nrow(data.set) > 2000, 500,
        round(nrow(data.set) / 4)
      )
      sample.row <- sort(sample(1:nrow(data.set), sample.num))
      output$previewappend.table <- renderDT(
        {
          temp.d <- as.data.frame(data.set[sample.row, ])
          row.names(temp.d) <- 1:nrow(temp.d)
          colnames(temp.d) <- colnames(data.set)
          temp.d
        },
        options = list(
          lengthMenu = c(5, 30, 50), pageLength = 5,
          columns.defaultContent = "NA", scrollX = T
        )
      )
    }
  })
})

observe({
  input$append_rows_button
  isolate({
    data <- FALSE
    if (!is.null(mergejoin$data.to.append)) {
      if (!is.null(input$attach_timestamp) &&
        length(input$attach_timestamp) > 0) {
        date <- input$attach_timestamp
      }
      data <- get.data.set()
      newdata <- mergejoin$data.to.append
      oldcols <- names(data)
      newcols <- names(newdata)
      common <- intersect(oldcols, newcols)
      if (length(common) != 0) {
        for (i in 1:length(common)) {
          colname <- common[i]
          if (class(data[[colname]]) != class(newdata[[colname]])) {
            colnames(data)[which(names(data) == colname)] <-
              paste0(colname, class(data[[colname]]))
            colnames(newdata)[which(names(newdata) == colname)] <-
              paste0(colname, class(newdata[[colname]]))
          }
        }
      }
      temp.append <- iNZightTools::append_rows(data, newdata, date)
      mergejoin$data.to.append <- NULL
      output$previewappend.table <- renderDT(
        {
          NULL
        },
        options = list(
          lengthMenu = c(5, 30, 50), pageLength = 5,
          columns.defaultContent = "NA", scrollX = T
        )
      )
      ## save code
      code.save$dataname <- paste(code.save$name, "joined", sep = ".")
      code <- code.data.modify(code.save$dataname, temp)
      code.save$variable <- c(code.save$variable, list(c("\n", code)))
      ## save data
      updatePanel$datachanged <- updatePanel$datachanged + 1
      values$data.set <- as.data.frame(temp.append)

      values <- sample_if_cas(rvalues = values, d = values$data.set)

      code.save$name <- code.save$dataname
      values$data.name <- code.save$dataname
    }
  })
})

output$append.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)

output$join.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)

output$mergejoin.datasets <- renderUI({
  mergejoin.datasets.panel()
})

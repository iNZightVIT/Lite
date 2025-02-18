## update input$name_unite_columns
observe({
  input$select_unite_columns
  isolate({
    if (!is.null(input$select_unite_columns) &&
      length(input$select_unite_columns) > 1) {
      temp_select_unite_columns <- input$select_unite_columns
      name_select_unite_columns <- temp_select_unite_columns[1]
      for (i in 2:length(temp_select_unite_columns)) {
        n <- temp_select_unite_columns[i]
        name_select_unite_columns <-
          paste(name_select_unite_columns, n, sep = ".")
      }
      updateTextInput(session, "name_unite_columns",
        value = name_select_unite_columns
      )
    }
  })
})


## when click "preview" button
observe({
  input$preview_unitecolumns_button
  isolate({
    name <- ""
    sep <- "_"
    if (!is.null(input$select_unite_columns) &&
      length(input$select_unite_columns) > 1) {
      col <- input$select_unite_columns
      name <- ifelse(
        input$name_unite_columns == "", "newcol",
        input$name_unite_columns
      )
      sep <- input$sep_unite_columns
      temp <- iNZightTools::combine_vars(
        data = get.data.set(),
        vars = col,
        sep = sep,
        name = name
      )

      data.set <- as.data.frame(temp)
      sample.num <- ifelse(nrow(data.set) > 2000, 500,
        round(nrow(data.set) / 4)
      )
      sample.row <- sort(sample(1:nrow(data.set), sample.num))
      output$previewunitecolumns.table <- renderDT(
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


## when click "unite" button
observe({
  input$unitecolumns_dataset_button
  isolate({
    name <- ""
    sep <- "_"
    
    if (!is.null(input$select_unite_columns) &&
      length(input$select_unite_columns) > 1) {
      col <- input$select_unite_columns
      name <- ifelse(input$name_unite_columns == "", "newcol",
        input$name_unite_columns
      )
      sep <- input$sep_unite_columns
      temp <- iNZightTools::combine_vars(
        data = get.data.set(),
        vars = col,
        sep = sep,
        name = name
      )
      output$previewunitecolumns.table <- renderDT(
        {
          NULL
        },
        options = list(
          lengthMenu = c(5, 30, 50), pageLength = 5,
          columns.defaultContent = "NA", scrollX = T
        )
      )
      ## save code
      code.save$dataname <- paste(code.save$name, "united", sep = ".")
      code <- code.data.modify(code.save$dataname, temp)
      code.save$variable <- c(code.save$variable, list(c("\n", code)))
      ## save data
      updatePanel$datachanged <- updatePanel$datachanged + 1

      values$data.set <- as.data.frame(temp)
      values <- sample_if_cas(rvalues = values, d = values$data.set)

      code.save$name <- code.save$dataname
      values$data.name <- code.save$dataname
    }
  })
})

output$unitecolumns.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)

output$unite.columns <- renderUI({
  unite.columns.panel()
})

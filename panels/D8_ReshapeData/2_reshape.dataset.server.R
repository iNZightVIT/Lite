
output$reshape_main_panel = renderUI({
  get.data.set()
  input$select_reshape_mode
  ret = NULL
  isolate({
    if(!is.null(input$select_reshape_mode) && input$select_reshape_mode == "Wide to long") {
      ret = list(selectInput(inputId = "select_colname",
                             label = "Select column(s) to gather together",
                             choices = colnames(get.data.set()),
                             selectize = FALSE,
                             multiple = T,
                             size = 10),
                 textInput("new_colname",
                           label = "Name the new column containing the old column names",
                           value = "key"),
                 textInput("new_colvalue",
                           label = "Name the new column containing the old column values",
                           value = "value"))
    }
    else if(!is.null(input$select_reshape_mode) && input$select_reshape_mode == "Long to wide") {
      ret = list(selectInput(inputId = "select_col1",
                             label = "Select the column to spread out to multiple columns",
                             choices = c("", colnames(get.data.set())),
                             selectize = FALSE),
                 selectInput(inputId = "select_col2",
                             label = "Select the column with the values to be put in these column",
                             choices = c("", colnames(get.data.set())),
                             selectize = FALSE))
    }
  })
  ret
})


observe({
  input$preview_dataset_button
  isolate({
    colname = ""; key = "key"; value = "value"; col1 = ""; col2 = ""
    if(!is.null(input$select_reshape_mode) && input$select_reshape_mode == "Wide to long") {
      if(!is.null(input$select_colname) && length(input$select_colname) > 0) {
        colname = input$select_colname
        key = ifelse(length(input$new_colname) == 0, "key", input$new_colname)
        value = ifelse(length(input$new_value) == 0, "value", input$new_value)
        temp = iNZightTools::reshape_data(get.data.set(), col1, col2, colname, key, value, check = "wide")
        
        data.set = as.data.frame(temp)
        sample.num = ifelse(nrow(data.set) > 2000, 500, round(nrow(data.set)/4))
        sample.row = sort(sample(1:nrow(data.set), sample.num))
        output$preview.reshape.table = renderDT({
          temp.d = as.data.frame(data.set[sample.row,])
          row.names(temp.d) = 1:nrow(temp.d)
          colnames(temp.d) = colnames(data.set)
          temp.d
        },options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))
      }
    }
    else if(!is.null(input$select_reshape_mode) && input$select_reshape_mode == "Long to wide") {
      if(!is.null(input$select_col1) && input$select_col1 != "" &&
         !is.null(input$select_col2) && input$select_col2 != "") {
        col1 = input$select_col1
        col2 = input$select_col2
        temp = iNZightTools::reshape_data(get.data.set(), col1, col2, colname, key, value, check = "long")
        
        
        data.set = as.data.frame(temp)
        sample.num = ifelse(nrow(data.set) > 2000, 500, round(nrow(data.set)/4))
        sample.row = sort(sample(1:nrow(data.set), sample.num))

        output$preview.reshape.table = renderDT({
          temp.d = as.data.frame(data.set[sample.row,])
          row.names(temp.d) = 1:nrow(temp.d)
          colnames(temp.d) = colnames(data.set)
          temp.d
        },options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))
      }
    }
  })
})


observe({
  input$reshape_dataset_button
  isolate({
    colname = ""; key = "key"; value = "value"; col1 = ""; col2 = ""
    if(!is.null(input$select_reshape_mode) && input$select_reshape_mode == "Wide to long") {
      if(!is.null(input$select_colname) && length(input$select_colname) > 0) {
        colname = input$select_colname
        key = ifelse(length(input$new_colname) == 0, "key", input$new_colname)
        value = ifelse(length(input$new_value) == 0, "value", input$new_value)
        temp = iNZightTools::reshape_data(get.data.set(), col1, col2, colname, key, value, check = "wide")
        output$preview.reshape.table = renderDT({
          NULL
        },options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))
        ## save code 
        code.save$dataname = paste(code.save$name, "reshaped", sep = ".")
        code = code.data.modify(code.save$dataname, temp)
        code.save$variable = c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged = updatePanel$datachanged+1
        
        values$data.set = as.data.frame(temp)
        values = sample_if_lite2(rvalues = values, d = values$data.set)
        
        code.save$name = code.save$dataname
        values$data.name = code.save$dataname
      }
    }
    else if(!is.null(input$select_reshape_mode) && input$select_reshape_mode == "Long to wide") {
      if(!is.null(input$select_col1) && input$select_col1 != "" &&
         !is.null(input$select_col2) && input$select_col2 != "") {
        col1 = input$select_col1
        col2 = input$select_col2
        temp = iNZightTools::reshape_data(get.data.set(), col1, col2, colname, key, value, check = "long")
        output$preview.reshape.table = renderDT({
          NULL
        },options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))
        ## save code 
        code.save$dataname = paste(code.save$name, "reshaped", sep = ".")
        code = code.data.modify(code.save$dataname, temp)
        code.save$variable = c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged = updatePanel$datachanged+1
        
        
        values$data.set = as.data.frame(temp)
        values = sample_if_lite2(rvalues = values, d = values$data.set)
        
        code.save$name = code.save$dataname
        values$data.name = code.save$dataname
      }
    }
  })
})

# TODO: check
output$reshape.table = renderDT({
  values$data.sample
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))

output$reshape.dataset = renderUI({
  reshape.dataset.panel()
})

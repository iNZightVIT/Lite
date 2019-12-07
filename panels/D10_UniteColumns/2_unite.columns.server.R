

## update input$name_unite_columns
observe({
  input$select_unite_columns
  isolate({
    if(!is.null(input$select_unite_columns) && length(input$select_unite_columns) > 1) {
      temp_select_unite_columns = input$select_unite_columns
      name_select_unite_columns = temp_select_unite_columns[1] 
      for (i in 2:length(temp_select_unite_columns)) {
        n = temp_select_unite_columns[i]
        name_select_unite_columns = paste(name_select_unite_columns, n, sep = ".")
      }
      updateTextInput(session, "name_unite_columns", value = name_select_unite_columns)
    }
  })
})


## when click "preview" button
observe({
  input$preview_unitecolumns_button
  isolate({
    name = ""; sep = "_"
    if(!is.null(input$select_unite_columns) && length(input$select_unite_columns) > 1) {
      col = input$select_unite_columns
      name = ifelse(input$name_unite_columns == "", "newcol", input$name_unite_columns)
      sep = input$sep_unite_columns
      temp = iNZightTools::unite(get.data.set(), name, col, sep)
      output$previewunitecolumns.table = renderDataTable({
        temp
      },options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))
    }
  })
})


## when click "unite" button
observe({
  input$unitecolumns_dataset_button
  isolate({
    name = ""; sep = "_"
    if(!is.null(input$select_unite_columns) && length(input$select_unite_columns) > 1) {
      col = input$select_unite_columns
      name = ifelse(input$name_unite_columns == "", "newcol", input$name_unite_columns)
      sep = input$sep_unite_columns
      temp = iNZightTools::unite(get.data.set(), name, col, sep)
      output$previewunitecolumns.table = renderDataTable({
        NULL
      },options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))
      ## save code 
      code.save$dataname = paste(code.save$name, "united", sep = ".")
      code = code.data.modify(code.save$dataname, temp)
      code.save$variable = c(code.save$variable, list(c("\n", code)))
      ## save data
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = temp
      code.save$name = code.save$dataname
      values$data.name = code.save$dataname
    }
  })
})

output$unitecolumns.table = renderDataTable({
  get.data.set()
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))

output$unite.columns = renderUI({
  unite.columns.panel()
})

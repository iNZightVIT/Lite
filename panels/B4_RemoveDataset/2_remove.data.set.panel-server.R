##  Data -> remove data (Remove an imported data set)
output$remove.data.panel <- renderUI({
  input$selector
  input$remove_set
  input$import_set
  isolate({
    remove.data.panel(get.data.dir.imported())
  })
})

output$removetable <- renderDataTable({
  if(!is.null(input$Importedremove)){
    load.data(get.data.dir.imported(),input$Importedremove)[[2]]
  } else {
    NULL
  }
}, options =
  list(lengthMenu = c(5, 30, 50), pageLength = 5,
       columns.defaultContent = "NA", scrollX = TRUE))
##  Manipulate variables -> Reshape dataset: Convert the dataset 
#into 2 columns with all variables stacked ont top of each other.

output$reshape.data = renderUI({
  reshape.data.panel(get.data.set())
})

output$reshape.data.table = renderDataTable({
  get.reshape.data(get.data.set())
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))

observe({
  input$reshape.data.submit
  isolate({
    if(!is.null(input$reshape.data.submit)&&input$reshape.data.submit>0){
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = get.reshape.data(get.data.set())
    }
  })
})
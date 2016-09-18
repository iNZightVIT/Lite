


observe({
  input$alphabetise.var.button
  
  isolate({
    if((!is.null(input$alphabetise.var.button) && input$alphabetise.var.button > 0)) {
      temp = get.data.set()
      col.names = colnames(temp)
      order.col.names = order(col.names)
      order.col.names.dec = order(col.names, decreasing = TRUE)
      if(input$select.alphabetical.order == "A(a) To Z(z)")
        temp = temp[, order.col.names]
      if(input$select.alphabetical.order == "Z(z) To A(a)")        
        temp = temp[, order.col.names.dec]
      if(!is.null(temp)){
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = temp
      }     
    }
  })
})

output$alphabetise.variables = renderUI({
  alphabetise.variables(get.data.set())
})


output$alphabetise.variables.table = renderDataTable({
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, 
               columns.defaultContent="NA",scrollX=T))








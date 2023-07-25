


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
        values$data.set = as.data.frame(temp, col.names = col.names(values$data.set))
        values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set)/4))
        values$sample.row = sort(sample(1:nrow(values$data.set), values$sample.num))
        values$data.sample = as.data.frame(values$data.set[values$sample.row,])
        row.names(values$data.sample) = 1:nrow(values$data.sample)
        colnames(values$data.sample) = colnames(values$data.set)
      }     
    }
  })
})




output$alphabetise.variables = renderUI({
  get.data.set()
  isolate({
    alphabetise.variables(get.data.set())
  })
})


output$alphabetise.variables.table = renderDT({
  values$data.sample
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, 
               columns.defaultContent="NA",scrollX=T))



output$alphabetise.var.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
})




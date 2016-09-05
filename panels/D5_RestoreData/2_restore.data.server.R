##  Row operations (Perform row operations) --> Restore data

observe({
  input$restore_data_button
  isolate({
    if(!is.null(input$restore_data_button)&&input$restore_data_button>0){
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = get.data.restore()
    }
  })
})

output$data.restore.table = renderDataTable({
  input$restore_data_button
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

output$restore.data = renderUI({
  restore.data.panel(get.data.set())
})



observe({  
  input$restore_data_button 
  isolate({
    updateSelectInput(session, "subs2", selected = "none")
    updateSelectInput(session, "subs1", selected = "none")
    updateSelectInput(session, "vari2", selected = "none")
    updateSelectInput(session, "vari1", selected = "none")
  })  
})

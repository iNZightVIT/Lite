##  Row operations (Perform row operations) --> Restore data

observe({
  if(input$selector == "Restore data"){
  isolate({
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = get.data.restore()
      values$data.name = get.name.restore()
      ## code history
      code = paste0(values$data.name, "_", input$restore_data_button, " <- ", gsub("_ex", "", values$data.name))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
      code.save$name = paste0(values$data.name, "_", input$restore_data_button)
  })
  }
})

output$data.restore.table = renderDataTable({
  if(input$selector == "Restore data"){
  get.data.set()
  }
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

output$restore.data = renderUI({
  restore.data.panel(get.data.set())
})



observe({  
  if(input$selector == "Restore data"){
  isolate({
    updateSelectInput(session, "subs2", selected = "none")
    updateSelectInput(session, "subs1", selected = "none")
    updateSelectInput(session, "vari2", selected = "none")
    updateSelectInput(session, "vari1", selected = names(get.data.set())[1])
  })  
  }
})

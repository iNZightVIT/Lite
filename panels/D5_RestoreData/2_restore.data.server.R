##  Row operations (Perform row operations) --> Restore data

observe({
  if(input$selector == "Restore data"){
  isolate({
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = get.data.restore()
      values$data.name = get.name.restore()
      
      if(LITE2) {
        values$sample.num = ifelse(nrow(get.data.restore()) > 2000, 500, round(nrow(get.data.restore())/4))
        values$sample.row = sort(sample(1:nrow(get.data.restore()), values$sample.num))
        values$data.sample = as.data.frame(get.data.restore()[values$sample.row,])
        row.names(values$data.sample) = 1:nrow(values$data.sample)
        colnames(values$data.sample) = colnames(values$data.set)
      }
      ## code history
      code = paste0(values$data.name, "_", input$restore_data_button, " <- ", gsub("_ex", "", values$data.name))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
      code.save$name = paste0(values$data.name, "_", input$restore_data_button)
  })
  }
})

# TODO: check
output$data.restore.table = renderDT({
  if(input$selector == "Restore data"){
    values$data.sample
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

# TODO: check
output$data.restore.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
})




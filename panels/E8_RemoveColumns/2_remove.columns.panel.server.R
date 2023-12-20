

observe({
  input$rem_column
  if(!is.null(get.data.set())&&!is.null(input$rem_column)&&input$rem_column>0){
    isolate({
      if(length(which(colnames(get.data.set())%in%input$select.remove.column))>0){
        #temp = as.data.frame(get.data.set()[,-which(colnames(get.data.set())%in%input$select.remove.column)])
        temp = iNZightTools::deleteVars(get.data.set(), input$select.remove.column)
        if(!is.null(temp)){
          updatePanel$datachanged = updatePanel$datachanged+1
          values$data.set = as.data.frame(temp, col.names = colnames(values$data.set))
          values = sample_if_cas(rvalues = values, d = temp, new_sample = FALSE)
          ## code history
          code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
          code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
          if(ncol(get.data.set())==0){
            values$data.set = NULL
            values$data.sample = NULL
            updateSelectInput(session, inputId="select.remove.column",choices=c(""),selected="")
          }else{
            updateSelectInput(session, inputId="select.remove.column", choices=colnames(get.data.set()),selected="")
          }
        }
      }
    })
  }
})

output$rem.col.table = renderDT({
  get.data.set.display()
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))

output$remove.columns = renderUI({
  remove.columns.panel(get.data.set())
})

output$rem.col.data.sample.info <- renderText({
  sample_info_cas()
}) 




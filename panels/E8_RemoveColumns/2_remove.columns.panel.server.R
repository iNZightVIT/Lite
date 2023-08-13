

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
          if(LITE2) {
            values$data.sample = as.data.frame(temp[values$sample.row,])
            colnames(values$data.sample) = colnames(values$data.set)
            row.names(values$data.sample) = 1:nrow(values$data.sample)
          }
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

# TODO: check
output$rem.col.table = renderDT({
  values$data.sample
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))

output$remove.columns = renderUI({
  remove.columns.panel(get.data.set())
})

# TODO: check
output$rem.col.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name()) && !is.null(values$data.sample)) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
}) 




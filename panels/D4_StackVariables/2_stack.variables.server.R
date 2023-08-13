##  Row operations (Perform row operations) --> Stack variables

observe({
  input$stack_vars
  isolate({
    if (!is.null(input$stack_vars) && input$stack_vars > 0 &&
        !is.null(input$stack_vars_column)) {
      temp = iNZightTools::stackVars(get.data.set(), input$stack_vars_column)
      if (!is.null(temp)) {
        ## save code
        code.save$dataname = paste(code.save$name, "stacked", sep = ".")
        code = code.data.modify(code.save$dataname, temp)
        code.save$variable = c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged = updatePanel$datachanged + 1
        
        values$data.set = as.data.frame(temp)
        
        if(LITE2) {
          values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set)/4))
          values$sample.row = sample(1:nrow(values$data.set), values$sample.num)
          values$data.sample = as.data.frame(values$data.set[values$sample.row,])
          row.names(values$data.sample) = 1:nrow(values$data.sample)
          colnames(values$data.sample) = colnames(values$data.set)
        }
        
        code.save$name = code.save$dataname
        values$data.name = code.save$dataname
        ## update
        updateSelectInput(
          session,
          "stack_vars_column",
          choices = get.numeric.column.names(get.data.set()),
          selected = 0
        )
      }
    }
  })
})

#observe({
#  input$stack_vars_which
#  isolate({
#    if(!is.null(input$stack_vars_which)&&!""%in%input$stack_vars_which){
#      if("categorical"%in%input$stack_vars_which){
#        updateSelectInput(session,inputId="stack_vars_column",
#                          choices=get.categorical.column.names(get.data.set()),
#                          selected=1)
#      }else{
#        updateSelectInput(session,inputId="stack_vars_column",
#                          choices=get.numeric.column.names(get.data.set()),
#                          selected=1)
#      }
#    }
#  })
#})

# TODO: check
output$stack.table = renderDT({
  values$data.sample
}, options = list(
  lengthMenu = c(5, 30, 50),
  pageLength = 5,
  columns.defaultContent = "NA",
  scrollX = T
))

output$stack.variables = renderUI({
  stack.variables.panel()
})
# TODO: check
output$stack.table.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
})


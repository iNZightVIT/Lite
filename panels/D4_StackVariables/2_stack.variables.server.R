##  Row operations (Perform row operations) --> Stack variables

observe({
  input$stack_vars
  isolate({
    if (!is.null(input$stack_vars) && input$stack_vars > 0 &&
      !is.null(input$stack_vars_column)) {
      temp <- iNZightTools::stackVars(get.data.set(), input$stack_vars_column)
      if (!is.null(temp)) {
        ## save code
        code.save$dataname <- paste(code.save$name, "stacked", sep = ".")
        code <- code.data.modify(code.save$dataname, temp)
        code.save$variable <- c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged <- updatePanel$datachanged + 1

        values$data.set <- as.data.frame(temp)

        values <- sample_if_cas(rvalues = values, d = values$data.set)

        code.save$name <- code.save$dataname
        values$data.name <- code.save$dataname
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

# observe({
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
# })

output$stack.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50),
    pageLength = 5,
    columns.defaultContent = "NA",
    scrollX = T
  )
)

output$stack.variables <- renderUI({
  stack.variables.panel()
})

output$stack.table.data.sample.info <- renderText({
  sample_info_cas()
})

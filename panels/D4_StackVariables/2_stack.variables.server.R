##  Row operations (Perform row operations) --> Stack variables

observe({
  input$stack_vars
  isolate({
    if(!is.null(input$stack_vars)&&input$stack_vars>0&&
         !is.null(input$stack_vars_column)){
      temp = stack.variables.perform(input$stack_vars_column,get.data.set())
      if(!is.null(temp)){
        values$data.set = temp
        updateSelectInput(session,"stack_vars_which",selected=0)
        updateSelectInput(session,inputId="stack_vars_column",
                          choices=get.categorical.column.names(get.data.set()),
                          selected=0)
      }
    }
  })
})

observe({
  input$stack_vars_which
  isolate({
    if(!is.null(input$stack_vars_which)&&!""%in%input$stack_vars_which){
      if("categorical"%in%input$stack_vars_which){
        updateSelectInput(session,inputId="stack_vars_column",
                          choices=get.categorical.column.names(get.data.set()),
                          selected=1)
      }else{
        updateSelectInput(session,inputId="stack_vars_column",
                          choices=get.numeric.column.names(get.data.set()),
                          selected=1)
      }
    }
  })
})

output$stack.table = renderDataTable({
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

output$stack.variables = renderUI({
  stack.variables.panel(get.data.set())
})

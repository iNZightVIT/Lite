##  Row operations (Perform row operations) --> Aggregate data

observe({
  input$aggregate_vars
  isolate({
    if(!is.null(input$aggregate_vars)&&input$aggregate_vars>0){
      vars = input$aggros
      rem = which(vars%in%"")
      if(length(rem)>0){
        vars = vars[-rem]
      }
      methods = input$aggregate.method
      rem  = which(methods%in%"")
      if(length(rem)>0){
        methods = methods[-rem]
      }
      if(length(vars)>0&length(methods)>0&!is.null(get.data.set())){
        temp = aggregate.data(aggregate.over=unique(vars),methods=methods,dafr=get.data.set())
        if(!is.null(temp)){
          values$data.set = temp          
          updateSelectInput(session,"aggros",selected=0,choices=get.categorical.column.names(get.data.set()))
          updateSelectInput(session,"aggregate.method",selected=0)
        }
      }
    }
  })
})

output$aggregate.variable = renderUI({
  aggregate.variable(get.data.set())
})

output$aggregate.table = renderDataTable({
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, 
               columns.defaultContent="NA",scrollX=T))
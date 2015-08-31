##  Row operations (Perform row operations) --> Sort data by variables

observe({
  input$sort_vars
  isolate({
    if(!is.null(input$sort_vars)&&input$sort_vars>0){
      indexes1= grep("^sort[0-9]+$",names(input))
      vars = unlist(lapply(indexes1,function(i,nams){
        input[[nams[i]]]
      },names(input)))
      indexes2 = grep("^increasing[0-9]+$",names(input))
      sort.type = unlist(lapply(indexes2,function(i,nams){
        input[[nams[i]]]
      },names(input)))
      if(anyDuplicated(vars)){
        dups = which(duplicated(vars))
        vars = vars[-dups]
        sort.type =sort.type[-dups]
      }
      if(""%in%vars){
        empties = which(vars%in%"")
        vars = vars[-empties]
        sort.type =sort.type[-empties]
      }
      temp = sort.data(vars,sort.type,get.data.set())
      if(!is.null(temp)){
        values$data.set = temp
      }
    }
  })
})

output$sort.table = renderDataTable({
  input$sort_vars
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

output$sort.variables = renderUI({
  get.data.set()
  isolate({
    sort.variables(get.data.set())
  })
})

output$num.select = renderUI({
  input$num_columns_sort
  isolate({
    num.select.panel(input$num_columns_sort,get.data.set())
  })
})

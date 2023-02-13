##  Row operations (Perform row operations) --> Aggregate data

output$aggros1_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "aggros1",
      label = NULL,
      choices = c("", get.categorical.column.names(get.data.set())),
      selected = input$aggros1,
      selectize = F
    )
    
  })
})

output$aggros2_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "aggros2",
      label = NULL,
      choices = c("", get.categorical.column.names(get.data.set())),
      selected = input$aggros2,
      selectize = F
    )
    
  })
})

output$aggros3_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "aggros3",
      label = NULL,
      choices = c("", get.categorical.column.names(get.data.set())),
      selected = input$aggros3,
      selectize = F
    )
    
  })
})



observe({
  input$aggregate_vars
  isolate({
    if (!is.null(input$aggregate_vars) && input$aggregate_vars > 0) {
      #      vars = input$aggros
      #      rem = which(vars %in% "")
      #      if(length(rem)>0){
      #        vars = vars[-rem]
      #      }
      vars = NULL
      if (!is.null(input$aggros1) && input$aggros1 != "")
        vars = c(vars, input$aggros1)
      if (!is.null(input$aggros2) && input$aggros2 != "")
        vars = c(vars, input$aggros2)
      if (!is.null(input$aggros3) && input$aggros3 != "")
        vars = c(vars, input$aggros3)
      
      if (!is.null(input$aggregate.method) &&
          length(input$aggregate.method) > 0) {
        methods = input$aggregate.method
        methods = tolower(methods)
        if ("iqr" %in% methods)
          methods[methods == "iqr"] = "IQR"
      }
      
      #      rem  = which(methods %in% "")
      #      if(length(rem) > 0){
      #        methods = methods[-rem]
      #      }
      #      print(vars)
      #      print(methods)
      #      print(get.data.set())
      if (length(vars) > 0 &
          length(methods) > 0 & !is.null(get.data.set())) {
        temp = iNZightTools::aggregateData(get.data.set(),
                                           vars = unique(vars),
                                           summaries = methods)
        if (!is.null(temp)) {
          ## save code
          code.save$dataname = paste(code.save$name, "aggregated", sep = ".")
          code = code.data.modify(code.save$dataname, temp)
          code.save$variable = c(code.save$variable, list(c("\n", code)))
          ## save data
          updatePanel$datachanged = updatePanel$datachanged + 1
          values$data.set = as.data.frame(temp)
          
          values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set)/4))
          values$sample.row = sample(1:nrow(values$data.set), values$sample.num)
          values$data.sample = as.data.frame(values$data.set[values$sample.row,])
          row.names(values$data.sample) = 1:nrow(values$data.sample)
          colnames(values$data.sample) = colnames(values$data.set)
          code.save$name = code.save$dataname
          values$data.name = code.save$dataname
          updateSelectInput(
            session,
            "aggros1",
            selected = 0,
            choices = get.categorical.column.names(get.data.set())
          )
          updateSelectInput(
            session,
            "aggros2",
            selected = 0,
            choices = get.categorical.column.names(get.data.set())
          )
          updateSelectInput(
            session,
            "aggros3",
            selected = 0,
            choices = get.categorical.column.names(get.data.set())
          )
          updateSelectInput(session, "aggregate.method", selected = 0)
        }
      }
    }
  })
})

output$aggregate.variable = renderUI({
  aggregate.variable.panel()
})

output$aggregate.table = renderDT({
  values$data.sample
}, options = list(
  lengthMenu = c(5, 30, 50),
  pageLength = 5,
  columns.defaultContent = "NA",
  scrollX = T
))


output$aggregate.table.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
})

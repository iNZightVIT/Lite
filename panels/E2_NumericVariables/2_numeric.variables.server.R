##  Manipulate variables -> Numeric variables

output$numeric.variables = renderUI({
  updatePanel$doit
  isolate({
    numeric.variables.panel(get.data.set())  
  })
})

##  Manipulate variables -> Numeric variables -> transform variables (Perform column transformations)

observe({
  input$transform
  isolate({
    if(!is.null(input$transform)&&input$transform>0){
      temp = iNZightTools::transformVar(get.data.set(),
                                        input$select.columns.transform,
                                        input$select.transform)
      #         print(head(temp))
      if(!is.null(temp)){
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = temp
        ## code history
        code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
        code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
        values$transform.text = "The transformation of the columns was succesful!"
      }
    }
  })
})

output$table_part <- renderDataTable({
  if(!is.null(input$select.transform) && !is.null(input$select.columns.transform)){
    transform.tempTable(get.data.set(),input$select.transform,input$select.columns.transform)
  }
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

output$status = renderText({
  get.transform.text()
})

output$transform.columns.side = renderUI({
  get.data.set()
  isolate({
    get.transform.sidebar(get.data.set())
  })
})

output$transform.columns.main = renderUI({
  get.data.set()
  get.transform.main()
})


output$standardise.variables.side = renderUI({
  get.data.set()
  
  ret = NULL
  isolate({
    ret = list(selectInput(inputId="standardise.variables.select",
                           label="Choose variables you want to standardise",
                           choices=get.numeric.column.names(get.data.set()),
                           multiple=T,
                           selectize=F,
                           size=7),
               
               actionButton("standardise.variables.button","Standardise",
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  })
  ret
})


output$standardise.variables.table = renderDataTable({
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))


observe({
  input$standardise.variables.button
  
  isolate({
    if(!is.null(input$standardise.variables.select) && length(input$standardise.variables.select) > 0) {
      varnames = input$standardise.variables.select
      names = paste0(varnames, ".std")
      data = iNZightTools::standardizeVars(get.data.set(), varnames, names)
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = data
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})




output$convert.to.cate.side = renderUI({
  get.data.set()
  
  ret = NULL
  isolate({
    ret = list(selectInput(inputId="convert.to.cate.select",
                           label="Choose variables you want to convert",
                           choices=get.numeric.column.names(get.data.set()),
                           multiple=T,
                           selectize=F,
                           size=7),
               
               actionButton("convert.to.cate.button","Convert",
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  })
  ret
})



output$convert.to.cate.table = renderDataTable({
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))



observe({
  input$convert.to.cate.button
  
  isolate({
    if(!is.null(input$convert.to.cate.select) && length(input$convert.to.cate.select) > 0) {
      vars = input$convert.to.cate.select
      varnames = paste(vars, "cat", sep = ".")
      data = iNZightTools::convertToCat(get.data.set(), vars, varnames)
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = data
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})




## Manipulate variables -> Numeric variables -> Form Class interval

output$form.class.interval.side = renderUI({
  get.form.class.interval.side(get.data.set())
})

output$form.class.interval.main = renderUI({
  get.data.set()
  get.form.class.interval.main()
})

output$form.class.interval.name = renderUI({
  textInput(inputId="form.class.interval.column.name",
            label="New variable",
            value = paste(input$form.class.interval.column.select, "f", sep = "."))
})




## Manipulate variables -> Numeric variables -> Rank numeric

output$rank.numeric.table = renderDataTable({
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))

output$rank.numeric.side = renderUI({
  rank.numeric.sidebar(get.data.set())
})

output$rank.numeric.main = renderUI({
  get.data.set()
  rank.numeric.main()
})

observe({
  input$rank.numeric.submit
  isolate({
    if(!is.null(input$rank.numeric.submit)&&
       input$rank.numeric.submit>0&&
       !is.null(input$rank.numeric.select.column)){
      data = iNZightTools::rankVars(get.data.set(), input$rank.numeric.select.column)
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = data
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})
##  Manipulate variables -> Missing to categorical



newvariablesadded_reactives = reactiveValues(
  success = F
)


output$missing.categorical = renderUI({
  missing.categorical.panel(get.data.set())
})

output$missing.categorical.table = renderDT({
  input$missing.categorical.column.select
  dafr = get.data.set()
  isolate({
    if(!is.null(dafr)&&
         !is.null(input$missing.categorical.column.select)&&
         all(input$missing.categorical.column.select%in%colnames(dafr))){
      combos = get.combinations(dafr[,input$missing.categorical.column.select])
      combos
    }
  })
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))

observe({
  input$missing.categorical.submit
  isolate({
    if(!is.null(input$missing.categorical.submit)&&input$missing.categorical.submit>0){
      temp = iNZightTools::missingToCat(get.data.set(), vars = input$missing.categorical.column.select)
      if(!is.null(temp)){
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = temp
        values = sample_if_cas(rvalues = values, d = temp, new_sample = FALSE)
        newvariablesadded_reactives$success = T
        ## code history
        code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
        code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
      }
    }
  })
})


  


#ntext = observeEvent(input$missing.categorical.submit, {
#  "New variables added to end of data set"
#})

#output$message.newvariablesadded = renderText({  
#  ntext() 
#})


observeEvent(input$missing.categorical.submit, {
  output$message.newvariablesadded = renderText({
    if(newvariablesadded_reactives$success)
      "New variables added to end of data set"
  })
})
















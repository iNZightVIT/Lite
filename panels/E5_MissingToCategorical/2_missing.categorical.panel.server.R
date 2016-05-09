##  Manipulate variables -> Missing to categorical



newvariablesadded_reactives = reactiveValues(
  success = F
)


output$missing.categorical = renderUI({
  missing.categorical.panel(get.data.set())
})

output$missing.categorical.table = renderDataTable({
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
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = display.missing.categorical(get.data.set(),columns=input$missing.categorical.column.select)
      newvariablesadded_reactives$success = T
    }
  })
})



output$message.newvariablesadded = renderText({
  input$missing.categorical.submit
  isolate({
    if(newvariablesadded_reactives$success){
      "New variables added to end of data set"
    }
  })
})









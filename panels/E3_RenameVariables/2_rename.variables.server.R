 ## Manipulate variables -> Rename Variables : rename a column.

output$rename.variables = renderUI({
  get.rename.variables.panel(get.data.set())
})

observe({
  input$rename.variables.column.select
  isolate({
    updateTextInput(session,
                    "rename.variables.new.name",
                    value=input$rename.variables.column.select)
  })
})

observe({
  input$rename.variables
  isolate({
    if(!is.null(input$rename.variables)&&
         input$rename.variables>0&&
         !is.null(input$rename.variables.new.name)&&
         !input$rename.variables.new.name%in%""){
      colnames(values$data.set)[
        which(colnames(get.data.set())%in%
                input$rename.variables.column.select)
        ] = input$rename.variables.new.name
    }
  })
})

output$rename.variables.out = renderPrint({
  data.summary(get.data.set())
})
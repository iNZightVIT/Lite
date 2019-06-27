

rename_variables = reactiveValues(
  num.cols = 0
)


output$rename_variables_two_columns = renderUI({
  get.data.set()
  ret = NULL
  isolate({
    old.variable.names = colnames(get.data.set())
    num.variables = length(old.variable.names)
    rename_variables$num.cols = num.variables
    ret = vector(mode = "list", length = num.variables + 1)
    ret[[1]] = fixedRow(column(6, h5("Old Variables")),
                        column(6, h5("New Variables")))
    for (i in 1:num.variables) {
      ret[[i+1]] = fixedRow(column(6, h5(old.variable.names[i])),
                            column(6, textInput(paste("variablenames", i, sep = ""),
                                                label = NULL,
                                                value = old.variable.names[i])))
    }
  })
  ret
})


observe({
  input$rename_variables_two_columns
  isolate({
    if(rename_variables$num.cols > 0) {
      rename.variables.test = FALSE
      namelist = c()
      for(i in 1:rename_variables$num.cols) {
        if(!is.null(eval(parse(text = paste0("input$variablenames", i)))) 
           && length(eval(parse(text = paste0("input$variablenames", i)))) > 0
           && !grepl("^\\s*$", eval(parse(text = paste0("input$variablenames", i))))) {
          rename.variables.test = TRUE
          namelist[i] = eval(parse(text = paste0("input$variablenames", i)))
        }
        else
          rename.variables.test = FALSE
      }
      
      if(rename.variables.test) {
        temp = get.data.set()
        names(temp) = namelist
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = temp
      }
    }
  })
})


output$rename.variables.table = renderDataTable({
  get.data.set()
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))


output$rename.variables = renderUI({
  rename.variables.panel()
})







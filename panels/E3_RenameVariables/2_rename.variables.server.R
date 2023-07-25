

rename_variables = reactiveValues(
  num.cols = 0
)



output$rename_variables_two_columns = renderUI({
  get.data.set()
  ret = list()
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
  input$rename_variables_two_columns_but
  isolate({
    tryCatch({
    if(rename_variables$num.cols > 0 && !is.null(input$rename_variables_two_columns_but) &&
       input$rename_variables_two_columns_but > 0) {
#      rename.variables.test = FALSE
      old.variable.names = colnames(get.data.set())
      indexes1= grep("^variablenames[0-9]+$",names(input))
      indexes1 = names(input)[indexes1]
      indexes1 = indexes1[indexes1 %in% paste0("variablenames", 1:length(old.variable.names))]
      idxmch = as.numeric(gsub("variablenames", "", indexes1))
      namelist = list()
      for(i in 1:rename_variables$num.cols) {
        if(!is.null(eval(parse(text = paste0("input$variablenames", i)))) 
           && length(eval(parse(text = paste0("input$variablenames", i)))) > 0
           && !grepl("^\\s*$", eval(parse(text = paste0("input$variablenames", i))))) {
#          rename.variables.test = TRUE
          namelist[i] = input[[indexes1[i]]]
          names(namelist)[i] = colnames(get.data.set())[idxmch[i]]
        }
      }
      changed <- sapply(seq_along(namelist), function(i){
        names(namelist)[i] != namelist[i]
      })
      namelist = namelist[changed]
      if(!is.null(namelist)){
        temp = iNZightTools::renameVars(get.data.set(), namelist)
      }
      if(!is.null(temp)){
        updatePanel$datachanged = updatePanel$datachanged + 1
        values$data.set = as.data.frame(temp)
        
        values$data.sample = as.data.frame(temp[values$sample.row,])
        row.names(values$data.sample) = 1:nrow(values$data.sample)
        ## code history
        code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
        code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
      }
    }
    }, error = function(e) {
      print(e)
    }, finally = {})
  })
})


output$rename.variables.table = renderDT({
  values$data.sample
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))


output$rename.variables = renderUI({
  rename.variables.panel()
})




output$rename.var.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
})




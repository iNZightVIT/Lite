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
        values = sample_if_lite2(rvalues = values, d = temp, new_sample = FALSE)
        ## code history
        code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
        code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
        values$transform.text = "The transformation of the columns was succesful!"
      }
    }
  })
})


output$table_part <- renderDT({
  if(!is.null(input$select.transform) && !is.null(input$select.columns.transform)){
    if(LITE2) {
    values$data.sample
    } else {
      iNZightTools::transformVar(get.data.set()[input$select.columns.transform],input$select.columns.transform,input$select.transform,)
    }
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

output$table_part.data.sample.info <- renderText({
  sample_info_lite2()
})

##  Manipulate variables -> Numeric variables -> Standardise variables (Perform column transformations)

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


output$standardise.variables.table = renderDT({
  if(!is.null(values$data.set)){
    get.data.set.display()
  }
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
      values = sample_if_lite2(rvalues = values, d = data, new_sample = FALSE)
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})

output$standardise.var.data.sample.info <- renderText({
  sample_info_lite2()
})


##  Manipulate variables -> Numeric variables -> Convert to categorical variables (Perform column transformations)


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


output$convert.to.cate.table = renderDT({
  if(!is.null(values$data.set)){
    get.data.set.display()
  }
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
      values = sample_if_lite2(rvalues = values, d = data, new_sample = FALSE)
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})

output$convert.to.cate.data.sample.info <- renderText({
  sample_info_lite2()
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

output$form_class_interval_specified_interval <- renderUI({
  ret = NULL
  input$form_class_interval_method
  input$form.class.interval.column.select
  input$form_class_interval_number
  isolate({
    if(input$form_class_interval_method == "Specified intervals") {
      VarValues <- get.data.set()[, input$form.class.interval.column.select]
      breaksNeeded = input$form_class_interval_number - 1
      title = list(fixedRow(column(12, paste("Specified", input$form_class_interval_number, "intervals.\n Need", breaksNeeded, "break points"))))
      valmin = list(fixedRow(column(12, paste0("The minimum value of variable ", input$form.class.interval.column.select, " is ", 
                                      as.character(min(VarValues, na.rm = TRUE))))))
      textbox = lapply(1:breaksNeeded, function(i) {
        textInput(paste0('form_class_interval_si', i), label = "", width = "300px")
      })
      valmax = list(fixedRow(column(12, paste0("The maximum value of variable ", input$form.class.interval.column.select, " is ", 
                                               as.character(max(VarValues, na.rm = TRUE))))))
      ret = c(title, valmin, textbox, valmax)
    }
  })
  ret
})


observe({
  if(input$form.class.interval.submit > 0 && !is.null(input$form.class.interval.submit)){
    isolate({
      bins = input$form_class_interval_number
      levelLabels <- TRUE
      if (req(input$form_class_interval_new_level_name) == "[closed left, open right)")
        levelLabels <- FALSE
      
      dataSet <- get.data.set()
      VarValues <- dataSet[, input$form.class.interval.column.select]
      newVarValues = NULL
      if (input$form_class_interval_method == "Equal width intervals")
        newVarValues <- try(cut(VarValues, bins,
                                right = levelLabels, include.lowest = TRUE))
      else if (req(input$form_class_interval_method) == "Equal count intervals")
        newVarValues <- try(cut(VarValues,
                                quantile(VarValues, probs=seq(0,1,1/bins),na.rm=TRUE),
                                include.lowest = TRUE,
                                right = levelLabels))
      else if(req(input$form_class_interval_method) == "Specified intervals"){
        breaksNeeded = bins - 1
        cutOffPoints = numeric(0)
        for(i in 1:breaksNeeded)
          cutOffPoints= c(cutOffPoints, gsub(pattern = '\\n+', replacement = "", x = input[[paste0('form_class_interval_si', i)]], perl = TRUE))
        cutOffPoints = c(min(VarValues, na.rm = TRUE),
                         gsub(pattern = '\\s+', replacement = "", x = cutOffPoints, perl = TRUE),
                         max(VarValues, na.rm = TRUE))
        
        if (any(cutOffPoints %in% c("", " ", "", "   ", "\n", "\n\n"))){
          shinyalert(title = "ERROR", text = "Fill in all text boxes", type = "error")
        } else if (length(unique(cutOffPoints[c(-1,-length(cutOffPoints))])) != length(cutOffPoints)-2){
          shinyalert(title = "ERROR", text = "Breaks must be unique values.", type = "error")
        } else {
          newVarValues = try(cut(VarValues, cutOffPoints, include.lowest = TRUE, right = levelLabels))
        }
      }
      
      if(!is.null(newVarValues)){
        if (class(newVarValues)[1] == "try-error"){
          shinyalert(title = "ERROR",
                     text = "Error in cutting intervals!",
                     type = "error")
        } else {
          data = data.frame(stringsAsFactors = T,
                            get.data.set(), newVarValues)
          colnames(data)[length(data)] = input$form.class.interval.column.name
          updatePanel$datachanged = updatePanel$datachanged+1
          values$data.set = data
          values = sample_if_lite2(rvalues = values, d = data, new_sample = FALSE)
          
          shinyalert(title = "Success",
                     text = paste("The new variable",
                                  input$form.class.interval.column.name,
                                  "will be inserted as the last column of the dataset"),
                     type = "success")
        }
      }
      
      ## reset radiobuttons
      updateRadioButtons(
        session,
        inputId = "form_class_interval_method",
        label = "Method:",
        choices = c("Equal width intervals",
                    "Equal count intervals", "Specified intervals"),
        selected = "Equal width intervals"
      )
    })
  }
})

output$form.class.interval.table <- renderDT({
  if(!is.null(values$data.set)){
    get.data.set.display()
  }
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))

output$form.class.data.sample.info <- renderText({
  sample_info_lite2()
})


## Manipulate variables -> Numeric variables -> Rank numeric
output$rank.numeric.table = renderDT({
  if(!is.null(values$data.set)){
    get.data.set.display()
  }
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))

output$rank.numeric.side = renderUI({
  rank.numeric.sidebar(get.data.set())
})

output$rank.numeric.main = renderUI({
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
      values = sample_if_lite2(rvalues = values, d = data, new_sample = FALSE)
      
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})

output$rank.numeric.data.sample.info <- renderText({
  sample_info_lite2()
})
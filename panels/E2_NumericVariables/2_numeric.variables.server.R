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
      temp = transform.perform(get.data.set(),
                               input$select.transform,
                               input$select.columns.transform)
      #         print(head(temp))
      if(!is.null(temp)){
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = temp
        values$transform.text = "The transformation of the columns was succesful!"
      }
    }
  })
})

output$table_part <- renderDataTable({
  transform.tempTable(get.data.set(),input$select.transform,input$select.columns.transform)
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

observe({
  input$form_class_interval_number
  isolate({
    if((!is.null(input$form_class_interval_number)&&
        !all(is.convertable.integer(input$form_class_interval_number)))||(
          !is.null(input$form_class_interval_number)&&
          all(is.convertable.integer(input$form_class_interval_number))&&
          as.numeric(input$form_class_interval_number)<=1)){
      updateTextInput(session,inputId="form_class_interval_number",
                      value="")
    }
  })
})

output$form.class.interval.table = renderDataTable({
  ret = get.data.set()
  if(all(is.convertable.integer(input$form_class_interval_number))){
    intervals = NULL
    labels=NULL
    if(input$form_class_interval_method_select%in%"specified"){
      ids = names(input)[grep("^specified_text[0-9]+$",names(input))]
      if(length(ids)>0){
        intervals = unlist(lapply(1:length(ids),function(i){
          input[[ids[i]]]
        }))
      }
    }
    if(input$form_class_interval_labels_provide){
      labels = names(input)[grep("^lable_class[0-9]+$",names(input))]
      for(i in 1:length(labels)){
        labels[i] = input[[labels[i]]]
      }
      if(any(is.null(labels))||
         any(labels%in%"")){
        labels=NULL
      }
    }
    if(all(is.convertable.numeric(intervals))){
      ret = get.form.class.interval(dafr=get.data.set(),
                                    intervals=sort(as.numeric(intervals)),
                                    method=input$form_class_interval_method_select,
                                    column=input$form.class.interval.column.select,
                                    num.intervals=as.numeric(input$form_class_interval_number),
                                    open.left.closed.right=input$form.class.interval.format,
                                    labels=labels)
    }
  }
  ret
},options=list(lengthMenu = c(5, 30, 50), 
               pageLength = 5, 
               columns.defaultContent="NA",
               scrollX=T))

observe({
  input$form.class.interval.submit
  isolate({
    if(!is.null(input$form.class.interval.submit)&&
       input$form.class.interval.submit>0&&
       all(is.convertable.integer(input$form_class_interval_number))){
      intervals = NULL
      labels=NULL
      if(input$form_class_interval_method_select%in%"specified"){
        ids = names(input)[grep("^specified_text[0-9]+$",names(input))]
        if(length(ids)>0){
          intervals = unlist(lapply(1:length(ids),function(i){
            input[[ids[i]]]
          }))
        }
      }
      if(input$form_class_interval_labels_provide){
        labels = names(input)[grep("^lable_class[0-9]+$",names(input))]
        for(i in 1:length(labels)){
          labels[i] = input[[labels[i]]]
        }
        if(any(is.null(labels))||
           any(labels%in%"")){
          labels=NULL
        }
      }
      if(all(is.convertable.numeric(intervals))){
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = get.form.class.interval(dafr=get.data.set(),
                                                  intervals=sort(as.numeric(intervals)),
                                                  method=input$form_class_interval_method_select,
                                                  column=input$form.class.interval.column.select,
                                                  num.intervals=as.numeric(input$form_class_interval_number),
                                                  open.left.closed.right=input$form.class.interval.format,
                                                  labels=labels)
      }
    }
  })
})

output$specified.range = renderUI({
  get.data.set()
  input$form_class_interval_number
  input$form.class.interval.column.select
  isolate({
    ret = NULL
    if(all(is.convertable.integer(input$form_class_interval_number))&&
       !is.null(input$form.class.interval.column.select)){
      ret = list(helpText(paste("Fill in the text fields below with numeric 
                                range cuttoffs. The minimum in the selected 
                                column is ",
                                min(get.data.set()[,input$form.class.interval.column.select],na.rm=T),
                                ". The maximum is ",
                                max(get.data.set()[,input$form.class.interval.column.select],na.rm=T),
                                ". All values must be in this range."
                                ,sep="")))
      for(i in 2:(input$form_class_interval_number)){
        ret[[i]] = textInput(inputId=paste0("specified_text",i),
                             label=paste("Range cutoff",(i-1),sep=" "),
                             value = "")
      }
    }
    ret
  })
})

output$labels.provide = renderUI({
  get.data.set()
  input$form_class_interval_number
  isolate({
    if(all(is.convertable.integer(input$form_class_interval_number))){
      ret = list(helpText("Fill in all the text fields to label the 
                          factors in the newly generated class interval 
                          variable."))
      for(i in 1:input$form_class_interval_number){
        ret[[i+1]] = textInput(paste0("lable_class",i),label=paste("Specify factor",i,sep=" "))
      }
    }
    ret
})
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
    }
  })
})
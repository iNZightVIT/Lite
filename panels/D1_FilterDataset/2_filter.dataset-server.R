##  Row operations (Perform row operations) --> Filter Dataset

observe({
  input$filter_data_perform
  isolate({
    if(!is.null(input$filter_data_perform)&&input$filter_data_perform>0){
      if(input$select_filter%in%"levels of categorical variable"){
        if(!is.null(input$select_categorical1)&&!input$select_categorical1%in%""){
          to.remove = which(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)]%in%input$levels1)
          if(length(to.remove)>0){
            updatePanel$datachanged = updatePanel$datachanged+1
            values$data.set = get.data.set()[-to.remove,]
            if(class(values$data.set[,which(colnames(get.data.set())%in%input$select_categorical1)]) == "factor") {
              values$data.set[,which(colnames(get.data.set())%in%input$select_categorical1)] = 
                droplevels(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)])
            }
            
            updateSelectInput(session=session,inputId="select_categorical1",
                              choices=c("",get.categorical.column.names(get.data.set())),
                              selected=1)
            updateSelectInput(session=session,inputId="levels1",
                              choices="",selected=1)
          }
        }
      }else if(input$select_filter%in%"numeric condition"){
        if(!input$select_numeric1%in%""&!input$select_operation1%in%""&all(is.convertable.numeric(input$numeric_input1))){
          indexes.keep = 1:nrow(get.data.set())
          if(input$select_operation1%in%"<"){
            indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]<as.numeric(input$numeric_input1)))
          }else if(input$select_operation1%in%">"){
            indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]>as.numeric(input$numeric_input1)))
          }else if(input$select_operation1%in%"<="){
            indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]<=as.numeric(input$numeric_input1)))
          }else if(input$select_operation1%in%">="){
            indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]>=as.numeric(input$numeric_input1)))
          }else if(input$select_operation1%in%"=="){
            indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]==as.numeric(input$numeric_input1)))
          }else if(input$select_operation1%in%"!="){
            indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]!=as.numeric(input$numeric_input1)))
          }
          if(length(indexes.keep)>0){
            updatePanel$datachanged = updatePanel$datachanged+1
            values$data.set = get.data.set()[indexes.keep,]
          }
        }
      }else if(input$select_filter%in%"row indices"){
        if(all(is.convertable.numeric(strsplit(input$row_op_indexes,",",fixed=T)[[1]]))){
          indices = as.numeric(strsplit(input$row_op_indexes,",",fixed=T)[[1]])
          indices = indices[which(indices%in%(1:nrow(get.data.set())))]
          if(length(indices)>0){
            updatePanel$datachanged = updatePanel$datachanged+1
            values$data.set = get.data.set()[-indices,] 
          }
        }
      }else if(input$select_filter%in%"randomly"){
        if(all(is.convertable.numeric(input$numeric_input2))&&
             all(is.convertable.numeric(input$numeric_input3))&&
             as.numeric(input$numeric_input2)<=nrow(get.data.set())&&
             (((as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))>=nrow(get.data.set())&
                 input$bootstrap_check)|
                (as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))<=nrow(get.data.set()))){
          temp = sample.data(df=get.data.set(),
                             sampleSize=as.numeric(input$numeric_input2),
                             numSample=as.numeric(input$numeric_input3),
                             bootstrap=input$bootstrap_check)
          if(!is.null(temp)){
            updatePanel$datachanged = updatePanel$datachanged+1
            values$data.set = temp
          }
        }
      }
    }
  })
})

output$message3 = renderPrint({
  input$numeric_input2
  input$numeric_input3
  input$bootstrap_check
  isolate({
    if(all(is.convertable.numeric(input$numeric_input2))&&
         all(is.convertable.numeric(input$numeric_input3))&&
         as.numeric(input$numeric_input2)<=nrow(get.data.set())&&
         (((as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))>=nrow(get.data.set())&
             input$bootstrap_check)|
            (as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))<=nrow(get.data.set()))){
      cat("Size of sample: ",input$numeric_input2,"\n",
          "Number of sample: ", input$numeric_input3)
    }else{
      cat("This input can not be processed. The data has ",
          nrow(get.data.set())," rows.")
    }
  })
})

observe({
  input$select_categorical1
  isolate({
    if(!is.null(input$select_categorical1)){
      if(is.null(levels(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)])))
        updateSelectInput(session=session,inputId="levels1",
                          choices=sort(unique(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)])))
      updateSelectInput(session=session,inputId="levels1",
                        choices=levels(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)]))
    }
  })
})

output$message2 = renderPrint({
  valid = all(is.convertable.numeric(strsplit(input$row_op_indexes,",",fixed=T)[[1]]))
  isolate({
    if(!valid){
      cat("Please provide a comma seperated list of indices.")
    }else{
      cat("")
    } 
  })
})

output$message1 = renderPrint({
  input$select_numeric1
  input$select_operation1
  input$numeric_input1
  isolate({
    if(!all(is.convertable.numeric(input$numeric_input1))){
      cat("Please provide a numeric variable.")
    }else{
      cat(input$select_numeric1,input$select_operation1,input$numeric_input1)
    } 
  })
})

output$filter.data.summary <- renderPrint({
  get.data.set()
  input$filter_data_perform
  isolate({
    data.summary(get.data.set())
  })
})

output$filter.dataset = renderUI({
  filter.data.panel(get.data.set())
})


observe({  
  input$filter_data_perform  
  isolate({
    updateSelectInput(session, "subs2", selected = "none")
    updateSelectInput(session, "subs1", selected = "none")
    updateSelectInput(session, "vari2", selected = "none")
    updateSelectInput(session, "vari1", selected = "none")
  })  
})
















## Manipulate variables --> Categorical variables

output$categorical.variables = renderUI({
  get.data.set()
  isolate({
    if(input$selector%in%"Categorical variables"){
      categorical.variables.panel(input$categorical_variables_select,get.data.set())
    }
  })
})

output$categorical.main.panel = renderUI({
  input$categorical_variables_select
  isolate({
    if(!is.null(input$categorical_variables_select)&&
         input$categorical_variables_select%in%"Reorder levels"){
      reorder.main.panel()
    }else if(!is.null(input$categorical_variables_select)&&
               input$categorical_variables_select%in%"Collapse levels"){
      collapse.main.panel()
    }else if(!is.null(input$categorical_variables_select)&&
               input$categorical_variables_select%in%"Rename levels"){
      rename.levels.main.panel()
    }else if(!is.null(input$categorical_variables_select)&&
               input$categorical_variables_select%in%"Combine categorical"){
      combine.main.panel()
    }
  })
})

output$categorical.side.panel = renderUI({
  input$categorical_variables_select
  get.data.set()
  isolate({
    if(!is.null(input$categorical_variables_select)&&
         input$categorical_variables_select%in%"Reorder levels"){
      reorder.sidebar.panel(get.data.set())
    }else if(!is.null(input$categorical_variables_select)&&
               input$categorical_variables_select%in%"Collapse levels"){
      collapse.sidebar.panel(get.data.set())
    }else if(!is.null(input$categorical_variables_select)&&
               input$categorical_variables_select%in%"Rename levels"){
      rename.levels.sidebar.panel(get.data.set())
    }else if(!is.null(input$categorical_variables_select)&&
               input$categorical_variables_select%in%"Combine categorical"){
      combine.sidebar.panel(get.data.set())
    }
  })
})

## Manipulate variables --> Categorical variables --> Reorder levels

observe({
  input$reorder
  isolate({
    items = input$select.reorder.item
    if(!is.null(items)&!is.null(input$select.reorder.column)){
      column = get.data.set()[,input$select.reorder.column]
      if(length(items)<length(unique(column))){
        not.in = sort(unique(get.data.set()[,input$select.reorder.column])
                      [which(!unique(get.data.set()[,input$select.reorder.column])%in%items)])
        levels.new = c(items,as.character(not.in))
      }else{
        levels.new = items
      }
      temp = reorder.levels(get.data.set(),input$select.reorder.column,levels.new)
      if(!is.null(temp)){
        values$data.set = temp
        updateSelectInput(session=session,inputId="select.reorder.item",selected="",choices="")
        updateSelectInput(session=session,inputId="select.reorder.column",selected="")
      }
    }
  })
})

output$text_reorder = renderPrint({
  if(!is.null(input$select.reorder.column)&&!""%in%input$select.reorder.column){
    print(table(get.data.set()[,input$select.reorder.column]))
  }else{
    print("Select a column!")
  }
})

observe({
  if(!is.null(input$select.reorder.column)){
    choices=""
    if(!"" %in% input$select.reorder.column){
      if(is.factor(get.data.set()[,input$select.reorder.column])){
        choices = levels(get.data.set()[,input$select.reorder.column])
      }else{
        choices = levels(as.factor(get.data.set()[,input$select.reorder.column]))
      }
    }
    updateSelectInput(session=session,inputId="select.reorder.item",selected="",choices=choices)
  }
})

## Manipulate variables --> Categorical variables --> Collapse levels

observe({
  if(!is.null(input$select.collapse.column)){
    choices=""
    if(!"" %in% input$select.collapse.column){
      if(is.factor(get.data.set()[,input$select.collapse.column])){
        choices = levels(get.data.set()[,input$select.collapse.column])
      }else{
        choices = levels(as.factor(get.data.set()[,input$select.collapse.column]))
      }
    }
    updateSelectInput(session=session,inputId="select.collapse.item",selected="",choices=choices)
  }
})

output$text_collapse_1st = renderPrint({
  input$collapse
  if(!is.null(input$select.collapse.column)&&!""%in%input$select.collapse.column){
    print(table(get.data.set()[,input$select.collapse.column]))
  }else{
    print("Select a column!")
  }
})

output$text_collapse_2nd = renderPrint({
  input$collapse
  if(!is.null(input$select.collapse.column)&&!""%in%input$select.collapse.column&&
       !is.null(input$select.collapse.item)&&!""%in%input$select.collapse.item){
    print(table(get.collapsed.column(get.data.set()[,input$select.collapse.column],input$select.collapse.item)))
  }else{
    print("")
  }
})

observe({
  input$collapse
  isolate({
    if(!is.null(input$collapse)&&input$collapse>0&&
         input$select.collapse.column%in%colnames(get.data.set())){
      if(any(input$select.collapse.item%in%
               get.data.set()[,which(colnames(get.data.set())%in%
                                       input$select.collapse.column)])){
        temp = get.collapsed.column(
          get.data.set()[,which(colnames(get.data.set())%in%input$select.collapse.column)],
          input$select.collapse.item)
        if(!is.null(temp)){
          values$data.set[,which(colnames(get.data.set())%in%input$select.collapse.column)] = temp
          updateSelectInput(session,"select.collapse.column",selected=NULL)
        }
      }
    }
  })
})

output$rename.factors.inputs = renderUI({
  input$select.rename.column
  get.data.set()
  isolate({
    if(!is.null(input$select.rename.column)&&!input$select.rename.column%in%""){
      rename.factors.textfields(levels(get.data.set()[,input$select.rename.column]))
    }
  })
})

output$text_rename = renderPrint({
  input$select.rename.column
  get.data.set()
  isolate({
    if(!is.null(input$select.rename.column)&&!input$select.rename.column%in%""){
      print(summary(get.data.set()[,input$select.rename.column]))
    }else{
      print("")
    }
  })
})

observe({
  input$rename.levs
  isolate({
    if(!is.null(input$rename.levs)&&input$rename.levs>0){
      indexes1= grep("^factor[0-9]+$",names(input))
      new.levels = c()
      for(i in 1:length(indexes1)){
        new.levels[i] = input[[names(input)[indexes1[i]]]]
        if(is.null(new.levels[i])||new.levels[i]%in%""){
          new.levels[i] = levels(get.data.set()[,input$select.rename.column])[i]
        }
      }
      temp = rename.levels(get.data.set(),input$select.rename.column,new.levels)
      if(!is.null(temp)){
        values$data.set = temp
        updateSelectInput(session,"select.rename.column",selected=0)
      }
    }
  })
})

## Manipulate variables --> Categorical variables --> Combine levels

output$text_combine = renderPrint({
  if(length(input$select.combine.columns)>0){
    temp = combine.levels(get.data.set(),input$select.combine.columns)
    print(table(temp[,ncol(temp)]))  
  }else{
    print("Please select a set of columns")
  }
})

observe({
  input$combine
  isolate({
    if(!is.null(input$combine)&&input$combine>0){
      temp = combine.levels(get.data.set(),input$select.combine.columns)
      if(!is.null(temp)){
        values$data.set = temp
      }
    }
  })
})

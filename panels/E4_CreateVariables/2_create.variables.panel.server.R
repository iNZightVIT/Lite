## Manipulate variables --> Create variables

output$create.variables = renderUI({
  get.create.variables.panel(get.data.set())
})

output$create.variables.out = renderPrint({
  data.summary(get.data.set())
})

observe({
  input$create.variables.operation.select
  isolate({
    if(!" "%in%input$create.variables.operation.select){
      values$create.variables.expression.text  = paste(
        get.create.variables.expression.text(),
        input$create.variables.operation.select,sep="")
    }
  })
})

observe({
  input$create.variables.column.select
  isolate({
    if(!" "%in%input$create.variables.column.select){
      values$create.variables.expression.text  = paste(
        get.create.variables.expression.text(),
        input$create.variables.column.select,sep="")
    }
  })
})

output$create.variables.expression = renderPrint({
  cat(get.create.variables.expression.text())
})

observe({
  input$create.variables.submit
  isolate({
    if(!is.null(input$create.variables.submit)&&
       input$create.variables.submit>0){
      # check if the new variable contains spaces " " or dashes "-"
      # replace it with an underscore "_" if found
      # browser()
      new_var = trimws(input$create.variables.name)
      new_var = gsub(" |-", "_", new_var)
      
      temp = iNZightTools::createNewVar(get.data.set(),
                                        new_var = new_var,
                                        get.create.variables.expression.text())
      if(!is.null(temp)){
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = temp
        values$create.variables.expression.text = "";
        ## code history
        code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
        code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
      }
    }
  })
})

output$create.variables.status.message = renderPrint({
  input$create.variables.column.select
  input$create.variables.operation.select
  input$create.variables.1
  input$create.variables.2
  input$create.variables.3
  input$create.variables.4
  input$create.variables.5
  input$create.variables.6
  input$create.variables.7
  input$create.variables.8
  input$create.variables.9
  input$create.variables.0
  input$create.variables.delete
  input$create.variables.dot
  isolate({
    if(is.null(get.create.variables(get.data.set(),
                                    get.create.variables.expression.text(),
                                    input$create.variables.name))||
       input$create.variables.name%in%c("",colnames(get.data.set()))){
      cat("This input can't be processed.")
    }else{
      cat("The expression is valid.")
    }
  })
})

observe({
  input$create.variables.1
  isolate({
    if(!is.null(input$create.variables.1)&&input$create.variables.1>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"1",sep="")
    }
  })
})

observe({
  input$create.variables.2
  isolate({
    if(!is.null(input$create.variables.2)&&input$create.variables.2>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"2",sep="")
    }
  })
})

observe({
  input$create.variables.3
  isolate({
    if(!is.null(input$create.variables.3)&&input$create.variables.3>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"3",sep="")
    }
  })
})

observe({
  input$create.variables.4
  isolate({
    if(!is.null(input$create.variables.4)&&input$create.variables.4>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"4",sep="")
    }
  })
})

observe({
  input$create.variables.5
  isolate({
    if(!is.null(input$create.variables.5)&&input$create.variables.5>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"5",sep="")
    }
  })
})

observe({
  input$create.variables.6
  isolate({
    if(!is.null(input$create.variables.6)&&input$create.variables.6>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"6",sep="")
    }
  })
})

observe({
  input$create.variables.7
  isolate({
    if(!is.null(input$create.variables.7)&&input$create.variables.7>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"7",sep="")
    }
  })
})

observe({
  input$create.variables.8
  isolate({
    if(!is.null(input$create.variables.8)&&input$create.variables.8>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"8",sep="")
    }
  })
})

observe({
  input$create.variables.9
  isolate({
    if(!is.null(input$create.variables.9)&&input$create.variables.9>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"9",sep="")
    }
  })
})

observe({
  input$create.variables.0
  isolate({
    if(!is.null(input$create.variables.0)&&input$create.variables.0>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,"0",sep="")
    }
  })
})

observe({
  input$create.variables.dot
  isolate({
    if(!is.null(input$create.variables.dot)&&input$create.variables.dot>0){
      values$create.variables.expression.text = paste(
        values$create.variables.expression.text,".",sep="")
    }
  })
})

observe({
  input$create.variables.delete
  isolate({
    if(!is.null(input$create.variables.delete)&&input$create.variables.delete>0){
      if(nchar(values$create.variables.expression.text)>0){
        values$create.variables.expression.text = substr(
          values$create.variables.expression.text,1,
          nchar(values$create.variables.expression.text)-1)
      }
    }
  })
})
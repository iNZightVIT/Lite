design.parameters = reactiveValues()
design.parameters$id = formula("~1")
design.parameters$strata = NULL
design.parameters$fpc = NULL
design.parameters$nest = F
design.parameters$weights = NULL

design.uitexts = reactiveValues()
design.uitexts$success.text = ""

output$survey.design = renderUI({
  create.design.panel(get.data.set())
})

output$create.design.summary = renderPrint({
  tryCatch({
    temp = list()
    if(!is.null(design.parameters$id)){
      temp$id = design.parameters$id
    }else{
      temp$id = design.parameters$id
    }
    if(!is.null(design.parameters$strata)){
      temp$strata=design.parameters$strata
    }
    if(!is.null(design.parameters$fpc)){
      temp$fpc=design.parameters$fpc
    }
    temp$nest = F
    if(design.parameters$nest){
      temp$nest = design.parameters$nest
    }
    if(!is.null(design.parameters$weights)){
      temp$weights = design.parameters$weights
    }
    dafr = get.data.set()
    print(summary(eval(parse(text=paste("svydesign(id = ",
                                        temp$id,
                                        ", weights = ",
                                        temp$weights,
                                        ", data = dafr, fpc = ",
                                        temp$fpc,
                                        ", nest = ",
                                        temp$nest,
                                        ")")))))
  }, error = function(e) {
    if(!is.null(parseQueryString_Lite(session$clientData$url_search)$debug)&&
         tolower(parseQueryString_Lite(session$clientData$url_search)$debug)%in%"true"){
      print(e)
    }
    cat("This input can not be used with the current data set to generate a design object")
  }, finally = {})
})

observe({
  input$create.design
  isolate({
    if(!is.null(input$create.design)&&
         input$create.design>0){
      tryCatch({
        temp = list()
        temp$id = design.parameters$id
        if(!is.null(design.parameters$strata)){
          temp$strata=design.parameters$strata
        }
        if(!is.null(design.parameters$fpc)){
          temp$fpc=design.parameters$fpc
        }
        temp$nest = F
        if(design.parameters$nest){
          temp$nest = design.parameters$nest
        }
        if(!is.null(design.parameters$weights)){
          temp$weights = design.parameters$weights
        }
        dafr = get.data.set()
        plot.par$design = eval(parse(text=paste("svydesign(id = ",
                                                temp$id,
                                                ", weights = ",
                                                temp$weights,
                                                ", data = dafr, fpc = ",
                                                temp$fpc,
                                                ", nest = ",
                                                temp$nest,
                                                ")")))
        design.uitexts$success.text = "Survey design added!"
      }, error = function(e) {
        if(!is.null(parseQueryString_Lite(session$clientData$url_search)$debug)&&
             tolower(parseQueryString_Lite(session$clientData$url_search)$debug)%in%"true"){
          print(e)
        }
        plot.par$design=NULL
        design.uitexts$success.text = "Survey design could not be generated!"
      }, finally = {})
    }
  })
})

observe({
  input$remove.design
  isolate({
    if(!is.null(input$remove.design)&&
         input$remove.design>0){
      plot.par$design=NULL
      design.uitexts$success.text = "Survey design removed!"
    }
  })
})

observe({
  input$strata.select
  isolate({
    if(!is.null(input$strata.select)&&
         !input$strata.select%in%"none"){
      design.parameters$strata = paste("~",input$strata.select,sep="")
    }else{
      design.parameters$strata = NULL
    }
  })
})

observe({
  input$clustering.select
  isolate({
    if(!is.null(input$clustering.select)&&
         length(input$clustering.select)!=0){
      temp = input$clustering.select
      if("none"%in%temp){
        temp = temp[-which(temp%in%"none")]
      }
      if("1"%in%temp&&
           length(temp)>1){
        temp = temp[-which(temp%in%"1")]
      }
      design.parameters$id = paste("~",paste(temp,collapse="+"),sep="")
    }else{
      design.parameters$id = NULL
    }
  })
})

observe({
  input$weights.select
  isolate({
    if(!is.null(input$weights.select)&&
         !input$weights.select%in%"none"){
      design.parameters$weights = paste("~",input$weights.select,sep="")
    }else{
      design.parameters$weights = NULL
    }
  })
})

observe({
  input$nest.check
  isolate({
    if(!is.null(input$nest.check)){
      design.parameters$nest = input$nest.check
    }else{
      design.parameters$nest = F
    }
  })
})

observe({
  input$fpc.select
  isolate({
    if(!is.null(input$fpc.select)&&
         length(input$fpc.select)!=0){
      temp = input$fpc.select
      if("none"%in%temp){
        temp = temp[-which(temp%in%"none")]
      }
      if(length(temp)==0){
        temp=NULL
      }
      design.parameters$fpc = paste("~",
                                    paste(temp,
                                          collapse="+"),sep="")
    }else{
      design.parameters$fpc = NULL
    }
  })
})

output$design.success.text = renderPrint({
  cat(design.uitexts$success.text)
})

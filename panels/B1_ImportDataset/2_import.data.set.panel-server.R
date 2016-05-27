options(shiny.maxRequestSize=2*1024^2)


import_reactives = reactiveValues(
  success = F
)


observeEvent(input$files, { 
  
  if(file.exists(input$files[1, "datapath"])) {
    isolate({
      temp = load.data(get.data.dir.imported(),
                       fileID = input$files[1, "name"],
                       path = input$files[1, "datapath"])[[2]]
      if(!is.null(temp)){  
        plot.par$design=NULL
        values$data.set = temp
        updatePanel$doit = updatePanel$doit+1
        values$data.restore <<- get.data.set()
        temp = strsplit(input$files[1, "name"],".",fixed=T)[[1]]
        if(length(temp)>1){
          temp = temp[1:(length(temp)-1)]
        } 
        values$data.name = paste(temp,collapse=".")
        import_reactives$success = T        
      }      
    })
  }
  
})


observeEvent(input$import_set, {
  
  if(!is.null(input$URLtext)&&!input$URLtext%in%"") {
    
    isolate({
      if(!is.null(input$files)&&file.exists(input$files[1, "datapath"]))
        unlink(input$files[1, "datapath"])
        
      data.vals = get.data.from.URL(input$URLtext,get.data.dir.imported())
      design.parameters$data.name = NULL
      values$data.set = data.vals$data.set
      updatePanel$doit = updatePanel$doit+1
      values$data.restore = get.data.set()
      values$data.name = data.vals$data.name
      import_reactives$success = T
    })
  }
  
})




output$load.data.panel = renderUI({
  input$selector
  isolate({
    # looks for get requests to pass in an URL for a dataset 
    load.data.panel(parseQueryString(session$clientData$url_search))
  })
})

output$filetable <- renderDataTable({

  get.data.set()
  
}, options =
  list(lengthMenu = c(5, 30, 50), pageLength = 5,
       columns.defaultContent="NA", scrollX = TRUE))

observe({
  input$remove_set
  isolate({
    if(!is.null(input$remove_set)&&input$remove_set>0){
      files = list.files(path = paste0(get.data.dir.imported(),"/Imported"),
                         pattern = input$Importedremove,
                         full.names = TRUE)
      if(!is.null(input$files)&&file.exists(input$files[1, "datapath"])&&
           grepl(get.data.name(),input$files[1, "name"])){
        unlink(input$files[1, "datapath"])
      }
      for(f in files){
        if (file.exists(f)) {
          unlink(f)
        }
      }
    }
  })
})

output$message.success = renderText({
  input$import_set
  input$files
  isolate({
    if(import_reactives$success){
      import_reactives$success = F
      "Import was successful"
    }
  })
})
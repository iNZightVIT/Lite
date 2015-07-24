observe({
  if(!is.null(input$import_set)&&input$import_set>0){
    isolate({
      if(!is.null(input$files)&&file.exists(input$files[1, "datapath"])){
        temp = load.data(get.data.dir.imported(),
                         fileID = input$files[1, "name"],
                         path = input$files[1, "datapath"])[[2]]
        if(!is.null(temp)){  
          values$data.set = temp
          values$data.restore <<- get.data.set()
          temp = strsplit(input$files[1, "name"],".",fixed=T)[[1]]
          if(length(temp)>1){
            temp = temp[1:(length(temp)-1)]
          } 
          values$data.name = paste(temp,collapse=".")
          if (!file.exists(paste(get.data.dir.imported(),"/Imported",sep=""))&&
                file.writable(get.data.dir.imported())) {
            dir.create(paste(get.data.dir.imported(),"/Imported",sep=""), recursive = TRUE)
          }
          saveRDS(get.data.set(),file = paste0(get.data.dir.imported(),"/Imported/", get.data.name(), ".RDS"))
        }
        unlink(input$files[1, "datapath"])
      }else if (!is.null(input$URLtext)&&!input$URLtext%in%""){
        data.vals = get.data.from.URL(input$URLtext,get.data.dir.imported())
        values$data.set = data.vals$data.set
        values$data.restore = get.data.set()
        values$data.name = data.vals$data.name
      }
    })
  }
})

output$load.data.panel = renderUI({
  input$selector
  isolate({
    # looks for get requests to pass in an URL for a dataset 
    url_search = parseQueryString(session$clientData$url_search)
    load.data.panel(url_search)
  })
})

output$filetable <- renderDataTable({
  input$selector
  input$files
  input$import_set
  get.data.set()
  isolate({
    if (!is.null(input$files)&&file.exists(input$files[1, "datapath"])) {
      load.data(get.data.dir.imported(),fileID = input$files[1, "name"], path = input$files[1, "datapath"])[[2]]
    } else if (!is.null(input$URLtext)&&!input$URLtext%in%"") {
      URL = input$URLtext
      name = strsplit(URL,"/")[[1]]
      name = strsplit(name[length(name)],"?",fixed=T)[[1]][1]
      if (!file.exists(paste(get.data.dir.imported(),"/Imported",sep=""))&&
            file.writable(get.data.dir.imported())) {
        dir.create(paste0(get.data.dir.imported(),"/Imported"), recursive = TRUE)
      }
      if(file.exists(paste0(get.data.dir.imported(),"/Imported/",name))){
        get.data.set()
      }else{
        NULL
      }
    }else{
      NULL
    }
  })
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
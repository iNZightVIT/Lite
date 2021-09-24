options(shiny.maxRequestSize=5*1024^2)


import_reactives = reactiveValues(
  success = F
)

observe({
  input$filter_data_perform
  isolate({
    
  })
})

observeEvent(input$files, { 
      if(file.exists(input$files[1, "datapath"])) {
        isolate({
            fpath <- input$files$datapath
            fexts <- tools::file_ext(fpath)
            fext <- fexts[1]
            temp <- tryCatch(if (any(grepl("txt|pdf|docx?|odt|rtf", fexts))) {
                                 readtext::readtext(fpath)
                             } else {
                                 switch(tolower(tools::file_ext(fpath[1])),
                                                "rdata"=,
                                                "rda"=as.data.frame(iNZightTools::load_rda(fpath)[[1]]),
                                                "tsv"=as.data.frame(iNZightTools::smart_read(fpath, delimiter="\t")),
                                                "numbers"=stop("Not a valid file extension: ", fext),
                                                as.data.frame(iNZightTools::smart_read(fpath)))
                             },
                             error=identity)

      if(!is.null(temp)){
        plot.par$design=NULL
        values$data.set = temp
        updatePanel$doit = updatePanel$doit+1
        values$data.restore <<- get.data.set()
        temp.name = make.names(tools::file_path_sans_ext(input$files[1, "name"]))
        
        if(length(temp.name)>1){
          temp.name = temp.name[1:(length(temp.name)-1)]
        }
        values$data.name = temp.name
        import_reactives$success = !inherits(temp, "condition")
        if(!(fext %in% c("RData", "rda", "Rda"))){
          code.save$name = temp.name
          code.save$variable = c(code.save$variable, list(c(sep(), "\n", paste0(sprintf("## Exploring the '%s' dataset", code.save$name), 
                                                                                "\n"))))
          code = c(paste0(code.save$name, " <- "), gsub(paste0("\".*(?=.", fext, ")"), paste0("\"", values$data.name), iNZightTools::code(temp), perl = T))
          code = do.call(c, lapply(code, function(x) {
            y <- try({
              formatR::tidy_source(text = x, width.cutoff = 80, output = F, indent = 4)$text.tidy
            }, silent = TRUE)
            if (inherits(y, "try-error")) x else c(y, "\n")
          }))
          code = gsub("(.*)\\).*","\\1)", paste0(code, collapse = "\n"))
          code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
        }
        values$name.restore = temp.name
        updateSelectInput(session, "subs2", selected = "none")
        updateSelectInput(session, "subs1", selected = "none")
        updateSelectInput(session, "vari2", selected = "none")
        updateSelectInput(session, "vari1", selected = "none")
        plot.par$design = NULL
        design_params$design = NULL
      }      
    })
  }
})


observeEvent(input$import_set, {
  
  if(!is.null(input$URLtext)&&!input$URLtext%in%"") {
    
    isolate({
      if(!is.null(input$files)&&file.exists(input$files[1, "datapath"]))
        unlink(input$files[1, "datapath"])
      
      #      if(grepl("docs.google.com", input$URLtext))
      #        data.vals = get.data.from.googledocs(input$URLtext, get.data.dir.imported())
      #      else
      data.vals = get.data.from.URL(input$URLtext, get.data.dir.imported())
      
      get.data.dir.imported()
      
      #design.parameters$data.name = NULL
      values$data.set = data.vals$data.set
      updatePanel$doit = updatePanel$doit+1
      values$data.restore = get.data.set()
      values$data.name = data.vals$data.name
      import_reactives$success = T
      plot.par$design = NULL
      design_params$design = NULL
    })
  }
  
})




output$load.data.panel = renderUI({
  input$selector
  isolate({
    # looks for get requests to pass in an URL for a dataset 
    if(grepl("docs.google.com", session$clientData$url_search)) {
      URL = session$clientData$url_search
      url.index1 = gregexpr("url=", URL)
      url.index1 = unlist(url.index1)
      url.index2 = gregexpr("&land=", URL)
      url.index2 = unlist(url.index2)
      temp = list()
      temp$url = substr(URL, url.index1+4, url.index2-1)
      temp$land = substr(URL, url.index2+6, nchar(URL))
      load.data.panel(temp[1])
    }
    else
      load.data.panel(parseQueryString(session$clientData$url_search)[1])
  })
})

output$filedisplay <- renderUI({
    if (is.data.frame(get.data.set())) {
        DTOutput('filetable')
    } else if (inherits(get.data.set(), "condition")) {
        textOutput('fileError')
    } else if (!is.null(get.data.set())) {
        verbatimTextOutput('fileprint')
    } else NULL
})

output$fileError <- renderText(safeError(message(get.data.set())))

output$fileprint <- renderPrint(get.data.set())

output$filetable <- renderDT(get.data.set(),
                             options = list(lengthMenu = c(5, 30, 50),
                                            pageLength = 5,
                                            columns.defaultContent="NA",
                                            scrollX = TRUE,
                                            columnDefs = list(list(
                                                targets = "_all",
                                                render = JS(
                                                    "function(data, type, row, meta) {",
                                                    "return type === 'display' && data != null && data.length > 30 ?",
                                                    "'<span>' + data.substr(0, 300) + '...</span>' : data;",
                                                    "}")))


                                            ))



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

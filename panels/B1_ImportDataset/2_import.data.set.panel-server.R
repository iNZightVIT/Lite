options(shiny.maxRequestSize=5*1024^2)


import_reactives = reactiveValues(
  success = F
)

observe({
  input$filter_data_perform
  isolate({
    
  })
})

# file input field (Browse) 
observeEvent(input$files, { 
      if(file.exists(input$files[1, "datapath"])) {
        isolate({
            fpath <- input$files$datapath
            fexts <- tools::file_ext(fpath)
            fext <- fexts[1]
            
            data_ext = tolower(tools::file_ext(fpath[1]))
            temp <- tryCatch(
              if (any(grepl("txt|pdf|docx?|odt|rtf", fexts))) {
                readtext::readtext(fpath)
              } else {
                if(data_ext == "rdta" | data_ext == "rda") {
                  # get available names
                  rda_data = iNZightTools::load_rda(fpath)
                  # store available names
                  values$data.available.dnames = names(rda_data)
                  # by default the first data is read in
                  values$data.current.dname = values$data.available.dnames[1]
                  
                  as.data.frame(iNZightTools::load_rda(fpath)[[1]])
                } else if(data_ext == "tsv") {
                  as.data.frame(iNZightTools::smart_read(fpath, delimiter = "\t"))
                } else if(data_ext == "numbers") {
                  stop("Not a valid file extension: ", fext)
                } else {
                  d = as.data.frame(iNZightTools::smart_read(fpath))
                  # if its an excel file
                  if(data_ext == "xls" | data_ext == "xlsx") {
                    # get available sheets
                    sheet_names = iNZightTools::sheets(d)
                    # store available sheets
                    values$data.available.dnames = sheet_names
                    # by default the first sheet is read in
                    values$data.current.dname = sheet_names[1]
                  }
                  
                  d
                }
              },
              error = identity)
            

      if(!is.null(temp)){
        plot.par$design=NULL
        values$data.set = temp
        values$data.type = data_ext
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

# "Import file from url" button
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

# select input for available sheets/data
output$data.info = renderUI({
  input$files
  dtype = values$data.type
  
  
  if(!is.null(dtype)) {
    label_name = switch(
      dtype,
      "xls" = ,
      "xlsx" = "Sheet:",
      "rdta" = ,
      "rda" = "Dataset:"
    )
    selectInput(
      inputId = "data.info",
      label = label_name,
      selected = values$data.current.dname,
      choices = values$data.available.dnames
    )
  }
})
# reload data when user chooses a different sheet/data
observeEvent(input$data.info, {
  # data type
  dtype = values$data.type
  # path of data
  fpath = input$files[1, "datapath"]
  tryCatch(
    if(!is.null(dtype) && !is.null(values$data.current.dname)) {
      # prevent loading data again, if the selected sheet name is the same
      if(input$data.info != values$data.current.dname) {
        if(dtype == "xls" | dtype == "xlsx") {
          values$data.set = as.data.frame(iNZightTools::smart_read(fpath, sheet = input$data.info))
        } else if(dtype == "rdta" | dtype == "rda") {
          ind = which(values$data.available.dnames == input$data.info)
          values$data.set = as.data.frame(iNZightTools::load_rda(fpath)[[ind]])
        }
        # update sheet name
        values$data.current.dname = input$data.info
      }
    },
    error = function(e) {
      shinyalert::shinyalert(
        title = "Import failed",
        text = glue::glue("Failed to switch to data '{input$data.info}'")
      )
    }
  )
})

# whole data import panel
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

# output of the imported file
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

options(shiny.maxRequestSize=5*1024^2)


import_reactives = reactiveValues(
  success = F
)

observe({
  input$filter_data_perform
  isolate({
    
  })
})

ext_choices = list(
  "Comma Seperated Values (.csv)" = "csv",
  "Tab-delimited Text Files (.txt, .tsv)" = "tsv",
  "Tab-delimited Text Files (.txt, .tsv)" = "txt",
  "SPSS Files (.sav)" = "sav",
  "SAS Data Files (.sas7bdat)" = "sas7dbat",
  "SAS XPORT Files (.xpt)" = "xpt",
  "97-2003 Excel Files (.xls)" = "xls",
  "2007 Excel Files (.xlsx)" = "xlsx",
  "SATA Files (.dta)" = "dta",
  "JSON (.json)" = "json",
  "R Object (.rds)" = "rds",
  "RData Files (.RData, .rda)" = "RData",
  "RData Files (.RData, .rda)" = "rda"
  # "Survey Design Files (.svydesign)" = "svydesign",
  # "Linked Data (.inzlnk)" = "inzlnk"
)
delim_choices = list(
  "Detected automatically" = NULL,
  "Comma (,)" = ",",
  "Semi-colon (;)" = ";",
  "Tab" = "\t"
)

preview_data = reactiveValues(
  fpath = NULL,
  data = NULL,
  preview_data = NULL,
  delimiter = NULL,
  ext = NULL,
  # use count to prevent rendering multiple times
  state = NULL
)

output$preview_data = renderDataTable({
  datatable(
    preview_data$preview_data, selection = "single",
    options = list(dom = "t")
  )
})

smart_delimiter = function(fpath) {
  ext = tolower(tools::file_ext(fpath[1]))
  switch(
    ext,
    "csv" = ",",
    "tsv" = "\t",
    "txt" = "\t",
    NULL
  )
}

lite_read = function(fpath, delimiter = NULL, ext = NULL) {
  # if ext not given then guess by its file type
  if (is.null(ext)) {
    ext = tolower(tools::file_ext(fpath[1]))
  }
  # if delimiter not given then guess by its file type
  if (is.null(delimiter)) {
    delimiter = smart_delimiter(fpath)
  }
  
  d = tryCatch(
    if (any(grepl("pdf|docx?|odt|rtf", ext))) {
      readtext::readtext(fpath)
    } else if(ext == "txt") {
       readtext::readtext(fpath)
    } else if(ext == "rdta" | ext == "rda") {
      as.data.frame(iNZightTools::load_rda(fpath)[[1]])
    } else if(ext == "tsv" | ext == "csv") {
      as.data.frame(iNZightTools::smart_read(fpath, delimiter = unlist(delimiter)))
    } else if(ext == "numbers") {
      stop("Not a valid file extension: ", ext)
    } else {
      as.data.frame(iNZightTools::smart_read(fpath))
    },
    error = identity
  )
  
  preview_data$preview_data = NULL
  if(is.data.frame(d)) {
    nr = min(nrow(d), 5)
    nc = min(ncol(d), 5)
    try({
      data_ext = tolower(tools::file_ext(fpath[1]))
      preview_data$fpath = fpath
      preview_data$data = d
      # ensure its a df
      preview_data$preview_data = as.data.frame(d[1:nr, 1:nc])
      preview_data$ext = ext
      preview_data$delimiter = delimiter
      preview_data$state = 0
    })
  }
}

show_preview_modal = function() {
  ext = preview_data$ext
  delimiter = preview_data$delimiter
  preview_data = preview_data$preview_data
  h3_title = ifelse(is.null(preview_data), "Failed to load data", "Preview")
  table_output = ifelse(is.null(preview_data), NULL, DT::dataTableOutput("preview_data"))
  
  select_inputs = list()
  if(!is.null(delimiter) && !(delimiter %in% c("txt", "tsv", "csv", "json"))) {
    select_inputs = fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "preview.filetype",
          label = "File type",
          selected = names(which(unlist(ext_choices) == ext)),
          choices = unique(names(ext_choices))
        )
      ),
      column(
        width = 6,
        selectInput(
          inputId = "preview.delim",
          label = "Delimiter",
          selected = "Detected automatically",
          choices = names(delim_choices)
        )
      )
    )
  }
  
  m = modalDialog(
    title = "Import file",
    h3(h3_title),
    table_output,
    hr(),
    select_inputs,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(session$ns("confirm_import"), "Confirm"),
    )
  )
  showModal(m)
}

observeEvent(c(input$preview.filetype, input$preview.delim), {
  # work around for preventing shiny rendering multiple times
  # `ignoreInit` dont seem to work
  if(preview_data$state == 0) {
    preview_data$state = preview_data$state + 1
  } else {
    delimiter = input$preview.delim
    if(input$preview.delim == "Detected automatically") {
      delimiter = preview_data$delimiter
    } else {
      delimiter = delim_choices[names(delim_choices) == input$preview.delim][1]
    }
    ext = ext_choices[names(ext_choices) == input$preview.filetype][1]
    
    lite_read(
      fpath = preview_data$fpath,
      delimiter = delimiter,
      ext = ext
    )
    show_preview_modal()
  }
})

# when user uploads a file
observeEvent(input$files, { 
  if(file.exists(input$files[1, "datapath"])) {
    # isolate({
      lite_read(fpath = input$files$datapath)
      show_preview_modal()
    # })
    }
})

# when user confirms the data in preview
observeEvent(input$confirm_import, {
  if(!is.null(preview_data$data)){
    plot.par$design = NULL
    values$data.set = preview_data$data
    values$data.type = preview_data$ext
    updatePanel$doit = updatePanel$doit+1
    values$data.restore <<- get.data.set()
    temp.name = make.names(tools::file_path_sans_ext(input$files[1, "name"]))
    
    if(length(temp.name)>1){
      temp.name = temp.name[1:(length(temp.name)-1)]
    }
    # TODO: multiple sheets
    values$data.name = temp.name
    
    # setting success status to show "Import sucessful" text
    import_reactives$success = !inherits(preview_data$data, "condition")
    
    if(!(preview_data$ext %in% c("RData", "rda", "Rda"))){
      code.save$name = temp.name
      code.save$variable = c(code.save$variable, list(c(sep(), "\n", paste0(sprintf("## Exploring the '%s' dataset", code.save$name),
                                                                            "\n"))))
      code = c(paste0(code.save$name, " <- "), gsub(paste0("\".*(?=.", preview_data$ext, ")"), paste0("\"", values$data.name), iNZightTools::code(preview_data$data), perl = T))
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
  removeModal()
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
    
    if(is.null(label_name)) {
      return(NULL)
    }

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

## "Switch data" - 'switches' to a different data set.

## loads and updates the switch data table Panel
output$paste.data.panel = renderUI({
  updatePanel$datachanged
  input$remove_set
  isolate({
    paste.data.panel(get.data.set())
  })
})

output$paste.data.info = renderUI({
  if(!is.null(get.data.set()) && !is.null(get.data.name())) {
    list(
      h3(paste("Selected data set:", get.data.name())),
      # p(paste("Selected data number of rows is: ", dim(get.data.set())[1])),
      p(paste("Selected data number of columns is: ", dim(get.data.set())[2])),
      p(paste("Column names: ", paste(colnames(get.data.set()), collapse = ", ")))
    )
  } else {
    h3("No data selected!")
  }
})

output$paste.type.delimiter = renderUI({
  ret = NULL
  if(input$paste.delimiter == "type delimiter") {
    ret = tags$div(style = "margin-top: -25px", textInput("paste.type.delimiter", label = "", value = "", width = "30%"))
  } 
  ret
})

paste.delimiter.val = reactive({
  if(req(!is.null(input$paste.delimiter))){
    if(input$paste.delimiter != "type delimiter"){
      input$paste.delimiter
    } else {
      input$paste.type.delimiter
    }
  } 
})

output$paste.view.title = renderUI({
  if(!is.null(input$paste.data.area) && input$paste.data.area != ""){
    h3("The pasted data:")
  } else {
    ""
  }
})
  

output$paste.table.preview = renderDT({
  if(req(!is.null(input$paste.data.area) && input$paste.data.area != "")){
    txt = input$paste.data.area
    if (length(txt) == 1) {
      txt <- sprintf("%s\n", txt)
    }
    
    data <- try(
      as.data.frame(stringsAsFactors = TRUE,
                    iNZightTools::read_text(
                      txt,
                      delim = paste.delimiter.val()
                    )),
      silent = TRUE
    )
    if (!inherits(data, "try-error")) {
      data
    } else {
        data.frame(stringsAsFactors = TRUE,
          Error = "Cannot parse data. Try specifying a different delimiter."
        )
    }
  }
})

observe({
  input$paste.reset
  reset("paste.delimiter")
  reset("paste.data.area")
})


observe({
  req(input$paste.load>0)
  isolate({
    if(req(!is.null(input$paste.data.area))){
      txt = input$paste.data.area
      if (length(txt) == 1) {
        txt <- sprintf("%s\n", txt)
      }
      
      data <- try(
        as.data.frame(stringsAsFactors = TRUE,
                      iNZightTools::read_text(
                        txt,
                        delim = paste.delimiter.val()
                        )),
        silent = TRUE
      )
      if (!inherits(data, "try-error")) {
        data = dplyr::mutate_if(data, is.character, as.factor)
        plot.par$design=NULL
        values$data.name = "data"
        values$data.set = as.data.frame(data)
        
        values = sample_if_lite2(rvalues = values, d = values$data.set)
        
        updatePanel$doit = updatePanel$doit+1
        values$data.restore = get.data.set()
        values$name.restore = values$data.name
        design_params$design = NULL
      }
    }
  })
})

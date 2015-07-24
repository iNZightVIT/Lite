## "Switch data" - 'switches' to a different data set.

## loads and updates the switch data table Panel
output$switch.data.panel = renderUI({
  get.data.set()
  input$remove_set
  isolate({
    switch.data.panel(get.data.set(),get.data.dir.global(),
                      get.data.dir.imported())
  })
})

output$temp_table = renderDataTable({
  if (!is.null(input[[input$data_select]])){
    if("Imported"%in%input$data_select){
      load.data(get.data.dir.imported(),
                strsplit(input[[input$data_select]],
                         "==>",fixed=T)[[1]][length(strsplit(input[[input$data_select]],
                                                             "==>",fixed=T)[[1]])])[[2]]
    }else{
      load.data(get.data.dir.global(),
                strsplit(input[[input$data_select]],
                         "==>",fixed=T)[[1]][length(strsplit(input[[input$data_select]],
                                                             "==>",fixed=T)[[1]])])[[2]]
    }
  } else {
    NULL
  }
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5,
                  columns.defaultContent = "NA", scrollX = TRUE))

set_to_change_reac <- reactive({
  if (is.null(input[[input$data_select]])){
    "No data to select!"
  } else {
    temp=NULL
    if("Imported"%in%input$data_select){
      if(!file.exists(get.data.dir.imported())&&
           file.writable(dirname(get.data.dir.imported()))){
        dir.create(get.data.dir.imported())
      }else if(!file.exists(get.data.dir.imported())&&
                 !file.writable(dirname(get.data.dir.imported()))){
        warning(paste("Directory : ",get.data.dir.imported(),
                      " : is not writable. Reset Imported dir 
                      to global dir",sep=""))
        
      }
      temp = load.data(get.data.dir.imported(),strsplit(input[[input$data_select]],"==>",fixed=T)[[1]]
                       [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])[[2]]
    }else{
      temp = load.data(get.data.dir.global(),strsplit(input[[input$data_select]],"==>",fixed=T)[[1]]
                       [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])[[2]]
    }
    if(is.null(temp[[1]])&is.null(temp[[2]])) {
      "No data to select!"
    }else{
      paste0("Data selected: ", input[[input$data_select]])  
    }
    }
})

output$set_to_change <- renderText({
  input[[input$data_select]]
  set_to_change_reac()
})

col_names_show_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Column names: ", paste(colnames(get.data.set()), collapse = ", "))
  } else {
    ""
  }
})

output$col_names_show <- renderText({
  input$change_set
  input$selector
  col_names_show_reac()
})

change_col_dim_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Selected data number of columns is: ", dim(get.data.set())[2])
  } else {
    ""
  }
})

output$col_dimension_show <- renderText({
  input$change_set
  input$selector
  input$selector
  change_col_dim_reac()
})

change_row_dim_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Selected data number of rows is: ", dim(get.data.set())[1])
  } else {
    ""
  }
})

output$row_dimension_show <- renderText({
  input$change_set
  input$selector
  change_row_dim_reac()
})

change_data_name_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Selected data set: ", get.data.name())
  } else {
    "No data selected!"
  }
})

output$data_name_show <- renderText({
  input$change_set
  input$selector
  change_data_name_reac()
})

observe({
  if (!is.null(input$change_set)) {
    isolate({
      if (!is.null(input[[input$data_select]])&&input$change_set > 0) {
        new.data = NULL
        if("Imported"%in%input$data_select){
          new.data =
            load.data(get.data.dir.imported(),strsplit(input[[input$data_select]],
                                                       "==>", fixed = TRUE)[[1]]
                      [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])
        }else{
          new.data =
            load.data(get.data.dir.global(),strsplit(input[[input$data_select]],
                                                     "==>", fixed = TRUE)[[1]]
                      [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])
        }
        values$data.name = new.data[[1]]
        values$data.set = new.data[[2]]
        values$data.restore = get.data.set()
      }
    })
  }
})
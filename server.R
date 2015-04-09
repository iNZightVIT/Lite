###-------------------------------------###
### Server Functions for iNZight Lite   ###
###-------------------------------------###
###
### Date Created : January 10, 2015
### Last Modified : March 23, 2015
###
### Please consult the comments before editing any code.
### This file sources the ui files for each panel separately.

###  We load the packages we require. This is done only ONCE per instance.
library(iNZightPlots)
library(iNZightTS)
library(markdown)
library(gpairs)

### We write the server function.
shinyServer(function(input, output, session) {
  
  ##Specify all the reactive values
  
  get.data.name = reactive({
    values$data.name
  })
  
  get.data.dir = reactive({
    values$data.dir
  })

  get.data.set = reactive({
    values$data.set
  })
  
  get.data.restore = reactive({
    values$data.restore
  })
  
  get.lite.version = reactive({
    values$lite.version
  })
  
  get.lite.update = reactive({
    values$lite.update
  })
  
  get.button = reactive({
    values$button
  })
  
  get.transform.text = reactive({
    values$transform.text
  })
  
  get.dataHasChanged = reactive({
    values$dataHasChanged
  })
  
  #################################
  
    ##  Turn errors and warnings off
    ## options(warn = -1, show.error.messages = FALSE)
    
    ##  Delete all imported files that are older than 1 day.
    ## Load all panels into memory.
    filepaths <- list.files(pattern = "[.]R$",
                            path = "gui-elements/",
                            full.names = TRUE)
    sapply(filepaths, source)
    ##---------------------##
    ##  1. "About" Module  ##
    ##---------------------##
    source("panels/1_About/1_about-panel-ui.R")
    output$about.panel <- renderUI({
        about.panel.ui(get.lite.version(),get.lite.update())
    })
    ## "Current data" - presents currently selected data to user.
    output$current.text <- renderText({
        input$selector
        if (!is.null(get.data.set())) {
            paste0("Current selected data: ", get.data.name())
        } else {
            "No data selected!"
        }
    })
    output$current.data <- renderUI({
        current.data()
    })
    output$current <- renderDataTable({
        input$selector
        get.data.set()
    }, options =
        list(lengthMenu = c(10, 30, 50), pageLength = 10,
             columns.defaultContent = "NA", scrollX = TRUE))
    ## "Switch data" - 'switches' to a different data set.
    set_to_change_reac <- reactive({
      if (is.null(input[[input$data_select]])){
        "No data to select!"
      } else {
        temp = load.data(get.data.dir(),strsplit(input[[input$data_select]],"==>",fixed=T)[[1]]
                         [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])[[2]]
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
            new.data =
                load.data(get.data.dir(),strsplit(input[[input$data_select]],
                                   "==>", fixed = TRUE)[[1]]
                          [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])
            values$data.name = new.data[[1]]
            new.data = new.data[[2]]
            values$data.set = new.data
            values$data.restore = get.data.set()
#            loaded <<- F
          }
        })
      }
    })

    ## loads and updates the switch data table Panel
    output$switch.data.panel = renderUI({
        input$selector
        input$remove_set
        isolate({
          switch.data.panel()
        })
    })

    output$temp_table = renderDataTable({
      if (!is.null(input[[input$data_select]])){
        load.data(get.data.dir(),strsplit(input[[input$data_select]],"==>",fixed=T)[[1]][length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])[[2]]
      } else {
          NULL
      }
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5,
                      columns.defaultContent = "NA", scrollX = TRUE))

    ##  Data -> load data (upload a data set)
    
    observe({
      if(!is.null(input$import_set)&&input$import_set>0){
        isolate({
          if(!is.null(input$files)&&file.exists(input$files[1, "datapath"])){
            values$data.set <<- load.data(get.data.dir(),fileID = input$files[1, "name"], path = input$files[1, "datapath"])[[2]]
            values$data.restore <<- get.data.set()
            temp = strsplit(input$files[1, "name"],".",fixed=T)[[1]]
            if(length(temp)>1){
              temp = temp[1:(length(temp)-1)]
            } 
            values$data.name = paste(temp,collapse=".")
            if (!file.exists("data/Imported")) {
              dir.create("data/Imported", recursive = TRUE)
            }
            saveRDS(get.data.set(),file = paste0("data/Imported/", get.data.name(), ".RDS"))
#             print(input$files[1, "datapath"])
#             unlink(input$files[1, "datapath"])
          }else if (!is.null(input$URLtext)&&!input$URLtext%in%""){
            URL = input$URLtext
            name = strsplit(URL,"/")[[1]]
            name = strsplit(name[length(name)],"?",fixed=T)[[1]][1]
            if (!file.exists("data/Imported")) {
              dir.create("data/Imported", recursive = TRUE)
            }
            tryCatch({
              download.file(url=URL,destfile=paste0("data/Imported/",name),method="wget")
              temp = load.data(get.data.dir(),fileID = name, path = paste0("data/Imported/",name))
              values$data.set = temp[[2]]
              values$data.restore <<- get.data.set()
              values$data.name = name
            },error = function(e){
              if(file.exists(paste0("data/Imported/",name))){
                unlink(paste0("data/Imported/",name))
              }
              print(e)
            },warning = function(w) {
              print(w)
            },finally = {})
          }
        })
      }
    })
    
    output$load.data.panel = renderUI({
        input$selector
#         input$import_set
        isolate({
          load.data.panel()
        })
    })

    output$filetable <- renderDataTable({
        input$selector
        input$files
        input$import_set
        isolate({
          if (!is.null(input$files)&&file.exists(input$files[1, "datapath"])) {
            load.data(get.data.dir(),fileID = input$files[1, "name"], path = input$files[1, "datapath"])[[2]]
          } else if (!is.null(input$URLtext)&&!input$URLtext%in%"") {
            URL = input$URLtext
            name = strsplit(URL,"/")[[1]]
            name = strsplit(name[length(name)],"?",fixed=T)[[1]][1]
            if (!file.exists("data/Imported")) {
              dir.create("data/Imported", recursive = TRUE)
            }
            if(file.exists(paste0("data/Imported/",name))){
              return(get.data.set())
            }
            NULL
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
          files = list.files(path = "data/Imported",
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

    ##  Data -> Export data (export the currently used data set)

    output$save.data.panel = renderUI({
      #input$selector
      save.data.panel(get.data.set())
    })

    output$save_table = renderDataTable({
      get.data.set()
    }, options =
      list(lengthMenu = c(5, 30, 50), pageLength = 5,
           columns.defaultContent = "NA", scrollX = TRUE))

    output$downloadData <- downloadHandler(
      filename = function() {
        print(paste(get.data.name(),".",input$select_filetype, sep=''))
        paste(get.data.name(),".",input$select_filetype, sep='')
      },
      content = function(file) {
        type = input$select_filetype
        if(type%in%"txt"&&!is.null(get.data.set())){
          write.table(get.data.set(), file, quote=F,row.names=F,sep="\t")
        }else if(type%in%"csv"&&!is.null(get.data.set())){
          write.table(get.data.set(), file, quote=F,row.names=F,sep=",")
        }else if(type%in%"RData"&&!is.null(get.data.set())){
          save(get.data.set(),file=file)
        }else if(type%in%"RDS"&&!is.null(get.data.set())){
          saveRDS(get.data.set(),file=file)
        }
      }
    )
    
    ##  Data -> remove data (Remove an imported data set)
    output$remove.data.panel <- renderUI({
      input$selector
      input$remove_set
      isolate({
        remove.data.panel()
      })
    })

    output$removetable <- renderDataTable({
      if(!is.null(input$Importedremove)){
        load.data(get.data.dir(),input$Importedremove)[[2]]
      } else {
          NULL
      }
    }, options =
        list(lengthMenu = c(5, 30, 50), pageLength = 5,
             columns.defaultContent = "NA", scrollX = TRUE))

  ##  Modify data -> transform columns (Perform column transformations)
  
  transform.temp.table = reactive({
    input$select.columns.transform
    input$select.transform
    isolate({
      transform.tempTable(get.data.set(),input$select.transform,input$select.columns.transform)
    })
  })
  
  perform.transform = reactive({
  #       input$select.columns.transform
  #       input$select.transform
    isolate({
      transform.perform(get.data.set(),
                        input$select.transform,
                        input$select.columns.transform)
    })
  })
  
  observe({
    input$transform
    isolate({
      if(!is.null(input$transform)&&input$transform>0){
        values$data.set = perform.transform()
        updateSelectInput(session, inputId="select.columns.transform", 
                          choices=colnames(get.data.set()), selected=input$select.columns.transform)
        values$dataHasChanged = T
      }
    })
  })
  
  output$table_part <- renderDataTable({
    transform.temp.table()
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
  
  output$status = renderText({
      input$transform
      input$select.columns.transform
      input$select.transform
      isolate({
        values$transform.text = ""
        if(get.dataHasChanged()){
          values$transform.text = "The transformation  of the columns was successful."
        }
        values$dataHasChanged = F
        get.transform.text()
      })
  })
  
  output$transform.columns =renderUI({
#       input$selector
  #         input$transform
      transform.data.panel(get.data.set())
  })

  ##  Row operations (Perform row operations) --> Filter Dataset
  
  observe({
    input$filter_data_perform
    isolate({
      if(!is.null(input$filter_data_perform)&&input$filter_data_perform>0){
        if(input$select_filter%in%"levels of categorical variable"){
          if(!is.null(input$select_categorical1)&&!input$select_categorical1%in%""){
            to.remove = which(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)]%in%input$levels1)
            if(length(to.remove)>0){
              values$data.set = get.data.set()[-to.remove,]
              values$data.set[,which(colnames(get.data.set())%in%input$select_categorical1)] = 
                droplevels(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)])
              updateSelectInput(session=session,inputId="select_categorical1",
                                choices=c("",get.categorical.column.names(get.data.set())),
                                selected=1)
              updateSelectInput(session=session,inputId="levels1",
                                choices="",selected=1)
            }
          }
        }else if(input$select_filter%in%"numeric condition"){
          if(!input$select_numeric1%in%""&!input$select_operation1%in%""&is.convertable.numeric(input$numeric_input1)){
            indexes.keep = 1:nrow(get.data.set())
            if(input$select_operation1%in%"<"){
              indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]<as.numeric(input$numeric_input1)))
            }else if(input$select_operation1%in%">"){
              indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]>as.numeric(input$numeric_input1)))
            }else if(input$select_operation1%in%"<="){
              indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]<=as.numeric(input$numeric_input1)))
            }else if(input$select_operation1%in%">="){
              indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]>=as.numeric(input$numeric_input1)))
            }else if(input$select_operation1%in%"=="){
              indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]==as.numeric(input$numeric_input1)))
            }else if(input$select_operation1%in%"!="){
              indexes.keep = which((get.data.set()[,which(colnames(get.data.set())%in%input$select_numeric1)]!=as.numeric(input$numeric_input1)))
            }
            values$data.set <<- get.data.set()[indexes.keep,]
          }
        }else if(input$select_filter%in%"row indices"){
          if(is.convertable.numeric(strsplit(input$row_op_indexes,",",fixed=T)[[1]])){
            indices = as.numeric(strsplit(input$row_op_indexes,",",fixed=T)[[1]])
            indices = indices[which(indices%in%(1:nrow(get.data.set())))]
            if(length(indices)>0){
              values$data.set = get.data.set()[-indices,] 
            }
          }
        }else if(input$select_filter%in%"randomly"){
          if(is.convertable.numeric(input$numeric_input2)&&
               is.convertable.numeric(input$numeric_input3)&&
               as.numeric(input$numeric_input2)<=nrow(get.data.set())&&
               (((as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))>=nrow(get.data.set())&
                   input$bootstrap_check)|
                  (as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))<=nrow(get.data.set()))){
            values$data.set = sample.data(df=get.data.set(),
                                 sampleSize=as.numeric(input$numeric_input2),
                                 numSample=as.numeric(input$numeric_input3),
                                 bootstrap=input$bootstrap_check)
          }
        }
      }
    })
  })

  output$message3 = renderPrint({
    input$numeric_input2
    input$numeric_input3
    input$bootstrap_check
    isolate({
      if(is.convertable.numeric(input$numeric_input2)&&
           is.convertable.numeric(input$numeric_input3)&&
           as.numeric(input$numeric_input2)<=nrow(get.data.set())&&
           (((as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))>=nrow(get.data.set())&
               input$bootstrap_check)|
              (as.numeric(input$numeric_input2)*as.numeric(input$numeric_input3))<=nrow(get.data.set()))){
        cat("Size of sample: ",input$numeric_input2,"\n",
            "Number of sample: ", input$numeric_input3)
      }else{
        cat("This input can not be processed. The data has ",
            nrow(get.data.set())," rows.")
      }
    })
  })

  observe({
    input$select_categorical1
    isolate({
      if(!is.null(input$select_categorical1)){
        updateSelectInput(session=session,inputId="levels1",
                             choices=levels(get.data.set()[,which(colnames(get.data.set())%in%input$select_categorical1)]))
      }
    })
  })

  output$message2 = renderPrint({
    valid = is.convertable.numeric(strsplit(input$row_op_indexes,",",fixed=T)[[1]])
    isolate({
      if(!valid){
        cat("Please provide a comma seperated list of indices.")
      }else{
        cat("")
      } 
    })
  })

  output$message1 = renderPrint({
    input$select_numeric1
    input$select_operation1
    input$numeric_input1
    isolate({
      if(!is.convertable.numeric(input$numeric_input1)){
        cat("Please provide a numeric variable.")
      }else{
        cat(input$select_numeric1,input$select_operation1,input$numeric_input1)
      } 
    })
  })

  output$filter.data.summary <- renderPrint({
    input$selector
    input$filter_data_perform
    isolate({
      data.summary(get.data.set())
    })
  })
  
  output$filter.dataset = renderUI({
#     input$selector
    filter.data.panel(get.data.set())
  })

  ##  Row operations (Perform row operations) --> Sort data by variables
  
  observe({
    input$sort_vars
    isolate({
      if(!is.null(input$sort_vars)&&input$sort_vars>0){
        indexes1= grep("^sort[0-9]+$",names(input))
        vars = unlist(lapply(indexes1,function(i,nams){
          input[[nams[i]]]
        },names(input)))
        indexes2 = grep("^increasing[0-9]+$",names(input))
        sort.type = unlist(lapply(indexes2,function(i,nams){
          input[[nams[i]]]
        },names(input)))
        if(anyDuplicated(vars)){
          dups = which(duplicated(vars))
          vars = vars[-dups]
          sort.type =sort.type[-dups]
        }
        if(""%in%vars){
          empties = which(vars%in%"")
          vars = vars[-empties]
          sort.type =sort.type[-empties]
        }
        values$data.set <<- sort.data(vars,sort.type,get.data.set())
      }
    })
  })

  output$sort.table = renderDataTable({
    input$selector
    input$sort_vars
    get.data.set()
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

  output$sort.variables = renderUI({
    input$selector
    isolate({
      sort.variables(get.data.set())
    })
  })
  
  output$num.select = renderUI({
    input$num_columns_sort
    isolate({
      num.select.panel(input$num_columns_sort,get.data.set())
    })
  })

  ##  Row operations (Perform row operations) --> Aggregate data

  observe({
    input$aggregate_vars
    isolate({
      if(!is.null(input$aggregate_vars)&&input$aggregate_vars>0){
        vars = input$aggros
        rem = which(vars%in%"")
        if(length(rem)>0){
          vars = vars[-rem]
        }
        methods = input$aggregate.method
        rem  = which(methods%in%"")
        if(length(rem)>0){
          methods = methods[-rem]
        }
        if(length(vars)>0&length(methods)>0&!is.null(get.data.set())){
          values$data.set = aggregate.data(aggregate.over=unique(vars),methods=methods,dafr=get.data.set())
          updateSelectInput(session,"aggros",selected=0,choices=get.categorical.column.names(get.data.set()))
          updateSelectInput(session,"aggregate.method",selected=0)
        }
      }
    })
  })

  output$aggregate.variable = renderUI({
    aggregate.variable(get.data.set())
  })

  output$aggregate.table = renderDataTable({
    get.data.set()
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, 
                 columns.defaultContent="NA",scrollX=T))

  ##  Row operations (Perform row operations) --> Stack variables
  
  observe({
    input$stack_vars
    isolate({
      if(!is.null(input$stack_vars)&&input$stack_vars>0&&
           !is.null(input$stack_vars_column)){
        values$data.set = stack.variables.perform(input$stack_vars_column,get.data.set())
        updateSelectInput(session,"stack_vars_which",selected=0)
        updateSelectInput(session,inputId="stack_vars_column",
                          choices=get.categorical.column.names(get.data.set()),
                          selected=0)
      }
    })
  })

  observe({
    input$stack_vars_which
    isolate({
      if(!is.null(input$stack_vars_which)&&!""%in%input$stack_vars_which){
        if("categorical"%in%input$stack_vars_which){
          updateSelectInput(session,inputId="stack_vars_column",
                            choices=get.categorical.column.names(get.data.set()),
                            selected=1)
        }else{
          updateSelectInput(session,inputId="stack_vars_column",
                            choices=get.numeric.column.names(get.data.set()),
                            selected=1)
        }
      }
    })
  })

  output$stack.table = renderDataTable({
#     input$selector
#     input$stack_vars
    get.data.set()
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

  output$stack.variables = renderUI({
#     input$selector
    stack.variables.panel(get.data.set())
  })

  ##  Row operations (Perform row operations) --> Restore data

  observe({
    input$restore_data_button
    isolate({
      if(!is.null(input$restore_data_button)&&input$restore_data_button>0){
        values$data.set = get.data.restore()
      }
    })
  })
  
  output$data.restore.table = renderDataTable({
    input$restore_data_button
    get.data.set()
  },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

  output$restore.data = renderUI({
    restore.data.panel(get.data.set())
  })

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

#   output$reorder.levels.side = renderUI({
#     input$selector
#     isolate({
#       if(input$categorical_variables_select%in%"Reorder levels"&
#            input$selector%in%"Categorical variables"){
#         reorder.sidebar.panel()
#       }
#     })
#   })

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
        values$data.set = reorder.levels(get.data.set(),input$select.reorder.column,levels.new)
        updateSelectInput(session=session,inputId="select.reorder.item",selected="",choices="")
        updateSelectInput(session=session,inputId="select.reorder.column",selected="")
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

#   output$collapse.levels.side = renderUI({
#     input$selector
#     isolate({
#       if(input$categorical_variables_select%in%"Collapse levels"&
#            input$selector%in%"Categorical variables"){
#         collapse.sidebar.panel()
#       }
#     })
#   })

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
      if(!is.null(input$collapse)&&input$collapse>0&&input$select.collapse.column%in%colnames(get.data.set())){
        if(any(input$select.collapse.item%in%get.data.set()[,which(colnames(get.data.set())%in%input$select.collapse.column)])){
          values$data.set()[,which(colnames(get.data.set())%in%input$select.collapse.column)] = get.collapsed.column(
            get.data.set()[,which(colnames(get.data.set())%in%input$select.collapse.column)],
            input$select.collapse.item)
          updateSelectInput(session,"select.collapse.column",selected=1)
        }
      }
    })
  })

  ## Manipulate variables --> Categorical variables --> Rename levels

#   output$rename.levels.side = renderUI({
#     input$selector
#     isolate({
#       if(input$categorical_variables_select%in%"Rename levels"&
#            input$selector%in%"Categorical variables"){
#         rename.levels.sidebar.panel()
#       }
#     })
#   })

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
        values$data.set <<- rename.levels(get.data.set(),input$select.rename.column,new.levels)
        updateSelectInput(session,"select.rename.column",selected=0)
      }
    })
  })

  ## Manipulate variables --> Categorical variables --> Combine levels

#   output$combine.levels.side = renderUI({
#     input$selector
#     isolate({
#       if(input$categorical_variables_select%in%"Combine categorical"&
#            input$selector%in%"Categorical variables"){
#         combine.sidebar.panel()
#       }
#     })
#   })

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
        values$data.set = combine.levels(get.data.set(),input$select.combine.columns)
      }
    })
  })

#     ##  modify -> Reorder levels : reorder the levels of a column of factors
#     selection.changed = observe({
#         if(!is.null(input$select.column)){
#             choices=""
#             if(!"" %in% input$select.column){
#                 if(is.factor(get.data.set()[,input$select.column])){
#                     choices = levels(get.data.set()[,input$select.column])
#                 }else{
#                     choices = levels(as.factor(get.data.set()[,input$select.column]))
#                 }
#             }
#             updateSelectInput(session=session,inputId="select.item",selected="",choices=choices)
#         }
#     })
# 
# 
#     output$maintext.reorder = renderPrint({
#         text = ""
#         if(!is.null(input$select.column)&&!""%in%input$select.column){
#             print(table(get.data.set()[,input$select.column]))
#         }else{
#             print("Select a column!")
#         }
#     })
# 
#     output$reorder.levels =renderUI({
#         input$selector
#         reorder.levels.panel()
#     })

    ## modify -> compare dates : reorder the levels of a column of factors

#     observe({
#         input$compare_dates
#         if(!is.null(get.data.set())&&!is.null(input$compare_dates)&&input$compare_dates>0){
#             isolate({
#                 columns = input$sel.compare.dates[1:2]
#                 if(!is.null(columns)&&columns[1]!=""){
#                     temp.col = NULL
#                     for(col in 1:length(columns)){
#                         if(col==1){
#                             tryCatch({
#                                 temp.col = as.numeric(as.Date(get.data.set()[,columns[col]], origin = "1900-01-01"))
#                             },
#                                      error=function(cond) {
#                                          temp.col = NULL
#                                      },
#                                      warning=function(cond) {
#                                          print(cond)
#                                      },
#                                      finally={})
#                         }else{
#                             tryCatch({
#                                 temp.col = temp.col - as.numeric(as.Date(get.data.set()[,columns[col]], origin = "1900-01-01"))
#                             },
#                                      error=function(cond) {
#                                          temp.col = NULL
#                                      },
#                                      warning=function(cond) {
#                                          print(cond)
#                                      },
#                                      finally={})
#                         }
#                     }
#                     if(!is.null(temp.col)){
#                         count=1
#                         while(paste0("date",count)%in%colnames(get.data.set())){
#                             count = count+1
#                         }
#                         values$data.set <<- cbind(get.data.set(),date.column.temp=round(temp.col,2))
#                         colnames(get.data.set())[which(colnames(get.data.set())%in%"date.column.temp")] <<- paste0("date",count)
#                         updateSelectInput(session=session,inputId="sel.compare.dates",selected="",choices=colnames(get.data.set()))
#                     }
#                 }
#             })
#         }
#     })
# 
#     output$comp.dates.table = renderDataTable({
#         columns = input$sel.compare.dates[1:2]
#         ret = NULL
#         if(!is.null(get.data.set())){
#             ret = get.data.set()[,test.for.dates(get.data.set())]
#             if(ncol(ret)==0){
#                 ret = NULL
#             }
#             if(!is.null(columns)&&columns[1]!=""){
#                 temp.col = NULL
#                 for(col in 1:length(columns)){
#                     if(col==1){
#                         tryCatch({
#                             temp.col = as.numeric(as.Date(get.data.set()[,columns[col]], origin = "1900-01-01"))
#                         },
#                                  error=function(cond) {
#                                      temp.col = NULL
#                                  },
#                                  warning=function(cond) {
#                                      print(cond)
#                                  },
#                                  finally={})
#                     }else{
#                         tryCatch({
#                             temp.col = temp.col - as.numeric(as.Date(get.data.set()[,columns[col]], origin = "1900-01-01"))
#                         },
#                                  error=function(cond) {
#                                      temp.col = NULL
#                                  },
#                                  warning=function(cond) {
#                                      print(cond)
#                                  },
#                                  finally={})
#                     }
#                 }
#                 if(!is.null(temp.col)){
#                     ret = cbind(ret,temp.date=round(temp.col,2))
#                 }
#             }
#         }
#         ret
#     },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
# 
#     output$compare.dates = renderUI({
#         input$selector
#         compare.dates.panel()
#     })

    ##  modify -> add columns : paste in data to add as additional column.
    observe({
        input$add_column
        isolate({
          temp=get.data.set()
          if(!is.null(get.data.set())&&!is.null(input$new.column)&&input$add_column>0){
              colu = strsplit(input$new.column,"\n",fixed=T)[[1]]
              if(length(colu)==1){
                  colu = strsplit(input$new.column,",",fixed=T)[[1]]
              }
              if(length(colu)<nrow(get.data.set())){
                  colu = rep(colu,length.out=nrow(get.data.set()))
              }
              if(length(colu)>nrow(get.data.set())){
                  colu = colu[1:nrow(get.data.set())]
              }
              NAs = which(is.na(colu))
              if(length(NAs)>0&&length(colu[-NAs])>0){
                  temp.colu = as.numeric(colu[-NAs])
                  if(!any(is.na(temp.colu))){
                      colu = as.numeric(colu)
                  }
              }
              count = 1
              name = "add.column1"
              while(name%in%colnames(get.data.set())){
                  count =  count +1
                  name = paste0("add.column",count)
              }
              temp = cbind(get.data.set(),temp.column=colu)
              colnames(temp)[which(colnames(temp)%in%"temp.column")] = name
              temp
          }
          values$data.set <<- temp
          updateTextInput(session, inputId="new.column", value="")
        })
    })

    output$add.table = renderDataTable({
        input$selector
        input$new.column
        input$add_column
        isolate({
          temp=get.data.set()
          if(!is.null(get.data.set())&&!is.null(input$new.column)){
              colu = strsplit(input$new.column,"\n",fixed=T)[[1]]
              if(length(colu)==1){
                  colu = strsplit(input$new.column,",",fixed=T)[[1]]
              }
              if(length(colu)<nrow(get.data.set())){
                  colu = rep(colu,length.out=nrow(get.data.set()))
              }
              if(length(colu)>nrow(get.data.set())){
                  colu = colu[1:nrow(get.data.set())]
              }
              NAs = which(is.na(colu))
              if(length(NAs)>0&&length(colu[-NAs])>0){
                  temp.colu = as.numeric(colu[-NAs])
                  if(!any(is.na(temp.colu))){
                      colu = as.numeric(colu)
                  }
              }
              count = 1
              name = "add.column1"
              while(name%in%colnames(get.data.set())){
                  count =  count +1
                  name = paste0("add.column",count)
              }
              temp = cbind(get.data.set(),temp.column=colu)
              colnames(temp)[which(colnames(temp)%in%"temp.column")] = name
              temp
          }
          temp
        })
    },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

    output$add.columns = renderUI({
        add.columns.panel()
    })

    ##  modify -> remove columns : remove selected columns from the data.

    observe({
        input$rem_column
        if(!is.null(get.data.set())&&!is.null(input$rem_column)&&input$rem_column>0){
            isolate({
                if(length(which(colnames(get.data.set())%in%input$select.remove.column))>0){
                    values$data.set = as.data.frame(get.data.set()[,-which(colnames(get.data.set())%in%input$select.remove.column)])
                    if(ncol(get.data.set())==0){
                        values$data.set <<- NULL
                        updateSelectInput(session, inputId="select.remove.column",choices=c(""),selected="")
                    }else{
                        updateSelectInput(session, inputId="select.remove.column", choices=colnames(get.data.set()),selected="")
                    }
                }
            })
        }
    })

    output$rem.col.table = renderDataTable({
        input$selector
        input$rem_column
        isolate({
          temp = get.data.set()
          if(!is.null(get.data.set())&&!is.null(input$select.remove.column)){
              temp = as.data.frame(get.data.set()[,-which(colnames(get.data.set())%in%input$select.remove.column)])
              if(ncol(temp)==0){
                  temp=NULL
              }
          }
          temp
        })
    },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

    output$remove.columns = renderUI({
        remove.columns.panel(get.data.set())
    })

    ##--------------------##
    ##  Visualize Module  ##
    ##--------------------##
    source("panels/5_Visualize/1_visualize-panel-ui.R", local = TRUE)
    source("panels/5_Visualize/2_visualize-panel-server.R", local = TRUE)
    output$visualize.panel <- renderUI({
        visualize.panel.ui(get.data.set())
    })

#   Advanced --> Time Series

    ##----------------------##
    ##  Time Series Module  ##
    ##----------------------##
    source("panels/6_TimeSeries/1_timeseries-panel-ui.R", local = TRUE)
    source("panels/6_TimeSeries/2_timeseries-panel-server.R", local = TRUE)
    output$timeseries.panel <- renderUI({
        input$selector
        timeseries.panel.ui(get.data.set())
    })

#   Advanced --> Quick explore

  output$quick.explore = renderUI({
    quick.explore.panel(get.data.set())
  })

#   Advanced --> Quick explore --> Data summary

  output$quick.summary.side = renderUI({
    get.quick.summary.sidebar(get.data.set())
  })
  
  output$quick.summary.main = renderUI({
    get.quick.summary.main()
  })

  output$all.summary = renderPrint({
    data.summary(get.data.set())
  })
  
  output$column.summary = renderPrint({
    if(!is.null(input$select.column.sum)){
      temp = get.data.set()[,which(colnames(get.data.set())%in%input$select.column.sum)]
      if(is.character(temp)){
        print(as.factor(temp))
        cat("\n\t")
        print(summary(as.factor(temp)))
      }else{
        print(summary(temp))
      }
    }else{
      NULL
    }
  })

#   Advanced --> Quick explore --> Single Column plot

  output$single.column.plot.side= renderUI({
#     input$selector
    get.single.col.sidebar(get.data.set())
  })

  output$single.column.plot.main= renderUI({
#     input$selector
    get.single.col.main(get.data.set())
  })

  observe({
    input$single.backward
    isolate({
      if(!is.null(input$single.backward)&&input$single.backward>0){
        index=1
        if(which(colnames(get.data.set())%in%input$select.column.plot)==1){
          index=ncol(get.data.set())
        }else{
          index = which(colnames(get.data.set())%in%input$select.column.plot)-1
        }
        updateSelectInput(session,inputId="select.column.plot",choices=colnames(get.data.set()),selected=colnames(get.data.set())[index])
        updateSliderInput(session,inputId="single.play",value=index)
      }
    })
  })

  observe({
    input$single.forward
    isolate({
      if(!is.null(input$single.forward)&&input$single.forward>0){
        index=1
        if(which(colnames(get.data.set())%in%input$select.column.plot)==ncol(get.data.set())){
          index=1
        }else{
          index = which(colnames(get.data.set())%in%input$select.column.plot)+1
        }
        updateSelectInput(session,inputId="select.column.plot",choices=colnames(get.data.set()),selected=colnames(get.data.set())[index])
        updateSliderInput(session,inputId="single.play",value=index)
      }
    })
  })

  observe({
    input$single.play
    isolate({
      if(!is.null(input$single.play)){
        updateSelectInput(session,inputId="select.column.plot",choices=colnames(get.data.set()),selected=colnames(get.data.set())[input$single.play])
      }
    })
  })

  output$column.plot = renderPlot({
    input$select.column.plot
    isolate({
      if(!is.null(get.data.set())&&!is.null(input$select.column.plot)){
        index=which(colnames(get.data.set())%in%input$select.column.plot)
        if(length(index)==0){
          index = 1
        }
        updateSliderInput(session,inputId="single.play",value=index)
        temp = get.data.set()[,index]
        if(is.character(temp)){
            temp = as.factor(temp)
        }
        iNZightPlot(temp,xlab=input$select.column.plot,main=get.data.name())
      }
    })
  })

#   Advanced --> Quick explore --> Column pair plot

  output$column.pair.plot.side = renderUI({
    get.pair.plot.sidebar(get.data.set())
  })

  output$column.pair.plot.main = renderUI({
    get.pair.plot.main(get.data.set())
  })

  observe({
    input$pair.player
    isolate({
      if(!is.null(input$pair.player)){
        indMat = rbind(1:(ncol(get.data.set())*(ncol(get.data.set())-1)),
                       rep(1:(ncol(get.data.set())-1),ncol(get.data.set())),
                       ceiling(seq(from=0.1,to=ncol(get.data.set()),by=1/(ncol(get.data.set())-1))))
        index1 = indMat[3,input$pair.player]
        index2 = indMat[2,input$pair.player]
        values$button = T
        updateSelectInput(session,inputId="select.column.plot1",selected=colnames(get.data.set())[index1],choices=colnames(get.data.set()))
        updateSelectInput(session,inputId="select.column.plot2",selected=colnames(get.data.set())[-index1][index2],
                          choices=colnames(get.data.set())[-index1])
      }
    })
  })

  observe({
    input$pair.backward
    isolate({
      if(!is.null(input$pair.backward)&&input$pair.backward>0){
        index1 = which(colnames(get.data.set())%in%input$select.column.plot1)
        index2 = which(colnames(get.data.set())[-index1]%in%input$select.column.plot2)
        if(index2==1){
          if(index1==1){
            index1 = ncol(get.data.set())
          }else{
            index1 = index1-1
          }
          values$button = T
          updateSelectInput(session,inputId="select.column.plot1",selected=colnames(get.data.set())[index1],choices=colnames(get.data.set()))
          index2 = ncol(get.data.set())-1
        }else{
          index2 = index2-1
        }
        updateSelectInput(session,inputId="select.column.plot2",selected=colnames(get.data.set())[-index1][index2],
                          choices=colnames(get.data.set())[-index1])
        matInd = which(colnames(get.data.set())%in%colnames(get.data.set())[-index1][index2])
        updateSliderInput(session,inputId="pair.player",
                          value=matrix(c(unlist(lapply(seq(from=ncol(get.data.set()),by=ncol(get.data.set()),
                                                           to=ncol(get.data.set())*(ncol(get.data.set())-1)),function(x,n){
                                                             c(0,(x-(n-1)):x)
                                                           },
                                                       ncol(get.data.set()))),0),nrow=ncol(get.data.set()))[matInd,index1]
                          )
      }
    })
  })

    observe({
      input$pair.forward
      isolate({
        if(!is.null(input$pair.forward)&&input$pair.forward>0){
          index1 = which(colnames(get.data.set())%in%input$select.column.plot1)
          index2 = which(colnames(get.data.set())[-index1]%in%input$select.column.plot2)
          if(index2==(ncol(get.data.set())-1)){
            if(index1==ncol(get.data.set())){
              index1 = 1
            }else{
              index1 = index1+1
            }
            values$button = T
            updateSelectInput(session,inputId="select.column.plot1",selected=colnames(get.data.set())[index1],choices=colnames(get.data.set()))
            index2 = 1
          }else{
            index2 = index2+1
          }
          updateSelectInput(session,inputId="select.column.plot2",selected=colnames(get.data.set())[-index1][index2],
                            choices=colnames(get.data.set())[-index1])
          matInd = which(colnames(get.data.set())%in%colnames(get.data.set())[-index1][index2])
          updateSliderInput(session,inputId="pair.player",
                            value=matrix(c(unlist(lapply(seq(from=ncol(get.data.set()),by=ncol(get.data.set()),
                                                             to=ncol(get.data.set())*(ncol(get.data.set())-1)),function(x,n){
                                                               c(0,(x-(n-1)):x)
                                                             },
                                                         ncol(get.data.set()))),0),nrow=ncol(get.data.set()))[matInd,index1]
          )
        }
      })
    })

    observe({
        input$select.column.plot1
        isolate({
          choice = input$select.column.plot1
          if(!is.null(choice)){
            i = input$select.column.plot2
            if(input$select.column.plot1==input$select.column.plot2){
              i = which(colnames(get.data.set())%in%input$select.column.plot1)
              if(i>(ncol(get.data.set())-1)){
                i=1
              }
            }
            ch = colnames(get.data.set())[-which(colnames(get.data.set())%in%choice)]
            if(!get.button()){
              updateSelectInput(session,"select.column.plot2",choices=ch,
                                selected=ch[i])  
            }
            values$button = F
          }
        })
    })

  output$plot.column.pair = renderPlot({
    if(!is.null(input$select.column.plot1)&&!is.null(input$select.column.plot2)&&
         !""%in%input$select.column.plot1&&!""%in%input$select.column.plot2){
      index1 = which(colnames(get.data.set())%in%input$select.column.plot1)
      index2 = which(colnames(get.data.set())%in%input$select.column.plot2)
      if(length(index1)==0){
        index1 = 1
      }
      if(length(index2)==0){
        if(index1+1>ncol(get.data.set())){
          index2 = 1
        }else{
          index2 = index1 + 1
        }
      }
      x = get.data.set()[,index1]
      y = get.data.set()[,index2]
      iNZightPlot(x,y,xlab=input$select.column.plot1,ylab=input$select.column.plot2,main=get.data.name())
    }
  })

#   Advanced --> Quick explore --> Compare pairs

  output$compare.pairs.main = renderUI({
    get.matrix.main(get.data.set())
  })

  output$compare.pairs.side = renderUI({
    get.matrix.sidebar(get.data.set())
  })

  output$plot.matrix = renderPlot({
      choices = input$select.matrix.plot
      if(is.null(choices)||length(choices)==1){
          plot(1, 1, type = "n", axes = FALSE, xlab = "" , ylab = "")
          text(1, 1, "You have to select more than 1 variable", cex = 2)
      }else{
          choices.ind = which(colnames(get.data.set()) %in% choices)
          temp =
              do.call(cbind,
                      lapply(get.data.set()[, choices.ind],
                             function(x) {
                                 if (is.character(x)) {
                                     data.frame(factor(x, levels = unique(x)))
                                 } else {
                                     data.frame(x)
                                 }
                             }))
          colnames(temp) = choices
          gpairs(temp)
      }
  })

#   Help

    ##---------------##
    ##  Help Module  ##
    ##---------------##
    source("panels/7_Help/1_help-panel-ui.R", local = TRUE)
    output$help.panel <- renderUI({
        input$selector
        help.panel.ui(get.lite.version(),get.lite.update())
    })
})

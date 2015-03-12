###-------------------------------------###
###  Server Functions for iNZight Lite  ###
###-------------------------------------###
###
###  Date Created   :   January 10, 2015
###  Last Modified  :   February 25, 2015 
###
###  Please consult the comments before editing any code.
###  This file sources the ui files for each panel separately.
###
###  If you have any questions and/or suggestions, please drop me an
###  e-mail: Chris Park <cpar137@aucklanduni.ac.nz>

###  We load the packages we require. This is done only ONCE per instance.
library(iNZightPlots)
library(iNZightTS)
library(markdown)
library(gpairs)


###  We write the server function.
shinyServer(function(input, output, session) {
    ##  Turn errors and warnings off
    ## options(warn = -1, show.error.messages = FALSE)
    
    ##  Delete all imported files that are older than 1 day.
    delete.old.files(1) # global.R

    ##  Load all panels into memory.
    filepaths <- list.files(pattern = "[.]R$",
                            path = "gui-elements/",
                            full.names = TRUE)
    sapply(filepaths, source)


    ##---------------------##
    ##  1. "About" Module  ##
    ##---------------------##
    source("panels/1_About/1_about-panel-ui.R")
    output$about.panel <- renderUI({
        about.panel.ui()
    })
    
    ##  "Current data" - presents currently selected data to user.
    output$current.text <- renderText({
        input$selector
        if (!is.null(data)) {
            paste0("Current selected data: ", data.name)
        } else {
            "No data selected!"
        }
    })

    output$current.data <- renderUI({
        current.data()
    })

    output$current <- renderDataTable({
        input$selector
        data
    }, options =
        list(lengthMenu = c(10, 30, 50), pageLength = 10,
             columns.defaultContent = "NA", scrollX = TRUE))

    ##  "Switch data" - 'switches' to a different data set.
    set_to_change_reac <- reactive({
      if (is.null(input[[input$data_select]])){
        "No data to select!"
      } else {
        temp = load.data(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]]
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
        if (!is.null(data) && !is.null(data.name)) {
            paste("Column names: ", paste(colnames(data), collapse = ", "))
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
        if (!is.null(data) && !is.null(data.name)) {
            paste("Selected data number of columns is: ", dim(data)[2])
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
        if (!is.null(data) && !is.null(data.name)) {
            paste("Selected data number of rows is: ", dim(data)[1])
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
        if (!is.null(data) && !is.null(data.name)) {
            paste("Selected data set: ", data.name)
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
                load.data(strsplit(input[[input$data_select]],
                                   "==>", fixed = TRUE)[[1]]
                          [length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])
            data.name <<- new.data[[1]]
            new.data = new.data[[2]]
            data <<- new.data
            loaded <<- F
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
        load.data(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]][length(strsplit(input[[input$data_select]],"==>",fixed=T)[[1]])])[[2]]
      } else {
          NULL
      }
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5,
                      columns.defaultContent = "NA", scrollX = TRUE))
    
    observe({
      if(!is.null(input$import_set)&&input$import_set>0){
        isolate({
          if(!is.null(input$files)&&file.exists(input$files[1, "datapath"])){
            data <<- load.data(fileID = input$files[1, "name"], path = input$files[1, "datapath"])[[2]]
            data.name <<- input$files[1, "name"]
            if (!file.exists("data/Imported")) {
              dir.create("data/Imported", recursive = TRUE)
            }
            saveRDS(data,file = paste0("data/Imported/", data.name, ".RDS"))
          }
        })
      }
    })

    ##  Data -> load data (upload a data set)
    output$load.data.panel = renderUI({
        input$selector
        input$import_set
        isolate({
          load.data.panel()
        })
    })

    output$filetable <- renderDataTable({
        input$selector
        input$files
        isolate({
          if (is.null(input$files)||!file.exists(input$files[1, "datapath"])) {
              ## If user hasn't uploaded a file yet
              return(NULL)
          }
          if (file.exists(input$files[1, "datapath"])) {
            load.data(fileID = input$files[1, "name"], path = input$files[1, "datapath"])[[2]]
          } else {
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
          if(!is.null(input$files)&&file.exists(input$files[1, "datapath"])){
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
        load.data(input$Importedremove)[[2]]
      } else {
          NULL
      }
    }, options =
        list(lengthMenu = c(5, 30, 50), pageLength = 5,
             columns.defaultContent = "NA", scrollX = TRUE))

    ##  Modify data -> transform columns (Perform column transformations)
    
    transform.temp.table = reactive({
      input$select.columns
      input$select.transform
      isolate({
        transform.tempTable(input$select.transform,input$select.columns)
      })
    })
    
    perform.transform = reactive({
#       input$select.columns
#       input$select.transform
      isolate({
        transform.perform(input$select.transform,input$select.columns)
      })
    })
    
    observe({
      input$transform
      isolate({
        if(!is.null(input$transform)&&input$transform>0){
          data <<- perform.transform()
          updateSelectInput(session, inputId="select.columns", 
                            choices=colnames(data), selected=input$select.columns)
          dataHasChanged <<- T
        }
      })
    })
    
    output$table_part <- renderDataTable({
      transform.temp.table()
    },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))
    
    output$status = renderText({
        input$transform
        input$select.columns
        input$select.transform
        isolate({
          transform.text = ""
          if(dataHasChanged){
            transform.text = "The transformation  of the columns was successful."
          }
          dataHasChanged <<- F
          transform.text
        })
    })

    output$transform.columns =renderUI({
        input$selector
#         input$transform
        transform.data.panel()
    })


    ##  modify -> Reorder levels : reorder the levels of a column of factors
    selection.changed = observe({
        if(!is.null(input$select.column)){
            choices=""
            if(!"" %in% input$select.column){
                if(is.factor(data[,input$select.column])){
                    choices = levels(data[,input$select.column])
                }else{
                    choices = levels(as.factor(data[,input$select.column]))
                }
            }
            updateSelectInput(session=session,inputId="select.item",selected="",choices=choices)
        }
    })

    button.pressed = observe({
        input$reorder
        updateSelectInput(session=session,inputId="select.item",selected="",choices="")
        updateSelectInput(session=session,inputId="select.column",selected="")
        isolate({
            items = input$select.item
            if(!is.null(items)&!is.null(input$select.column)){
                column = data[,input$select.column]
                if(length(items)<length(column)){
                    not.in = sort(unique(data[,input$select.column])[which(!unique(data[,input$select.column])%in%items)])
                    levels.new = c(items,not.in)
                }else{
                    levels.new = c(items)
                }
                data[,input$select.column] <<- as.factor(data[,input$select.column])
                levels(data[,input$select.column]) <<- levels.new
            }
        })
    })

    output$maintext.reorder = renderPrint({
        text = ""
        if(!is.null(input$select.column)&&!""%in%input$select.column){
            print(table(data[,input$select.column]))
        }else{
            print("Select a column!")
        }
    })

    output$reorder.levels =renderUI({
        input$selector
        reorder.levels.panel()
    })

    ## modify -> compare dates : reorder the levels of a column of factors

    observe({
        input$compare_dates
        if(!is.null(data)&&!is.null(input$compare_dates)&&input$compare_dates>0){
            isolate({
                columns = input$sel.compare.dates[1:2]
                if(!is.null(columns)&&columns[1]!=""){
                    temp.col = NULL
                    for(col in 1:length(columns)){
                        if(col==1){
                            tryCatch({
                                temp.col = as.numeric(as.Date(data[,columns[col]], origin = "1900-01-01"))
                            },
                                     error=function(cond) {
                                         temp.col = NULL
                                     },
                                     warning=function(cond) {
                                         print(cond)
                                     },
                                     finally={})
                        }else{
                            tryCatch({
                                temp.col = temp.col - as.numeric(as.Date(data[,columns[col]], origin = "1900-01-01"))
                            },
                                     error=function(cond) {
                                         temp.col = NULL
                                     },
                                     warning=function(cond) {
                                         print(cond)
                                     },
                                     finally={})
                        }
                    }
                    if(!is.null(temp.col)){
                        count=1
                        while(paste0("date",count)%in%colnames(data)){
                            count = count+1
                        }
                        data <<- cbind(data,date.column.temp=round(temp.col,2))
                        colnames(data)[which(colnames(data)%in%"date.column.temp")] <<- paste0("date",count)
                        updateSelectInput(session=session,inputId="sel.compare.dates",selected="",choices=colnames(data))
                    }
                }
            })
        }
    })

    output$comp.dates.table = renderDataTable({
        columns = input$sel.compare.dates[1:2]
        ret = NULL
        if(!is.null(data)){
            ret = data[,test.for.dates()]
            if(ncol(ret)==0){
                ret = NULL
            }
            if(!is.null(columns)&&columns[1]!=""){
                temp.col = NULL
                for(col in 1:length(columns)){
                    if(col==1){
                        tryCatch({
                            temp.col = as.numeric(as.Date(data[,columns[col]], origin = "1900-01-01"))
                        },
                                 error=function(cond) {
                                     temp.col = NULL
                                 },
                                 warning=function(cond) {
                                     print(cond)
                                 },
                                 finally={})
                    }else{
                        tryCatch({
                            temp.col = temp.col - as.numeric(as.Date(data[,columns[col]], origin = "1900-01-01"))
                        },
                                 error=function(cond) {
                                     temp.col = NULL
                                 },
                                 warning=function(cond) {
                                     print(cond)
                                 },
                                 finally={})
                    }
                }
                if(!is.null(temp.col)){
                    ret = cbind(ret,temp.date=round(temp.col,2))
                }
            }
        }
        ret
    },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

    output$compare.dates = renderUI({
        input$selector
        compare.dates.panel()
    })

    ##  modify -> add columns : paste in data to add as additional column.
    observe({
        input$add_column
        isolate({
          temp=data
          if(!is.null(data)&&!is.null(input$new.column)&&input$add_column>0){
              colu = strsplit(input$new.column,"\n",fixed=T)[[1]]
              if(length(colu)==1){
                  colu = strsplit(input$new.column,",",fixed=T)[[1]]
              }
              if(length(colu)<nrow(data)){
                  colu = rep(colu,length.out=nrow(data))
              }
              if(length(colu)>nrow(data)){
                  colu = colu[1:nrow(data)]
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
              while(name%in%colnames(data)){
                  count =  count +1
                  name = paste0("add.column",count)
              }
              temp = cbind(data,temp.column=colu)
              colnames(temp)[which(colnames(temp)%in%"temp.column")] = name
              temp
          }
          data <<- temp
          updateTextInput(session, inputId="new.column", value="")
        })
    })

    output$add.table = renderDataTable({
        input$selector
        input$new.column
        input$add_column
        isolate({
          temp=data
          if(!is.null(data)&&!is.null(input$new.column)){
              colu = strsplit(input$new.column,"\n",fixed=T)[[1]]
              if(length(colu)==1){
                  colu = strsplit(input$new.column,",",fixed=T)[[1]]
              }
              if(length(colu)<nrow(data)){
                  colu = rep(colu,length.out=nrow(data))
              }
              if(length(colu)>nrow(data)){
                  colu = colu[1:nrow(data)]
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
              while(name%in%colnames(data)){
                  count =  count +1
                  name = paste0("add.column",count)
              }
              temp = cbind(data,temp.column=colu)
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
        if(!is.null(data)&&!is.null(input$rem_column)&&input$rem_column>0){
            isolate({
                if(length(which(colnames(data)%in%input$select.remove.column))>0){
                    data <<- as.data.frame(data[,-which(colnames(data)%in%input$select.remove.column)])
                    if(ncol(data)==0){
                        data <<- NULL
                        updateSelectInput(session, inputId="select.remove.column",choices=c(""),selected="")
                    }else{
                        updateSelectInput(session, inputId="select.remove.column", choices=colnames(data),selected="")
                    }
                }
            })
        }
    })

    output$rem.col.table = renderDataTable({
        input$selector
        input$rem_column
        isolate({
          temp = data
          if(!is.null(data)&&!is.null(input$select.remove.column)){
              temp = as.data.frame(data[,-which(colnames(data)%in%input$select.remove.column)])
              if(ncol(temp)==0){
                  temp=NULL
              }
          }
          temp
        })
    },options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))

    output$remove.columns = renderUI({
        input$selector
        remove.columns.panel()
    })

    ## Quick Explore -> Data summary : display a quick summary of the data
    output$all.summary = renderPrint({
        if(!is.null(data)){
            for(col in 1:length(colnames(data))){
                cat(colnames(data)[col],"\n")
                print(summary(data[,col]))
            }
        }
    })

    output$column.summary = renderPrint({
        if(!is.null(input$select.column.sum)){
            temp = data[,which(colnames(data)%in%input$select.column.sum)]
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

    output$quick.summary = renderUI({
        input$selector
        quick.summary.panel()
    })

    ##  Quick Explore -> Single column plot : Generate a plot of a single column in the data
    
    observe({
      input$single.backward
      isolate({
        if(!is.null(input$single.backward)&&input$single.backward>0){
          index=1
          if(which(colnames(data)%in%input$select.column.plot)==1){
            index=ncol(data)
          }else{
            index = which(colnames(data)%in%input$select.column.plot)-1
          }
          updateSelectInput(session,inputId="select.column.plot",choices=colnames(data),selected=colnames(data)[index])
          updateSliderInput(session,inputId="single.play",value=index)
        }
      })
    })

    observe({
      input$single.forward
      isolate({
        if(!is.null(input$single.forward)&&input$single.forward>0){
          index=1
          if(which(colnames(data)%in%input$select.column.plot)==ncol(data)){
            index=1
          }else{
            index = which(colnames(data)%in%input$select.column.plot)+1
          }
          updateSelectInput(session,inputId="select.column.plot",choices=colnames(data),selected=colnames(data)[index])
          updateSliderInput(session,inputId="single.play",value=index)
        }
      })
    })

    observe({
      input$single.play
      isolate({
        if(!is.null(input$single.play)){
          updateSelectInput(session,inputId="select.column.plot",choices=colnames(data),selected=colnames(data)[input$single.play])
        }
      })
    })

    output$column.plot = renderPlot({
      input$select.column.plot
      isolate({
        if(!is.null(data)&&!is.null(input$select.column.plot)){
          index=which(colnames(data)%in%input$select.column.plot)
          updateSliderInput(session,inputId="single.play",value=index)
          temp = data[,which(colnames(data)%in%input$select.column.plot)]
          if(is.character(temp)){
              temp = as.factor(temp)
          }
          iNZightPlot(temp,xlab=input$select.column.plot,main=data.name)
        }
      })
    })

    output$single.column.plot = renderUI({
        input$selector
        single.column.plot.panel()
    })
    ##  Quick Explore -> Column pair plot : Generate a plot of all possible pairs of columns

    observe({
        input$select.column.plot1
        choice = input$select.column.plot1
        if(!is.null(choice)){
            updateSelectInput(session,"select.column.plot2",choices=colnames(data)[-which(colnames(data)%in%choice)])
        }
    })

    output$plot.column.pair = renderPlot({
        if(!is.null(input$select.column.plot1)&&!is.null(input$select.column.plot2)&&
           !""%in%input$select.column.plot1&&!""%in%input$select.column.plot2){
            x = data[,which(colnames(data)%in%input$select.column.plot1)]
            y = data[,which(colnames(data)%in%input$select.column.plot2)]
            iNZightPlot(x,y,xlab=input$select.column.plot1,ylab=input$select.column.plot2,main=data.name)
        }
    })

    output$column.pair.plot = renderUI({
        input$selector
        column.pair.plot.panel()
    })

    ##  Quick Explore -> Compare pairs : Generate plots of all possible pairs of columns
    output$plot.matrix = renderPlot({
        choices = input$select.matrix.plot
        if(is.null(choices)||length(choices)==1){
            plot(1, 1, type = "n", axes = FALSE, xlab = "" , ylab = "")
            text(1, 1, "You have to select more than 1 variable", cex = 2)
        }else{
            choices.ind = which(colnames(data) %in% choices)
            temp =
                do.call(cbind,
                        lapply(data[, choices.ind],
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

    output$matrix.plot = renderUI({
        input$selector
        matrix.plot.panel()
    })

    ##--------------------##
    ##  Visualize Module  ##
    ##--------------------##
    source("panels/5_Visualize/1_visualize-panel-ui.R", local = TRUE)
    source("panels/5_Visualize/2_visualize-panel-server.R", local = TRUE)
    output$visualize.panel <- renderUI({
        input$selector
        visualize.panel.ui()
    })

    ##----------------------##
    ##  Time Series Module  ##
    ##----------------------##
    source("panels/6_TimeSeries/1_timeseries-panel-ui.R", local = TRUE)
    source("panels/6_TimeSeries/2_timeseries-panel-server.R", local = TRUE)
    output$timeseries.panel <- renderUI({
        input$selector
        timeseries.panel.ui()
    })

    ##---------------##
    ##  Help Module  ##
    ##---------------##
    source("panels/7_Help/1_help-panel-ui.R", local = TRUE)
    output$help.panel <- renderUI({
        input$selector
        help.panel.ui()
    }) 

})

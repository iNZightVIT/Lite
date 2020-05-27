output$frequency.tables = renderUI({
  frequency.tables.panel(get.data.set())
})


##  Row operations (Perform row operations) --> Restore data

observe({
  input$expand_table_button
  isolate({
    tryCatch({
      if(!is.null(input$expand_table_button)&&input$expand_table_button>0){
        dat <- get.data.set()
        dat <- tryCatch({as.numeric(rownames(dat)); dat},
                        warning = function(w) {
                          ## cannot convert rownames to numeric - create column
                          dat$Row <- rownames(dat)
                          dat
                        })
        numIndices <- sapply(dat, function(x) iNZightTools::is_num(x))
        long <- reshape2:::melt.data.frame(
          dat, measure.vars = colnames(dat)[numIndices],
          variable.name = "Column", value.name = "Count", na.rm = TRUE)
        out <- long[rep(rownames(long), long$Count), ]
        rownames(out) <- 1:nrow(out)
        ## for 1-way tables, don't need the "Count" column!
        if (length(unique(out$Column)) == 1)
          out$Column <- NULL
        out$Count <- NULL
        
        
        updatePanel$datachanged = updatePanel$datachanged+1
        values$data.set = out
        values$data.name = "data"
      }
    }, error = function(e){})
  })
})

output$ft.expand.data.table = renderDataTable({
  input$expand_table_button
  get.data.set()
},options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent="NA",scrollX=T))





observe({  
  input$expand_table_button 
  isolate({
    updateSelectInput(session, "subs2", selected = "none")
    updateSelectInput(session, "subs1", selected = "none")
    updateSelectInput(session, "vari2", selected = "none")
    updateSelectInput(session, "vari1", selected = "none")
  })  
})

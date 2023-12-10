##  Data -> Export data (export the currently used data set)



output$save.data.panel = renderUI({
  if (!is.null(session$userData$LITE_VERSION) &&
      session$userData$LITE_VERSION == "CAS") {
    return(
      mainPanel(
        h4("This feature is not available with the current configuration.")
      )
    )
  }
  save.data.panel(get.data.set())
})

output$save_table = renderDT({
  get.data.set()
}, options =
  list(lengthMenu = c(5, 30, 50), pageLength = 5,
       columns.defaultContent = "NA", scrollX = TRUE))

output$downloadData <- downloadHandler(
  filename = function() {
    #         print(paste(get.data.name(),".",input$select_filetype, sep=''))
    paste(get.data.name(),".",input$select_filetype, sep='')
  },
  content = function(file) {
    type = input$select_filetype
    if(type%in%"txt"&&!is.null(get.data.set())){
      write.table(get.data.set(), file, quote=T,row.names=F,sep="\t")
    }else if(type%in%"csv"&&!is.null(get.data.set())){
      write.table(get.data.set(), file, quote=T,row.names=F,sep=",")
    }else if(type%in%"RData"&&!is.null(get.data.set())){
      save(get.data.set(),file=file)
    }else if(type%in%"RDS"&&!is.null(get.data.set())){
      saveRDS(get.data.set(),file=file)
    }
  }
)

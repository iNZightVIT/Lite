##  Manipulate variables -> remove columns : remove selected columns from the data.

observe({
  input$rem_column
  if(!is.null(get.data.set())&&!is.null(input$rem_column)&&input$rem_column>0){
    isolate({
      if(length(which(colnames(get.data.set())%in%input$select.remove.column))>0){
        temp = as.data.frame(get.data.set()[,-which(colnames(get.data.set())%in%input$select.remove.column)])
        if(!is.null(temp)){
          updatePanel$datachanged = updatePanel$datachanged+1
          values$data.set = temp
          if(ncol(get.data.set())==0){
            values$data.set = NULL
            updateSelectInput(session, inputId="select.remove.column",choices=c(""),selected="")
          }else{
            updateSelectInput(session, inputId="select.remove.column", choices=colnames(get.data.set()),selected="")
          }
        }
      }
    })
  }
})

output$rem.col.table = renderDataTable({
  temp = get.data.set()
  input$rem_column
  isolate({
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
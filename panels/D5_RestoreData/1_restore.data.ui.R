restore.data.sidebar =  function(){
  list(
    actionButton("restore_data_button","Restore data"),br(),br(),
    help.display('Restore data','restore_data_help',
                 "panels/D5_RestoreData/3_restore.data.help.md"),
    br())
}

restore.data.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Restore data','restore_data_help',
                                "panels/D5_RestoreData/3_restore.data.help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(restore.data.sidebar()),
      mainPanel(dataTableOutput("data.restore.table"))
    )
  }
}
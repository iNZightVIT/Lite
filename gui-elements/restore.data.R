restore.data.sidebar =  function(){
  list(
    actionButton("restore_data_button","Restore data"),br(),br(),
    help.display('Restore data','restore_data_help',"gui-elements/notes/restore.data.md"),
    br())
}

restore.data.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Restore data','restore_data_help',"gui-elements/notes/restore.data.md")),
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
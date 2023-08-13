restore.data.sidebar =  function(){
  list(
    helpText("The data set has been restored to the way when it was initially imported."),
    br())
}
# TODO: check
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
      mainPanel(
        textOutput('data.restore.data.sample.info'), br(), br(),
        DTOutput("data.restore.table"))
    )
  }
}
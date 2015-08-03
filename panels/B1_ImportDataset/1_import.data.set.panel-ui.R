get.sidebar.load = function(url_load){
  if(is.null(url_load$url)){
    url_load=""
  }
  list(
    fileInput("files", "File data", multiple=F),
    helpText("The button above will import a data set from a file and will make it
             available to other users of this instance. Otherwise, paste a URL in
             the text field below to import a file from a web resource. Press the
             \"Import file\" button below to finalize your choice"),
    br(),textInput(inputId="URLtext",label="URL",value=url_load),br(),
    actionButton(inputId="import_set",label="Import file",),
    br(),br(),help.display('Load data','load_data',"panels/B1_ImportDataset/3_import.data.set.panel.help.md")
  )
}

load.data.panel = function(url_load){
  sidebarLayout(
    sidebarPanel(get.sidebar.load(url_load)),
    mainPanel(dataTableOutput("filetable")
    )
  )
}
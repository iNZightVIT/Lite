get.sidebar.load = function(){
  list(
    fileInput("files", "File data", multiple=F),
    helpText("The button above will import a data set from a file and will make it
             available to other users of this instance. Otherwise, paste a URL in
             the text field below to import a file from a web resource. Press the
             \"Import file\" button below to finalize your choice"),
    br(),textInput(inputId="URLtext",label="URL"),br(),
    actionButton(inputId="import_set",label="Import file",),
    br(),br(),help.display('Load data','load_data',"gui-elements/notes/load.data.md")
  )
}

load.data.panel = function(){
  sidebarLayout(
    sidebarPanel(get.sidebar.load()),
    mainPanel(dataTableOutput("filetable")
    )
  )
}
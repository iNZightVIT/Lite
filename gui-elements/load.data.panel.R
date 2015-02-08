help.load = function(){
  helpModal('Load data','load_data',inclMD("gui-elements/notes/load.data.md"))
}

get.sidebar.load = function(){
  list(
    fileInput("files", "File data", multiple=F),
    helpText("The button below will import the data set and will make it available to other users of this instance."),
    br(),br(),
    actionButton(inputId="import_set",label="Import file",),
    br(),br(),help.load(),"HELP"
  )
}

load.data.panel = function(){
  sidebarLayout(
    sidebarPanel(get.sidebar.load()),
    mainPanel( dataTableOutput("filetable")
    )
  )
}
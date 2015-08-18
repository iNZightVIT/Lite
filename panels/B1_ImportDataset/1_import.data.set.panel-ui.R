get.sidebar.load = function(url_load){
  if(is.null(url_load$url)){
    url_load=""
  }
  list(
    helpText("Select a file,"),
    fileInput("files",label="", multiple=F),
    helpText("or provide a URL pointing to a file on a web location"),
    br(),textInput(inputId="URLtext",label="paste/enter URL",value=url_load),br(),
    helpText("Finalise your choice by pressing the button below."),
    actionButton(inputId="import_set",label="Import file"),br(),br(),
    verbatimTextOutput("message.success"),
    br(),help.display('Load data','load_data',"panels/B1_ImportDataset/3_import.data.set.panel.help.md")
  )
}

load.data.panel = function(url_load){
  sidebarLayout(
    sidebarPanel(get.sidebar.load(url_load)),
    mainPanel(dataTableOutput("filetable")
    )
  )
}
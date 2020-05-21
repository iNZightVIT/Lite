get.sidebar.load = function(url_load){
  if(is.null(url_load$url)){
    url_load=""
  }
  list(
    HTML("Please let us know if you have difficulty importing data, if you can include information about the operating system, browser and a copy of the data that would be extremely helpful. <br/> Email: inzightlite_support@stat.auckland.ac.nz<br/>"),
    hr(),
    helpText("Select a file (Size Limit: 5MB),"),
    fileInput("files",label="", multiple=F),
    helpText("or provide a URL pointing to a file on a web location"),
    br(),textInput(inputId="URLtext",label="paste/enter URL",value=url_load),br(),
    helpText("Finalise your choice by pressing the button below."),
    actionButton(inputId="import_set",label="Import file from url"),br(),br(),
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
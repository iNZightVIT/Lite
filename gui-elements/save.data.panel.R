
get.sidebar.save = function(){
  list(
    selectInput(inputId="select_filetype",label="select the file type",choices=c("txt","csv","RData","RDS"),selected=1),
    downloadButton('downloadData', 'Download'),
    br(),br(),help.display('Export data','export_data',"gui-elements/notes/export.data.md")
  )
}

save.data.panel = function(){
  m.panel = NULL
  s.panel = NULL
  if(is.null(data)){
    m.panel = h1("Please Import or select a data set.")
  }else{
    s.panel = get.sidebar.save()
    m.panel = dataTableOutput("save_table")
  }
  sidebarLayout(
    sidebarPanel(s.panel),
    mainPanel(m.panel)
  )
}
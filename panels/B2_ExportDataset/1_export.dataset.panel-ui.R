get.sidebar.save = function(){
  list(
    selectInput(inputId="select_filetype",label="select the file type",
                choices=c("txt","csv","RData","RDS"),selected=1),
    downloadButton('downloadData', 'Download'),
    br(),br(),help.display('Export data','export_data',
                           "panels/B2_ExportDataset/3_export.dataset.panel-help.md"))
}

save.data.panel = function(data.set){
  m.panel = NULL
  s.panel = NULL
  if(is.null(data.set)){
    s.panel = help.display('Export data','export_data',
                           "panels/B2_ExportDataset/3_export.dataset.panel-help.md")
    m.panel = h1("Please Import or select a data set.")
  }else{
    s.panel = get.sidebar.save()
    m.panel = DTOutput("save_table")
  }
  sidebarLayout(
    sidebarPanel(s.panel),
    mainPanel(m.panel)
  )
}

# help.remove = function(){
#   helpModal('Remove data','remove_data',inclMD("gui-elements/notes/remove.data.md"))
# }

# puts together a list of shiny widgets to fill the sidebar
get.sidebar.imported = function(data.path){
  ret = list()
  radio.list = get.radio.list(paste0(data.path,"/Imported"),"remove")
  if(!is.null(radio.list)){
    ret[[1]] = radio.list
  }
  ret
}

remove.data.panel = function(data.path){
  if(!file.exists(paste0(data.path,"/Imported"))){
    dir.create(paste0(data.path,"/Imported"),recursive=T)
  }
  sidebar.widgets = get.sidebar.imported(data.path)
  if(length(sidebar.widgets)==0){
    sidebarLayout(
      sidebarPanel(help.display('Remove data','remove_data',"gui-elements/notes/remove.data.md"),br(),HTML("&nbsp;")),
      mainPanel(h1("No data set to delete!"))
    )
  }else{
    sidebarLayout(
      sidebarPanel(sidebar.widgets,
                   actionButton(inputId="remove_set",label="Remove Set"),
                   br(),br(),help.display('Remove data','remove_data',"gui-elements/notes/remove.data.md")
                   ,br(),HTML("&nbsp;")),
      mainPanel(
        dataTableOutput("removetable")
        )
    )
  }
}
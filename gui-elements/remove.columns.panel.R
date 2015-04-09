# help.add = function(){
#   helpModal('Remove columns','rem_columns',inclMD("gui-elements/notes/remove.columns.md"))
# }

get.sidebar.remove= function(data.set){
  choices = c()
  if(!is.null(data.set)){
    choices = colnames(data.set)
  }
  list(
    helpText("Select columns to remove. The table on the left shows what the data will look like when the 
             \"Remove columns\" button is pressed. If the selection is empty, there is no columns to be 
             deleted anymore."),
    selectInput("select.remove.column","Select Column",choices=c("",choices),multiple=T,selectize=T,selected=""),br(),
    actionButton(inputId="rem_column",label="Remove columns",),
    br(),br(),help.display('Remove columns','rem_columns',"gui-elements/notes/remove.columns.md")
  )
}

remove.columns.panel = function(data.set){
  sidebarLayout(
    sidebarPanel(get.sidebar.remove(data.set)),
    mainPanel( dataTableOutput("rem.col.table")
    )
  )
}
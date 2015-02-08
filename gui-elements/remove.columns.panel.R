help.add = function(){
  helpModal('Remove columns','rem_columns',inclMD("gui-elements/notes/remove.columns.md"))
}

get.sidebar.remove= function(){
  choices = c()
  if(!is.null(data)){
    choices = colnames(data)
  }
  list(
    helpText("Select columns to remove. The table on the left shows what the data will look like when the 
             \"Remove columns\" button is pressed. If the selection is empty, there is no columns to be 
             deleted anymore."),
    selectInput("select.remove.column","Select Column",choices=c("",choices),multiple=T,selectize=T,selected=""),br(),
    actionButton(inputId="rem_column",label="Remove columns",),
    br(),br(),help.remove(),"HELP"
  )
}

remove.columns.panel = function(){
  sidebarLayout(
    sidebarPanel(get.sidebar.remove()),
    mainPanel( dataTableOutput("rem.col.table")
    )
  )
}


get.sidebar.remove= function(data.set){
  choices = c()
  if(!is.null(data.set)){
    choices = colnames(data.set)
  }
  list(
    h5(strong("Select variables to delete")),
    selectInput("select.remove.column",
                NULL,
                choices = c("",choices),
                multiple = T,
                selectize = FALSE,
                selected = "",
                size = 7),
    br(),
    actionButton(inputId="rem_column", label="Delete",
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    br(),br(),help.display('Remove columns',
                           'rem_columns',
                           "panels/E8_RemoveColumns/3_remove.columns.help.md")
  )
}

remove.columns.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Remove columns',
                                'rem_columns',
                                "panels/E8_RemoveColumns/3_remove.columns.help.md")),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(get.sidebar.remove(data.set)),
      mainPanel(DTOutput("rem.col.table"))
    )
  }
}
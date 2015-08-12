##  help.add = function(){
##  helpModal('Add a column','add_columns',inclMD("gui-elements/notes/add.columns.md"))
##  }
get.sidebar.add = function(){
  list(
    helpText("Paste in a comma seperated list or have all values on a new line to add 
             as column. If the list is shorter than there are rows in the current data 
             set, the list of values is repeated until length equals the number of rows 
             in the data. If the list is longer it will be shortened to the number of 
             rows in the data. To have all values on a new line is preferered. 
             Therefore if there are commas inside your values use the second method."),
    tags$textarea(id="new.column", rows=5, cols=35, ""),br(),
    checkboxInput("convert.numeric",label="Numeric input"),br(),
    actionButton(inputId="add_column",label="Add as column",),br(),br(),
    help.display('Add a column','add_columns',"gui-elements/notes/add.columns.md")
  )
}

add.columns.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Add a column','add_columns',"gui-elements/notes/add.columns.md")),
      mainPanel(h1("Please select or import a data set."))
    )  
  }else{
    sidebarLayout(
      sidebarPanel(get.sidebar.add()),
      mainPanel( dataTableOutput("add.table"))
    )  
  }
}
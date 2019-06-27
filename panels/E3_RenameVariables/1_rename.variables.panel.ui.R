
rename.variables.sidebar = function(){
  list(uiOutput("rename_variables_two_columns"),
       
       actionButton("rename_variables_two_columns","Rename",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
}


rename.variables.panel = function(){
  if(is.null(get.data.set())){
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(rename.variables.sidebar()),
      mainPanel(dataTableOutput("rename.variables.table"))
    )
  }
}
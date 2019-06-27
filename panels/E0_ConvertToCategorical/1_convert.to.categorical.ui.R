
convert.to.categorical.sidebar =  function(){
  list(h5(strong("Select a numeric variable to create a categorical version of that variable")),
       
       selectInput(inputId = "select.to.convert",
                   label = "Select variable",
                   choices = c("", get.numeric.column.names(get.data.set())),
                   multiple = F,
                   selectize = FALSE),
       
       textInput("convert_to_name",
                 label = "Type name for the new variable",
                 value = ""),
       
       actionButton("convert_to_categorical_button","Update data",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
}


convert.to.categorical.panel = function(){
  if(is.null(get.data.set())){
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(convert.to.categorical.sidebar()),
      mainPanel(dataTableOutput("convert.to.categorical.table"))
    )
  }
}




# help.modify = function(){
#   helpModal('Modify data','transform_columns',inclMD("gui-elements/notes/transform.explanation.md"))
# }

get.transform.sidebar =  function(){
  list(selectInput("select.columns.transform", "Select Columns", choices = c("",colnames(data)),multiple=T,selectize=T),br(),
       selectInput("select.transform", "Select Transformation", 
                   choices = c("", "change to factor","add","subtract","multiply","divide","log","root","square","abs","center",
                               "standardize","median split","reverse-coding","copy","change sign"),
                   multiple=F,selectize=F),br(),
       actionButton("transform","Transform"),br(),br(),textOutput("status"),br(),br(),
       help.display('Modify data','transform_columns',"gui-elements/notes/transform.explanation.md"),br())
}

transform.data.panel =function(){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(help.display('Modify data','transform_columns',"gui-elements/notes/transform.explanation.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
        sidebarPanel(get.transform.sidebar()),
        mainPanel(dataTableOutput(outputId="table_part"))
    )
  }
}

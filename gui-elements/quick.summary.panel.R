# help.quick.summary = function(){
#   helpModal('Quick summary','quick_summary',inclMD("gui-elements/notes/quick.summary.md"))
# }

get.quick.summary.sidebar =  function(){
  choices1 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = colnames(data)
  }else{
    choices2=c()
  }
  list(helpText("Select a column from the dropdown menu to display a short column summary."),
       selectInput("select.column.sum","Select Column",choices=choices1,multiple=F,selectize=T,selected=choices1[1]),
       br(),br(),help.display('Quick summary','quick_summary',"gui-elements/notes/quick.summary.md"),br(),HTML(""))
}
get.quick.summary.main = function(){
  list(mainPanel(verbatimTextOutput("all.summary"),
            verbatimTextOutput("column.summary")))
}

quick.summary.panel =function(choices=c(),selected=""){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(get.quick.sidebar()),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(get.quick.sidebar()),
      mainPanel(verbatimTextOutput("all.summary"),
                verbatimTextOutput("column.summary"))
    )
  }
}
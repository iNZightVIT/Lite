get.quick.summary.sidebar =  function(data.set){
  choices1 = c()
  if(!is.null(data.set)&&!is.null(ncol(data.set))&&ncol(data.set)>0){
    choices1 = colnames(data.set)
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
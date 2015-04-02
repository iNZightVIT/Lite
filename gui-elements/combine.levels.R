combine.sidebar.panel =  function(){
  choices1 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = get.categorical.column.names()
  }else{
    choices1 = c()
  }
  list(helpText("Select factor columns to combine. All combined 
                columns will be added as additional column to 
                the data."),
       selectInput("select.combine.columns", "Select columns to combine",choices=choices1,multiple=T,selectize=T),br(),
       actionButton("combine","Combine levels"),br(),br(),
       help.display('Combine Levels','combine_levels',"gui-elements/notes/combine.levels.md"),br(),HTML(""))
}

combine.main.panel = function(){
  list(helpText("New levels after input is submitted"),br(),
       div(verbatimTextOutput("text_combine")))
}
reorder.sidebar.panel =  function(){
  choices1 = c()
  choices2 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = get.categorical.column.names()
  }else{
    choices1 = c()
  }
  list(helpText("Select a column from the first dropdown menu. The second dropdown menu will be filled when a column is 
                selected. Select from the second dropdown menu in the desired order. Numeric values are ignored, 
                therefore columns of type numeric can not be selected. Please convert those to factors first. See 
                \"HELP\" below for more information."),
       selectInput("select.reorder.column","Select Column",choices=c("",choices1),multiple=F,selectize=F,selected=1),br(),
       selectInput("select.reorder.item", "Select in new Order",choices=choices2,multiple=T,selectize=T),br(),
       actionButton("reorder","Reorder"),br(),br(),
       help.display('Reorder Levels','reorder_levels',"gui-elements/notes/reorder.levels.md"),br(),HTML(""))
}

reorder.main.panel = function(){
  verbatimTextOutput(outputId="text_reorder")
}
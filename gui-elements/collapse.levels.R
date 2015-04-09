collapse.sidebar.panel =  function(data.set){
  choices1 = c()
  choices2 = c()
  if(!is.null(data.set)&&!is.null(ncol(data.set))&&ncol(data.set)>0){
    choices1 = get.categorical.column.names(data.set)
  }else{
    choices1 = c()
  }
  list(helpText("Select a column from the first dropdown menu. The second 
                  dropdown menu will be filled when a column is selected. 
                  Select from the second dropdown menu all factors which 
                  should be collapsed into one. Numeric values are ignored, 
                  therefore columns of type numeric can not be selected. 
                  Please convert those to factors first."),
       selectInput("select.collapse.column","Select Column",choices=c("",choices1),multiple=F,selectize=F,selected=1),br(),
       selectInput("select.collapse.item", "Select levels to collapse",choices=choices2,multiple=T,selectize=T),br(),
       actionButton("collapse","Collapse levels"),br(),br(),
       help.display('Collapse Levels','collapse_levels',"gui-elements/notes/collapse.levels.md"),br(),HTML(""))
}

collapse.main.panel = function(){
  list(div(verbatimTextOutput("text_collapse_1st")),br(),
       helpText("New levels after input is submitted"),
       div(verbatimTextOutput("text_collapse_2nd")))
}
rename.levels.sidebar.panel =  function(){
  choices1 = c()
  choices2 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = get.categorical.column.names()
  }else{
    choices1 = c()
  }
  list(helpText("Select a column from the first dropdown menu. As many input 
                variable will appear as there are factors in the selected 
                column. Rename the factors using the text files next to it."),
       selectInput("select.rename.column","Select Column",
                   choices=c("",choices1),multiple=F,selectize=F,selected=1),
       br(),
       uiOutput(outputId="rename.factors.inputs"),
       actionButton("rename.levs","Rename levels"),br(),br(),
       help.display('Rename Levels','rename_levels',"gui-elements/notes/rename.levels.md"),br(),HTML(""))
}

rename.levels.main.panel = function(){
  verbatimTextOutput("text_rename")
}

rename.factors.textfields = function(factors){
  ret = list()
  for(fac in 1:length(factors)){
    ret[[fac]] = textInput(inputId=paste0("factor",fac),label=factors[fac],value=factors[fac])
  }
  ret
}
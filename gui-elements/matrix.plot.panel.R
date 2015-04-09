get.matrix.sidebar =  function(data.set){
  if(is.null(data.set)){
    help.display('Pair matrix plot',
                 'pair_matrix_plot',
                 "gui-elements/notes/matrix.plot.md")
  }else{
    choices1 = c()
    if(!is.null(data.set)&&!is.null(ncol(data.set))&&ncol(data.set)>0){
      choices1 = colnames(data.set)
    }else{
      choices2=c()
    }
    list(helpText("Select a column from the dropdown 
                  menu to display all posible pair 
                  combination plots."),
         selectInput("select.matrix.plot","Select Column",
                     choices=choices1,multiple=T,selectize=T,
                     selected=choices1[1]),
         br(),br(),help.display('Pair matrix plot',
                                'pair_matrix_plot',
                                "gui-elements/notes/matrix.plot.md"),
         br(),HTML(""))  
  }
}

get.matrix.main = function(data.set){
  if(is.null(data.set)){
    h1("Please select or import a data set.")
  }else{
    tabPanel(NULL,plotOutput("plot.matrix", height = 700, width = 700))
  }
}

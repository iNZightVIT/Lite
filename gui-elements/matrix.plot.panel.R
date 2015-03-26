get.matrix.sidebar =  function(){
  if(is.null(data)){
    help.display('Pair matrix plot',
                 'pair_matrix_plot',
                 "gui-elements/notes/matrix.plot.md")
  }else{
    choices1 = c()
    if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
      choices1 = colnames(data)
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

get.matrix.main = function(){
  if(is.null(data)){
    h1("Please select or import a data set.")
  }else{
    tabPanel(NULL,plotOutput("plot.matrix", height = 700, width = 700))
  }
}

# matrix.plot.panel =function(){
#   if(is.null(data)){
#     sidebarLayout(
#       sidebarPanel(get.matrix.sidebar()),
#       mainPanel(h1("Please select or import a data set."))
#     )
#   }else{
#     sidebarLayout(
#       sidebarPanel(get.matrix.sidebar()),
#       mainPanel(tabPanel(NULL,plotOutput("plot.matrix", height = 700, width = 700)))#plotOutput("plot.matrix"))#,width="100%",height="100%"))
#     )
#   }
# }
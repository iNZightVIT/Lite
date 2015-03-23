# help.column.pair.plot = function(){
#   helpModal('Column pair plot','column_pair_plot',inclMD("gui-elements/notes/column.pair.plot.md"))
# }

get.pair.plot.sidebar =  function(){
  choices1 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = colnames(data)
  }else{
    choices1=c()
  }
  list(helpText("Select a column from the first and second drop down menu to display a appropriate plot of the selected columns."),
       selectInput("select.column.plot1","Select Column 1",choices=choices1,multiple=F,selectize=T,selected=choices1[1]),
       selectInput("select.column.plot2","Select Column 2",choices=choices1[-1],,multiple=F,selectize=T,selected=choices1[-1][1]),
       br(),br(),help.display('Column pair plot','column_pair_plot',"gui-elements/notes/column.pair.plot.md"),br(),HTML(""))
}

column.pair.plot.panel =function(){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(get.pair.plot.sidebar()),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(get.pair.plot.sidebar()),
      mainPanel(plotOutput("plot.column.pair"),
                get.player(ID.forward="pair.forward",ID.player="pair.player",ID.backward="pair.backward",maxi=ncol(data)*(ncol(data)-1)))
    )
  }
}


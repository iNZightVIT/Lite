get.single.col.sidebar <- function(){
    choices1 = ""
    if (!is.null(data) && !is.null(ncol(data)) && ncol(data) > 0){
        choices1 = colnames(data)
    } else {
        choices2 = ""
    }
    list(helpText("Select a column from the dropdown menu to display a
                  approproate plot of the selected column."),
         selectInput("select.column.plot", "Select Column", choices = choices1,
                     multiple = FALSE, selectize = TRUE,
                     selected = choices1[1]),
         br(),
         help.display('Single column plot','single_column_plot',"gui-elements/notes/single.column.plot.md"),
         br(),
         HTML(""))
}

single.column.plot.panel =function(){
  if(is.null(data)){
    sidebarLayout(
        sidebarPanel(get.single.col.sidebar()),
            mainPanel(h1("Please select or import a data set."))
    )
    }else{
    sidebarLayout(
        sidebarPanel(get.single.col.sidebar()),
            mainPanel(plotOutput("column.plot"),
                get.player(ID.backward="single.backward",ID.player="single.play",
                           ID.forward="single.forward",maxi=ncol(data))
            )
    )   
    }
}

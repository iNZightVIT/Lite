help.single.column.plots <- function() {
    helpModal('Single column plot',
              'single_column_plot',
              inclMD("gui-elements/notes/single.column.plot.md"))
}

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
         br(),
         help.single.column.plots(),
         "HELP",
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
      mainPanel(plotOutput("column.plot"))#,width="100%",height="100%"))
    )
  }
}

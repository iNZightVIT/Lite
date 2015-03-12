# help.single.column.plots <- function() {
#     helpModal('Single column plot',
#               'single_column_plot',
#               inclMD("gui-elements/notes/single.column.plot.md"))
# }

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
                fixedRow(column(width=8,offset=2,
                                div(class='player',
                                    fixedRow(
                                      column(width=1,offset=1,
                                             div(class="seper",actionButton(inputId="single.backward",label="",icon=icon("backward")))),
                                      column(width=6,offset=1,
                                             sliderInput(inputId="single.play",label="",min=1,max=ncol(data),step=1,
                                                         animate=animationOptions(interval=500,loop=T,play=T),
                                                         width="100%",value=1,ticks=F)),
#                                       column(width=1,offset=1,
#                                              actionButton(inputId="single.play",label="",icon=icon("play"))),
#                                       column(width=1,offset=0,
#                                              actionButton(inputId="single.pause",label="",icon=icon("pause"))),
                                      column(width=1,offset=1,
                                             div(class="seper",actionButton(inputId="single.forward",label="",icon=icon("forward"))))
                                      ))))
      )
    )
  }
}

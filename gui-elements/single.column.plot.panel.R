
get.single.col.sidebar <- function(data.set){
  if(is.null(data.set)){
    help.display('Single column plot',
                 'single_column_plot',
                 "gui-elements/notes/single.column.plot.md")
  }else{
    choices1 = ""
    if (!is.null(data.set) && !is.null(ncol(data.set)) && ncol(data.set) > 0){
      choices1 = colnames(data.set)
    } else {
      choices2 = ""
    }
    list(helpText("Select a column from the dropdown menu to display a
                appropriate plot of the selected column."),
         selectInput("select.column.plot", "Select Column", choices = choices1,
                     multiple = FALSE, selectize = TRUE,
                     selected = choices1[1]),
         br(),
         help.display('Single column plot','single_column_plot',
                      "gui-elements/notes/single.column.plot.md"),
         br(),
         HTML(""))
  }
}

get.single.col.main <- function(data.set){
  if(is.null(data.set)){
    h1("Please select or import a data set.")
  }else{
    list(plotOutput("column.plot"),
         get.player(ID.backward="single.backward",
                    ID.player="single.play",
                    ID.forward="single.forward",
                    maxi=ncol(data.set)))
  }
}


# single.column.plot.panel =function(){
#   if(is.null()){
#     sidebarLayout(
#       sidebarPanel(get.single.col.sidebar()),
#       mainPanel(h1("Please select or import a data set."))
#     )
#   }else{
#     sidebarLayout(
#       sidebarPanel(get.single.col.sidebar()),
#       mainPanel(plotOutput("column.plot"),
#                 
#       )
#     )
#   }
#   if(is.null()){
#     sidebarLayout(
#         sidebarPanel(get.single.col.sidebar()),
#             mainPanel(h1("Please select or import a data set."))
#     )
#     }else{
#     sidebarLayout(
#         sidebarPanel(get.single.col.sidebar()),
#             mainPanel(plotOutput("column.plot"),
#                 get.player(ID.backward="single.backward",ID.player="single.play",
#                            ID.forward="single.forward",maxi=ncol())
#             )
#     )   
#     }
# }

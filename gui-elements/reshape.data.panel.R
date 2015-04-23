reshape.data.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display("Reshape dataset",
                                "reshape_data",
                                "gui-elements/notes/reshape.data.md")),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(actionButton("reshape.data.submit",label="reshape"),br(),br(),
                   help.display("Reshape dataset",
                                "reshape_data",
                                "gui-elements/notes/reshape.data.md")),
      mainPanel(helpText("The table below shows the reshapd dataset. If the 
                         reshape button is pressed on the right, the 
                         selected dataset is reshaped and the reshaped version 
                         of the reshaped dataset is displyed."),
                dataTableOutput("reshape.data.table"))
    )
  }
}
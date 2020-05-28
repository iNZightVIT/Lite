frequency.tables.sidebar =  function(){
  list(
    selectInput(inputId="frequency_tables_select1",
                label=h5(strong("Frequency tables")),
                choices=c("Expand table"),
                selected="Expand table"),
    br(),
    helpText(HTML(paste("This will expand the table to individual rows.",
          "Use Dataset > Restore data to go back to revert this change.",
          "Note: this is a temporary workaround for small tables until we integrate frequency tables.",
          sep = "<br/><br/>"))),
    br(),
    actionButton("expand_table_button","Expand table",
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    br(),
    br()
    )
}

frequency.tables.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(frequency.tables.sidebar()),
      mainPanel(dataTableOutput("ft.expand.data.table"))
      )
  }
}
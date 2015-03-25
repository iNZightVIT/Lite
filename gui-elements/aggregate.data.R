aggregate.data.sidebar =  function(){
  list(selectInput(inputId="aggros",
                   choices=c("",get.categorical.column.names()),
                   selected=1,
                   multiple=T,
                   label="Select categorical Variable"),
       selectInput(inputId="aggregate.method",
                   label="Select Method for aggregation",
                   choices=c("","mean","median","sum","sd","IQR","count"),
                   selected=1,
                   multiple=T),
       actionButton("aggregate_vars","aggregate variables"),
       br(),br(),
       help.display('Aggregate data',
                    'aggregate_help',
                    "gui-elements/notes/aggregate.data.md"),
       br())
}

aggregate.variable =function(){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(help.display('Aggregate data','aggregate_help',"gui-elements/notes/aggregate.data.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(aggregate.data.sidebar()),
      mainPanel(dataTableOutput("aggregate.table"))
    )
  }
}
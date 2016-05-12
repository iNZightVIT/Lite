aggregate.data.sidebar =  function(data.set){
  list(selectInput(inputId="aggros",
                   choices=c("",get.categorical.column.names(data.set)),
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
                    "panels/D3_AggregateData/3_aggregate.data.help.md"),
       br())
}

aggregate.variable =function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Aggregate data','aggregate_help',"panels/D3_AggregateData/3_aggregate.data.help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(aggregate.data.sidebar(data.set)),
      mainPanel(dataTableOutput("aggregate.table"))
    )
  }
}
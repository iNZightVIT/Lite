aggregate.data.sidebar <- function() {
  list(
    h5(strong("Aggregate over variables:")),
    fixedRow(
      column(3, h5("1st")),
      column(9, uiOutput("aggros1_panel"))
    ),
    fixedRow(
      column(3, h5("2nd")),
      column(9, uiOutput("aggros2_panel"))
    ),
    fixedRow(
      column(3, h5("3rd")),
      column(9, uiOutput("aggros3_panel"))
    ),

    #       selectInput(inputId="aggros",
    #                   choices=c("",get.categorical.column.names(data.set)),
    #                   selected=1,
    #                   multiple=T,
    #                   label="Select categorical Variable"),

    selectInput(
      inputId = "aggregate.method",
      label = "Summaries:",
      choices = c("Mean", "Median", "Sum", "Sd", "IQR", "Count"),
      multiple = T,
      selectize = FALSE,
      size = 7
    ),
    actionButton("aggregate_vars", "Aggregate Now",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    br(), br(),
    help.display(
      "Aggregate data",
      "aggregate_help",
      "panels/D3_AggregateData/3_aggregate.data.help.md"
    ),
    br()
  )
}

aggregate.variable.panel <- function() {
  if (is.null(get.data.set())) {
    sidebarLayout(
      sidebarPanel(help.display("Aggregate data", "aggregate_help", "panels/D3_AggregateData/3_aggregate.data.help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(aggregate.data.sidebar()),
      mainPanel(
        textOutput("aggregate.table.data.sample.info"), br(), br(),
        DTOutput("aggregate.table")
      )
    )
  }
}

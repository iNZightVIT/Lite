get.quick.explore =  function(data.set){
  list(list(selectInput(inputId="quick_explore_select",
                        label="Quick explore",
                        choices=c("Missing values summary",
                                  "Data Summary",
                                  "Single column plot",
                                  "Column pair plot",
                                  "Compare pairs"),
                        selectize=F),
            conditionalPanel("input.quick_explore_select=='Missing values summary'",
                             uiOutput("quick.missing.summary.side")),
            conditionalPanel("input.quick_explore_select=='Data Summary'",
                             uiOutput("quick.summary.side")),
            conditionalPanel("input.quick_explore_select=='Single column plot'",
                             uiOutput("single.column.plot.side")),
            conditionalPanel("input.quick_explore_select=='Column pair plot'",
                             uiOutput("column.pair.plot.side")),
            conditionalPanel("input.quick_explore_select=='Compare pairs'",
                             uiOutput("compare.pairs.side"))),
            list(conditionalPanel("input.quick_explore_select=='Missing values summary'",
                                  uiOutput("quick.missing.summary.main")),
                 conditionalPanel("input.quick_explore_select=='Data Summary'",
                                  uiOutput("quick.summary.main")),
                 conditionalPanel("input.quick_explore_select=='Single column plot'",
                                  uiOutput("single.column.plot.main")),
                 conditionalPanel("input.quick_explore_select=='Column pair plot'",
                                  uiOutput("column.pair.plot.main")),
                 conditionalPanel("input.quick_explore_select=='Compare pairs'",
                                  uiOutput("compare.pairs.main"))))
}

quick.explore.panel =function(data.set){
  sidebarLayout(
    sidebarPanel(get.quick.explore(data.set)[[1]]),
    mainPanel(get.quick.explore(data.set)[[2]])
  )
}
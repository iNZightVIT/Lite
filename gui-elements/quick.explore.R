get.quick.explore =  function(data.set){
  list(list(selectInput(inputId="quick_explore_select",
                        label="Quick explore",
                        choices=c("Missing values",
                                  "Data Summary",
                                  "All 1-variable plots",
                                  "Explore 2-variable plots",
                                  "Compare pairs"),
                        selectize=F),
            conditionalPanel("input.quick_explore_select=='Missing values'",
                             uiOutput("quick.missing.summary.side")),
            conditionalPanel("input.quick_explore_select=='Data Summary'",
                             uiOutput("quick.summary.side")),
            conditionalPanel("input.quick_explore_select=='All 1-variable plots'",
                             uiOutput("single.column.plot.side")),
            conditionalPanel("input.quick_explore_select=='Explore 2-variable plots'",
                             uiOutput("column.pair.plot.side")),
            conditionalPanel("input.quick_explore_select=='Compare pairs'",
                             uiOutput("compare.pairs.side"))),
            list(conditionalPanel("input.quick_explore_select=='Missing values'",
                                  uiOutput("quick.missing.summary.main")),
                 conditionalPanel("input.quick_explore_select=='Data Summary'",
                                  uiOutput("quick.summary.main")),
                 conditionalPanel("input.quick_explore_select=='All 1-variable plots'",
                                  uiOutput("single.column.plot.main")),
                 conditionalPanel("input.quick_explore_select=='Explore 2-variable plots'",
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
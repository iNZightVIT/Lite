get.quick.explore =  function(){
  if(is.null(data)){
    list(list(help.display('Quick explore','quick_explore',
                           "gui-elements/notes/quick.explore.md"),
              br(),HTML("")),
         h1("Please select or import a data set."))
  }else{
    list(list(div(class="wraper",selectInput(inputId="quick_explore_select",
                                             label="Quick explore",
                                             choices=c("Data Summary",
                                                       "Single column plot",
                                                       "Column pair plot",
                                                       "Compare pairs"))),
              conditionalPanel("input.quick_explore_select=='Data Summary'",
                               uiOutput("quick.summary.side")),
              conditionalPanel("input.quick_explore_select=='Single column plot'",
                               uiOutput("single.column.plot.side")),
              conditionalPanel("input.quick_explore_select=='Column pair plot'",
                               uiOutput("column.pair.plot.side")),
              conditionalPanel("input.quick_explore_select=='Compare pairs'",
                               uiOutput("compare.pairs.side"))),
              list(conditionalPanel("input.quick_explore_select=='Data Summary'",
                                    uiOutput("quick.summary.main")),
                   conditionalPanel("input.quick_explore_select=='Single column plot'",
                                    uiOutput("single.column.plot.main")),
                   conditionalPanel("input.quick_explore_select=='Column pair plot'",
                                    uiOutput("column.pair.plot.main")),
                   conditionalPanel("input.quick_explore_select=='Compare pairs'",
                                    uiOutput("compare.pairs.main"))))
  }
}

quick.explore.panel =function(){
  sidebarLayout(
    sidebarPanel(get.quick.explore()[[1]]),
    mainPanel(get.quick.explore()[[2]])
  )
}
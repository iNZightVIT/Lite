

numeric.variables.panel = function(data.set){
  temp = NULL
  if(is.null(data.set)){
    temp = list(list(help.display('Numeric variables','numeric_variables',
                           "gui-elements/notes/numeric.variables.md"),
              br(),HTML("")),
         h1("Please select or import a data set."))
  }else{
    choices=c("Transform variables",
              "Form Class interval",
              "Rank numeric")
    temp = list(list(selectInput(inputId="numeric_variables_select",
                          label="Numeric variables",
                          choices=choices,
                          selectize=F),
                     conditionalPanel("input.numeric_variables_select=='Transform variables'",
                                      uiOutput("transform.columns.side")),
                     conditionalPanel("input.numeric_variables_select=='Form Class interval'",
                                      uiOutput("form.class.interval.side")),
                     conditionalPanel("input.numeric_variables_select=='Rank numeric'",
                                      uiOutput("rank.numeric.side"))
                     ),
                list(conditionalPanel("input.numeric_variables_select=='Transform variables'",
                                      uiOutput("transform.columns.main")),
                     conditionalPanel("input.numeric_variables_select=='Form Class interval'",
                                      uiOutput("form.class.interval.main")),
                     conditionalPanel("input.numeric_variables_select=='Rank numeric'",
                                      uiOutput("rank.numeric.main"))
                   )
         )
  }
  if(!is.null(temp)){
    sidebarLayout(
      sidebarPanel(temp[[1]]),
      mainPanel(temp[[2]])
    )  
  }
}
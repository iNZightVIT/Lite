get.categorical.variables =  function(){
  if(is.null(data)){
    list(list(help.display('Categorical variables','categorical_variables',
                           "gui-elements/notes/categorical.variables.md"),
              br(),HTML("")),
         h1("Please select or import a data set."))
  }else{
    list(list(selectInput(inputId="categorical_variables_select",
                          label="Categorical variables",
                          choices=c("Reorder levels",
                                    "Collapse levels",
                                    "Rename levels",
                                    "Combine categorical"),
                          selectize=F),
              conditionalPanel("input.categorical_variables_select=='Reorder levels'",
                               uiOutput("reorder.levels.side")),
              conditionalPanel("input.categorical_variables_select=='Collapse levels'",
                               uiOutput("collapse.levels.side")),
              conditionalPanel("input.categorical_variables_select=='Rename levels'",
                               uiOutput("rename.levels.side")),
              conditionalPanel("input.categorical_variables_select=='Combine categorical'",
                               uiOutput("combine.levels.side"))),
         uiOutput("categorical.main.panel"))
  }
}

categorical.variables.panel = function(){
  sidebarLayout(
    sidebarPanel(get.categorical.variables()[[1]]),
    mainPanel(get.categorical.variables()[[2]])
  )
}
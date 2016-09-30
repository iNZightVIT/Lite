
alphabetise.variables.sidebar =  function(data.set){
  list(selectInput(inputId="select.alphabetical.order",
                   choices=c("", "A(a) To Z(z)", "Z(z) To A(a)"),
                   selected=1,
                   multiple=FALSE,
                   label="Select alphabetical order"),
       actionButton("alphabetise.var.button", "Alphabetise Variables"),
       br(),br(),
       help.display('Alphabetise Variables', 'alphabetise_variables_help',
                    "panels/D7_AlphabetiseVariables/3_alphabetise.variables-help.md"),
       br())
}



alphabetise.variables = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Alphabetise Variables', 'alphabetise_variables_help',
                                "panels/D7_AlphabetiseVariables/3_alphabetise.variables-help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(alphabetise.variables.sidebar(data.set)),
      mainPanel(dataTableOutput("alphabetise.variables.table"))
    )
  }
}

















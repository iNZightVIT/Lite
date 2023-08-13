
alphabetise.variables.sidebar =  function(data.set){
  list(h5(strong("Select alphabetical order")),
       selectInput(inputId="select.alphabetical.order",
                   choices=c("", "A(a) To Z(z)", "Z(z) To A(a)"),
                   selected=1,
                   multiple=FALSE,
                   label=NULL),
       actionButton("alphabetise.var.button", "Alphabetise Variables",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
       br(),br(),
       help.display('Alphabetise Variables', 'alphabetise_variables_help',
                    "panels/D7_AlphabetiseVariables/3_alphabetise.variables-help.md"),
       br())
}


# TODO: check
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
      mainPanel(
        textOutput('alphabetise.var.data.sample.info'), br(), br(),
        DTOutput("alphabetise.variables.table"))
    )
  }
}

















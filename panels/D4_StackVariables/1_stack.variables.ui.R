stack.variables.sidebar =  function(){
  list(h5(strong("Choose variables to stack")),
       
#       selectInput(inputId="stack_vars_which",
#                   label="Select categorical or numeric",
#                   choices=c("","categorical","numeric"),
#                   selected=1,
#                   selectize=F),
       
       selectInput(inputId="stack_vars_column",
                label = "Variables",
                choices = get.numeric.column.names(get.data.set()),
#                selected = 1,
                selectize = FALSE,
                multiple = T,
                size = 10),

       actionButton("stack_vars","Stack",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
       br(),br(),
         help.display('Stack variables','stack_vars_help',
                      "panels/D4_StackVariables/3_stack.variables.help.md"),
         br())
}

stack.variables.panel =function(){
  if(is.null(get.data.set())){
    sidebarLayout(
      sidebarPanel(help.display('Stack variables','stack_vars_help',
                                "panels/D4_StackVariables/3_stack.variables.help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(stack.variables.sidebar()),
      mainPanel(
        textOutput('stack.table.data.sample.info'), br(), br(),
        DTOutput("stack.table")
      )
    )
  }
}
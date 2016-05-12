stack.variables.sidebar =  function(){
  list(selectInput(inputId="stack_vars_which",
                label="Select categorical or numeric",
                choices=c("","categorical","numeric"),
                selected=1,
                selectize=F),
       selectInput(inputId="stack_vars_column",
                label="Select variables to stack",
                choices="",
                selected=1,
                selectize=T,
                multiple=T),
       actionButton("stack_vars","Stack variables"),br(),br(),
         help.display('Stack variables','stack_vars_help',
                      "panels/D4_StackVariables/3_stack.variables.help.md"),
         br())
}

stack.variables.panel =function(data.set){
  if(is.null(data.set)){
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
      mainPanel(dataTableOutput("stack.table"))
    )
  }
}
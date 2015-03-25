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
         help.display('Stack variables','stack_vars_help',"gui-elements/notes/stack.variables.md"),
         br())
}

stack.variables =function(){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(help.display('Stack variables','stack_vars_help',"gui-elements/notes/stack.variables.md")),
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
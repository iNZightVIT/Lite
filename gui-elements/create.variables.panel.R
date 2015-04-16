get.create.variables.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Create variables','create_variables',
                                "gui-elements/notes/create.variables.md"),
                   br(),HTML("")),
      mainPanel(h1("Please select or import a data set.")))
  }else{
    sidebarLayout(
      sidebarPanel(helpText("Select the columns to be included in the 
                            expression or type the column name in the 
                            expression text field. In case you want to 
                            add the same column twice select the empty 
                            field in the select field before reselecting 
                            the column again. Delete by changing the text 
                            field directly."),
                   selectInput(inputId="create.variables.column.select",
                               label="Select a column to add to the expression",
                               choices=c(" ",get.numeric.column.names(data.set)),
                               selected=1),
                   selectInput(inputId="create.variables.operation.select",
                               label="Select an operation to rename",
                               choices=c(" ","+","-","*","/","^","(",")"),
                               selected=1),
                   textInput("create.variables.expression",
                             label="The expression to be evaluated"),
                   actionButton(inputId="create.variables.submit",
                                label="Create Variable"),br(),br(),
                   verbatimTextOutput("create.variables.status.message"),
                   help.display('Create variables','create_variables',
                                "gui-elements/notes/create.variables.md")),
      mainPanel(verbatimTextOutput("create.variables.out"))
    )  
  }
}
get.create.variables.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Create variables','create_variables',
                                "gui-elements/notes/create.variables.md"),
                   br(),HTML("")),
      mainPanel(h1("Please select or import a data set.")))
  }else{
    count = 1
    new.name = paste("formula.new",count,sep=".")
    while(new.name%in%colnames(data.set)){
      count=count+1
      new.name = paste("formula.new",count,sep=".")
    }
    sidebarLayout(
      sidebarPanel(helpText("Select the columns to be included in the 
                            expression. In case you want to add the same 
                            column twice, select the empty field in the 
                            select field before reselecting the column 
                            again. Delete by pressing \"del\"."),
                   selectInput(inputId="create.variables.column.select",
                               label="Select a column to add to the expression",
                               choices=c(" ",get.numeric.column.names(data.set)),
                               selected=" "),
                   selectInput(inputId="create.variables.operation.select",
                               label="Select an operation",
                               choices=c(" ","+","-","*","/","^","(",")",":"),
                               selected=" ",
                               size=9,
                               selectize=F),
                   div(fixedRow(column(width=4,
                                       actionButton("create.variables.1",
                                                    label="1")),
                                column(width=4,
                                       actionButton("create.variables.2",
                                                    label="2")),
                                column(width=4,
                                       actionButton("create.variables.3",
                                                    label="3"))),br(),
                       fixedRow(column(width=4,
                                       actionButton("create.variables.4",
                                                    label="4")),
                                column(width=4,
                                       actionButton("create.variables.5",
                                                    label="5")),
                                column(width=4,
                                       actionButton("create.variables.6",
                                                    label="6"))),br(),
                       fixedRow(column(width=4,
                                       actionButton("create.variables.7",
                                                    label="7")),
                                column(width=4,
                                       actionButton("create.variables.8",
                                                    label="8")),
                                column(width=4,
                                       actionButton("create.variables.9",
                                                    label="9"))),br(),
                       fixedRow(column(width=4,
                                       actionButton("create.variables.delete",
                                                    label="del")),
                                column(width=4,
                                       actionButton("create.variables.0",
                                                    label="0")),
                                column(width=4,
                                       actionButton("create.variables.dot",
                                                    label=".")))),br(),
                   div(fixedRow(column(width=4,textInput("create.variables.name",
                                                         label="Variable name",
                                                         value="new.name")),
                                column(width=1,"="),
                                column(width=7,"The expression to be evaluated",
                                       br(),br(),
                                       verbatimTextOutput("create.variables.expression")))
                       ),
                   actionButton(inputId="create.variables.submit",
                                label="Create Variable"),br(),br(),
                   verbatimTextOutput("create.variables.status.message"),
                   help.display('Create variables','create_variables',
                                "gui-elements/notes/create.variables.md")),
      mainPanel(verbatimTextOutput("create.variables.out"))
    )  
  }
}
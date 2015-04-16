get.rename.variables.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Rename variable','rename_variable',
                           "gui-elements/notes/rename.variables.md"),
                      br(),HTML("")),
      mainPanel(h1("Please select or import a data set.")))
  }else{
    sidebarLayout(
      sidebarPanel(selectInput(inputId="rename.variables.column.select",
                               label="Select a column to rename",
                               choices=colnames(data.set),selected=1),
                   textInput("rename.variables.new.name",
                             label="Type in a new name for the column",
                             value=colnames(data.set)[1]),
                   actionButton(inputId="rename.variables",
                                label="Rename Variable"),br(),br(),
                   help.display('Rename variable','rename_variable',
                                "gui-elements/notes/rename.variables.md")),
      mainPanel(verbatimTextOutput("rename.variables.out"))
    )  
  }
}
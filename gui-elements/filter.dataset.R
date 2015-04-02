
filter.data.sidebar =  function(){
  list(
    selectInput(inputId="select_filter",
                label="Select Filter to apply",
                choices=c("","levels of categorical variable",
                          "numeric condition","row indices",
                          "randomly"),selected=1),
    conditionalPanel("input.select_filter=='levels of categorical variable'",
                     selectInput(inputId="select_categorical1",
                                 label="Select a categorical variable to filter the data on",
                                 choices=c("",get.categorical.column.names()),
                                 selected=1,selectize=F),
                     selectInput(inputId="levels1",label="Select levels to remove from the data",
                                 choices="",selected=1,multiple=T)),
    conditionalPanel("input.select_filter=='numeric condition'",
                     selectInput(inputId="select_numeric1",
                                 label="Select a numerical variable to filter the data on",
                                 choices=c("",get.numeric.column.names()),
                                 selected=1,selectize=F),
                     selectInput(inputId="select_operation1",
                                 label="Select a condition ",
                                 choices=c("",c("","<",">","<=",">=","==","!=")),
                                 selected=1,selectize=F),
                     textInput(inputId="numeric_input1",label="Provide a numeric value to test for"),
                     verbatimTextOutput("message1")),
    conditionalPanel("input.select_filter=='row indices'",
                     helpText("Paste or type in a comma seperated list of index values to remove from the data."),
                     tags$textarea(id="row_op_indexes", rows=8, cols=25, ""),
                     verbatimTextOutput("message2")),
    conditionalPanel("input.select_filter=='randomly'",
                     textInput(inputId="numeric_input2",label="Type in the size of the sample"),
                     textInput(inputId="numeric_input3",label="Specify the number of samples to take"),
                     checkboxInput("bootstrap_check",label="Check to sample with replacement", value=F),
                     verbatimTextOutput("message3")),
    actionButton("filter_data_perform","PERFORM OPERATION"),br(),br(),textOutput("op_status"),br(),br(),
    help.display('Filter datset','row_op_help',"gui-elements/notes/filter.dataset.md"),br())
}

filter.data.panel =function(){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(help.display('Filter datset','row_op_help',"gui-elements/notes/filter.dataset.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(filter.data.sidebar()),
      mainPanel(verbatimTextOutput("filter.data.summary"))
    )
  }
}

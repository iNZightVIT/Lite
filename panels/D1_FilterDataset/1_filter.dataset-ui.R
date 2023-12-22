filter.data.sidebar <- function(data.set) {
  list(
    h5(strong("Select Filter to apply")),
    selectInput(
      inputId = "select_filter",
      label = NULL,
      choices = c(
        "", "levels of categorical variable",
        "numeric condition", "row indices",
        "randomly"
      ), selected = 1
    ),
    conditionalPanel(
      "input.select_filter=='levels of categorical variable'",
      selectInput(
        inputId = "select_categorical1",
        label = "Select a categorical variable to filter the data on",
        choices = c("", get.categorical.column.names(data.set)),
        selected = 1, selectize = F
      ),
      selectInput(
        inputId = "levels1", label = "Select levels to include",
        choices = "",
        selected = 1, multiple = T
      )
    ),
    conditionalPanel(
      "input.select_filter=='numeric condition'",
      selectInput(
        inputId = "select_numeric1",
        label = "Select a numerical variable to filter the data on",
        choices = c("", get.numeric.column.names(data.set)),
        selected = 1, selectize = F
      ),
      selectInput(
        inputId = "select_operation1",
        label = "Select a condition ",
        choices = c("", c("", "<", ">", "<=", ">=", "==", "!=")),
        selected = 1, selectize = F
      ),
      textInput(
        inputId = "numeric_input1",
        label = "Provide a numeric value to test for"
      ),
      verbatimTextOutput("message1")
    ),
    conditionalPanel(
      "input.select_filter=='row indices'",
      helpText("Paste or type in a comma seperated list of index values to remove from the data."),
      tags$textarea(id = "row_op_indexes", rows = 8, cols = 25, ""),
      verbatimTextOutput("message2")
    ),
    conditionalPanel(
      "input.select_filter=='randomly'",
      textInput(
        inputId = "numeric_input2",
        label = "Type in the size of the sample"
      ),
      textInput(
        inputId = "numeric_input3",
        label = "Specify the number of samples to take"
      ),
    ),
    actionButton("filter_data_perform", "PERFORM OPERATION",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    br(), br(),
    help.display(
      "Filter datset", "row_op_help",
      "panels/D1_FilterDataset/3_filter.dataset-help.md"
    ),
    br()
  )
}

filter.data.panel <- function(data.set) {
  if (is.null(data.set)) {
    sidebarLayout(
      sidebarPanel(
        help.display(
          "Filter datset", "row_op_help",
          "panels/D1_FilterDataset/3_filter.dataset-help.md"
        )
      ),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(filter.data.sidebar(data.set)),
      mainPanel(verbatimTextOutput("filter.data.summary"))
    )
  }
}

separate.columns.sidebar <- function() {
  list(
    h5(strong("Separate columns")),
    selectInput(
      inputId = "select_separate_mode",
      label = "Select separate mode",
      choices = c("", "Separate a column into several columns", "Separate a column to make several rows"),
      selectize = FALSE,
      multiple = F
    ),
    selectInput(
      inputId = "select_column_to_separate",
      label = "Select column to separate out",
      choices = c("", colnames(get.data.set())),
      selectize = FALSE,
      multiple = F
    ),
    textInput("separator",
      label = "Enter the separator between values",
      value = ""
    ),
    uiOutput("separate_change_column_names"),
    fixedRow(
      column(3, actionButton("preview_separatecolumns_button", "Preview",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )),
      column(3, actionButton("separatecolumns_dataset_button", "Separate",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ))
    )
  )
}



separate.columns.panel <- function() {
  if (is.null(get.data.set())) {
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(separate.columns.sidebar()),
      mainPanel(
        h5(strong("Original dataset")),
        DTOutput("separatecolumns.table"),
        h5(strong("New dataset")),
        DTOutput("previewseparatecolumns.table")
      )
    )
  }
}

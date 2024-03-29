mergejoin.datasets.sidebar <- function() {
  list(
    h5(strong("Select join/append datasets")),
    selectInput(
      inputId = "select_mergejoin_columns",
      label = NULL,
      choices = c("Join by column values", "Append new rows"),
      selectize = FALSE,
      multiple = FALSE
    ),
    conditionalPanel(
      "input.select_mergejoin_columns == 'Join by column values'",
      uiOutput("join_data_panel")
    ),
    conditionalPanel(
      "input.select_mergejoin_columns == 'Append new rows'",
      uiOutput("append_rows_panel")
    )
  )
}



mergejoin.datasets.panel <- function() {
  if (is.null(get.data.set())) {
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(mergejoin.datasets.sidebar()),
      mainPanel(
        conditionalPanel(
          "input.select_mergejoin_columns == 'Join by column values'",
          fixedRow(
            column(6, h5(strong("Preview of the original dataset"))),
            column(6, h5(strong("Preview of the imported dataset")))
          ),
          fixedRow(
            column(6, DTOutput("join.table")),
            column(6, DTOutput("previewimport.table"))
          ),
          h5(strong("Preview")),
          DTOutput("previewjoin.table")
        ),
        conditionalPanel(
          "input.select_mergejoin_columns == 'Append new rows'",
          h5(strong("Original dataset")),
          DTOutput("append.table"),
          h5(strong("New dataset")),
          DTOutput("previewappend.table")
        )
      )
    )
  }
}

rename.variables.panel <- function() {
  if (is.null(get.data.set())) {
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(
        uiOutput("rename_variables_two_columns"),
        actionButton("rename_variables_two_columns_but", "Rename",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ),
      mainPanel(
        textOutput("rename.var.data.sample.info"), br(), br(),
        DTOutput("rename.variables.table")
      )
    )
  }
}

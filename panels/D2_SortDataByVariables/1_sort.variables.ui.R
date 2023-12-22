sort.variables.sidebar <- function() {
  list(
    h5(strong("Sort by")),
    br(),
    h5("Variable"),
    fixedRow(
      column(1, h5("1st")),
      column(4, uiOutput("sort1_panel")),
      column(7, radioButtons(
        inputId = "sort1_order",
        label = NULL,
        choices = c("increasing" = 1, "decreasing" = 2),
        selected = 1,
        inline = TRUE
      ))
    ),
    fixedRow(
      column(1, h5("2nd")),
      column(4, uiOutput("sort2_panel")),
      column(7, radioButtons(
        inputId = "sort2_order",
        label = NULL,
        choices = c("increasing" = 1, "decreasing" = 2),
        selected = 1,
        inline = TRUE
      ))
    ),
    fixedRow(
      column(1, h5("3rd")),
      column(4, uiOutput("sort3_panel")),
      column(7, radioButtons(
        inputId = "sort3_order",
        label = NULL,
        choices = c("increasing" = 1, "decreasing" = 2),
        selected = 1,
        inline = TRUE
      ))
    ),
    fixedRow(
      column(1, h5("4th")),
      column(4, uiOutput("sort4_panel")),
      column(7, radioButtons(
        inputId = "sort4_order",
        label = NULL,
        choices = c("increasing" = 1, "decreasing" = 2),
        selected = 1,
        inline = TRUE
      ))
    ),
    actionButton("sort_vars", "Sort Now",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    br(), br(),
    help.display(
      "Sort data by variables", "sort_vars_help",
      "panels/D2_SortDataByVariables/3_sort.variables.help.md"
    ),
    br()
  )
}

sort.variables.panel <- function() {
  if (is.null(get.data.set())) {
    sidebarLayout(
      sidebarPanel(help.display(
        "Sort data by variables", "sort_vars_help",
        "panels/D2_SortDataByVariables/3_sort.variables.help.md"
      )),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(sort.variables.sidebar()),
      mainPanel(
        textOutput("sort.table.data.sample.info"), br(), br(),
        DTOutput("sort.table")
      )
    )
  }
}

missing.categorical.panel <- function(data.set) {
  if (is.null(data.set)) {
    sidebarLayout(
      sidebarPanel(help.display(
        "Missing to category", "missing_categorical",
        "panels/E5_MissingToCategorical/3_missing.categorical.help.md"
      )),
      mainPanel(h1("Please select or import a data set."))
    )
  } else {
    sidebarLayout(
      sidebarPanel(
        helpText(HTML("For each selected variable <em>varname</em>,
        a new variable <em>varname_missing</em>
        will be created at the end of the dataset in which any missing values
        in the variable are replaced by <q>missing</q>.
        <br/>
        If a source variable <em>varname</em>
        is categorical, <em>varname_missing</em> will contain one
        additional category (<q>missing</q>).
        If the source variable <em>varname</em> is numeric,
        <em>varname_missing</em> will be binary with
        two categories (<q>observed</q> and <q>missing</q>).")),
        selectInput("missing.categorical.column.select",
          label = "Select columns",
          choices = colnames(data.set),
          selected = 1,
          multiple = T
        ),
        actionButton("missing.categorical.submit",
          label = "Create the new variables"
        ), br(), br(),
        verbatimTextOutput("message.newvariablesadded"), br(), br(),
        help.display(
          "Missing to category", "missing_categorical",
          "panels/E5_MissingToCategorical/3_missing.categorical.help.md"
        )
      ),
      mainPanel(
        helpText("The table shows the distribution of missing values in
                  the data. All possible combiantions of NA (missing) and
                  not NA (observed) are shown. The row count of how often
                  the row combination is seen in the data is in the last
                  column of the table"),
        DTOutput("missing.categorical.table")
      )
    )
  }
}

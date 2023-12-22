### -----------------------------------------------------###
###  User Interface for the "Multivariate" Module  ###
### -----------------------------------------------------###
###
###
###  The UI is divided into two parts:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: This is to be sourced within "server.R" *

### ----------------###
###  Sidebar Panel ###
### ----------------###


multivariate.sidebarPanel <- function() {
  sidebarPanelUI <- list(
    useShinyalert(),
    useShinyjs(),
    fluidRow(
      column(
        12,
        selectInput("multivarate.method",
          label = h5(strong("Method:")), choices = c(
            "Pairs Plot"                          = "pairs",
            "Correlation Pairs  Plot"             = "pairs_corr",
            "Parallel Coordinates"                = "pcp",
            "Principal Components Analysis"       = "pca",
            "Multidimensional Scaling"            = "mds" # ,
            # "Non-Metric Multidimensional Scaling" = "nmds"
          ), selected = "pairs_corr",
          selectize = FALSE
        ),
        uiOutput("multivarate.widgets")
      )
    )
  ) ## end of list
}

### now, we set up the main panel
multivariate.mainPanel <- function() {
  mainPanelUI <- list(
    uiOutput("multivarate.ui.main")
  ) ## end of mainPanelUI
}

### -------------------###
###  Multivariate UI  ###
### -------------------###

###  We combine the 2 sidebarPanel() and 2 mainPanel() functions to
###  complete the UI for the Mixed Model module.

multivariate.panel.ui <- function(data.set) {
  fluidPage(
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/F7_Multivariate/4_Multivariate-panel-null.md"
        )
      )
    } else {
      fluidRow(
        column(3, multivariate.sidebarPanel()),
        column(9, multivariate.mainPanel())
      )
    }
  )
}

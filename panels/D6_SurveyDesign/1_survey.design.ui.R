survey.design.sidebar <- function() {
  list(
    useShinyalert(),
    useShinyjs(),
    selectInput("svytype",
      label = "Select survey design",
      c = list(
        "Specify design" = "survey",
        "Specify replicate design" = "replicate",
        "Post stratify" = "post",
        "Read from file" = "read"
      )
    ),
    conditionalPanel(
      "input.svytype == 'survey'",
      helpText("The design generated is always used with the current data set.
             Please make sure the right data set is selected first. In case
             the data is changed, the current design object is lost."),
      fluidRow(column(12, selectInput("stratVar",
        label = "Strata variable",
        choices = c(
          " ",
          colnames(get.data.set())
        ),
        selected = " ",
        selectize = F
      ))),
      fluidRow(
        column(12, selectInput("clus1Var",
          label = "1st stage clustering variable",
          choices = c(" ", colnames(get.data.set())),
          selected = " ",
          selectize = F
        )),
        column(12, selectInput("clus2Var",
          label = "2nd stage clustering variable",
          choices = c(" ", colnames(get.data.set())),
          selected = " ",
          selectize = F
        )),
        column(12, checkboxInput("nestChk",
          label = "Use nested sampling",
          value = F
        )),
        column(12, selectInput("wtVar",
          label = "Weighting variable",
          choices = c(
            " ",
            colnames(get.data.set())
          ),
          selected = " ",
          selectize = F
        )),
        column(12, uiOutput("estimate.pop.size")),
        column(12, selectInput("fpcVar",
          label = "Finite population correction",
          choices = c(" ", colnames(get.data.set())),
          selected = " ",
          selectize = F
        )),
        column(12, selectInput("fpcVar2",
          label = "2nd finite population correction variable",
          choices = c(" ", colnames(get.data.set())),
          selected = " ",
          selectize = F
        ))
      ),
      hr(),
      fluidRow(
        column(6, actionButton("create.design", "Create design",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(6, actionButton("remove.design", "Remove design",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      ), br()
    ),
    conditionalPanel(
      "input.svytype == 'replicate'",
      fluidRow(
        column(6, selectInput("sample.weight.Var",
          label = "Sampling weights: ",
          choices = c(
            " ",
            colnames(get.data.set())
          ),
          selected = " ",
          selectize = F
        )),
        column(6, selectInput("repType",
          label = "Type of replication weights: ",
          choices = c("BRR", "Fay", "JK1", "JKn", "bootstrap", "other"),
          selected = "other",
          selectize = F
        )),
        column(
          6,
          checkboxInput("combWts",
            label = "Replication weights incorporate sampling weights",
            value = T
          ),
          selectInput("repVars",
            label = "Select replicate weights: ",
            choices = c(colnames(get.data.set())),
            multiple = T,
            selectize = F,
            size = 18
          ),
          helpText("To select a range, click the first, then hold SHIFT while clicking the last.\n\nHold CTRL while clicking to add and remove invidividual variables.")
        ),
        conditionalPanel(
          condition = "input.repType == 'bootstrap' | input.repType == 'other'",
          column(
            6, h5(strong("Specify at least one of overall scale and individual replicate scales")),
            textInput("repScale", "Overall scale: "),
            h5("Replicate scales: "),
            fluidRow(
              column(8, div(style = "display: inline-block;;", fileInput("repRscalesBtn", label = "Read from file ...", multiple = F))),
              column(2, div(style = "display: inline-block; margin-top: 25px;", actionButton("repRscalesClear", "Clear",
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ))),
            ),
            tags$div(
              style = "margin-top: -1px;
                                                   border: 1px solid #BBB;
                                                   width: 200px;
                                                   height: 300px;
                                                   overflow-y: auto;
                                                   overflow-x: auto;",
              tags$head(tags$style("#rscalesTbl table {background-color: #FFFFFF; }", media = "screen", type = "text/css")),
              tableOutput("rscalesTbl")
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(6, actionButton("create.design1", "Create design",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(6, actionButton("remove.design1", "Remove design",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      ), br()
    ),
    conditionalPanel(
      "input.svytype == 'post'",
      uiOutput("svypost_ui"),
      fluidRow(
        column(6, actionButton("create.design2", "Create design",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )),
        column(6, actionButton("remove.design2", "Remove design",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      ), br()
    ),
    conditionalPanel(
      "input.svytype == 'read'",
      helpText("Select a survey specification file"),
      br(),
      fileInput("svy.design.spec",
        label = "Read from file ...",
        multiple = F,
        accept = ".svydesign"
      ),
      fluidRow(column(6, actionButton("remove.design3", "Remove design",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ))), br()
    ),
    br(),
    help.display(
      "Create design", "create_design_help",
      "panels/D6_SurveyDesign/3_survey.design.help.md"
    ),
    br()
  )
}




create.design.panel <- function(data.set) {
  if (is.null(data.set)) {
    sidebarLayout(
      sidebarPanel(help.display(
        "Create design", "create_design_help",
        "panels/D6_SurveyDesign/3_survey.design.help.md"
      )),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  } else {
    sidebarLayout(
      sidebarPanel(survey.design.sidebar()),
      mainPanel(verbatimTextOutput("create.design.summary"))
    )
  }
}

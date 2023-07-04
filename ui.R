### -----------------------###
###  UI for iNZight Lite  ###
### -----------------------###
###
###  Date Created   :   January 13, 2015
###  Last Modified  :   December 15, 2015
###
###  Please consult the comments before editing any code.
###  This file sources the ui files for each panel separately.
shinyUI(
  fluidPage(
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet-pure-css.css")),
    ##  Set Tabpanel font to be size 16.
    tags$head(
      shinyjs::useShinyjs(),
      tags$script(src = "vit.js"),
      tags$script(src = "js/disconnect.js"),
      tags$link(href = "disconnectedModal.css", rel = "stylesheet", type = "text/css"),
      tags$script(src = "js/ticker.js"),
      tags$link(href = "ticker.css", rel = "stylesheet", type = "text/css"),
      tags$style(HTML("
      body > .container-fluid {
        padding: 0 !important;
      }
			.fill-frame {
				object-fit: fill;
				width: 100%;
				height: 600px;
			}
      "))
    ),
    ## This code fixes the DataTables warning coming up from time to time.
    tags$head(tags$script("window.alert = (function() {
          var nativeAlert = window.alert;
          return function(message) {
            //window.alert = nativeAlert;
            message.indexOf('DataTables warning') === 0 ?
            console.warn(message) :
              nativeAlert(message);
          }
        })();")),
    tags$head(tags$script(src = "js/testNumeric.js")),
    tags$head(tags$script(src = "js/google-analytics.js")),
    # tags$head(tags$script(src = "js/download-logs.js")),
    ##  Load the "Lumen" Theme (from http://bootswatch.com).
    theme = "bootstrap.css",
    tags$head(
      tags$link(href = "global-styles.css", rel = "stylesheet", type = "text/css")
    ),
    navbarPage(
      ##  Set Window Title
      windowTitle = "iNZight Lite",
      ##  Add logo and link it to the iNZight website.
      title =
        HTML(
          "<img src = 'inzight_lite_logo_web.svg' alt = 'iNZight Lite' height='150%' />"
        ),
      ## footer = img(src = "pendred_footer.png"),
      ##  Set ID
      id = "selector",
      ##  Set custom colour and collapse options.
      inverse = TRUE, collapsible = TRUE,

      ##  "About" tab.
      tabPanel(
        "About",
        uiOutput("about.panel")
      ),
      ##  "Data" tab.
      navbarMenu(
        "File",
        tabPanel(
          "Import Dataset",
          uiOutput("load.data.panel")
        ),
        tabPanel(
          "Paste Dataset",
          uiOutput("paste.data.panel")
        ),
        tabPanel(
          "Export Dataset",
          uiOutput("save.data.panel")
        ),
        tabPanel(
          "Display Dataset",
          uiOutput("current.data")
        ),
        tabPanel(
          "Dataset Examples",
          uiOutput("switch.data.panel")
        )
      ),
      tabPanel("Visualize",
        value = "visualize",
        uiOutput("visualize.panel")
      ),
      ## Row operations tab
      navbarMenu(
        "Dataset",
        tabPanel(
          "Filter Dataset",
          uiOutput("filter.dataset")
        ),
        tabPanel(
          "Sort data by variables",
          uiOutput("sort.variables")
        ),
        tabPanel(
          "Aggregate data",
          uiOutput("aggregate.variable")
        ),
        tabPanel(
          "Stack variables",
          uiOutput("stack.variables")
        ),
        tabPanel(
          "Reshape data",
          uiOutput("reshape.dataset")
        ),
        tabPanel(
          "Separate columns",
          uiOutput("separate.columns")
        ),
        tabPanel(
          "Unite columns",
          uiOutput("unite.columns")
        ),
        tabPanel(
          "Merge/Join datasets",
          uiOutput("mergejoin.datasets")
        ),
        tabPanel(
          "Alphabetise Variables",
          uiOutput("alphabetise.variables")
        ),
        tabPanel(
          "Restore data",
          uiOutput("restore.data")
        ),
        tabPanel(
          "Survey design",
          uiOutput("survey.design")
        ),
        tabPanel(
          "Frequency tables",
          uiOutput("frequency.tables")
        )
      ),
      ##  "Manipulate variables" tab.
      navbarMenu(
        "Manipulate variables",
        tabPanel(
          "Convert to categorical",
          uiOutput("convert.to.categorical")
        ),
        tabPanel(
          "Categorical variables",
          uiOutput("categorical.variables")
        ),
        tabPanel(
          "Numeric variables",
          uiOutput("numeric.variables")
        ),
        tabPanel(
          "Dates and Times",
          uiOutput("dates.times")
        ),
        tabPanel(
          "Rename Variables",
          uiOutput("rename.variables")
        ),
        tabPanel(
          "Create Variables",
          uiOutput("create.variables")
        ),
        tabPanel(
          "Missing to category",
          uiOutput("missing.categorical")
        ),
        # tabPanel("Add columns",
        #          uiOutput("add.columns")),
        # tabPanel("Reshape dataset",
        #          uiOutput("reshape.data")),
        tabPanel(
          "Delete variables",
          uiOutput("remove.columns")
        )
      ),

      ##  "Quick Explore" tab.
      navbarMenu(
        "Advanced",
        tabPanel(
          "Quick explore",
          uiOutput("quick.explore")
        ),
        tabPanel("Time Series",
          value = "timeSeries",
          uiOutput("timeseries.panel")
        ),
        tabPanel("Model Fitting",
          value = "regression",
          uiOutput("modelfitting.panel")
        ),
        tabPanel(
          "Maps",
          uiOutput("newmaps.panel")
        ),
        tabPanel(
          "Design of Experiments",
          uiOutput("mixedmodel.panel")
        ),
        tabPanel(
          "Multiple Response",
          uiOutput("multiple.response")
        ),
        tabPanel(
          "Multivariate",
          uiOutput("multivariate.panel")
        ),
        tabPanel(
          "VIT",
          uiOutput("VIT.panel")
        )
      ),
      tabPanel("R code history",
        value = "rhistory",
        uiOutput("code.panel")
      )

      ## Backup Link
      #            navbarMenu("Backup Link",
      #                       tabPanel(HTML("</a><a href=\"http://litebackup1.test-pods.auckland.ac.nz\">Backup Link 1")),
      #                       tabPanel(HTML("</a><a href=\"http://litebackup2.test-pods.auckland.ac.nz\">Backup Link 2")),
      #                       tabPanel(HTML("</a><a href=\"http://litebackup3.test-pods.auckland.ac.nz\">Backup Link 3")),
      #                       tabPanel(HTML("</a><a href=\"http://litebackup4.test-pods.auckland.ac.nz\">Backup Link 4"))
      #            )
    )
  )
)

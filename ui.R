### -----------------------###
###  UI for iNZight Lite  ###
### -----------------------###
###
###  Date Created   :   January 13, 2015
###  Last Modified  :   December 15, 2015
###
###  Please consult the comments before editing any code.
###  This file sources the ui files for each panel separately.

print(shiny::getCurrentOutputInfo())
# LITE2 = as.logical(as.integer(Sys.getenv("LITE2", 0)))

css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"


shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(css),
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet-pure-css.css")),
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
    tags$head(tags$script(src = "js/user-info.js")),
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
      # ##  "Data" tab.
      # import_tabs,
      # ## "Visualize" tab.
      # visualize_tabs,
      # ## Row operations tab
      # row_ops_tabs,
      # ##  "Manipulate variables" tab.
      # manipulate_tabs,
      # ##  "Quick Explore" tab.
      # advance_tabs,
      # tabPanel("R code history",
      #   value = "rhistory",
      #   uiOutput("code.panel")
      # )

      ## Backup Link
      #            navbarMenu("Backup Link",
      #                       tabPanel(HTML("</a><a href=\"http://litebackup1.test-pods.auckland.ac.nz\">Backup Link 1")),
      #                       tabPanel(HTML("</a><a href=\"http://litebackup2.test-pods.auckland.ac.nz\">Backup Link 2")),
      #                       tabPanel(HTML("</a><a href=\"http://litebackup3.test-pods.auckland.ac.nz\">Backup Link 3")),
      #                       tabPanel(HTML("</a><a href=\"http://litebackup4.test-pods.auckland.ac.nz\">Backup Link 4"))
      #            )
    )
))

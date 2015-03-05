###  iNZight Online main UI
###
###  Date modified: January 13, 2015.
###
###  We define the UI for iNZight Online

shinyUI(
    fluidPage(
        ##  Set Tabpanel font to be size 16.
        tags$head(
                tags$style(
                    type = "text/css",
                    ".nav {font-size:16px} ")),
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
        ##  Load the "Lumen" Theme (from http://bootswatch.com).
        theme = "bootstrap.css",
        navbarPage(
            ##  Set Window Title
            windowTitle = "iNZight Lite",
            ##  Add logo and link it to the iNZight website.
            title =
                HTML(
                "<a href = 'https://www.stat.auckland.ac.nz/~wild/iNZight/'>
                <img src = 'pendred_transp.png', width = 125, height = 32,
                     alt = 'iNZight Lite'/></a>"
                ),
            ## footer = img(src = "pendred_footer.png"),
            ##  Set ID
            id = "selector",
            ##  Set custom colour and collapse options.
            inverse = TRUE, collapsible = TRUE,
            ##  "About" tab.
            tabPanel("About",
                     uiOutput('about.panel')),
            ##  "Data" tab.
            navbarMenu("Datasets",id = "data",
                       tabPanel("Import Dataset",
                                uiOutput('load.data.panel')),
                       tabPanel("Display Dataset",
                                uiOutput('current.data')),
                       tabPanel("Remove Dataset",
                                uiOutput("remove.data.panel")),
                       tabPanel("Dataset Examples",
                                uiOutput('switch.data.panel'))),

            ##  "Modify Data" tab.
            navbarMenu("Manipulate",
                       tabPanel("Transform columns",
                                uiOutput('transform.columns')),
                       tabPanel("Reorder Levels",
                                uiOutput('reorder.levels')),
                       tabPanel("Compare dates",
                                uiOutput("compare.dates")),
                       tabPanel("Add columns",
                                uiOutput("add.columns")),
                       tabPanel("Remove columns",
                                uiOutput("remove.columns"))),

            ##  "Quick Explore" tab.
            navbarMenu("Quick Explore",
                       tabPanel("Data Summary",
                                uiOutput("quick.summary")),
                       tabPanel("Single column plot",
                                uiOutput("single.column.plot")),
                       tabPanel("Column Pair plot",
                                uiOutput("column.pair.plot")),
                       tabPanel("Compare pairs",
                                uiOutput("matrix.plot"))),

            ##  "Explore data" tab.
            tabPanel("Visualize",
                     uiOutput("visualize.panel")),

            ## "Advanced" tab, incl. time series.
            ## navbarMenu("Advanced",
            tabPanel("Time Series",
                     uiOutput("timeseries.panel")),
            ##  "Help" tab.
            tabPanel("Help",
                     uiOutput("help.panel"))
        )
    )
)

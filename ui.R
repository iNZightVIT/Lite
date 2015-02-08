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
                     uiOutput("visualize.module")),

            ## "Advanced" tab, incl. time series.
            ## navbarMenu("Advanced",
            tabPanel("Time Series",
                     uiOutput("time.series.module"))
        ),
        HTML(
          '  <div id = "wrapper">
		<div id = "footer">            
		  <span style = "float:left;">
                  <br> &nbsp;
		    <a href = "https://www.stat.auckland.ac.nz/~wild/iNZight/">
		      iNZight Project
		    </a> |		    
                    <a href = "https://github.com/iNZightVIT/Lite/">
                      R Source Code 
                    </a> |
		    <a href = "mailto:cpar137@aucklanduni.ac.nz?Subject=iNZight-Lite%20Feedback"
		       target = "_top">
		      Contact Us
		    </a>
                    <br><br>
                    &nbsp;© 2015 iNZight | All Rights Reserved 
		  </span>
		  <span style = "float:right;">
                    <a href = "http://new.censusatschool.org.nz/">
		      <img src = "census_logo.png"/, height = 75>
		    </a> &nbsp; &nbsp;
                    <a href = "http://www.stats.govt.nz/">
		      <img src = "stats_nz.png"/, height = 75>
		    </a> &nbsp; &nbsp;
                    <a href = "http://www.minedu.govt.nz/">
		      <img src = "minedu_logo.png"/, height = 60, width = 170>
		    </a> &nbsp; &nbsp;
		    <a href = "http://stat.auckland.ac.nz">
		      <!-- <img src = "statistics_logo.png", height = 75, width = 160> -->
                      <img src = "uoa_logo.png", height = 65>
                    </a> &nbsp;
		  </span>
		</div>
	      </div>'
        )       
    )
)

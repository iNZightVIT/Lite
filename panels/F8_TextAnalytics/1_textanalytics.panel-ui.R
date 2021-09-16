# panel: textanalytics.panel 
textanalytics.sidebarPanel <- function(){
	sidebarPanel(uiOutput("textanalytics.ui.sidebar"))
}

textanalytics.mainPanel = function(){
	mainPanel(uiOutput("textanalytics.ui.main"))
}

textanalytics.panel.ui = function(data.set) {
	fluidPage(
		  if (is.null(data.set)) {
			  fluidRow(includeMarkdown("panels/F8_TextAnalytics/4_textanalytics-panel-null.md"))
		  } else {
			  sidebarLayout(textanalytics.sidebarPanel(),
					textanalytics.mainPanel())
		  }
	)
}

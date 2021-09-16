output$textanalytics.panel <- renderUI({
  textanalytics.panel.ui(get.data.set())
})

output$textanalytics.ui.sidebar <- renderUI({
	tabsetPanel(id = "sidebarstate", type = "hidden",
		    tabPanelBody("process", sidebarprocess()),
		    tabPanelBody("visualisation", sidebarvisualisation()))
})

output$textanalytics.ui.main <- renderUI({
	tabsetPanel(id="mainstate", type="tabs",
		    tabPanel("Process", value="process"),
		    tabPanel("Visualisation", value="visualisation"))
})

observeEvent(input$mainstate,
	     updateTabsetPanel(inputId="sidebarstate",
			       selected = input$mainstate)
)

################################################################################

sidebarprocess <- reactive({
	tagList(
		checkboxInput("lemmatize", "Lemmatize"),
		selectInput("stopwords", "Select the Stopword Lexicon",
			    list(`Good` = list("a", "b", "c"),
				 `Bad` = list("d", "e", "f"))),
		textOutput("result"),
		selectInput("section", "Select what to Section by",
			    list("Sentence", "Paragraph", "Chapter")),
		actionButton("processEvent", "Prepare Text")
	)
})

output$result <- renderText({
	paste("You chose", input$stopwords)
})

sidebarvisualisation <- reactive({
	tagList(
		selectInput("insight", "Select what you want to Visualise",
			    list(`Term Insight` = list("Term Frequency",
						       "n-gram Frequency",
						       "Key Words",
						       "Term Sentiment",
						       "Moving Average Term Sentiment"),
				 `Aggregate Insight` = list("Term Count",
							    "Key Sections",
							    "Aggregate Sentiment",
							    "Word Correlation"))),
		selectInput("visualisation", "Select how to visualise it",
			    list("Bar Chart", "etc", "etc...")),
		uiOutput("options"),
		checkboxInput("scale", "Scale Fixed")
	)
})

output$options <- renderText(input$insight)

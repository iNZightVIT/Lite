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
		    tabPanel("Process", mainprocess(), value="process"),
		    tabPanel("Visualisation", mainvisualisation(), value="visualisation"))
})



observeEvent(input$mainstate,
	     updateTabsetPanel(inputId="sidebarstate",
			       selected = input$mainstate)
)

## Sidebar panel

sidebarprocess <- reactive({
	tagList(
		checkboxInput("lemmatize", "Lemmatize"),
		selectInput("stopwords", "Select the Stopword Lexicon",
			    list(`Good` = list("a", "b", "c"),
				 `Bad` = list("d", "e", "f"))),
                renderText({
                	paste("You chose", input$stopwords)
                }),
		selectInput("section", "Select what to Section by",
			    list("Sentence", "Paragraph", "Chapter")),
		actionButton("processEvent", "Prepare Text")
	)
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
                renderText(input$insight),
		checkboxInput("scale", "Scale Fixed")
	)
})

## Object creation

processed_corpus <- bindEvent(reactive({
                                        quanteda::corpus(get.data.set())
                                        }),
                            input$processEvent)
processed_tokens <- reactive({
    quanteda::tokens(processed_corpus())
    })
processed_dfm <- reactive({
    quanteda::dfm(processed_tokens())
    })

## Main panel

mainprocess <- reactive(tagList(
                radioButtons("text_process_type", "Text Data Structure",
                			choices = list("Original"),
                			inline = TRUE),
                            renderUI({
                                    if (identical(input$text_process_type, "Original")) {
                                        dataTableOutput("original_rendering")
                                    } else if (identical(input$text_process_type, "Corpus")) {
                                        verbatimTextOutput("corpus_rendering")
                                    } else if (identical(input$text_process_type, "Tokens")) {
                                        verbatimTextOutput("tokens_rendering")
                                    } else if (identical(input$text_process_type, "DFM")) {
                                        verbatimTextOutput("DFM_rendering")
                                    }})
))

output$original_rendering <- renderDataTable(get.data.set())
output$corpus_rendering <- renderPrint(processed_corpus())
output$tokens_rendering <- renderPrint(processed_tokens())
output$DFM_rendering <- renderPrint(processed_dfm())

observeEvent(input$processEvent,
                 updateRadioButtons(inputId="text_process_type",
                                     choices = list("Original", "Corpus", "Tokens", "DFM"),
                                     inline = TRUE,
                                     selected = input$text_process_type))

mainvisualisation <- reactive(renderText("goodbye"))

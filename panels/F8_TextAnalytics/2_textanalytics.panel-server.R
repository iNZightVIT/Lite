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
			    list("Keywords in Context",
			         "Hierarchical Cluster",
			         "Lexical Diversity",
			         "Term Frequency",
			         "Text LDA",
			         "Keyness")),
		uiOutput("insight_options"),
		selectInput("visualisation", "Select how to visualise it", choices = NULL),
		uiOutput("visualisation_options")
	)
})

output$insight_options <- renderUI({
    if (identical(input$insight, "Keywords in Context")) {
        updateSelectInput(inputId = "visualisation", choices = list("verbatim"))
        tagList(
            textInput("kwic_text", "Enter key words you wish to see in context")
            )
    } else if (identical(input$insight, "Hierarchical Cluster")){
        updateSelectInput(inputId = "visualisation", choices = list("verbatim", "hclust"))
        tagList(
            textInput("cluster_op", "Enter Cluster")
            )
    } else tagList()
})

output$visualisation_options <- renderText({
	"placeholder for vis options"
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
insight <- reactive({
    if (identical(input$insight, "Keywords in Context")) {
        quanteda::kwic(processed_tokens(), input$kwic_text)
    } else if (identical(input$insight, "Hierarchical Cluster")) {
        hclust(as.dist(quanteda.textstats::textstat_dist(processed_dfm())))
    }
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

mainvisualisation <- reactive(
    renderUI({
        if (identical(input$visualisation, "verbatim")) {
            verbatimTextOutput("verbatim_rendering")
        } else if (identical(input$visualisation, "hclust")) {
            plotOutput("hclust_rendering")
            }
        })
    )

output$verbatim_rendering <- renderPrint(insight())
output$hclust_rendering <- renderPlot(plot(insight(), xlab = "Distance", ylab = NULL))

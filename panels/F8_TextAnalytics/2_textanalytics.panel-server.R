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
    		selectInput("corpus_aggregation",
            		"Select the level of Aggregation for the corpus",
            		list("documents", "paragraphs", "sentences")),
            	checkboxInput("remove_punct", "Remove Punctuation"),
		checkboxInput("stopwords_remove", "Remove Stopwords"),
		sliderInput("termfreq_trim", "Keep features with quantile term frequency",
                            min=0, max=1, value=c(0,1), step = 0.05),
		sliderInput("docfreq_trim", "Keep features with proportional document frequency",
                            min=0, max=1, value=c(0,1), step = 0.05),
                fileInput("dictionary", "Upload Lookup Dictionary"),
                helpText("Supported dictionary file formats include WordStat, LIWC, Lexicoder v2 and v3, and Yoshikoder formats"),
		actionButton("processEvent", "Prepare Text")
	)
})

sidebarvisualisation <- reactive({
	tagList(
		selectInput("insight", "Select what you want to Visualise",
			    list("Keywords in Context",
			         "Hierarchical Cluster",
			         "Lexical Diversity",
			         "Feature Frequency",
			         "Topic Models",
			         "Keyness")),
		uiOutput("insight_options"),
		uiOutput("visualisation_options")
	)
})

output$insight_options <- renderUI({
    if (identical(input$insight, "Keywords in Context")) {
        tagList(
            selectInput("visualisation", "Select how to visualise it", choices = "verbatim"),
            textInput("kwic_text", "Enter key words you wish to see in context")
            )
    } else if (identical(input$insight, "Hierarchical Cluster")){
        tagList(
            selectInput("visualisation", "Select how to visualise it", choices = list("hclust", "verbatim")),
            textInput("cluster_op", "Enter Cluster")
            )
    } else if (identical(input$insight, "Lexical Diversity")) {
        tagList(
            selectInput("visualisation", "Select how to visualise it", choices = list("verbatim"))
            )
    } else if (identical(input$insight, "Feature Frequency")) {
        tagList(
            selectInput("visualisation", "Select how to visualise it", choices = list("verbatim"))
            )
    } else if (identical(input$insight, "Topic Models")) {
        tagList(
            selectInput("visualisation", "Select how to visualise it", choices = list("verbatim"))
            )
    } else if (identical(input$insight, "Keyness")) {
        tagList(
            selectInput("visualisation", "Select how to visualise it", choices = list("keyness"))
            )
    }  else tagList()
})

output$visualisation_options <- renderText({
	"placeholder for vis options"
    })

## Object creation

processed_corpus <- bindEvent(reactive({
    quanteda::corpus_reshape(quanteda::corpus(get.data.set()), to=input$corpus_aggregation)
                                }),
                            input$processEvent)
processed_tokens <- reactive({
    toks <- quanteda::tokens(processed_corpus(), remove_punct = input$remove_punct)
    if (input$stopwords_remove) quanteda::tokens_remove(toks, pattern = quanteda::stopwords("en")) else toks
})
processed_dfm <- reactive({
    dfmat <- quanteda::dfm(processed_tokens())
    if (any(sort(input$termfreq_trim) != c(0,1)))
        dfmat <- quanteda::dfm_trim(dfmat,
                                    min_termfreq=min(input$termfreq_trim),
                                    max_termfreq=max(input$termfreq_trim),
                                    termfreq_type = "quantile")
    if (any(sort(input$docfreq_trim) != c(0,1)))
        dfmat <- quanteda::dfm_trim(dfmat,
                                    min_docfreq=min(input$docfreq_trim),
                                    max_docfreq=max(input$docfreq_trim),
                                    docfreq_type = "prop")
    if (!is.null(input$dictionary$datapath))
        dfmat <- quanteda::dfm_lookup(dfmat, quanteda::dictionary(file = input$dictionary$datapath))
    dfmat
})
insight <- reactive({
    if (identical(input$insight, "Keywords in Context")) {
        quanteda::kwic(processed_tokens(), input$kwic_text)
    } else if (identical(input$insight, "Hierarchical Cluster")) {
        # if (nrow(processed_dfm()) < 3)
        #     stop("Need at least 3 documents for Hierarchical Clustering")
        hclust(as.dist(quanteda.textstats::textstat_dist(processed_dfm())))
    } else if (identical(input$insight, "Lexical Diversity")) {
        quanteda.textstats::textstat_lexdiv(processed_dfm())
    } else if (identical(input$insight, "Feature Frequency")) {
        quanteda.textstats::textstat_frequency(processed_dfm(), n = 15)
    } else if (identical(input$insight, "Topic Models")) {
        seededlda::terms(seededlda::textmodel_lda(processed_dfm(), k = 4))
    } else if (identical(input$insight, "Keyness")) {
        quanteda.textstats::textstat_keyness(processed_dfm(), target = 1L)
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
        } else if (identical(input$visualisation, "keyness")) {
            plotOutput("keyness_rendering")
        }
        })
    )

output$verbatim_rendering <- renderPrint(insight())
output$hclust_rendering <- renderPlot(plot(insight(), xlab = "Distance", ylab = NULL))
output$keyness_rendering <- renderPlot(quanteda.textplots::textplot_keyness(insight()))

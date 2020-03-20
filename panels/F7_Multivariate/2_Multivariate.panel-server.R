###-------------------------------------------------------###
###  Server Functions for the "Multiple Response" Module  ###
###-------------------------------------------------------###
###
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *



## initialize gui
output$multivariate.panel <- renderUI({
  multivariate.panel.ui(get.data.set())
})

mr.par = reactiveValues(plotSet = list(),
                        objName = "response",
                        guessName = TRUE,
                        mrObject = NULL,
                        combp = NULL)


################
## left panel ##
################

output$multivarate.widgets <- renderUI({
  ret = NULL
  get.data.set()
  input$multivarate.method
  isolate({
    numeric.vars <- names(which(sapply(get.data.set(), is.numeric)))
    n.numeric <- sum(sapply(get.data.set(), is.numeric))
    multi.var <- selectInput("multivarate.select.var",
                             label = h5(strong("Select variables:")),
                             choices = numeric.vars,
                             multiple = T,
                             selectize = F,
                             size = 10,
                             selected = mrOptions$vars.save)
    
    color.by <- selectInput("multivarate.colour.by",
                            label = h5("Colour by: "),
                            selected = mrOptions$group,
                            choices = c("None", names(which(!sapply(get.data.set(), is.numeric)))),
                            selectize = FALSE)
    
    trans <- sliderInput("multivarate.trans", label = h5("Transparency: "),
                         0, 100, value = mrOptions$trans.save, step = 10)
    
    shape.by <- selectInput("multivarate.shape.by",
                            label = h5("Shape by: "),
                            selected = mrOptions$shape,
                            choices = c("None", names(which(!sapply(get.data.set(), is.numeric)))),
                            selectize = FALSE)
    dim.to.plot <- fixedRow(column(12, h5("Dimensions to Plot: ")),
                            column(5, selectInput("multivarate.select.x",
                                                              label = h5("x: "),
                                                              choices = 1:n.numeric, selected = mrOptions$dim1,
                                                  selectize = FALSE)),
                            column(5, selectInput("multivarate.select.y",
                                                  label = h5("y: "),
                                                  choices = 1:n.numeric, selected = mrOptions$dim2,
                                                  selectize = FALSE)))
    
    decompose.to <- sliderInput("multivarate.compose.to", label = h5("N dimensions to decompose to:"),
                                2, n.numeric, value = mrOptions$k, step = 1)
    
    
    if(input$multivarate.method == "pairs" || input$multivarate.method == "pairs_corr"){
      ret = multi.var
    } else if (input$multivarate.method == "pcp"){
      ret = list(multi.var,
                 color.by,
                 trans)
      } else if (input$multivarate.method == "pca") {
        ret = list(multi.var,
                   color.by,
                   shape.by,
                   dim.to.plot)
      } else if (input$multivarate.method == "mds") {
        ret = list(color.by,
                   shape.by,
                   decompose.to,
                   dim.to.plot)
      }
  })
  ret
})



mrOptions <- reactiveValues(
  group = NULL,
  shape = NULL,
  vars.save = NULL,
  trans.save = 0,
  dim1 = 1,
  dim2 = 2
)




observe({
  input$multivarate.colour.by
  isolate({
    if(!is.null(input$multivarate.colour.by)){
      if(input$multivarate.colour.by == "None"){
        mrOptions$group = NULL
      } else {
        mrOptions$group = input$multivarate.colour.by
      }
    }
  })
})

observe({
  input$multivarate.shape.by
  isolate({
    if(!is.null(input$multivarate.shape.by)){
      if(input$multivarate.shape.by == "None"){
        mrOptions$shape = NULL
      } else {
        mrOptions$shape = input$multivarate.shape.by
      }
    }
  })
})


observe({
  input$multivarate.select.var
  isolate({
    if((is.null(input$multivarate.select.var) || length(input$multivarate.select.var) == 0) && !is.null(get.data.set())){
      mrOptions$vars = get.numeric.column.names(get.data.set())
    } else {
      mrOptions$vars = input$multivarate.select.var
      mrOptions$vars.save = input$multivarate.select.var
    }
  })
})

observe({
  input$multivarate.select.var
  mrOptions$type
  isolate({
    if (mrOptions$type %in% c("pca", "mds") && length(mrOptions$type) > 0) {
      if (length(mrOptions$vars) > 1) {
        updateSelectInput(session, inputId = "multivarate.select.x",
                          choices = 1:length(mrOptions$vars), selected = mrOptions$dim1)
        updateSelectInput(session, inputId = "multivarate.select.y",
                          choices = 1:length(mrOptions$vars), selected = min(mrOptions$dim2, length(mrOptions$vars)))
      } else {
        mrOptions$vars <- names(which(sapply(get.data.set(), is.numeric)))
      }
    }
  })
})

observe({
  input$multivarate.trans
  isolate({
    if(!is.null(input$multivarate.trans) ){
      mrOptions$alpha <- 1 - input$multivarate.trans / 100
      mrOptions$trans.save = input$multivarate.trans
    }
  })
})

method.labels <- c(
  "Pairs Plot"                          = "pairs",
  "Correlation Pairs  Plot"             = "pairs_corr",
  "Parallel Coordinates"                = "pcp",
  "Principal Components Analysis"       = "pca",
  "Multidimensional Scaling"            = "mds"# ,
  # "Non-Metric Multidimensional Scaling" = "nmds"
)

observe({
  input$multivarate.method
  isolate({
    if(!is.null(input$multivarate.method) ){
      mrOptions$type <- input$multivarate.method
    }
  })
})


observe({
  input$multivarate.select.x
  isolate({
    if(!is.null(input$multivarate.select.x) ){
      mrOptions$dim1 <- as.numeric(input$multivarate.select.x)
    }
  })
})

observe({
  input$multivarate.select.y
  isolate({
    if(!is.null(input$multivarate.select.y) ){
      mrOptions$dim2 <- as.numeric(input$multivarate.select.y)
    }
  })
})

observe({
  mrOptions$k = if(is.null(get.data.set())) NULL else sum(sapply(get.data.set(), is.numeric))
})


observe({
  input$multivarate.compose.to
  isolate({
    if(!is.null(input$multivarate.compose.to) ){
      mrOptions$k <- as.numeric(input$multivarate.compose.to)
    }
  })
})


################
##            ##
## main panel ##
##            ##
################

output$multivarate.ui.main <- renderUI({
  ret = NULL
  mrOptions$type
  isolate({
    if (mrOptions$type %in% c("pairs", "pairs_corr")) {
      ret = list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" is you wish...
          ##  Tab 1: Time Series Plot
          tabPanel(
            title = "Plot",
              br(),
            plotOutput("mv.plot", height = "500px")
          ))
      )
    } else if (mrOptions$type == "pcp"){
      ret = list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" is you wish...
          ##  Tab 1: Time Series Plot
          tabPanel(
            title = "Plot",
            br(),
            plotOutput("mv.plot", height = "500px")),
          tabPanel(
            title = "Interactive Plot (via plotly)",
            plotlyOutput("plotly_pairs_corrmain", height = "500px") %>% withSpinner()
          ))
      )
    } else if (mrOptions$type == "pca") {
      ret = list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" is you wish...
          ##  Tab 1: Time Series Plot
          tabPanel(
            title = "Plot",
            br(),
            plotOutput("mv.pca", height = "500px")),
          tabPanel(
            title = "Summary",
            br(),
            verbatimTextOutput("mv.summary")),
          tabPanel(
            title = "Screeplot",
            br(),
            plotOutput("mv.screeplot", height = "500px")),
          tabPanel(
            title = "Interactive Plot (via plotly)",
            uiOutput("plotly_pcamainnw"),
            plotlyOutput("plotly_pcamain", height = "500px") %>% withSpinner()
          ))
      )
    } else if (mrOptions$type == "mds"){
      ret = list(
        tabsetPanel(
          type = "pills", # Try type = "tabs" is you wish...
          ##  Tab 1: Time Series Plot
          tabPanel(
            title = "Plot",
            br(),
            plotOutput("mv.pca", height = "500px")
          ),
          tabPanel(
            title = "Interactive Plot (via plotly)",
            plotlyOutput("plotly_pcamain", height = "500px") %>% withSpinner()
          ))
      )
    }
  })
}) 

mul.plot.parm <- reactive({
  plot_fun <- list(
    pcp = iNZightMultivariate::inz.parcoord,
    pairs = iNZightMultivariate::inzight.ggpairs,
    pairs_corr = iNZightMultivariate::inzight.corr
  )[[mrOptions$type]]
  
  plot_arg_names <- list(
    pcp = c("vars", "group", "alpha"),
    pairs = c("vars"),
    pairs_corr = c("vars")
  )[[mrOptions$type]]
  
  mrOptions.list = reactiveValuesToList(mrOptions)
  plot_args <- mrOptions.list[plot_arg_names]
  plot_args <- plot_args[!is.na(names(plot_args))]
  
  if (mrOptions$type == "pairs") {
    plot(c(0, 1), c(0, 1), ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")
    text(0.5, 0.5, "Generating pairs plot - please wait... ")
  }
  plot_exprs <- do.call(plot_fun, c(list(values$data.name), plot_args))
  
  eval_env <- rlang::env(!!rlang::sym(values$data.name) := get.data.set())
  eval_results <- lapply(plot_exprs, eval, envir = eval_env)
  plot_object <- eval_results[[length(eval_results)]]
  dev.hold()
  tryCatch(
    print(plot_object),
    finally = dev.flush()
  )
})

output$mv.plot <- renderPlot({
  get.data.set()
  input$multivarate.method
  mrOptions$vars
  mrOptions$group
  mrOptions$alpha
  isolate({
    tryCatch({
      mul.plot.parm()
      }, error = function(e) {
      print(e)
    }, finally = {})
  })  
})


plotly_pairs_corrmain <- renderPlotly({
  get.data.set()
  input$multivarate.method
  mrOptions$vars
  mrOptions$group
  mrOptions$alpha
  isolate({
    pdf(NULL)
    mul.plot.parm()
    print(plotly::ggplotly())
  })
})


  
mul.plot.pca <- reactive({
  tryCatch({
    analysis_fun <- list(
      pca = iNZightMultivariate::inzight.pca,
      mds = iNZightMultivariate::inzight.mds,
      nmds = iNZightMultivariate::inzight.nmds
    )[[mrOptions$type]]
    
    plot_arg_names <- list(
      pca = c("vars", "dim1", "dim2"),
      mds = c("vars", "k"),
      nmds = c("vars", "k")
    )[[mrOptions$type]]
    mrOptions.list = reactiveValuesToList(mrOptions)
    plot_args <- mrOptions.list[plot_arg_names]
    plot_args <- plot_args[!is.na(names(plot_args))]
    
    names(plot_args) <- replace(names(plot_args), names(plot_args) == "dim1", "x")
    names(plot_args) <- replace(names(plot_args), names(plot_args) == "dim2", "y")
    
    plot_exprs <- do.call(analysis_fun, c(list(values$data.name), plot_args))
    
    eval_env <- rlang::env(!!rlang::sym(values$data.name) := get.data.set())
    
    eval_results <- lapply(plot_exprs, eval, envir = eval_env)
    
    plot_object <- eval_results[[length(eval_results)]]
    
    
    mrOptions$mrObject <- plot_object
    attr(mrOptions$mrObject, "var_name") <<- as.character(as.list(plot_exprs[[1]])[[2]])
    attr(mrOptions$mrObject, "code") <<- list(
      "## Perform analysis",
      analysis = paste0(unname(unlist(lapply(plot_exprs, rlang::expr_text))), collapse = "\n\n")
    )
    
    plot_fun <- list(
      pca = iNZightMultivariate::plot_inzight.pca,
      mds = iNZightMultivariate::plot_inzight.mds,
      nmds = iNZightMultivariate::plot_inzight.nmds
    )[[mrOptions$type]]
    
    mvObject_name <- attr(mrOptions$mrObject, "var_name")
    
    plot_exprs <- plot_fun(
      mvObject_name,
      data = values$data.name,
      colour = mrOptions$group,
      shape = mrOptions$shape,
      x = mrOptions$dim1,
      y = mrOptions$dim2
    )
    
    eval_env <- rlang::env(
      !!rlang::sym(values$data.name) := get.data.set(),
      !!rlang::sym(mvObject_name) := mrOptions$mrObject
    )
    
    eval_results <- lapply(plot_exprs, eval, envir = eval_env)
    
    plot_object <- eval_results[[length(eval_results)]]
    
    if (length(mrOptions$vars) > 1) {
      dev.hold()
      tryCatch(
        print(plot_object),
        finally = dev.flush()
      )
    } else {
      dev.hold()
      tryCatch({
        plot(1, 1, type = "n")
        text(0.5, 0.5, labels = "Please select more than one variable")
      }, finally = dev.flush())
    }
    mrOptions$text <- paste0(iNZightMultivariate::summary_inzight.pca(mrOptions$mrObject), sep = "\n")
  }, error = function(e) {
    print(e)
  }, finally = {})
})

output$mv.pca <- renderPlot({
  get.data.set()
  input$multivarate.method
  mrOptions$vars
  mrOptions$group
  mrOptions$alpha
  mrOptions$shape
  mrOptions$dim1
  mrOptions$dim2
  mrOptions$k
  isolate({
    mul.plot.pca()
  })
})


output$mv.summary <- renderText({
  mrOptions$text
})

output$mv.screeplot <- renderPlot({
  plot(mrOptions$mrObject, type = "l", main = sprintf("Screeplot of PCA on %s", values$data.name))
})


output$plotly_pcamain <- renderPlotly({
  get.data.set()
  input$multivarate.method
  mrOptions$vars
  mrOptions$group
  mrOptions$alpha
  mrOptions$shape
  mrOptions$dim1
  mrOptions$dim2
  mrOptions$k
  isolate({
    pdf(NULL)
    mul.plot.pca()
    plotly::ggplotly()
  })
})




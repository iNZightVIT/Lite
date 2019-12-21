

output$survey.design = renderUI({
  create.design.panel(get.data.set())
})

## specify design
setDesign = function(strata = NULL, clus1 = NULL, clus2 = NULL,
                     wt = NULL, nest = NULL, fpc = NULL,
                     repweights = NULL, reptype = NULL,
                     scale = NULL, rscales = NULL,
                     poststrat = NULL, name,
                     type = c("survey", "replicate")) {
  if (is.null(strata) & is.null(clus1) & is.null(clus2) &
      is.null(wt) & is.null(nest) & is.null(fpc) &
      is.null(repweights) & is.null(poststrat)) {
    list(dataDesign = NULL,
         dataDesignName = name)
  } else {
    dataDesign = 
      switch(type,
             "survey" = list(
               strata = strata,
               clus1  = clus1,
               clus2  = clus2,
               wt     = wt,
               fpc    = fpc,
               nest   = nest,
               poststrat = poststrat,
               type = type
             ),
             "replicate" = list(
               wt = wt,
               repweights = repweights,
               reptype = reptype,
               scale = scale,
               rscales = rscales,
               poststrat = poststrat,
               type = type
             )
      )
    dataDesignName =
      sprintf("%s.%s",
              name,
              switch(type, "survey" = "svy", "replicate" = "repsvy")
      )
    list(dataDesign = dataDesign,
         dataDesignName = dataDesignName)
  }
}

## create survey object
createSurveyObject = function(design) {
  des <- design$dataDesign
  dataSet <- get.data.set()
  weights <- if (is.null(des$wt)) "NULL" else paste("~", des$wt)
  if (!is.null(des$type) && length(des$type) > 0 && des$type == "survey") {
    id <- if (is.null(des$clus1) & is.null(des$clus2)) {
      "~ 1"
    } else if (is.null(des$clus1)) {
      paste("~", des$clus2)
    } else if (is.null(des$clus2)) {
      paste("~", des$clus1)
    } else {
      paste("~", des$clus1, "+", des$clus2)
    }
    
    strata <- if (is.null(des$strata)) "NULL" else paste("~", des$strata)
    fpcs <- if (is.null(des$fpc)) "NULL" else paste("~", des$fpc)
    obj <-
      parse(text =
              paste0(
                "survey::svydesign(",
                "id = ", id, ", ",
                if (!is.null(des$strata)) sprintf("strata = %s, ", strata),
                if (!is.null(des$wt) || !is.null(des$freq))
                  sprintf("weights = %s, ", weights),
                if (!is.null(des$fpc)) sprintf("fpc = %s, ", fpcs),
                if (!is.null(des$nest) && des$nest) "nest = TRUE, ",
                "data = dataSet)"
              )
      )
  } else {
    ## replicate weights specified
    repweights <- if(is.null(des$repweights)) "NULL"
    else paste("~", paste(des$repweights, collapse = " + "))
    type <- des$reptype
    rscales <- if (is.null(des$rscales)) "NULL"
    else sprintf("c(%s)", paste(des$rscales, collapse = ", "))
    obj <-
      parse(text =
              paste0("survey::svrepdesign(",
                     if (!is.null(des$wt))
                       sprintf("weights = %s, ", weights),
                     sprintf("repweights = %s, ", repweights),
                     sprintf("type = '%s', ", type),
                     if (!is.null(des$scale))
                       sprintf("scale = %s, ", des$scale),
                     if (!is.null(des$rscales))
                       sprintf("rscales = %s, ", rscales),
                     "data = dataSet)"
              )
      )
  }
  
  if (!is.null(des$poststrat)) {
    design_obj <- eval(obj)
    ## Note: if allowing continuous variables in future,
    ##       this needs a better name:
    pop.totals <- structure(
      do.call(c,
              c(
                list(sum(des$poststrat[[1]]$Freq)),
                lapply(des$poststrat, function(df) df$Freq[-1])
              )
      ),
      .Names = do.call(c,
                       c(
                         list("(Intercept)"),
                         lapply(des$poststrat, function(df)
                           paste0(names(df)[1], as.character(df[-1,1]))
                         )
                       )
      )
    )
    obj <- parse(
      text = sprintf(
        "survey::calibrate(design_obj, ~%s, pop.totals)",
        paste(names(des$poststrat), collapse = " + ")
      )
    )
  }
  
  eval(obj)
}


svalue_or_null <- function(x) {
  if (x == " ") return(NULL)
  x
}

## reactive design object
design_param = reactive({
  input$create.design
  input$create.design1
  isolate({
    if(req(input$svytype) == "survey" && req(input$create.design) > 0) {
      strat <- svalue_or_null(input$stratVar)
      clus1 <- svalue_or_null(input$clus1Var)
      clus2 <- svalue_or_null(input$clus2Var)
      wts <- svalue_or_null(input$wtVar)
      fpc <- svalue_or_null(input$fpcVar)
      nest <- as.logical(input$nestChk)
      name <- values$data.name
      clear <- is.null(input$strat) && is.null(input$clus1) &&
        is.null(input$clus2) && is.null(input$wts) && is.null(input$fpc)
      setDesign(
        strata = strat,
        clus1 = clus1,
        clus2 = clus2,
        wt = wts,
        nest = nest,
        fpc = fpc,
        type = "survey",
        name = name
      )
    } else if (req(input$svytype) == "replicate" && req(input$create.design1) > 0) {
      wts <- svalue_or_null(input$sample.weight.Var)
      repWts <- input$repVars
      reptype <- input$repType
      scale <- as.numeric(input$repScale)
      rscales <- as.numeric(repRscales$rscales)
      name <- values$data.name
      if (length(rscales) == 0)
        rscales <- rep(scale, length(repWts))
      else if(any(is.na(rscales)))
        rscales <- NULL
      clear <- is.null(wts) && length(repWts) == 0
      setDesign(
        wt = wts,
        repweights = repWts,
        reptype = reptype,
        scale = scale,
        rscales = rscales,
        type = "replicate",
        name = name
      )
    }
  })
})



## create data frame
repRscales <- reactiveValues(
  rep.weight = character(),
  rscales = numeric()
)

observe({
  if(req(input$repRscalesClear) > 0){
    isolate({
      repRscales$rep.weight = character()
      repRscales$rscales = numeric()
      repRscales$df = NULL
    })
  }
})

observe({
  if(req(design_param()$dataDesign$type) == "replicate"){
    isolate({
      repRscales$rep.weight = design_param()$dataDesign$repweights
      repRscales$rscales = design_param()$dataDesign$rscales
    })
  }
})



observeEvent(input$repRscalesBtn, { 
  if(file.exists(input$repRscalesBtn[1, "datapath"])) {
    isolate({
      x1 <- readLines(input$repRscalesBtn[1, "datapath"], n = 1)
      file_has_header <- suppressWarnings(is.na(as.numeric(x1)))
      df <- read.csv(input$repRscalesBtn[1, "datapath"], header = file_has_header)
      if (nrow(df) != length(input$repVars)) {
        shinyalert("You need to specify one scale per replicate.", type = "error")
      } else {
        repRscales$rscales <- df[,1]
        repRscales$rep.weight = input$repVars
        repRscales$df = df[,-1]
      }
    })
  }
})

## table
output$rscalesTbl = renderTable({
  if(!is.null(repRscales$df)){
    cbind(data.frame(rep.weight = repRscales$rep.weight, rscales = repRscales$rscales), repRscales$df)
  } else {
    data.frame(rep.weight = repRscales$rep.weight, rscales = repRscales$rscales)
  }
})





#######################
##                   ##
##   Post stratify   ##
##                   ## 
#######################

observe({
  if (is.null(plot.par$design) && req(input$svytype) == "post") {
     shinyalert("Please specify a survey design first", type = "warning")
  }
})


## initial value

lvldf = reactiveValues(df = NULL)

observe({
  req(input$PSvar)
  factorvars <- names(get.data.set())[sapply(
    get.data.set(),
    function(v)
      length(levels(v)) > 0 && sum(is.na(v)) == 0
  )]
   for (v in factorvars) {
    if (is.null(lvldf$df[[v]])) {
      d <- data.frame(
        a = levels(get.data.set()[[v]]),
        b = NA
      )
      names(d) <- c(v, "Freq")
      lvldf$df[[v]] <<- d
    }
  }
})

  

output$svypost_ui <- renderUI({
  ret = NULL
  if (!is.null(plot.par$design) && req(input$svytype) == "post") {
    isolate({
      h5(strong("Specify post stratification"))
      
      factorvars <- names(get.data.set())[sapply(
        get.data.set(),
        function(v)
          length(levels(v)) > 0 && sum(is.na(v)) == 0
      )]
      
      title = fluidRow(column(12, h5(strong("Specify post stratification"))))
      main =  fluidRow(column(4, selectInput("PSvar",
                                             label="Choose variables: ",
                                             choices=factorvars,
                                             multiple = T,
                                             selectize = F,
                                             size = 18),
                              helpText("Hold CTRL or SHIFT to select multiple")),
                       column(8, tags$div(style = "margin-top: -1px;
                                                   border: null;
                                                   height: 436px;
                                                   overflow-y: auto;",
                                          tags$head(
                                            tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                                                       .inline .form-group{display: table-row;}")
                                          ),
                                          uiOutput("PSlevel"))))
    })
    ret = list(
      title,
      main 
      )
  }
})

output$PSlevel <- renderUI({
  req(input$PSvar)
  ret = tagList()
  isolate({
    for (v in input$PSvar) {
      ret[[v]] = tagList()
      ret[[v]][[1]] = fluidRow(column(12, h5(strong(paste(v, 'Frequency')))))
      for (i in seq_along(1:nrow(lvldf$df[[v]]))){
        ret[[v]][[i+1]] = fluidRow(column(8, tags$div(class = "inline", textInput(paste0("PS", v, i), label = as.character(lvldf$df[[v]][, 1][i])))
                                          ))
      }
    }
  })
  ret
})

observe(print(lvldf$df[[input$PSvar]]))


## create design
observe({
  input$create.design
  input$create.design1
  isolate({
    req(design_param())
    setOk <- try(createSurveyObject(design_param()))
    if (!inherits(setOk, "try-error")) {
      call <- do.call(paste, c(as.list(deparse(setOk$call)), sep = "\n"))

      call <- sprintf("%s <- %s",
                      design_param()$dataDesignName,
                      gsub("dataSet", values$data.name, call))
      code.save$variable = c(code.save$variable, list(c("\n", "## create survey design object")))
      code.save$variable = c(code.save$variable, list(c("\n", call, "\n")))
      
      plot.par$design = createSurveyObject(design_param())
      ## print result
      output$create.design.summary <- renderPrint({
        summary(createSurveyObject(design_param()))
      })
    } else if(inherits(setOk, "try-error")){
      output$create.design.summary <- renderText({
        paste0(
          "There is a problem with the specification of the survey design:\n\n",
          setOk)
      })
    }
  })
})
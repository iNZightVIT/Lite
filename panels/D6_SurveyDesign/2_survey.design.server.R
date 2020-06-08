

output$survey.design = renderUI({
  create.design.panel(get.data.set())
})

design.model.fit <- reactiveValues()


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

fpc.f <-reactive({
  if(req(input$fpcVar) == ' ') {
    return(NULL)
  } else if(req(input$fpcVar) != ' ' && req(input$fpcVar2) == ' ') {
    return(input$fpcVar)
  } else if(req(input$fpcVar) != ' ' && req(input$fpcVar2) != ' '){
    return(paste0(input$fpcVar, ' + ', input$fpcVar2))
  } else {
    NULL
  }
})

observe({
  if(req(input$fpcVar) == ' ') {
    shinyjs::reset("fpcVar2")
    shinyjs::disable("fpcVar2")
  } else if(req(input$fpcVar) != ' ' && req(input$fpcVar2) == ' ') {
    shinyjs::enable("fpcVar2")
  } else if(req(input$fpcVar) != ' ' && req(input$fpcVar2) != ' '){
    shinyjs::enable("fpcVar2")
  } else {
    shinyjs::disable("fpcVar2")
  }
})

design_params = reactiveValues()


observe({
  input$create.design
  input$create.design1
  isolate({
    if(req(input$svytype) == "survey" && req(input$create.design) > 0) {
      strat <- svalue_or_null(input$stratVar)
      clus1 <- svalue_or_null(input$clus1Var)
      clus2 <- svalue_or_null(input$clus2Var)
      wts <- svalue_or_null(input$wtVar)
      fpc <- fpc.f()
      nest <- as.logical(input$nestChk)
      name <- values$data.name
      clear <- is.null(input$strat) && is.null(input$clus1) &&
        is.null(input$clus2) && is.null(input$wts) && is.null(input$fpc)
      design_params$design = setDesign(
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
      if (reptype %in% c("bootstrap", "other")) {
        scale <- as.numeric(input$repScale)
        rscales <- as.numeric(repRscales$rscales)
        if (length(rscales) == 0)
          rscales <- rep(scale, length(repWts))
        else if(any(is.na(rscales)))
          rscales <- NULL
      } else {
        scale <- NULL
        rscales <- NULL
      }
      name <- values$data.name
      clear <- is.null(wts) && length(repWts) == 0
      design_params$design = setDesign(
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
  if(req(design_params$design$dataDesign$type) == "replicate"){
    isolate({
      repRscales$rep.weight = design_params$design$dataDesign$repweights
      repRscales$rscales = design_params$design$dataDesign$rscales
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
  input$svytype
  isolate({
    if (is.null(plot.par$design) && !is.null(input$svytype) && input$svytype == "post") {
      shinyalert(text = "Please specify a survey design first", title = "No design specified", type = "warning")
    }
  })
  
})

observe({
  if(input$selector == "Survey design"){
    updateSelectInput(session, inputId = "svytype", label = "Select survey design", choices = list("Specify design" = "survey",
                                                                                                   "Specify replicate design" = "replicate",
                                                                                                   "Post stratify" = "post"),
                      selected = "survey")
  }
})


## initial value

lvldf = reactiveValues(df = NULL,
                       upload = logical())

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
        ret[[v]][[i+1]] = fluidRow(column(10, textInput(paste0("PS", v, i), label = as.character(lvldf$df[[v]][, 1][i]))
        ) )
      }
      ret[[v]][[nrow(lvldf$df[[v]]) + 2]] = fluidRow(column(10, fileInput(paste0("PS", v, "data"),
                                                                          label="Read from file ...", multiple=F),
                                                            hr()))
    }
  })
  ret
})

observe({
  req(input$PSvar)
  lvldf$df
  for (v in input$PSvar) {
    for (i in seq_along(1:nrow(lvldf$df[[v]]))){
      if(is.null(input[[paste0("PS", v, "data")]])){
        if(!is.null(input[[paste0("PS", v, i)]]) && input[[paste0("PS", v, i)]] != ""){
          lvldf$df[[v]]$Freq[lvldf$df[[v]][,1] == as.character(lvldf$df[[v]][, 1][i])] = as.numeric(input[[paste0("PS", v, i)]])
          updateTextInput(session, inputId = paste0("PS", v, i), label = as.character(lvldf$df[[v]][, 1][i]), 
                          value = lvldf$df[[v]]$Freq[lvldf$df[[v]][,1] == as.character(lvldf$df[[v]][, 1][i])])
        } else  {
          lvldf$df[[v]]$Freq[lvldf$df[[v]][,1] == as.character(lvldf$df[[v]][, 1][i])] = NA
          updateTextInput(session, inputId = paste0("PS", v, i), label = as.character(lvldf$df[[v]][, 1][i]), 
                          value = "")
        }
      } else {
        ## import data
        x1 <- readLines(input[[paste0("PS", v, "data")]][1, "datapath"], n = 1)
        file_has_header <- suppressWarnings(is.na(as.numeric(x1)))
        df <- read.csv(input[[paste0("PS", v, "data")]][1, "datapath"], header = file_has_header)
        if (nrow(df) != 2) {
          shinyalert("File needs to have 2 columns: one for variable names, and one for frequencies.", type = "error")
          shinyjs::reset(paste0("PS", v, "data"))
        } else if (nrow(df) != nrow(lvldf$df[[v]])){
          shinyalert("File needs to have one row for each level.", type = "error")
          shinyjs::reset(paste0("PS", v, "data"))
        } else {
          names(df) <- c(v, "Freq")
          lvldf$df[[v]] <- df
          updateTextInput(session, inputId = paste0("PS", v, i), label = as.character(df[, 1][i]), 
                          value = df[, 2][i])
          shinyjs::disable(paste0("PS", v, i))
        }
      }  
    }
  }
})


## create design
observe({
  input$create.design
  input$create.design1
  isolate({
    req(design_params$design)
    setOk <- try(createSurveyObject(design_params$design))
    if (!inherits(setOk, "try-error")) {
      call <- do.call(paste, c(as.list(deparse(setOk$call)), sep = "\n"))
      
      call <- sprintf("%s <- %s",
                      design_params$design$dataDesignName,
                      gsub("dataSet", values$data.name, call))
      design.model.fit$code <- call
      code.save$variable = c(code.save$variable, list(c("\n", "## create survey design object")))
      code.save$variable = c(code.save$variable, list(c("\n", call, "\n")))
      
      plot.par$design = createSurveyObject(design_params$design)
      ## print result
      output$create.design.summary <- renderPrint({
        summary(plot.par$design)
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



## create design
observe({
  input$create.design2
  isolate({
    req(design_params$design)
    PSDesign <- setDesign(
      strata = design_params$design$dataDesign$strat,
      clus1 = design_params$design$dataDesign$clus1, clus2 = design_params$design$dataDesign$clus2,
      wt = design_params$design$dataDesign$wt, nest = design_params$design$dataDesign$nest,
      fpc = design_params$design$dataDesign$fpc, repweights = design_params$design$dataDesign$repWts, 
      type = design_params$design$dataDesign$type,
      name = design_params$design$dataDesign$name,
      poststrat = if (length(input$PSvar) != 0) lvldf$df[input$PSvar] else NULL
    )
    setOk <- try(createSurveyObject(PSDesign))
    if (!inherits(setOk, "try-error")) {
      design_params$design$dataDesign <-  PSDesign$dataDesign
      
      
      call <- do.call(paste, c(as.list(deparse(setOk$call)), sep = "\n"))
      call <- sprintf("%s <- %s",
                      paste0(design_params$design$dataDesignName, ".ps"),
                      gsub("design_obj", design_params$design$dataDesignName, call))
      design_params$design$dataDesignName <-  paste0(design_params$design$dataDesignName, ".ps")
      design.model.fit$code <- call
      code.save$variable = c(code.save$variable, list(c("\n", "## create survey design object")))
      code.save$variable = c(code.save$variable, list(c("\n", call, "\n")))
      
      plot.par$design = createSurveyObject(PSDesign)
      ## print result
      output$create.design.summary <- renderPrint({
        summary(plot.par$design)
      })
    } else if(inherits(setOk, "try-error")){
      output$create.design.summary <- renderText({
        paste0(
          "Something went wrong during post stratification ...",
          PSDesign)
      })
    }
  })
})


## remove design

observe({
  input$remove.design
  input$remove.design1
  input$remove.design2
  isolate({
    plot.par$design=NULL
    design_params$design = NULL
  })
})



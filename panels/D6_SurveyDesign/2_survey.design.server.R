output$survey.design <- renderUI({
  create.design.panel(get.data.set())
})

design.model.fit <- reactiveValues()

setDesign <- function(x) {
  if (missing(x)) {
    design_params$design$dataDesign <- NULL
    design_params$design$dataDesignName <- values$data.name
    return()
  }

  if (inherits(x, "inzsvyspec")) {
    if (is.null(x$design)) {
      x <- surveyspec::make_survey(values$data.set, x)
    }
  } else {
    spec <- structure(
      list(
        spec = list(
          ids = if (is.null(x$ids)) 1 else x$ids,
          probs = x$probs,
          strata = x$strata,
          fpc = x$fpc,
          nest = as.logical(x$nest),
          weights = x$weights,
          survey_type = x$survey_type,
          repweights = x$repweights,
          scale = x$scale,
          rscales = x$rscales,
          type = x$type,
          calibrate = x$calibrate,
          calfun = if (is.null(x$calibrate)) NULL else "linear"
        )
      ),
      class = "inzsvyspec"
    )
    x <- surveyspec::make_survey(values$data.set, spec)
  }
  design_params$design$dataDesign <- unclass(x)
  design_params$design$dataDesignName <- sprintf(
    "%s.%s",
    values$data.name,
    switch(x$spec$survey_type,
      "survey" = "svy",
      "replicate" = "repsvy"
    )
  )
  # when design changed, update the object
  invisible(createSurveyObject(reload = TRUE))
}

currentDesign <- reactiveValues()
currentDesign$info <- NULL

createSurveyObject <- function(reload = FALSE) {
  if (!is.null(currentDesign$info$design) && !reload) {
    return(currentDesign$info$design)
  }
  currentDesign$info <- design_params$design$dataDesign
  currentDesign$info$design
}




svalue_or_null <- function(x) {
  if (x == " ") {
    return(NULL)
  }
  x
}

fpc.f <- reactive({
  if (req(input$fpcVar) == " ") {
    return(NULL)
  } else if (req(input$fpcVar) != " " && req(input$fpcVar2) == " ") {
    return(input$fpcVar)
  } else if (req(input$fpcVar) != " " && req(input$fpcVar2) != " ") {
    return(paste0(input$fpcVar, " + ", input$fpcVar2))
  } else {
    NULL
  }
})

observe({
  if (req(input$fpcVar) == " ") {
    shinyjs::reset("fpcVar2")
    shinyjs::disable("fpcVar2")
  } else if (req(input$fpcVar) != " " && req(input$fpcVar2) == " ") {
    shinyjs::enable("fpcVar2")
  } else if (req(input$fpcVar) != " " && req(input$fpcVar2) != " ") {
    shinyjs::enable("fpcVar2")
  } else {
    shinyjs::disable("fpcVar2")
  }
})

design_params <- reactiveValues()


observe({
  input$create.design
  input$create.design1
  isolate({
    if (req(input$svytype) == "survey" && req(input$create.design) > 0) {
      strat <- svalue_or_null(input$stratVar)
      clus1 <- svalue_or_null(input$clus1Var)
      clus2 <- svalue_or_null(input$clus2Var)
      if (is.null(clus1) && is.null(clus2)) {
        clus <- NULL
      } else if (!is.null(clus1) && !is.null(clus2)) {
        clus <- paste(clus1, clus2, sep = " + ")
      } else {
        clus <- ifelse(is.null(clus1), clus2, clus1)
      }
      wts <- svalue_or_null(input$wtVar)
      fpc <- fpc.f()
      nest <- as.logical(input$nestChk)
      clear <- is.null(input$strat) && is.null(input$clus1) &&
        is.null(input$clus2) && is.null(input$wts) && is.null(fpc.f())
      set <- try(setDesign(
        list(
          strata = strat,
          ids = clus,
          weights = wts,
          nest = nest,
          fpc = fpc,
          survey_type = "survey"
        )
      ), silent = TRUE)
    } else if (req(input$svytype) == "replicate" &&
      req(input$create.design1) > 0) {
      wts <- svalue_or_null(input$sample.weight.Var)
      repWts <- input$repVars
      reptype <- input$repType
      if (reptype %in% c("bootstrap", "other")) {
        scale <- as.numeric(input$repScale)
        rscales <- as.numeric(repRscales$rscales)
        if (length(rscales) == 0) {
          rscales <- rep(scale, length(repWts))
        } else if (any(is.na(rscales))) {
          rscales <- NULL
        }
      } else {
        scale <- NULL
        rscales <- NULL
      }
      clear <- is.null(wts) && length(repWts) == 0
      set <- try(setDesign(
        list(
          weights = wts,
          repweights = repWts,
          type = reptype,
          scale = scale,
          rscales = rscales,
          survey_type = "replicate"
        )
      ), silent = TRUE)
    }


    setOk <- try(createSurveyObject())
    if (!inherits(set, "try-error")) {
      call <- do.call(paste, c(as.list(deparse(setOk$call)), sep = "\n"))

      call <- sprintf(
        "%s <- %s",
        design_params$design$dataDesignName,
        gsub("dataSet", values$data.name, call)
      )
      design.model.fit$code <- call
      code.save$variable <- c(
        code.save$variable,
        list(c("\n", "## create survey design object"))
      )
      code.save$variable <- c(code.save$variable, list(c("\n", call, "\n")))

      plot.par$design <- createSurveyObject()
      ## print result
      output$create.design.summary <- renderPrint({
        summary(plot.par$design)
      })
    } else if (inherits(set, "try-error")) {
      output$create.design.summary <- renderText({
        paste0(
          "There is a problem with the specification of the survey design:\n\n",
          set
        )
      })
    }
  })
})



## create data frame
repRscales <- reactiveValues(
  rep.weight = character(),
  rscales = numeric()
)

observe({
  if (req(input$repRscalesClear) > 0) {
    isolate({
      repRscales$rep.weight <- character()
      repRscales$rscales <- numeric()
      repRscales$df <- NULL
    })
  }
})

observe({
  if (req(design_params$design$dataDesign$type) == "replicate") {
    isolate({
      repRscales$rep.weight <- design_params$design$dataDesign$repweights
      repRscales$rscales <- design_params$design$dataDesign$rscales
    })
  }
})



observeEvent(input$repRscalesBtn, {
  if (file.exists(input$repRscalesBtn[1, "datapath"])) {
    isolate({
      x1 <- readLines(input$repRscalesBtn[1, "datapath"], n = 1)
      file_has_header <- suppressWarnings(is.na(as.numeric(x1)))
      df <- read.csv(input$repRscalesBtn[1, "datapath"],
        header = file_has_header, stringsAsFactors = TRUE
      )
      if (nrow(df) != length(input$repVars)) {
        shinyalert("You need to specify one scale per replicate.",
          type = "error"
        )
      } else {
        repRscales$rscales <- df[, 1]
        repRscales$rep.weight <- input$repVars
        repRscales$df <- df[, -1]
      }
    })
  }
})

## table
output$rscalesTbl <- renderTable({
  if (!is.null(repRscales$df)) {
    cbind(data.frame(stringsAsFactors = TRUE, rep.weight = repRscales$rep.weight, rscales = repRscales$rscales), repRscales$df)
  } else {
    data.frame(stringsAsFactors = TRUE, rep.weight = repRscales$rep.weight, rscales = repRscales$rscales)
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
    if (is.null(plot.par$design) && !is.null(input$svytype) &&
      input$svytype == "post") {
      shinyalert(text = "Please specify a survey design first", title = "No design specified", type = "warning")
    }
  })
})

observe({
  if (input$selector == "Survey design") {
    updateSelectInput(session,
      inputId = "svytype", label = "Select survey design",
      choices = list(
        "Specify design" = "survey",
        "Specify replicate design" = "replicate",
        "Post stratify" = "post",
        "Read from file" = "read"
      ),
      selected = "survey"
    )
  }
})


## initial value

lvldf <- reactiveValues(
  df = NULL,
  upload = logical()
)

observe({
  req(input$PSvar)
  factorvars <- names(get.data.set())[sapply(
    get.data.set(),
    function(v) {
      length(levels(v)) > 0 && sum(is.na(v)) == 0
    }
  )]
  for (v in factorvars) {
    if (is.null(lvldf$df[[v]])) {
      d <- data.frame(
        stringsAsFactors = TRUE,
        a = levels(get.data.set()[[v]]),
        b = NA
      )
      names(d) <- c(v, "Freq")
      lvldf$df[[v]] <<- d
    }
  }
})



output$svypost_ui <- renderUI({
  ret <- NULL
  if (!is.null(plot.par$design) && req(input$svytype) == "post") {
    isolate({
      h5(strong("Specify post stratification"))

      factorvars <- names(get.data.set())[sapply(
        get.data.set(),
        function(v) {
          length(levels(v)) > 0 && sum(is.na(v)) == 0
        }
      )]

      title <- fluidRow(column(12, h5(strong("Specify post stratification"))))
      main <- fluidRow(
        column(
          4, selectInput("PSvar",
            label = "Choose variables: ",
            choices = factorvars,
            multiple = T,
            selectize = F,
            size = 18
          ),
          helpText("Hold CTRL or SHIFT to select multiple")
        ),
        column(8, tags$div(
          style = "margin-top: -1px;
                   border: null;
                   height: 436px;
                   overflow-y: auto;",
          uiOutput("PSlevel")
        ))
      )
    })
    ret <- list(
      title,
      main
    )
  }
})

output$PSlevel <- renderUI({
  req(input$PSvar)
  ret <- tagList()
  isolate({
    for (v in input$PSvar) {
      ret[[v]] <- tagList()
      ret[[v]][[1]] <- fluidRow(column(12, h5(strong(paste(v, "Frequency")))))
      for (i in seq_along(1:nrow(lvldf$df[[v]]))) {
        ret[[v]][[i + 1]] <- fluidRow(column(
          10,
          textInput(paste0("PS", v, i),
            label = as.character(lvldf$df[[v]][, 1][i])
          )
        ))
      }
      ret[[v]][[nrow(lvldf$df[[v]]) + 2]] <- fluidRow(column(
        10, fileInput(paste0("PS", v, "data"),
          label = "Read from file ...", multiple = F
        ),
        hr()
      ))
    }
  })
  ret
})

observe({
  req(input$PSvar)
  lvldf$df
  for (v in input$PSvar) {
    for (i in seq_along(1:nrow(lvldf$df[[v]]))) {
      if (is.null(input[[paste0("PS", v, "data")]])) {
        if (!is.null(input[[paste0("PS", v, i)]]) &&
          input[[paste0("PS", v, i)]] != "") {
          lvldf$df[[v]]$Freq[
            lvldf$df[[v]][, 1] == as.character(lvldf$df[[v]][, 1][i])
          ] <- as.numeric(input[[paste0("PS", v, i)]])
          updateTextInput(session,
            inputId = paste0("PS", v, i),
            label = as.character(lvldf$df[[v]][, 1][i]),
            value = lvldf$df[[v]]$Freq[
              lvldf$df[[v]][, 1] == as.character(lvldf$df[[v]][, 1][i])
            ]
          )
        } else {
          lvldf$df[[v]]$Freq[
            lvldf$df[[v]][, 1] == as.character(lvldf$df[[v]][, 1][i])
          ] <- NA
          updateTextInput(session,
            inputId = paste0("PS", v, i),
            label = as.character(lvldf$df[[v]][, 1][i]),
            value = ""
          )
        }
      } else {
        ## import data
        x1 <- readLines(input[[paste0("PS", v, "data")]][1, "datapath"], n = 1)
        file_has_header <- suppressWarnings(is.na(as.numeric(x1)))
        df <- read.csv(input[[paste0("PS", v, "data")]][1, "datapath"],
          header = file_has_header, stringsAsFactors = TRUE
        )
        if (nrow(df) != 2) {
          shinyalert("File needs to have 2 columns: one for variable names, and one for frequencies.",
            type = "error"
          )
          shinyjs::reset(paste0("PS", v, "data"))
        } else if (nrow(df) != nrow(lvldf$df[[v]])) {
          shinyalert("File needs to have one row for each level.",
            type = "error"
          )
          shinyjs::reset(paste0("PS", v, "data"))
        } else {
          names(df) <- c(v, "Freq")
          lvldf$df[[v]] <- df
          updateTextInput(session,
            inputId = paste0("PS", v, i), label = as.character(df[, 1][i]),
            value = df[, 2][i]
          )
          shinyjs::disable(paste0("PS", v, i))
        }
      }
    }
  }
})







## create design
observe({
  input$create.design2
  isolate({
    req(design_params$design)
    curDes <- design_params$design$dataDesign$spec

    cal_list <- lapply(
      names(lvldf$df),
      function(var) {
        x <- lvldf$df[[var]]$Freq
        names(x) <- lvldf$df[[var]][[var]]
        x
      }
    )
    names(cal_list) <- names(lvldf$df)
    set <- try(setDesign(
      modifyList(
        curDes,
        list(calibrate = if (length(input$PSvar)) {
          cal_list[input$PSvar]
        } else {
          NULL
        })
      )
    ), silent = TRUE)

    setOk <- try(createSurveyObject())
    if (!inherits(set, "try-error")) {
      call <- do.call(paste, c(as.list(deparse(setOk$call)), sep = "\n"))
      call <- sprintf(
        "%s <- %s",
        paste0(design_params$design$dataDesignName, ".ps"),
        gsub("design_obj", design_params$design$dataDesignName, call)
      )
      design_params$design$dataDesignName <-
        paste0(design_params$design$dataDesignName, ".ps")
      design.model.fit$code <- call
      code.save$variable <- c(
        code.save$variable,
        list(c("\n", "## create survey design object"))
      )
      code.save$variable <- c(code.save$variable, list(c("\n", call, "\n")))

      plot.par$design <- createSurveyObject()
      ## print result
      output$create.design.summary <- renderPrint({
        summary(plot.par$design)
      })
    } else if (inherits(set, "try-error")) {
      output$create.design.summary <- renderText({
        "Something went wrong during post stratification ..."
      })
    }
  })
})


## remove design

observe({
  input$remove.design
  input$remove.design1
  input$remove.design2
  input$remove.design3
  isolate({
    plot.par$design <- NULL
    design_params$design <- NULL
  })
})


## read the design from file


observeEvent(input$svy.design.spec, {
  if (file.exists(input$svy.design.spec[1, "datapath"])) {
    isolate({
      svyspec <- surveyspec::import_survey(input$svy.design.spec[1, "datapath"])
      set <- try(setDesign(svyspec), silent = TRUE)
      setOK <- try(
        createSurveyObject(),
        silent = TRUE
      )

      if (!inherits(set, "try-error")) {
        ## write design call
        call <- paste(deparse(setOK$call), collapse = "\n")
        plot.par$design <- createSurveyObject()
        ## print result
        output$create.design.summary <- renderPrint({
          summary(plot.par$design)
        })
      } else {
        output$create.design.summary <- renderText({
          paste0(
            "There is a problem with the survey specification file:\n\n",
            set
          )
        })
      }
    })
  }
})



output$estimate.pop.size <- renderUI({
  input$wtVar
  isolate({
    if (!is.null(input$wtVar) && length(input$wtVar) > 0 &&
      input$wtVar != " ") {
      size <- round(sum(get.data.set()[[input$wtVar]]))
      h5(paste0("Estimated population size: ", size))
    }
  })
})

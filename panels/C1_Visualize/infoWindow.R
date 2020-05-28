
inf.par <- reactiveValues()

output$interence_test = renderUI({
  get.data.set()
  ret = NULL
  input$vari1
  input$vari2
  input$type.inference.select
  design_params$design
  isolate({
    values.list = reactiveValuesToList(plot.par)
    if (!is.null(plot.par$freq))
      plot.par$freq <- get.data.set()[[plot.par$freq]]
    if (!is.null(plot.par$x)) {
      ## Figure out what type of inference will be happening:
      xnum <- iNZightTools::is_num(plot.par$x)
      if (is.null(plot.par$y)) {
        inf.par$INFTYPE <- INFTYPE <- ifelse(xnum, "onesample-ttest", "oneway-table")
      } else {
        ynum <- iNZightTools::is_num(plot.par$y)
        if (xnum && ynum) {
          inf.par$INFTYPE <- INFTYPE <- "regression"
        } else if (xnum | ynum) {
          M <-
            if (xnum) length(levels(plot.par$y))
          else length(levels(plot.par$x))
          if (M == 2) inf.par$INFTYPE <- INFTYPE <- "twosample-ttest"
          if (M > 2) inf.par$INFTYPE <- INFTYPE <- "anova"
        } else {
          inf.par$INFTYPE <- INFTYPE <- "twoway-table"
        }
      }
      if (INFTYPE == "regression") {
        tmp.x <- values.list$y
        values.list$y <- values.list$x
        values.list$x <- tmp.x
        v <- values.list$varnames
        values.list$varnames$x <- v$y
        values.list$varnames$y <- v$x
      }
      ## Design or data?
      inf.par$is_survey <- is_survey <- FALSE
      if (!is.null(design_params$design$dataDesign)) {
        plot.par$data <- NULL
        plot.par$design <- createSurveyObject(design_params$design)
        inf.par$is_survey <- is_survey <- TRUE
      }
      doHypTest <- grepl("ttest|anova|table", INFTYPE)
      if (doHypTest &&
          is_survey &&
          grepl("oneway-table", INFTYPE)) {
        ## don't do it for chi-square (yet)
        doHypTest <- length(levels(plot.par$x)) == 2
      }
      test.type <- switch(
        INFTYPE,
        "onesample-ttest" = "One Sample t-test",
        "twosample-ttest" = "Two Sample t-test",
        "anova"           = "ANOVA",
        "regression"      = "Regression Analysis",
        "oneway-table"    =
          ifelse(is_survey && length(levels(plot.par$x)) == 2,
                 "Test proportion",
                 "Chi-square test"
          ),
        "twoway-table"    = "Chi-square test"
      )
      inf.par$TTEST <- TTEST <- grepl("ttest", INFTYPE)
      inf.par$TTEST2 <- TTEST2 <- INFTYPE == "twosample-ttest"
      inf.par$CHI2 <- CHI2 <- grepl("twoway-table", INFTYPE) ||
        (grepl("oneway-table", INFTYPE) && !is_survey)
      inf.par$PROPTEST <- PROPTEST <- INFTYPE == "oneway-table"
      inf.par$PROPTEST2 <- PROPTEST2 <- PROPTEST && length(levels(plot.par$x)) == 2
      if (doHypTest) {
        if(TTEST2){
          ret = list(column(3, radioButtons("hypTest", 
                                            label = h5(strong("Hypothesis Testing")), 
                                            choices = c("None" = 1, "Two Sample t-test" = 2, "ANOVA" = 3),
                                            selected = NULL)))
        } else if  (PROPTEST2 && !is_survey) {
          ret = list(column(3, radioButtons("hypTest", 
                                            label = h5(strong("Hypothesis Testing")), 
                                            choices = c("None" = 1, "Chi-square test" = 2, "Test proportion" = 3),
                                            selected = NULL)))
        } else {
          ret = list(
            column(3, checkboxInput("hypTest1",
                                label = test.type, 
                                value = TRUE)))
        }
      }
      
      if (doHypTest) {
        if (TTEST) {
          ret = list(ret, 
                     column(3, h5("Null Value:")),
                     column(6, textInput(inputId = "hypVal", value = 0, label = NULL))
          )
        } else if (PROPTEST2) {
          ret = list(ret, 
                     column(3, h5("Null Value:")),
                     column(6, textInput(inputId = "hypVal", value = 0.5, label = NULL))
          )
        }
        ## alternative hypothesis
        
        if (PROPTEST2) {
          if(is_survey){
            ret = list(ret,
                       column(3, h5("Alternative Hypothesis:"), offset = 3),
                       column(6, selectInput(inputId = "hypAlt",
                                             label = NULL,
                                             choices = c("two sided", "greater than", "less than"),
                                             #selected = input$hypothesis_twosample,
                                             selectize = F)))
          } else {
            ret = list(ret,
                       column(3, h5("Alternative Hypothesis:")),
                       column(6, selectInput(inputId = "hypAlt",
                                             label = NULL,
                                             choices = c("two sided", "greater than", "less than"),
                                             #selected = input$hypothesis_twosample,
                                             selectize = F)))
          }
        } else if (TTEST){
          if(TTEST2){
            ret = list(ret,
                       column(3, h5("Alternative Hypothesis:")),
                       column(6, selectInput(inputId = "hypAlt",
                                             label = NULL,
                                             choices = c("two sided", "greater than", "less than"),
                                             #selected = input$hypothesis_twosample,
                                             selectize = F)))
          } else {
            ret = list(ret,
                       column(3, h5("Alternative Hypothesis:"), offset = 3),
                       column(6, selectInput(inputId = "hypAlt",
                                             label = NULL,
                                             choices = c("two sided", "greater than", "less than"),
                                             #selected = input$hypothesis_twosample,
                                             selectize = F)))
          }
        }
        
        ## use equal variance assumption?
        if (TTEST2 && !is_survey) {
          ret = list(ret,
                     column(6, offset = 3,
                            checkboxInput("hypEqualVar", label = "Use equal-variance", value = FALSE, width = NULL)))
        }
        
        ## Simulate p-value
        if (CHI2 && !is_survey) {
          ret = list(ret,
                     column(6, offset = 3,
                            checkboxInput("hypSimPval", label = "Simulate p-value", value = FALSE, width = NULL)))
        }
        
        ## exact p-value
        if (PROPTEST2 && !is_survey) {
          ret = list(ret,
                     column(6, offset = 6,
                            checkboxInput("hypExactPval", label = "Calculate exact p-value", value = FALSE, width = NULL)))
        }
      }
      
      #if (INFTYPE == "regression") {
      #  ret = list(column(5, checkboxGroupInput(inputId = 'inf.trend.chk', label = h5(strong("Trend options (select at least one)")), 
      #                                          choices = c("Linear trend", "Quadratic trend", "Cubic trend"), selected = NULL,
      #                                          inline = FALSE)))
      #}
      
    }
    
    
  })
  ret
})

observe({
  input$inf.trend.chk
  isolate({
    plot.par$trend = input$inf.trend.chk
  })
})


output$visualize.inference = renderPrint({
  if(input$plot_selector%in%"Inference"){
    input$hypTest
    input$hypTest1
    input$hypVal
    input$hypAlt
    input$hypEqualVar
    input$hypSimPval
    input$hypExactPval
    input$inf.trend.chk
    input$vari1
    input$vari2
    input$subs1
    input$type.inference.select
    design_params$design

    isolate({
        xnum <- iNZightTools::is_num(plot.par$x)
        if (is.null(plot.par$y)) {
          INFTYPE <- ifelse(xnum, "onesample-ttest", "oneway-table")
        } else {
          ynum <- iNZightTools::is_num(plot.par$y)
          if (xnum && ynum) {
            INFTYPE <- "regression"
          } else if (xnum | ynum) {
            M <-
              if (xnum) length(levels(plot.par$y))
            else length(levels(plot.par$x))
            if (M == 2) INFTYPE <- "twosample-ttest"
            if (M > 2) INFTYPE <- "anova"
          } else {
            INFTYPE <- "twoway-table"
          }
        }
        ## Design or data?
        is_survey <- FALSE
        if (!is.null(design_params$design$dataDesign)) {
          is_survey <- TRUE
        }
        doHypTest <- grepl("ttest|anova|table", INFTYPE)
        if (doHypTest &&
            is_survey &&
            grepl("oneway-table", INFTYPE)) {
          ## don't do it for chi-square (yet)
          doHypTest <- length(levels(plot.par$x)) == 2
        }
        test.type <- switch(
          INFTYPE,
          "onesample-ttest" = "One Sample t-test",
          "twosample-ttest" = "Two Sample t-test",
          "anova"           = "ANOVA",
          "regression"      = "Regression Analysis",
          "oneway-table"    =
            ifelse(is_survey && length(levels(plot.par$x)) == 2,
                   "Test proportion",
                   "Chi-square test"
            ),
          "twoway-table"    = "Chi-square test"
        )
        TTEST <- grepl("ttest", INFTYPE)
        TTEST2 <- INFTYPE == "twosample-ttest"
        CHI2 <- grepl("twoway-table", INFTYPE) ||
          (grepl("oneway-table", INFTYPE) && !is_survey)
        PROPTEST <- INFTYPE == "oneway-table"
        PROPTEST2 <- PROPTEST && length(levels(plot.par$x)) == 2
        curSet <- modifyList(reactiveValuesToList(plot.par),
                             reactiveValuesToList(graphical.par), keep.null = TRUE)
        curSet$plottype <- NULL
        if (!is.null(design_params$design$dataDesign)) {
          curSet$data <- NULL
          curSet$design <- createSurveyObject(design_params$design)
        }
        if (!is.null(curSet$freq))
          curSet$freq <- get.data.set()[[curSet$freq]]
        if (!is.null(curSet$x)) {
          if (is.numeric(curSet$x) && is.numeric(curSet$y)) {
            tmp.x <- curSet$y
            curSet$y <- curSet$x
            curSet$x <- tmp.x
            v <- curSet$varnames
            curSet$varnames$x <- v$y
            curSet$varnames$y <- v$x
          }
        }
        if (is.null(curSet$g1) && !is.null(curSet$g2)) {
          if (curSet$g2.level != "_ALL") {
            curSet$g1 <- curSet$g2
            curSet$g1.level <- curSet$g2.level
            curSet$varnames$g1 <- curSet$varnames$g2
          }
          curSet$g2 <- NULL
          curSet$g2.level <- NULL
          curSet$varnames$g2 <- NULL
        }
        
        if(input$type.inference.select == 1){
          bs.inf = F
        } else if(input$type.inference.select == 2){
          bs.inf= T
        }

        curSet <- modifyList(
          curSet,
          list(bs.inference = bs.inf,
               summary.type = "inference",
               inference.type = "conf",
               inference.par = NULL),
          keep.null = TRUE
        )
        tryCatch({
          ## one sample t-test
          if(iNZightTools::is_num(plot.par$x) && is.null(plot.par$y)){
            if(input$hypTest1){
              curSet <- modifyList(
                curSet,
                list(hypothesis.value = as.numeric(input$hypVal),
                     hypothesis.alt = switch(input$hypAlt, 
                                             "two sided" = "two.sided", 
                                             "greater than" = "greater", 
                                             "less than" = "less"),
                     hypothesis.test = "t.test"),
                keep.null = TRUE
              )
            } else {
              curSet <- modifyList(
                curSet,
                list(hypothesis = NULL),
                keep.null = TRUE
              )
            }
          } else if(length(levels(plot.par$x)) == 2 && is.null(plot.par$y)){
            ## test for binary x
            ## for survey obj
            if(is_survey){
              if(input$hypTest1){
                curSet <- modifyList(
                  curSet,
                  list(hypothesis.value = as.numeric(input$hypVal),
                       hypothesis.alt = switch(input$hypAlt, 
                                               "two sided" = "two.sided", 
                                               "greater than" = "greater", 
                                               "less than" = "less"),
                       hypothesis.test = "proportion"),
                  keep.null = TRUE
                )
              } else {
                curSet <- modifyList(
                  curSet,
                  list(hypothesis = NULL),
                  keep.null = TRUE
                )
              }
            } else {
              ## non-survey obj
              if(input$hypTest == 2){
                curSet <- modifyList(
                  curSet,
                  list(hypothesis.simulated.p.value = input$hypSimPval,
                       hypothesis.test = "chi2"),
                  keep.null = TRUE
                )
              } else if (input$hypTest == 3) {
                curSet <- modifyList(
                  curSet,
                  list(hypothesis.value = as.numeric(input$hypVal),
                       hypothesis.use.exact = input$hypExactPval,
                       hypothesis.alt = switch(input$hypAlt, 
                                               "two sided" = "two.sided", 
                                               "greater than" = "greater", 
                                               "less than" = "less"),
                       hypothesis.test = "proportion"),
                  keep.null = TRUE
                )
              } else {
                curSet <- modifyList(
                  curSet,
                  hypothesis.test = "default",
                  keep.null = TRUE
                )
              } 
            }
          } else if ((length(levels(plot.par$x)) > 2 && is.null(plot.par$y)) || 
                     (is.factor(plot.par$x) && is.factor(plot.par$y))){
            ## chi-square test
              if(input$hypTest1){
                curSet <- modifyList(curSet,
                                     list(hypothesis.simulated.p.value = input$hypSimPval,
                                          hypothesis.test = "chi2"),
                                     keep.null = TRUE
                )
              } else {
                curSet <- modifyList(
                  curSet,
                  list(hypothesis = NULL),
                  keep.null = TRUE
                )
              }
          } else if ((length(levels(plot.par$x)) == 2 && is.numeric(plot.par$y)) || 
                     (is.numeric(plot.par$x) && length(levels(plot.par$y)) == 2)){
            ## two sample t-test
            if(input$hypTest == 2) {
              curSet <- modifyList(
                curSet,
                list(hypothesis.value = as.numeric(input$hypVal),
                     hypothesis.var.equal = input$hypEqualVar,
                     hypothesis.alt = switch(input$hypAlt, 
                                             "two sided" = "two.sided", 
                                             "greater than" = "greater", 
                                             "less than" = "less"),
                     hypothesis.test = "t.test"),
                keep.null = TRUE
              )
            } else if (input$hypTest == 3){
              curSet <- modifyList(
                curSet,
                list(hypothesis.test = "anova"),
                keep.null = TRUE
              )
            } else {
              curSet <- modifyList(
                curSet,
                list(hypothesis.test = "default"),
                keep.null = TRUE
              )
            }
          } else if ((length(levels(plot.par$x)) > 2 && is.numeric(plot.par$y)) || 
                     (is.numeric(plot.par$x) && length(levels(plot.par$y)) > 2)){
            if(input$hypTest1){
              curSet <- modifyList(
                curSet,
                list(hypothesis.test = "anova"),
                keep.null = TRUE)
              } else {
                curSet <- modifyList(
                  curSet,
                  list(hypothesis = NULL),
                  keep.null = TRUE
                )
              }
            }
        }, error = function(e) {})
        
        #if(is.numeric(plot.par$x) && is.numeric(plot.par$x)){
        #  curSet <- modifyList(
        #    curSet,
        #    list(trend = plot.par$trend),
        #    keep.null = TRUE)
        #}
        pdf(NULL)
        
        tryCatch({
          do.call(iNZightPlots:::getPlotSummary, curSet)
          #saveRDS(values.list, file = "/Users/tongchen/Documents/work/Lite/b.rds")
        }, warning = function(w) {
          print(w)
        }, error = function(e) {
          print(e)
        }, finally = {})
        
        #      if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
        #           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
        #        tryCatch({
        #          cat(do.call(iNZightPlots:::getPlotSummary, values.list), sep = "\n")
        #        }, warning = function(w) {
        #          print(w)
        #        }, error = function(e) {
        #          print(e)
        #        }, finally = {})
        #      }else{
        #        suppressWarnings(try(cat(do.call(iNZightPlots:::getPlotSummary, values.list), sep = "\n")))
        #      }
        
      
    })
  }
})


## enable/disable widgets

observe({
  tryCatch({
    if(!is.null(input$hypTest)){
      if (inf.par$TTEST2) {
        if(input$hypTest == 2){
          shinyjs::enable("hypAlt")
          shinyjs::enable("hypVal")
        } else {
          shinyjs::disable("hypAlt")
          shinyjs::disable("hypVal")
        }
      } else if (inf.par$PROPTEST2 && !inf.par$is_survey) {
        if(input$hypTest == 3){
          shinyjs::enable("hypAlt")
          shinyjs::enable("hypVal")
        } else {
          shinyjs::disable("hypAlt")
          shinyjs::disable("hypVal")
        }
      } else if(input$hypTest1){
        shinyjs::enable("hypAlt")
        shinyjs::enable("hypVal")
      } else {
        shinyjs::disable("hypAlt")
        shinyjs::disable("hypVal")
      }
    }
    
    if(!is.null(input$hypTest)){
      if (inf.par$TTEST2 && !inf.par$is_survey){
        if(input$hypTest == 2){
          shinyjs::enable("hypEqualVar")
        } else{
          shinyjs::disable("hypEqualVar")
        }
      }
    }
    
    if(!is.null(input$hypTest)){
      if (inf.par$CHI2 && !inf.par$is_survey) {
        if(inf.par$PROPTEST2){
          if(input$hypTest == 2){
            shinyjs::enable("hypSimPval")
          } else{
            shinyjs::disable("hypSimPval")
          }
        } else if (input$hypTest1){
          shinyjs::enable("hypSimPval")
        } else {
          shinyjs::disable("hypSimPval")
        }
      }
    }
    
    if(!is.null(input$hypTest)){
      if (inf.par$PROPTEST2 && !inf.par$is_survey){
        if(input$hypTest == 3){
          shinyjs::enable("hypExactPval")
        } else{
          shinyjs::disable("hypExactPval")
        }
      }
    }
    
    if(!is.null(input$hypTest)){
      if (inf.par$CHI2){
        if(inf.par$PROPTEST2 && !inf.par$is_survey){
          if(input$hypTest == 2){
            shinyjs::enable("hypSimPval")
          } else{
            shinyjs::disable("hypSimPval")
          }
        } else if (input$hypTest1) {
          shinyjs::enable("hypSimPval")
        } else {
          shinyjs::disable("hypSimPval")
        }
      }
    }
    
    if(!is.null(input$hypTest)){
      if (inf.par$PROPTEST2 && !inf.par$is_survey){
        if(input$hypTest == 3){
          shinyjs::enable("hypExactPval")
        } else{
          shinyjs::disable("hypExactPval")
        }
      } else {
        shinyjs::disable("hypExactPval")
      }
    }
    
    if(!is.null(input$hypTest)){
      if (inf.par$TTEST2){
        if(input$hypTest == 2){
          shinyjs::enable("hypVal")
          shinyjs::enable("hypAlt")
          shinyjs::enable("hypEqualVar")
        } else{
          shinyjs::disable("hypVal")
          shinyjs::disable("hypAlt")
          shinyjs::disable("hypEqualVar")
        }
      } else if (inf.par$PROPTEST2 && !inf.par$is_survey){
        if(input$hypTest == 3){
          shinyjs::enable("hypVal")
          shinyjs::enable("hypAlt")
          shinyjs::enable("hypEqualVar")
        } else{
          shinyjs::disable("hypVal")
          shinyjs::disable("hypAlt")
          shinyjs::disable("hypEqualVar")
        }
      }
    }
  }, error = function(e) {print(e)})
})


output$visualize.summary = renderPrint({
  if (is.null(plot.par$x)) {
    return(cat("Please select a variable"))
  }
  values.list = modifyList(reactiveValuesToList(plot.par),
                           reactiveValuesToList(graphical.par), keep.null = TRUE)
  if(is.numeric(plot.par$x)&
     is.numeric(plot.par$y)){
    values.list.x = values.list$x
    values.list$x=values.list$y
    values.list$y=values.list.x
    values.list.varnames.x = values.list$varnames$x
    values.list$varnames$x = values.list$varnames$y
    values.list$varnames$y = values.list.varnames.x
  }
  if(!is.null(values.list$design)){
    values.list$data = NULL
  }
  
  
  tmp.list <- values.list
  tmp.list$plottype = "hist"
  
  
  if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
     tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
    tryCatch({
      cat(do.call(iNZightPlots:::getPlotSummary, tmp.list), sep = "\n")
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      print(e)
    }, finally = {})
  }else{
    suppressWarnings(try(cat(do.call(iNZightPlots:::getPlotSummary, tmp.list), sep = "\n")))
  }
})


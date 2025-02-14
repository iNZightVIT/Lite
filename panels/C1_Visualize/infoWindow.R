###  Date Created  : May 25, 2020

inf.par <- reactiveValues()

inf.def.par <- reactiveValues(
  hypothesis.value = 0,
  hypothesis.alt = c("two.sided", "less", "greater"),
  hypothesis.var.equal = FALSE,
  hypothesis.use.exact = FALSE,
  hypothesis.test = c("default", "t.test", "anova", "chi2", "proportion"),
  hypothesis.simulated.p.value = FALSE
  # hypothesis <- if(!is.null(input$hypTest) && input$hypTest == "None") "NULL" else NULL
)

ci_width <- reactiveVal(95)
output$inference_test <- renderUI({
  get.data.set()
  ret <- NULL
  input$vari1
  input$vari2
  input$type.inference.select
  design_params$design
  isolate({
    if (!is.null(plot.par$x)) {
      xvar <- vis.data()[[plot.par$x]]
      yvar <- if (!is.null(plot.par$y)) vis.data()[[plot.par$y]] else NULL
      ## Figure out what type of inference will be happening:
      xnum <- iNZightTools::is_num(vis.data()[[plot.par$x]])
      if (is.null(yvar)) {
        INFTYPE <- ifelse(xnum, "onesample-ttest", "oneway-table")
      } else {
        ynum <- iNZightTools::is_num(vis.data()[[plot.par$y]])
        if (xnum && ynum) {
          INFTYPE <- "regression"
        } else if (xnum | ynum) {
          M <-
            if (xnum) {
              length(levels(yvar))
            } else {
              length(levels(xvar))
            }
          if (M == 2) INFTYPE <- "twosample-ttest"
          if (M > 2) INFTYPE <- "anova"
        } else {
          INFTYPE <- "twoway-table"
        }
      }

      ## Design or data?
      inf.par$is_survey <- is_survey <- FALSE
      if (!is.null(design_params$design$dataDesign)) {
        inf.par$is_survey <- is_survey <- TRUE
      }

      if (!is_survey) {
        output$inference_type <- renderUI({
          radioButtons("type.inference.select",
            selected = input$type.inference.select,
            choices = c(
              "normal" = 1,
              "bootstrap" = 2
            ),
            label = h5(strong("Select type of inference"))
          )
        })
      } else {
        output$inference_type <- renderUI({
          radioButtons("type.inference.select",
            choices = c("normal" = 1),
            label = h5(strong("Select type of inference"))
          )
        })
      }

      # UI for "Additional Options: Confidence level (%):"
      output$inference_opts <- renderUI({
        fixedRow(
          column(
            4,
            numericInput(
              inputId = "ci.width",
              label = "Confidence level (%):",
              value = ci_width(),
              min = 10,
              max = 99
            )
          ),
          column(
            3,
            numericInput("global.sig.level.inf",
              label = "Round (signifcant figures)",
              value = graphical.par$signif,
              min = 1, step = 1
            )
          ),
          column(3, numericInput("global.p.val",
            label = "Min P-value",
            value = graphical.par$min_pval,
            min = 0, max = 0.05, step = 0.0001
          )),
          column(12, h5("Warning: clicking the inputs rapidly may cause the app to crash."))
        )
      })

      do_hyp_test <- grepl("ttest|anova|table", INFTYPE)

      if (is_survey && do_hyp_test && INFTYPE == "oneway-table") {
        # survey lets us do prop.test, but not chi-square (one-way)
        do_hyp_test <- length(levels(xvar)) == 2
      }

      if (do_hyp_test && input$type.inference.select == 1 && !is.null(input$type.inference.select)) {
        hyp_tests <- switch(INFTYPE,
          "onesample-ttest" = "t.test",
          "twosample-ttest" = c("t.test2", "anova"),
          "anova" = "anova",
          "oneway-table" =
            if (is_survey) {
              "proportion"
            } else if (length(levels(xvar)) == 2L) {
              c("proportion", "chi2")
            } else {
              "chi2"
            },
          "twoway-table" = "chi2"
        )

        test_names <- c(
          t.test = "One sample t-test",
          t.test2 = "Two sample t-test",
          anova = "ANOVA",
          proportion = "Test proportion",
          chi2 = "Chi-square test"
        )

        test_options <- c("None", unname(test_names[hyp_tests]))
        if (!is.null(test_options)) {
          ret <- list(radioButtons("hypTest",
            label = h5(strong("Hypothesis Testing")),
            choices = test_options,
            selected = NULL
          ))
        }
      }
      if (INFTYPE == "regression") {
        ret <- list(
          column(12, checkboxInput("inf.trend.linear",
            label = "linear",
            value = ifelse((!is.null(input$check_linear) && length(input$check_linear) > 0), input$check_linear,
              ifelse((!is.null(input$inf.trend.linear) && length(input$inf.trend.linear) > 0), input$inf.trend.linear, FALSE)
            )
          )),
          column(12, checkboxInput("inf.trend.quadratic",
            label = "quadratic",
            value = ifelse((!is.null(input$check_quadratic) && length(input$check_quadratic) > 0), input$check_quadratic,
              ifelse((!is.null(input$inf.trend.quadratic) && length(input$inf.trend.quadratic) > 0), input$inf.trend.quadratic, FALSE)
            )
          )),
          column(12, checkboxInput("inf.trend.cubic",
            label = "cubic",
            ifelse((!is.null(input$inf.trend.cubic) && length(input$inf.trend.cubic) > 0), input$inf.trend.cubic, FALSE)
          ))
        )
      }
    }
  })
  ret
})

output$inference_epi <- renderUI({
  get.data.set()
  ret <- NULL
  input$vari1
  input$vari2

  if (!is.null(plot.par$x) && iNZightTools::is_cat(vis.data()[[plot.par$x]]) &&
    !is.null(plot.par$y) && iNZightTools::is_cat(vis.data()[[plot.par$y]]) &&
    length(levels(vis.data()[[plot.par$y]])) >= 2 && length(levels(vis.data()[[plot.par$x]])) == 2
  ) {
    ret <- list(
      h5(strong("Epidemiology options")),
      checkboxInput("inf_epi_out",
        label = "Show Output",
        value = FALSE
      )
    )

    ret
  } else {
    NULL
  }
})

observe({
  updateCheckboxInput(session, inputId = "check_linear", label = "linear", value = input$inf.trend.linear)
})
observe({
  updateCheckboxInput(session, inputId = "inf.trend.linear", label = "linear", value = input$check_linear)
})

observe({
  updateCheckboxInput(session, inputId = "check_quadratic", label = "quadratic", value = input$inf.trend.quadratic)
})
observe({
  updateCheckboxInput(session, inputId = "inf.trend.quadratic", label = "quadratic", value = input$check_quadratic)
})

observe({
  updateCheckboxInput(session, inputId = "check_cubic", label = "cubic", value = input$inf.trend.cubic)
})
observe({
  updateCheckboxInput(session, inputId = "inf.trend.cubic", label = "cubic", value = input$check_cubic)
})

observeEvent(input$inf.trend.linear, {
  #    graphical.par$bs.inference = F
  #    graphical.par$inference.type = NULL
  # cat("\n---update inf.trend.linear ---\n")
  # cat("input$inf.trend.linear: ", input$inf.trend.linear, "\n")
  # cat("graphical.par$trend: ", graphical.par$trend, "\n")
  if (is.null(input$check_linear) && !is.null(input$inf.trend.linear)) {
    if (input$inf.trend.linear) {
      if (length(which(graphical.par$trend %in% "linear")) == 0) {
        graphical.par$trend <- c(graphical.par$trend, "linear")
      }
      graphical.par$col.trend[["linear"]] <- "blue"
      graphical.par$lty.trend[["linear"]] <- 1
    } else {
      if (length(which(graphical.par$trend %in% "linear")) > 0) {
        graphical.par$trend <- graphical.par$trend[-which(graphical.par$trend %in% "linear")]
        if (length(graphical.par$trend) == 0) {
          graphical.par$trend <- NULL
        }
      }
    }
  }
})



observe({
  input$inf.trend.quadratic
  isolate({
    #    graphical.par$bs.inference = F
    if (is.null(input$check_quadratic) && !is.null(input$inf.trend.quadratic)) {
      if (input$inf.trend.quadratic) {
        if (length(which(graphical.par$trend %in% "quadratic")) == 0) {
          graphical.par$trend <- c(graphical.par$trend, "quadratic")
        }
        graphical.par$col.trend[["quadratic"]] <- "red"
        graphical.par$lty.trend[["quadratic"]] <- 1
      } else {
        if (length(which(graphical.par$trend %in% "quadratic")) > 0) {
          graphical.par$trend <- graphical.par$trend[-which(graphical.par$trend %in% "quadratic")]
          if (length(graphical.par$trend) == 0) {
            graphical.par$trend <- NULL
          }
        }
      }
    }
  })
})


# observe cubic trend
observe({
  input$inf.trend.cubic
  isolate({
    #    graphical.par$bs.inference = F
    if (is.null(input$check_cubic) && !is.null(input$inf.trend.cubic)) {
      if (input$inf.trend.cubic) {
        if (length(which(graphical.par$trend %in% "cubic")) == 0) {
          graphical.par$trend <- c(graphical.par$trend, "cubic")
        }
        graphical.par$col.trend[["cubic"]] <- "green4"
        graphical.par$lty.trend[["cubic"]] <- 1
      } else {
        if (length(which(graphical.par$trend %in% "cubic")) > 0) {
          graphical.par$trend <- graphical.par$trend[-which(graphical.par$trend %in% "cubic")]
          if (length(graphical.par$trend) == 0) {
            graphical.par$trend <- NULL
          }
        }
      }
    }
  })
})



output$inference_out <- renderUI({
  get.data.set()
  ret <- NULL
  input$vari1
  input$vari2
  input$type.inference.select
  design_params$design
  input$hypTest
  isolate({
    # null value/alternative [t.test, t.test2, proportion]
    if (!is.null(input$hypTest) && input$hypTest != "None" && input$type.inference.select == 1) {
      if (input$hypTest %in% c("One sample t-test", "Two sample t-test", "Test proportion")) {
        ret <- list(
          ret,
          column(3, h5("Null Value:")),
          column(9, textInput(inputId = "hypVal", value = ifelse(input$hypTest == "proportion", 0.5, 0), label = NULL))
        )
        if (!inf.par$is_survey) {
          ret <- list(
            ret,
            column(3, h5("Alternative Hypothesis:")),
            column(9, selectInput(
              inputId = "hypAlt",
              label = NULL,
              choices = c("two sided", "greater than", "less than"),
              # selected = input$hypothesis_twosample,
              selectize = F
            ))
          )
        }


        if (input$hypTest == "Two sample t-test") {
          ret <- list(
            ret,
            column(9,
              offset = 3,
              checkboxInput("hypEqualVar", label = "Use equal-variance", value = FALSE, width = NULL)
            )
          )
        }

        # exact p-value [proportion]
        if (input$hypTest == "Test proportion") {
          ret <- list(
            ret,
            column(9,
              offset = 3,
              checkboxInput("hypExactPval", label = "Calculate exact p-value", value = FALSE, width = NULL)
            )
          )
        }
      }

      if (input$hypTest == "Chi-square test") {
        ret <- list(
          ret,
          checkboxInput("hypSimPval", label = "Simulate p-value", value = FALSE, width = NULL)
        )
      }
    }
  })
  ret
})



output$visualize.inference <- renderPrint({
  if (input$plot_selector %in% "Inference") {
    input$hypTest
    input$hypVal
    input$hypAlt
    input$hypEqualVar
    input$hypSimPval
    input$hypExactPval
    input$inf.trend.chk
    input$vari1
    input$vari2
    input$subs1
    input$inf.trend.linear
    input$inf.trend.quadratic
    input$inf.trend.cubic
    # input$confirm_inf_button
    input$type.inference.select
    input$ci.width
    design_params$design
    input$inf_epi_out
    graphical.par$signif
    graphical.par$round_percent
    graphical.par$min_pval
    isolate({
      ## Design or data?
      is_survey <- !is.null(design_params$design$dataDesign)
      curSet <- modifyList(reactiveValuesToList(plot.par),
        reactiveValuesToList(graphical.par),
        keep.null = TRUE
      )
      curSet <- modifyList(curSet,
        reactiveValuesToList(inf.def.par),
        keep.null = TRUE
      )
      curSet$plottype <- NULL
      if (!is.null(curSet$freq)) {
        curSet$freq <- get.data.set()[[curSet$freq]]
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
      bs.inf <- F
      if (!is.null(input$type.inference.select) && input$type.inference.select == 1) {
        bs.inf <- F
      } else if (!is.null(input$type.inference.select) && input$type.inference.select == 2) {
        bs.inf <- T
      }

      curSet <- modifyList(
        curSet,
        list(
          bs.inference = bs.inf,
          inference.type = "conf",
          inference.par = NULL
        ),
        keep.null = TRUE
      )

      tryCatch(
        {
          ## one sample t-test
          if (iNZightTools::is_num(vis.data()[[curSet$x]]) && is.null(plot.par$y)) {
            if (input$hypTest == "One sample t-test") {
              curSet <- modifyList(
                curSet,
                list(
                  hypothesis.value = as.numeric(input$hypVal),
                  hypothesis.alt = switch(input$hypAlt,
                    "two sided" = "two.sided",
                    "greater than" = "greater",
                    "less than" = "less"
                  ),
                  hypothesis.test = "t.test"
                ),
                keep.null = TRUE
              )
            } else {
              curSet <- modifyList(
                curSet,
                list(hypothesis = "NULL"),
                keep.null = TRUE
              )
            }
          } else if (length(levels(vis.data()[[plot.par$x]])) == 2 && is.null(plot.par$y)) {
            ## test for binary x
            ## for survey obj
            if (is_survey) {
              if (input$hypTest == "Test proportion") {
                curSet <- modifyList(
                  curSet,
                  list(
                    hypothesis.value = as.numeric(input$hypVal),
                    hypothesis.test = "proportion"
                  ),
                  keep.null = TRUE
                )
              } else {
                curSet <- modifyList(
                  curSet,
                  list(hypothesis = "NULL"),
                  keep.null = TRUE
                )
              }
            } else {
              ## non-survey obj
              if (input$hypTest == "Chi-square test") {
                curSet <- modifyList(
                  curSet,
                  list(
                    hypothesis.simulated.p.value = input$hypSimPval,
                    hypothesis.test = "chi2"
                  ),
                  keep.null = TRUE
                )
              } else if (input$hypTest == "Test proportion") {
                curSet <- modifyList(
                  curSet,
                  list(
                    hypothesis.value = as.numeric(input$hypVal),
                    hypothesis.use.exact = input$hypExactPval,
                    hypothesis.alt = switch(input$hypAlt,
                      "two sided" = "two.sided",
                      "greater than" = "greater",
                      "less than" = "less"
                    ),
                    hypothesis.test = "proportion"
                  ),
                  keep.null = TRUE
                )
              } else {
                curSet <- modifyList(
                  curSet,
                  list(hypothesis = "NULL"),
                  keep.null = TRUE
                )
              }
            }
          } else if ((length(levels(vis.data()[[plot.par$x]])) > 2 && is.null(plot.par$y)) ||
            (is.factor(vis.data()[[plot.par$x]]) && is.factor(vis.data()[[plot.par$y]]))) {
            ## chi-square test
            if (input$hypTest == "Chi-square test") {
              curSet <- modifyList(curSet,
                list(
                  hypothesis.simulated.p.value = input$hypSimPval,
                  hypothesis.test = "chi2"
                ),
                keep.null = TRUE
              )
            } else {
              curSet <- modifyList(
                curSet,
                list(hypothesis = "NULL"),
                keep.null = TRUE
              )
            }
          } else if ((length(levels(vis.data()[[plot.par$x]])) == 2 && is.numeric(vis.data()[[plot.par$y]])) ||
            (is.numeric(vis.data()[[plot.par$x]]) && length(levels(vis.data()[[plot.par$y]])) == 2)) {
            ## two sample t-test
            if (input$hypTest == "Two sample t-test") {
              curSet <- modifyList(
                curSet,
                list(
                  hypothesis.value = as.numeric(input$hypVal),
                  hypothesis.var.equal = input$hypEqualVar,
                  hypothesis.alt = switch(input$hypAlt,
                    "two sided" = "two.sided",
                    "greater than" = "greater",
                    "less than" = "less"
                  ),
                  hypothesis.test = "t.test"
                ),
                keep.null = TRUE
              )
            } else if (input$hypTest == "ANOVA") {
              curSet <- modifyList(
                curSet,
                list(hypothesis.test = "anova"),
                keep.null = TRUE
              )
            } else {
              curSet <- modifyList(
                curSet,
                list(hypothesis = "NULL"),
                keep.null = TRUE
              )
            }
          } else if ((length(levels(vis.data()[[plot.par$x]])) > 2 && is.numeric(vis.data()[[plot.par$y]])) ||
            (is.numeric(vis.data()[[plot.par$x]]) && length(levels(vis.data()[[plot.par$y]])) > 2)) {
            if (input$hypTest == "ANOVA") {
              curSet <- modifyList(
                curSet,
                list(hypothesis.test = "anova"),
                keep.null = TRUE
              )
            } else {
              curSet <- modifyList(
                curSet,
                list(hypothesis = "NULL"),
                keep.null = TRUE
              )
            }
          }
        },
        error = function(e) {}
      )

      if (!is.null(plot.par$x) && iNZightTools::is_num(vis.data()[[plot.par$x]]) &&
        !is.null(plot.par$y) && iNZightTools::is_num(vis.data()[[plot.par$y]])) {
        chosen <- c(input$inf.trend.linear, input$inf.trend.quadratic, input$inf.trend.cubic)
        # cat("chosen: ", chosen, "\n")
        curSet$trend <- if (any(chosen)) c("linear", "quadratic", "cubic")[chosen] else NULL
      }

      vartypes <- list(
        x = iNZightTools::vartype(vis.data()[[curSet$x]]),
        y = NULL
      )
      if (!is.null(curSet$y)) {
        vartypes$y <- iNZightTools::vartype(vis.data()[[curSet$y]])
      }


      if (!is.null(design_params$design$dataDesign)) {
        curSet$data <- NULL
        curSet$design <- as.name(".design")
        .design <- createSurveyObject()
        # designname <<- curMod$dataDesignName
        # curSet$design <<- as.name(designname)
        # assign(designname, curMod$createSurveyObject(), envir = env)
      }

      if (!is.null(plot.par$x) && iNZightTools::is_cat(vis.data()[[plot.par$x]]) &&
        !is.null(plot.par$y) && iNZightTools::is_cat(vis.data()[[plot.par$y]]) &&
        length(levels(vis.data()[[plot.par$y]])) >= 2 && length(levels(vis.data()[[plot.par$x]])) == 2 &&
        input$inf_epi_out == TRUE) {
        if (input$inf_epi_out == TRUE) {
          curSet <- modifyList(
            curSet,
            list(epi.out = TRUE),
            keep.null = TRUE
          )
        }
      } else {
        curSet <- modifyList(
          curSet,
          list(epi.out = NULL),
          keep.null = TRUE
        )
      }

      # Adjust CI width
      if (!is.null(input$ci.width)) {
        ci_width(input$ci.width)
        curSet <- modifyList(
          curSet,
          list(ci.width = ci_width() / 100),
          keep.null = TRUE
        )
      }

      .dataset <- get.data.set()
      tryCatch({
        inf_call <- construct_call(curSet, design_params$design,
          vartypes,
          data = quote(.dataset),
          what = "inference"
        )
        suppressWarnings(inf.print <- eval(inf_call))

        if (input$hypTest == "Chi-square test" && !is.null(input$hypTest)) {
          exp_match <- any(grepl("since some expected counts <", inf.print, fixed = TRUE))
          if (exp_match) {
            updateCheckboxInput(session, "hypSimPval", label = "Simulate p-value", value = TRUE)
            shinyjs::disable("hypSimPval")
          }
          if (!exp_match) {
            shinyjs::enable("hypSimPval")
          }
        }

        inf.print
        # saveRDS(values.list, file = "/Users/tongchen/Documents/work/Lite/b.rds")
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




output$visualize.summary <- renderPrint({
  if (is.null(plot.par$x)) {
    return(cat("Please select a variable"))
  } else {
    values.list <- modifyList(reactiveValuesToList(plot.par),
      reactiveValuesToList(graphical.par),
      keep.null = TRUE
    )

    if (!is.null(values.list$design)) {
      values.list$data <- NULL
    }


    curSet <- values.list
    curSet$plottype <- "hist"

    vartypes <- list(
      x = NULL,
      y = NULL
    )
    if (!is.null(curSet$x)) {
      vartypes$x <- iNZightTools::vartype(vis.data()[[curSet$x]])
      if (!is.null(curSet$y)) {
        vartypes$y <- iNZightTools::vartype(vis.data()[[curSet$y]])
      }
    }

    if (!is.null(design_params$design$dataDesign)) {
      curSet$data <- NULL
      curSet$design <- as.name(".design")
      .design <- createSurveyObject()
      # designname <<- curMod$dataDesignName
      # curSet$design <<- as.name(designname)
      # assign(designname, curMod$createSurveyObject(), envir = env)
    }
    .dataset <- get.data.set()

    if (!is.null(parseQueryString(session$clientData$url_search)$debug) &&
      tolower(parseQueryString(session$clientData$url_search)$debug) %in% "true") {
      tryCatch({
        eval(construct_call(curSet, design_params$design,
          vartypes,
          data = quote(.dataset),
          what = "summary"
        ))
      }, error = function(e) {
        print(e)
      }, finally = {})
    } else {
      suppressWarnings(try(eval(construct_call(curSet, design_params$design,
        vartypes,
        data = quote(.dataset),
        what = "summary"
      ))))
    }
  }
})

<<<<<<< HEAD
observeEvent(input$global.sig.level.inf, {
  updateNumericInput(session,
    inputId = "global.sig.level",
    value = input$global.sig.level.inf
  )
  graphical.par$signif <- input$global.sig.level.inf
})
# other way around
observeEvent(input$global.sig.level, {
  updateNumericInput(session,
    inputId = "global.sig.level.inf",
    value = input$global.sig.level
  )
})

# same for rounding
observeEvent(input$global.round.pct.inf, {
  updateNumericInput(session,
    inputId = "global.round.pct",
    value = input$global.round.pct.inf
  )
  graphical.par$round_percent <- input$global.round.pct.inf
})
observeEvent(input$global.round.pct, {
  updateNumericInput(session,
    inputId = "global.round.pct.inf",
    value = input$global.round.pct
  )
})
=======
gloablSigLevel <- reactive(input$global.sig.level.inf)
observeEvent(input$global.sig.level.inf, {
  graphical.par$signif <- input$global.sig.level.inf
})

>>>>>>> master

observeEvent(input$global.p.val, {
  graphical.par$min_pval <- input$global.p.val
})

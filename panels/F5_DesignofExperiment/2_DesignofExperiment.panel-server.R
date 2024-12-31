### -------------------------------------------------###
###  Server Functions for the "Mixed Model" Module  ###
### -------------------------------------------------###
###
###  Date Created   :   March 25, 2018
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *

HELP_TEXT_CSS = "color: blue; background: none; border: none; text-decoration: underline; padding: 0; padding-bottom: 5px;"
lsd_formula = function(is_eff) {
  eff_constant = ifelse(
    is_eff,
    "<li><strong>e</strong>: <strong>Eff</strong>iciency of the design</li>",
    ""
  )
  formula = ifelse(
    is_eff,
    '\\[LSD = t \\times \\sqrt{\\frac{2 \\times ResMS}{reps \\times e}}\\]',
    '\\[LSD = t \\times \\sqrt{\\frac{2 \\times ResMS}{reps}}\\]'
  )
  return(paste0(
    '<div class="math-content">',
    '<div class="math-formula">',
    '\\[LSD = t \\times \\sqrt{\\frac{2 \\times ResMS}{reps}}\\]',
    '</div>',
    'Where:',
    '<ul>',
    '<li><strong>t</strong>: t-distribution with degrees of freedom <strong>df</strong> and significance level <strong>alpha</strong> (α)</li>',
    '<li><strong>ResMS</strong>: Residual Mean Square</li>',
    '<li><strong>reps</strong>: Number of replications</li>',
    eff_constant,
    '</ul>',
    '</div>'
  ))
}
tsr_formula = function(is_eff) {
  eff_constant = ifelse(
    is_eff,
    "<li><strong>e</strong>: <strong>Eff</strong>iciency of the design</li>",
    ""
  )
  formula = ifelse(
    is_eff,
    '\\[TSR = q \\times \\sqrt{\\frac{ResMS}{reps \\times e}}\\]',
    '\\[TSR = q \\times \\sqrt{\\frac{ResMS}{reps}}\\]'
  )
  return(paste0(
    '<div class="math-content">',
    '<div class="math-formula">',
    formula,
    '</div>',
    'Where:',
    '<ul>',
    '<li><strong>q</strong>: Critical value from Tukey\'s studentized range distribution, with ',
    'significance level <strong>alpha</strong> (α), ',
    'number of <strong>means</strong> and ',
    'residual degrees of freedom <strong>df</strong>.',
    '</li>',
    '<li><strong>ResMS</strong>: Residual Mean Square</li>',
    '<li><strong>reps</strong>: Number of replications</li>',
    eff_constant,
    '</ul>',
    '</div>'
  ))
}
show_ct_help = function(is_eff, is_lsd) {
  alert_title = ifelse(
    is_lsd,
    "Least Significant Difference (LSD)",
    "Tukey’s Studentised Range (TSR)"
  )
  if(is_lsd) {
    formula = lsd_formula(is_eff = is_eff)
  } else {
    formula = tsr_formula(is_eff = is_eff)
  }
  shinyalert(
    title = alert_title,
    text = formula,
    html = TRUE,
    size = "m",
    callbackR = function() {
      runjs("initMathJax();")
    }
  )
}

## initialize gui
output$mixedmodel.panel <- renderUI({
  mixedModel.panel.ui(get.data.set())
})

## get data set
mix.data <- reactive({
  get.data.set()
})


## reactive value for fit mixed model

model_Vals <- reactiveValues(
  num = 0,
  model = list(), ## list to save model
  aov = list() ## save aov table
)

mix.model.par <- reactiveValues(
  y = NULL,
  x = NULL,
  data = NULL,
  blocking = NULL,
  name = NULL,
  data.name = NULL
)

mix.model.list.par <- reactive({
  mix.model.list.par <- reactiveValuesToList(mix.model.par)
})



### -------------------------------------------------###
###              Model Fitting part                 ###
### -------------------------------------------------###

## UI of selecting variables
output$ep_anova <- renderUI({
  get.data.set()
  ret <- NULL
  input$model_design
  isolate({
    var_name_numeric <- c(" ", get.numeric.column.names(get.data.set()))
    var_name_factor <- c(" ", get.categorical.column.names(get.data.set()))


    base.design <- fluidRow( ## select response variable(numeric)
      column(12, selectInput(
        inputId = "mm_vari1",
        label = "Response variable",
        choices = var_name_numeric,
        selected = var_name_numeric[1],
        selectize = F
      )),

      ## select treatment varibale(factor)
      column(12, selectInput(
        inputId = "mm_vari2",
        label = "Treatment factor",
        choices = var_name_factor,
        selected = var_name_factor[1],
        selectize = F
      ))
    )

    ## select second treatment factor
    sec.fac <- fluidRow(column(12, selectInput(
      inputId = "mm_vari3",
      label = "Second treatment factor",
      choices = var_name_factor,
      selected = var_name_factor[1],
      selectize = F
    )))

    ## select third treatment factor
    third.fac <- fluidRow(column(12, selectInput(
      inputId = "mm_vari4",
      label = "Third treatment factor",
      choices = var_name_factor,
      selected = var_name_factor[1],
      selectize = F
    )))

    ## select block factor
    block.fac <- fluidRow(column(12, selectInput(
      inputId = "mm_vari5",
      label = "Block factor",
      choices = var_name_factor,
      selected = var_name_factor[1],
      selectize = F
    )))

    ## treatment means
    trt.mean.block <- fluidRow(column(12, radioButtons(
      inputId = "mm_trt.mean",
      label = "Treatment means",
      choices = c("No blocking or complete blocks", "balanced incomplete blocks"),
      selected = "No blocking or complete blocks"
    )))

    trt.mean.noblock <- fluidRow(column(12, radioButtons(
      inputId = "mm_trt.mean",
      label = "Treatment means",
      choices = c("No blocking or complete blocks"),
      selected = "No blocking or complete blocks"
    )))

    if (req(input$model_design) == 1) {
      ret <- list(
        base.design,
        trt.mean.noblock
      )
    } else if (req(input$model_design) == 2) {
      ret <- list(
        base.design,
        block.fac,
        trt.mean.block
      )
    } else if (req(input$model_design) == 3) {
      ret <- list(
        base.design,
        sec.fac,
        trt.mean.noblock
      )
    } else if (req(input$model_design) == 4) {
      ret <- list(
        base.design,
        sec.fac,
        block.fac,
        trt.mean.block
      )
    } else if (req(input$model_design) == 5) {
      ret <- list(
        base.design,
        sec.fac,
        third.fac,
        trt.mean.noblock
      )
    } else if (req(input$model_design) == 6) {
      ret <- list(
        base.design,
        sec.fac,
        third.fac,
        block.fac,
        trt.mean.block
      )
    }
  })
  ret
})


### ----------------------------###
###     update selectInput     ###
### ----------------------------###



observe({
  if (req(input$mm_vari2) != " ") {
    isolate({
      ch <- get.categorical.column.names(get.data.set())
      ch <- c(" ", ch[-which(ch %in% input$mm_vari2)])
      sel <- input$mm_vari3
      if (!is.null(sel) && !sel %in% ch) {
        sel <- ch[1]
      }
      updateSelectInput(session, "mm_vari3", choices = ch, selected = sel)
    })
  }
})


observe({
  if (req(input$mm_vari2) != " " | req(input$mm_vari3) != " ") {
    isolate({
      ch <- get.categorical.column.names(get.data.set())
      if (req(input$mm_vari2) != " ") {
        ch <- ch[-which(ch %in% input$mm_vari2)]
      }
      if (req(input$mm_vari3) != " ") {
        ch <- ch[-which(ch %in% input$mm_vari3)]
      }
      ch <- c(" ", ch)
      sel <- input$mm_vari4
      if (!is.null(sel) && !sel %in% ch) {
        sel <- ch[1]
      }
      updateSelectInput(session, "mm_vari4", choices = ch, selected = sel)
    })
  }
})

## update the Block factor
observe({
  if (req(input$mm_vari2) != " " | req(input$mm_vari3) != " " |
    req(input$mm_vari4) != " ") {
    isolate({
      ch <- get.categorical.column.names(get.data.set())
      if (req(input$mm_vari2) != " ") {
        ch <- ch[-which(ch %in% input$mm_vari2)]
      }
      if (req(input$mm_vari3) != " ") {
        ch <- ch[-which(ch %in% input$mm_vari3)]
      }
      if (req(input$mm_vari4) != " ") {
        ch <- ch[-which(ch %in% input$mm_vari4)]
      }
      ch <- c(" ", ch)
      sel <- input$mm_vari5
      if (!is.null(sel) && !sel %in% ch) {
        sel <- ch[1]
      }
      updateSelectInput(session, "mm_vari5", choices = ch, selected = sel)
    })
  }
})

### ------------------------------------###
###          fit own model             ###
### ------------------------------------###

## select response variable
output$own_model_var1_panel <- renderUI({
  get.data.set()
  isolate({
    var_name_numeric <- c(" ", get.numeric.column.names(get.data.set()))
    sel <- input$mm_own_model_vari1
    div(
      hr(),
      selectInput(
        inputId = "mm_own_model_vari1",
        label = "Response variable",
        choices = var_name_numeric,
        selected = sel,
        selectize = F
      )
    )
  })
})

## select fixed effect
output$own_model_fixed_panel <- renderUI({
  get.data.set()
  isolate({
    sel <- input$mm_own_model_fixed
    selectInput(
      inputId = "mm_own_model_fixed",
      label = "Fixed effect",
      choices = c(" ", colnames(mix.data())),
      selected = NULL,
      size = length(colnames(mix.data())) + 1,
      selectize = F
    )
  })
})


##  Update fixed effect.
observe({
  if (req(input$mm_own_model_vari1) != " ") {
    isolate({
      ch <- colnames(mix.data())
      ch <- c(" ", ch[-which(ch %in% input$mm_own_model_vari1)])
      sel <- input$mm_own_model_fixed
      if (!is.null(sel) && !sel %in% ch) {
        sel <- ch[1]
      }
      updateSelectInput(session, "mm_own_model_fixed",
        choices = ch, selected = sel
      )
    })
  }
})


observe({
  input$mm_own_model_fixed
  isolate({
    req(input$mm_own_model_fixed != " ")
    updateTextInput(session,
      inputId = "fixed_effect", label = "Fixed effect:",
      value = paste0(input$fixed_effect, input$mm_own_model_fixed)
    )
  })
})


## select random effect(factor)
output$own_model_random_panel <- renderUI({
  get.data.set()
  isolate({
    var_name_factor <- c(" ", get.categorical.column.names(get.data.set()))
    sel <- input$mm_own_model_random
    selectInput(
      inputId = "mm_own_model_random",
      label = "Random effect",
      choices = var_name_factor,
      selected = NULL,
      size = length(var_name_factor),
      selectize = F
    )
  })
})

observe({
  input$mm_own_model_random
  isolate({
    req(input$mm_own_model_random != " ")
    updateTextInput(session,
      inputId = "random_effect", label = "Random effect:",
      value = paste0(input$random_effect, input$mm_own_model_random)
    )
  })
})


### --------------------------------------------###
###             fit the model                  ###
### --------------------------------------------###

## we fit model separately

observeEvent(input$fit_model_aov, {
  isolate({
    mix.model.name <- ""
    temp.model <- NULL
    ## fit model
    ## set parameters
    temp <- mix.model.list.par()
    temp$data <- mix.data()
    ## generate model name
    model_Vals$num <- model_Vals$num + 1
    if (req(input$fit_design) == 1 &&
      req(input$model_design) == 1 && req(input$mm_vari1) != " " &&
      req(input$mm_vari2) != " ") {
      temp$y <- input$mm_vari1
      temp$x <- input$mm_vari2
    } else if (req(input$fit_design) == 1 &&
      req(input$model_design) == 2 && req(input$mm_vari1) != " " &&
      req(input$mm_vari2) != " " &&
      req(input$mm_vari5) != " ") {
      temp$y <- input$mm_vari1
      temp$x <- input$mm_vari2
      temp$blocking <- input$mm_vari5
    } else if (req(input$fit_design) == 1 &&
      req(input$model_design) == 3 && req(input$mm_vari1) != " " &&
      req(input$mm_vari2) != " " &&
      req(input$mm_vari3) != " ") {
      temp$y <- input$mm_vari1
      temp$x <- c(input$mm_vari2, input$mm_vari3)
    } else if (req(input$fit_design) == 1 &&
      req(input$model_design) == 4 && req(input$mm_vari1) != " " &&
      req(input$mm_vari2) != " " &&
      req(input$mm_vari3) != " " && req(input$mm_vari5) != " ") {
      temp$y <- input$mm_vari1
      temp$x <- c(input$mm_vari2, input$mm_vari3)
      temp$blocking <- input$mm_vari5
    } else if (req(input$fit_design) == 1 &&
      req(input$model_design) == 5 && req(input$mm_vari1) != " " &&
      req(input$mm_vari2) != " " &&
      req(input$mm_vari3) != " " && req(input$mm_vari4) != " ") {
      temp$y <- input$mm_vari1
      temp$x <- c(input$mm_vari2, input$mm_vari3, input$mm_vari4)
    } else if (req(input$fit_design) == 1 && req(input$model_design) == 6 &&
      req(input$mm_vari1) != " " && req(input$mm_vari2) != " " &&
      req(input$mm_vari3) != " " && req(input$mm_vari4) != " " &&
      req(input$mm_vari5) != " ") {
      temp$y <- input$mm_vari1
      temp$x <- c(input$mm_vari2, input$mm_vari3, input$mm_vari4)
      temp$blocking <- input$mm_vari5
    } else {
      temp.aov <- NULL
    }
    mix.model.name <- paste0("Model_", model_Vals$num)
    temp$name <- mix.model.name
    temp$data.name <- values$data.name
    temp.aov <- try(do.call(aov.fit, temp))
    # temp.model <- do.call(anova.fit, temp)

    if (!is.null(temp.aov)) {
      if (class(temp.aov)[1] == "try-error") {
        model_Vals$num <- model_Vals$num - 1
        shinyalert(
          title = "ERROR",
          text = temp.aov[1],
          type = "error"
        )
      } else {
        model_Vals$aov[[mix.model.name]] <- temp.aov
        model_Vals$trt[[mix.model.name]] <- input$mm_trt.mean
        updateSelectInput(session, "model_select",
          choices = names(model_Vals$aov),
          selected = mix.model.name
        )
      }
    }
  })
})



## fit customized model
observeEvent(input$fit_model_own, {
  isolate({
    mix.model.name <- ""
    temp.model <- NULL
    ## fit model
    ## set parameters
    temp <- mix.model.list.par()
    temp$data <- mix.data()
    ## generate model name
    model_Vals$num <- model_Vals$num + 1
    mix.model.name <- paste0("Model_", model_Vals$num)
    if (req(input$fit_design) == 2 && req(input$fit_model_own) > 0 &&
      req(input$mm_own_model_vari1) != " " && !is.null(input$fixed_effect) &&
      !is.null(input$random_effect)) {
      ## fit model
      temp$name <- mix.model.name
      temp$y <- input$mm_own_model_vari1
      temp$x <- input$fixed_effect
      temp$blocking <- input$random_effect
      temp$data.name <- values$data.name
    }
    temp.aov <- try(do.call(aov.own, temp))
    if (!is.null(temp.aov)) {
      if (class(temp.aov)[1] == "try-error") {
        shinyalert(
          title = "ERROR",
          text = temp.aov[1],
          type = "error"
        )
        model_Vals$num <- model_Vals$num - 1
      } else {
        # model_Vals$model[[mix.model.name]] = temp.model
        model_Vals$aov[[mix.model.name]] <- temp.aov
        model_Vals$trt[[mix.model.name]] <- input$mm_trt.mean1
        updateSelectInput(session, "model_select",
          choices = names(model_Vals$aov),
          selected = mix.model.name
        )
      }
    }
  })
})



## remove model
observe({
  input$remove.model
  isolate({
    if (!is.null(input$remove.model) &&
      input$remove.model > 0) {
      model_Vals$aov[[input$model_select]] <- NULL
      ch <- names(model_Vals$aov)
      updateSelectInput(session, "model_select",
        choices = ch,
        selected = names(model_Vals$aov)[length(ch)]
      )
    }
  })
})

### -------------------------------###
###          Main Panel           ###
### -------------------------------###


### -------------------------------###
###          Model Summary        ###
### -------------------------------###


### --------------------###
###       ANOVA        ###
### --------------------###

## print code
output$aov.code <- renderPrint({
  input$model_select
  isolate({
    if (!is.null(input$model_select) && !input$model_select %in% "") {
      cat(attr(model_Vals$aov[[input$model_select]], "code")[1])
    } else {
      cat("No ANOVA code to show!")
    }
  })
})



## print summary and table of treatment means


observe({
  input$model_select
  input$fit_model_own
  input$fit_model_aov
  isolate({
    if (length(model_Vals$aov) > 0 &&
      (!is.null(model_Vals$aov[[input$model_select]]) &&
        !is.null(input$model_select))) {
      ## summary
      smry.tab <- try(summary(model_Vals$aov[[input$model_select]]))
      if (!is.null(smry.tab)) {
        if (class(smry.tab)[1] == "try-error") {
          output$aov.summary <- renderPrint(smry.tab[1])
        } else {
          output$aov.summary <- renderPrint(smry.tab)
        }
      }


      ## treatment means
      if (model_Vals$trt[[input$model_select]] ==
        "No blocking or complete blocks") {
        trt.table <- try(model.tables(
          model_Vals$aov[[input$model_select]], "means"
        ))
        if (!is.null(trt.table)) {
          if (class(trt.table)[1] == "try-error") {
            output$aov.trtmean <- renderPrint(cat(trt.table[1]))
          } else {
            output$aov.trtmean <- renderPrint(trt.table)
          }
        }
      } else if (model_Vals$trt[[input$model_select]] ==
        "balanced incomplete blocks") {
        trt.table <- try(
          dummy.coef(model_Vals$aov[[input$model_select]])$"(Intercept)"[[1]] +
            dummy.coef(model_Vals$aov[[input$model_select]])$Within[[1]]
        )
        if (!is.null(trt.table)) {
          if (class(trt.table)[1] == "try-error") {
            output$aov.trtmean <- renderPrint(cat(trt.table[1]))
          } else {
            output$aov.trtmean <- renderPrint(trt.table)
          }
        }
      }
    }
  })
})

### ------- ----------###
###       LSD        ###
### ------------------###
output$Doe.smy <- renderUI({
  if (length(model_Vals$aov) > 0 &&
    (!is.null(model_Vals$aov[[input$model_select]]) &&
      !is.null(input$model_select))) {
    if (model_Vals$trt[[input$model_select]] ==
      "No blocking or complete blocks") {
      list(
        helpText("Computing LSD"),
        actionButton("lsd.complete.help", label = "HELP", style = HELP_TEXT_CSS),
        fixedRow(
          column(2, textInput("Doe.lsd.df", label = "df")),
          column(2, textInput("Doe.lsd.rms", label = "ResMS")),
          column(2, textInput("Doe.lsd.rp", label = "rep")),
          column(2, textInput("Doe.lsd.sig", label = "alpha"))
        ),
        fixedRow(column(3, actionButton(
          inputId = "Doe.comp.lsd",
          label = "Comfirm",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: -5px; margin-bottom:10px"
        ))),
        uiOutput("Doe.lsd.res"),
        helpText("Computing TSR"),
        actionButton("tsr.complete.help", label = "HELP", style = HELP_TEXT_CSS),
        fixedRow(
          column(2, textInput("Doe.tsr.df", label = "df")),
          column(2, textInput("Doe.tsr.rms", label = "ResMS")),
          column(2, textInput("Doe.tsr.rp", label = "rep")),
          column(2, textInput("Doe.tsr.lev", label = "means")),
          column(2, textInput("Doe.tsr.sig", label = "alpha")),
          column(3, actionButton(
            inputId = "Doe.comp.tsr",
            label = "Comfirm",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: -5px; margin-bottom:10px"
          ))
        ),
        uiOutput("Doe.tsr.res")
      )
    } else if (model_Vals$trt[[input$model_select]] ==
      "balanced incomplete blocks") {
      list(
        helpText("Computing LSD"),
        actionButton("lsd.incomplete.help", label = "HELP", style = HELP_TEXT_CSS),
        fixedRow(
          column(2, textInput("Doe.lsd.df", label = "df")),
          column(2, textInput("Doe.lsd.rms", label = "ResMS")),
          column(2, textInput("Doe.lsd.rp", label = "rep")),
          column(2, textInput("Doe.lsd.sig", label = "alpha")),
          column(2, textInput("Doe.lsd.eff", label = "eff"))
        ),
        fixedRow(column(3, actionButton(
          inputId = "Doe.comp.lsd",
          label = "Comfirm",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: -5px; margin-bottom:10px"
        ))),
        uiOutput("Doe.lsd.res"),
        helpText("Computing TSR"),
        actionButton("tsr.incomplete.help", label = "HELP", style = HELP_TEXT_CSS),
        fixedRow(
          column(2, textInput("Doe.tsr.df", label = "df")),
          column(2, textInput("Doe.tsr.rms", label = "ResMS")),
          column(2, textInput("Doe.tsr.rp", label = "rep")),
          column(2, textInput("Doe.tsr.lev", label = "means")),
          column(2, textInput("Doe.tsr.sig", label = "alpha")),
          column(2, textInput("Doe.tsr.eff", label = "eff")),
          column(3, actionButton(
            inputId = "Doe.comp.tsr",
            label = "Comfirm",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: -5px; margin-bottom:10px"
          ))
        ),
        uiOutput("Doe.tsr.res")
      )
    }
  }
})


observeEvent(input$Doe.comp.lsd, {
  isolate({
    if (!stringr::str_detect(input$Doe.lsd.df, "[^.\\d]") &&
      !is.null(input$Doe.lsd.df) && input$Doe.lsd.df != "" &&
      !stringr::str_detect(input$Doe.lsd.rms, "[^.\\d]") &&
      !is.null(input$Doe.lsd.rms) && input$Doe.lsd.rms != "" &&
      !stringr::str_detect(input$Doe.lsd.rp, "[^.\\d]") &&
      !is.null(input$Doe.lsd.rp) && input$Doe.lsd.rp != "" &&
      !stringr::str_detect(input$Doe.lsd.sig, "[^.\\d]") &&
      !is.null(input$Doe.lsd.sig) && input$Doe.lsd.sig != "") {
      if (model_Vals$trt[[input$model_select]] ==
        "No blocking or complete blocks") {
        output$Doe.lsd.res <- renderUI({
          verbatimTextOutput("Doe.lsd.result")
        })
      } else if (model_Vals$trt[[input$model_select]] ==
        "balanced incomplete blocks" &&
        !stringr::str_detect(input$Doe.lsd.eff, "[^.\\d]") &&
        !is.null(input$Doe.lsd.eff) && input$Doe.lsd.eff != "") {
        output$Doe.lsd.res <- renderUI({
          verbatimTextOutput("Doe.lsd.result")
        })
      } else {
        output$Doe.lsd.res <- renderUI({
          list(NULL)
        })
        shinyalert(
          title = "ERROR",
          text = "The computation cannot be processed", type = "error"
        )
      }
    } else {
      output$Doe.lsd.res <- renderUI({
        list(NULL)
      })
      shinyalert(
        title = "ERROR",
        text = "The computation cannot be processed", type = "error"
      )
    }
  })
})


output$Doe.lsd.result <- renderPrint({
  input$Doe.comp.lsd
  isolate({
    if (model_Vals$trt[[input$model_select]] ==
      "No blocking or complete blocks") {
      qt(
        1 - as.numeric(input$Doe.lsd.sig) / 2,
        as.numeric(input$Doe.lsd.df)
      ) *
        sqrt(2 * as.numeric(input$Doe.lsd.rms) / (as.numeric(input$Doe.lsd.rp)))
    } else {
      qt(
        1 - as.numeric(input$Doe.lsd.sig) / 2,
        as.numeric(input$Doe.lsd.df)
      ) * sqrt(2 * as.numeric(input$Doe.lsd.rms) /
        (as.numeric(input$Doe.lsd.rp) * as.numeric(input$Doe.lsd.eff)))
    }
  })
})


### ------- ---------###
###      TSR        ###
### -----------------###

observeEvent(input$Doe.comp.tsr, {
  isolate({
    if (!stringr::str_detect(input$Doe.tsr.df, "[^.\\d]") &&
      !is.null(input$Doe.tsr.df) && input$Doe.tsr.df != "" &&
      !stringr::str_detect(input$Doe.tsr.rms, "[^.\\d]") &&
      !is.null(input$Doe.tsr.rms) && input$Doe.tsr.rms != "" &&
      !stringr::str_detect(input$Doe.tsr.rp, "[^.\\d]") &&
      !is.null(input$Doe.tsr.rp) && input$Doe.tsr.rp != "" &&
      !stringr::str_detect(input$Doe.tsr.lev, "[^.\\d]") &&
      !is.null(input$Doe.tsr.lev) && input$Doe.tsr.lev != "" &&
      !stringr::str_detect(input$Doe.tsr.sig, "[^.\\d]") &&
      !is.null(input$Doe.tsr.sig) && input$Doe.tsr.sig != "") {
      if (model_Vals$trt[[input$model_select]] ==
        "No blocking or complete blocks") {
        output$Doe.tsr.res <- renderUI({
          verbatimTextOutput("Doe.tsr.result")
        })
      } else if (model_Vals$trt[[input$model_select]] ==
        "balanced incomplete blocks" &&
        !stringr::str_detect(input$Doe.tsr.eff, "[^.\\d]") &&
        !is.null(input$Doe.tsr.eff) && input$Doe.tsr.eff != "") {
        output$Doe.tsr.res <- renderUI({
          verbatimTextOutput("Doe.tsr.result")
        })
      } else {
        output$Doe.tsr.res <- renderUI({
          list(NULL)
        })
        shinyalert(
          title = "ERROR",
          text = "The computation cannot be processed", type = "error"
        )
      }
    } else {
      output$Doe.tsr.res <- renderUI({
        list(NULL)
      })
      shinyalert(
        title = "ERROR",
        text = "The computation cannot be processed", type = "error"
      )
    }
  })
})


output$Doe.tsr.result <- renderPrint({
  input$Doe.comp.tsr
  isolate({
    if (model_Vals$trt[[input$model_select]] ==
      "No blocking or complete blocks") {
      qtukey(
        1 - as.numeric(input$Doe.tsr.sig),
        as.numeric(input$Doe.tsr.lev),
        as.numeric(input$Doe.tsr.df)
      ) * sqrt(as.numeric(input$Doe.tsr.rms) / (as.numeric(input$Doe.tsr.rp)))
    } else {
      qtukey(
        1 - as.numeric(input$Doe.tsr.sig),
        as.numeric(input$Doe.tsr.lev),
        as.numeric(input$Doe.tsr.df)
      ) * sqrt(as.numeric(input$Doe.tsr.rms) / (as.numeric(input$Doe.tsr.rp) *
        as.numeric(input$Doe.tsr.eff)))
    }
  })
})






### ------- ----------------------###
###      Interaction plot        ###
### ------------------------------###
output$Doe.int.var <- renderUI({
  get.data.set()
  ret <- NULL
  isolate({
    var_name_numeric <- c(" ", get.numeric.column.names(get.data.set()))
    var_name_factor <- c(" ", get.categorical.column.names(get.data.set()))


    base.design <- fluidRow( ## select response variable(numeric)
      column(12, selectInput(
        inputId = "Doe.int_vari1",
        label = "Response variable",
        choices = var_name_numeric,
        selected = var_name_numeric[1],
        selectize = F
      )),

      ## select treatment varibale(factor)
      column(12, selectInput(
        inputId = "Doe.int_vari2",
        label = "X-axis variable",
        choices = var_name_factor,
        selected = var_name_factor[1],
        selectize = F
      ))
    )

    ## select second treatment factor
    sec.fac <- fluidRow(column(12, selectInput(
      inputId = "Doe.int_vari3",
      label = "trace variable",
      choices = var_name_factor,
      selected = var_name_factor[1],
      selectize = F
    )))
    ret <- list(
      base.design,
      sec.fac
    )
  })
  ret
})


output$Doe.interaction.plot <- renderPlot({
  get.data.set()
  input$Doe.int_vari1
  input$Doe.int_vari2
  input$Doe.int_vari3
  isolate({
    if (req(input$Doe.int_vari1 != " ") && req(input$Doe.int_vari2 != " ") &&
      req(input$Doe.int_vari3 != " ")) {
      data <- get.data.set()
      interaction.plot(data[[input$Doe.int_vari2]],
        data[[input$Doe.int_vari3]], data[[input$Doe.int_vari1]],
        xlab = input$Doe.int_vari2,
        ylab = paste0("mean of ", input$Doe.int_vari1),
        trace.label = input$Doe.int_vari3
      )
    }
  })
})

output$Doe.interaction.plot.save <- renderUI({
  get.data.set()
  input$Doe.int_vari1
  input$Doe.int_vari2
  input$Doe.int_vari3
  isolate({
    if (req(input$Doe.int_vari1 != " ") &&
      req(input$Doe.int_vari2 != " ") &&
      req(input$Doe.int_vari3 != " ")) {
      fixedRow(
        column(
          width = 3,
          NULL
        ),
        column(
          width = 3,
          downloadButton(outputId = "saveDoe.int.plot", label = "Download Plot")
        ),
        column(
          width = 3,
          radioButtons(
            inputId = "saveDoe.int.plottype",
            label = strong("Select the file type"),
            choices = list("jpg", "png", "pdf"), inline = TRUE
          )
        )
      )
    }
  })
})



output$saveDoe.int.plot <- downloadHandler(
  filename = function() {
    paste("InteractionPlot",
      switch(input$saveDoe.int.plottype,
        "jpg" = "jpg",
        "png" = "png",
        "pdf" = "pdf"
      ),
      sep = "."
    )
  },
  content = function(file) {
    if (input$saveDoe.int.plottype %in% c("jpg", "png", "pdf")) {
      if (input$saveDoe.int.plottype == "jpg") {
        jpeg(file)
      } else if (input$saveDoe.int.plottype == "png") {
        png(file)
      } else if (input$saveDoe.int.plottype == "pdf") {
        pdf(file, useDingbats = FALSE)
      }
      suppressWarnings(tryCatch(
        {
          data <- get.data.set()
          interaction.plot(data[[input$Doe.int_vari2]],
            data[[input$Doe.int_vari3]], data[[input$Doe.int_vari1]],
            xlab = input$Doe.int_vari2,
            ylab = paste0("mean of ", input$Doe.int_vari1),
            trace.label = input$Doe.int_vari3
          )
        },
        error = function(e) {
          print(e)
        }, finally = {}
      ))

      dev.off()
    }
  }
)

### ------- ----------------------###
###           Help popup          ###
### ------------------------------###
observeEvent(input$tsr.complete.help, {
  show_ct_help(
    is_eff = FALSE,
    is_lsd = FALSE
  )
})

observeEvent(input$lsd.complete.help, {
  show_ct_help(
    is_eff = FALSE,
    is_lsd = TRUE
  )
})

observeEvent(input$tsr.incomplete.help, {
  show_ct_help(
    is_eff = TRUE,
    is_lsd = FALSE
  )
})

observeEvent(input$lsd.incomplete.help, {
  show_ct_help(
    is_eff = TRUE,
    is_lsd = TRUE
  )
})
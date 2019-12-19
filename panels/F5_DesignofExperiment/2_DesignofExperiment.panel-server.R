###-------------------------------------------------###
###  Server Functions for the "Mixed Model" Module  ###
###-------------------------------------------------###
###
###  Date Created   :   March 25, 2018
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *



## initialize gui
output$mixedmodel.panel <- renderUI({
  mixedModel.panel.ui(get.data.set())
})

## get data set
mix.data <- reactive({
  get.data.set()
})


## reactive value for fit mixed model

model_Vals = reactiveValues(
  num = 0,
  model = list(),  ## list to save model
  aov = list() ## save aov table
)

mix.model.par = reactiveValues(
  y = NULL,
  x = NULL,
  data = NULL,
  blocking = NULL,
  name = NULL,
  data.name = NULL
)

mix.model.list.par = reactive({
  mix.model.list.par = reactiveValuesToList(mix.model.par)
})

fit_message <- reactiveValues(
  msg1 = as.logical(),
  msg2 = as.logical()
)


###-------------------------------------------------###
###              Model Fitting part                 ###
###-------------------------------------------------###

## UI of selecting variables
output$ep_anova <- renderUI({
  get.data.set()
  ret = NULL
  input$model_design
  isolate({
    var_name_numeric = c(" ", get.numeric.column.names(get.data.set()))
    var_name_factor = c(" ", get.categorical.column.names(get.data.set()))
    
    
    base.design = fluidRow(## select response variable(numeric)
      column(12, selectInput(inputId = "mm_vari1",
                             label = "Response variable",
                             choices = var_name_numeric,
                             selected = var_name_numeric[1],
                             selectize = F)),
      
      ## select treatment varibale(factor)
      column(12, selectInput(inputId = "mm_vari2",
                             label = "Treatment factor",
                             choices = var_name_factor,
                             selected = var_name_factor[1],
                             selectize = F)))
    
    ## select second treatment factor
    sec.fac = fluidRow(column(12, selectInput(inputId = "mm_vari3",
                                              label = "Second Treatment factor",
                                              choices = var_name_factor,
                                              selected = var_name_factor[1],
                                              selectize = F)))
    
    ## select third treatment factor
    third.fac = fluidRow(column(12, selectInput(inputId = "mm_vari4",
                                                label = "Third Treatment factor",
                                                choices = var_name_factor,
                                                selected = var_name_factor[1],
                                                selectize = F)))
    
    ## select block factor
    block.fac = fluidRow(column(12, selectInput(inputId = "mm_vari5",
                                                label = "Block factor",
                                                choices = var_name_factor,
                                                selected = var_name_factor[1],
                                                selectize = F)))
    if(req(input$model_design) == 1){
      ret = base.design
    } else if (req(input$model_design) == 2) {
      ret = list(
        base.design,
        block.fac
      )} else if (req(input$model_design) == 3) {
        ret = list(
          base.design,
          sec.fac
        )} else if (req(input$model_design) == 4) {
          ret = list(
            base.design,
            sec.fac,
            block.fac
          )} else if (req(input$model_design) == 5) {
            ret = list(
              base.design,
              sec.fac,
              third.fac
            )} else if (req(input$model_design) == 6) {
              ret = list(
                base.design,
                sec.fac,
                third.fac,
                block.fac
              )}
  })
  ret
})


###----------------------------###
###     update selectInput     ###
###----------------------------###



observe({
  if(req(input$mm_vari2) != " "){
    isolate({
      ch  = get.categorical.column.names(get.data.set())
      ch  = c(" ", ch[-which(ch %in% input$mm_vari2)])
      sel = input$mm_vari3
      if(!is.null(sel)&&!sel%in%ch){
        sel = ch[1]
      }
      updateSelectInput(session, "mm_vari3", choices=ch, selected=sel)
    })
  }
})


observe({
  if(req(input$mm_vari2) != " " | req(input$mm_vari3) != " "){
    isolate({
      ch  = get.categorical.column.names(get.data.set())
      if(req(input$mm_vari2) != " "){
        ch  = ch[-which(ch %in% input$mm_vari2)]
      }
      if(req(input$mm_vari3) != " "){
        ch  = ch[-which(ch %in% input$mm_vari3)]
      }
      ch = c(" ", ch)
      sel = input$mm_vari4
      if(!is.null(sel)&&!sel%in%ch){
        sel = ch[1]
      }
      updateSelectInput(session, "mm_vari4", choices=ch, selected=sel)
    })
  }
})

## update the Block factor
observe({
  if(req(input$mm_vari2) != " " | req(input$mm_vari3) != " " |
     req(input$mm_vari4) != " "){
    isolate({
      ch  = get.categorical.column.names(get.data.set())
      if(req(input$mm_vari2) != " "){
        ch  = ch[-which(ch %in% input$mm_vari2)]
      }
      if(req(input$mm_vari3) != " "){
        ch  = ch[-which(ch %in% input$mm_vari3)]
      }
      if(req(input$mm_vari4) != " "){
        ch  = ch[-which(ch %in% input$mm_vari4)]
      }
      ch = c(" ", ch)
      sel = input$mm_vari5
      if(!is.null(sel)&&!sel%in%ch){
        sel = ch[1]
      }
      updateSelectInput(session, "mm_vari5", choices=ch, selected=sel)
    })
  }
})

###------------------------------------###
###          fit own model             ###
###------------------------------------###

## select response variable
output$own_model_var1_panel <- renderUI({
  get.data.set()
  isolate({
    var_name_numeric = c(" ", get.numeric.column.names(get.data.set()))
    sel = input$mm_own_model_vari1
    div(
      hr(),
      selectInput(inputId = "mm_own_model_vari1",
                  label = "Response variable",
                  choices = var_name_numeric,
                  selected = sel,
                  selectize = F))
  })
})

## select fixed effect
output$own_model_fixed_panel <- renderUI({
  get.data.set()
  isolate({    
    sel = input$mm_own_model_fixed    
    selectInput(inputId = "mm_own_model_fixed",
                label = "Fixed effect",
                choices =c(" ", colnames(mix.data())),
                selected = NULL,
                size = length(colnames(mix.data())) + 1,
                selectize = F)
  })
})

##  Update fixed effect.
observe({
  if(req(input$mm_own_model_vari1) != " "){
    isolate({
      ch  = colnames(mix.data())
      ch  = c(" ", ch[-which(ch %in% input$mm_own_model_vari1)])
      sel = input$mm_own_model_fixed
      if(!is.null(sel)&&!sel%in%ch){
        sel = ch[1]
      }
      updateSelectInput(session, "mm_own_model_fixed", choices=ch, selected=sel)
    })
  }
})


observe({
  input$mm_own_model_fixed
  isolate({
    updateTextInput(session, inputId = "fixed_effect", label = "Fixed effect:",
                    value = paste0(input$fixed_effect, input$mm_own_model_fixed))
  })
})


## select random effect(factor)
output$own_model_random_panel <- renderUI({
  get.data.set()
  isolate({
    var_name_factor = c(" ", get.categorical.column.names(get.data.set()))
    sel = input$mm_own_model_random    
    selectInput(inputId = "mm_own_model_random",
                label = "Random effect",
                choices = var_name_factor,
                selected = NULL,
                size = length(var_name_factor),
                selectize = F)
  })
})


observe({
  input$mm_own_model_random
  isolate({
    updateTextInput(session, inputId = "random_effect", label = "Random effect:",
                    value = paste0(input$random_effect, input$mm_own_model_random))
  })
})


###--------------------------------------------###
###             fit the model                  ###
###--------------------------------------------###

## we fit model separately

observeEvent(input$fit_model_aov, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if (req(input$fit_design) == 1 && req(input$model_design) == 1 && req(input$mm_vari1) != " " && 
        req(input$mm_vari2) != " ") {
      temp$y = input$mm_vari1
      temp$x = input$mm_vari2
    } else if (req(input$fit_design) == 1 && req(input$model_design) == 2 && req(input$mm_vari1) != " " && req(input$mm_vari2) != " " &&
               req(input$mm_vari5) != " ") {
      temp$y = input$mm_vari1
      temp$x = input$mm_vari2
      temp$blocking = input$mm_vari5
    } else if (req(input$fit_design) == 1 && req(input$model_design) == 3 && req(input$mm_vari1) != " " && req(input$mm_vari2) != " " &&
               req(input$mm_vari3) != " ") {
      temp$y = input$mm_vari1
      temp$x = c(input$mm_vari2, input$mm_vari3)
    } else if (req(input$fit_design) == 1 && req(input$model_design) == 4 && req(input$mm_vari1) != " " && req(input$mm_vari2) != " " &&
               req(input$mm_vari3) != " " && req(input$mm_vari5) != " ") {
      temp$y = input$mm_vari1
      temp$x = c(input$mm_vari2, input$mm_vari3)
      temp$blocking = input$mm_vari5
    } else if (req(input$fit_design) == 1 && req(input$model_design) == 5 && req(input$mm_vari1) != " " && req(input$mm_vari2) != " " &&
               req(input$mm_vari3) != " " && req(input$mm_vari4) != " ") {
      temp$y = input$mm_vari1
      temp$x = c(input$mm_vari2, input$mm_vari3, input$mm_vari4)
    } else if (req(input$fit_design) == 1 && req(input$model_design) == 6 && req(input$mm_vari1) != " " && req(input$mm_vari2) != " " &&
               req(input$mm_vari3) != " " && req(input$mm_vari4) != " " && req(input$mm_vari5) != " ") {
      temp$y = input$mm_vari1
      temp$x = c(input$mm_vari2, input$mm_vari3, input$mm_vari4)
      temp$blocking = input$mm_vari5
    }
    mix.model.name = paste0("Model_", model_Vals$num)
    temp$name = mix.model.name
    temp$data.name = values$data.name
    temp.model <- do.call(anova.fit, temp)
    temp.aov <- do.call(aov.fit, temp)
  }, error = function(e){print(e)}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg1 = T
  } else if(is.null(temp.model) && input$fit_model_aov > 0){
    model_Vals$num = model_Vals$num - 1
    fit_message$msg1 = F
  }
  })
})

## fit customized model
observeEvent(input$fit_model_own, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    mix.model.name = paste0("Model_", model_Vals$num)
    if(req(input$fit_design) == 2 && req(input$fit_model_own) > 0){
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_own_model_vari1
      temp$x = input$fixed_effect
      temp$blocking = input$random_effect
      temp$data.name = values$data.name
      temp.model <- do.call(fit.own, temp)
    }
  }, error = function(e){print(e)}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg2 = T
  } else if(is.null(temp.model) && input$fit_model_own > 0){
    fit_message$msg2 = F
    model_Vals$num = model_Vals$num - 1
  }
  })
})



## remove model
observe({
  input$remove.model
  isolate({
    if(!is.null(input$remove.model)&&
       input$remove.model>0){
      model_Vals$model[[input$model_select]] = NULL
      ch = names(model_Vals$model)
      updateSelectInput(session,"model_select",
                        choices=ch,
                        selected=names(model_Vals$model)[length(ch)])
    }
  })
})

###-------------------------------###
###          Main Panel           ###
###-------------------------------###


###-------------------------------###
###          Model Summary        ###
###-------------------------------###


## give error message if the model is invalid
lapply(1:2, function(x) {
  output[[paste0("error_msg_d", x)]] <- renderUI({
    if (!is.null(fit_message[[paste0("msg", x)]]) && length(fit_message[[paste0("msg", x)]]) > 0 && fit_message[[paste0("msg", x)]] == FALSE) {
      shiny::tags$div(shiny::code("The model can't be processed."), style = "margin-top: 10px")    
    }
  })
})



###--------------------###
###       ANOVA        ###
###--------------------###

## print code
output$aov.code = renderPrint({
  input$model_select
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "") {
      cat(attr(model_Vals$aov[[input$model_select]], 'code')[1])
    } else {
      cat("No ANOVA code to show!")
    }
  })
})

## print summary
output$aov.summary = renderPrint({
  input$model_select
  isolate({
    if (length(model_Vals$aov)>0&&
        (!is.null(model_Vals$aov[[input$model_select]])&&
         !is.null(input$model_select))){
      summary(model_Vals$aov[[input$model_select]])
    } else {
      cat("No model to show!")
    }
  })
})


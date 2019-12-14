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
  #   values$data.set
  get.data.set()
})


values$create.fix.expression.text = ""

get.create.fix.expression.text = reactive({
  values$create.fix.expression.text
})

values$create.random.expression.text = ""

get.create.random.expression.text = reactive({
  values$create.random.expression.text
})


## reactive value for fit mixed model

mix.list.par = reactive({
  mix.list.par = reactiveValuesToList(mix.par)
})

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
  name = NULL
)

mix.model.list.par = reactive({
  mix.model.list.par = reactiveValuesToList(mix.model.par)
})

fit_message <- reactiveValues(
  msg1 = as.logical(),
  msg2 = as.logical(),
  msg3 = as.logical(),
  msg4 = as.logical(),
  msg5 = as.logical(),
  msg6 = as.logical(),
  msg7 = as.logical()
)


save_success_msg <- reactiveValues(
  aov = F,
  resip = F,
  tabmi = F,
  pht = F,
  meanLSD = F
)


## find out the last clicked button
lbc <- reactiveValues(lastBtn = character())


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
    sel = input$mm_own_model_vari1
    div(
      hr(),
      selectInput(inputId = "mm_own_model_vari1",
                  label = "Response variable",
                  choices = c(" ", var_name_numeric()),
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
  if(!is.null(input$mm_own_model_vari1)){
    isolate({
      if(!is.null(mix.data())){
        ch  = colnames(mix.data())
        if(!is.null(input$mm_own_model_vari1)&&input$mm_own_model_vari1%in%ch){
          ch  = ch[-which(colnames(mix.data())%in%input$mm_own_model_vari1)]
        }
        sel = input$mm_own_model_fixed
        if(!is.null(sel)&&!sel%in%ch){
          sel = ch[1]
        }
        updateSelectInput(session, "mm_own_model_fixed", choices=ch, selected=sel)
      }
    })
  }
})

## add the operation to string of fixed effect when select a new operation
observe({
  input$select_operation
  isolate({
    if(!" "%in%input$select_operation){
      values$create.fix.expression.text  = paste(
        get.create.fix.expression.text(),
        input$select_operation,sep="")
    }
  })
})

## add variable name to string of fixed effect when select a new variable
observe({
  input$mm_own_model_fixed
  isolate({
    if(!" "%in%input$mm_own_model_fixed){
      values$create.fix.expression.text  = paste(
        get.create.fix.expression.text(),
        input$mm_own_model_fixed,sep="")
    }
  })
})

## fixed effect string
output$fixed_expr = renderPrint({
  cat(get.create.fix.expression.text())
})


## delete button for fixed effect
observe({
  input$delete_mm_fix
  isolate({
    if(!is.null(input$delete_mm_fix)&&input$delete_mm_fix>0){
      if(nchar(values$create.fix.expression.text)>0){
        values$create.fix.expression.text = substr(
          values$create.fix.expression.text,1,
          nchar(values$create.fix.expression.text)-1)
      }
    }
  })
})


## select random effect(factor)
output$own_model_random_panel <- renderUI({
  get.data.set()
  isolate({    
    sel = input$mm_own_model_random    
    selectInput(inputId = "mm_own_model_random",
                label = "Random effect",
                choices =c(" ", var_name_factor()),
                selected = NULL,
                size = length(var_name_factor()) + 1,
                selectize = F)
  })
})



## update character string for random effect
observe({
  input$select_operation_random
  isolate({
    if(!" "%in%input$select_operation_random){
      values$create.random.expression.text  = paste(
        get.create.random.expression.text(),
        input$select_operation_random,sep="")
    }
  })
})

## update character string for random effect
observe({
  input$mm_own_model_random
  isolate({
    if(!" "%in%input$mm_own_model_random){
      values$create.random.expression.text  = paste(
        get.create.random.expression.text(),
        input$mm_own_model_random,sep="")
    }
  })
})

## random effect string
output$random_expr = renderPrint({
  cat(get.create.random.expression.text())
})



## delete button
observe({
  input$delete_mm_random
  isolate({
    if(!is.null(input$delete_mm_random)&&input$delete_mm_random>0){
      if(nchar(values$create.random.expression.text)>0){
        values$create.random.expression.text = substr(
          values$create.random.expression.text,1,
          nchar(values$create.random.expression.text)-1)
      }
    }
  })
})


###--------------------------------------------###
###             fit the model                  ###
###--------------------------------------------###

## we fit model separately
## fit oneway anova
observeEvent(input$fit_model1, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if(input$fit_design == 1 && input$model_design == 1 && !is.null(input$mm_oneway_nb_vari1) && !is.null(input$mm_oneway_nb_vari2)){
      mix.model.name = paste0("Model_", model_Vals$num)
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_oneway_nb_vari1
      temp$x = input$mm_oneway_nb_vari2
      temp.model <- do.call(anova.fit, temp)
      temp.aov <- do.call(aov.fit, temp)
    }
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg1 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model1 > 0){
    model_Vals$num = model_Vals$num - 1
    fit_message$msg1 = F
  }
  })
})

## fit one-way anova with blocking
observeEvent(input$fit_model2, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if(input$fit_design == 1 && input$model_design == 2 && !is.null(input$mm_oneway_rb_vari1) && !is.null(input$mm_oneway_rb_vari2) &&
       !is.null(input$mm_oneway_rb_vari3) && !is.null(input$fit_model2) && input$fit_model2 > 0){
      mix.model.name = paste0("Model_", model_Vals$num)
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_oneway_rb_vari1
      temp$x = input$mm_oneway_rb_vari2
      temp$blocking = input$mm_oneway_rb_vari3
      temp.model <- do.call(anova.fit, temp)
      temp.aov <- do.call(aov.fit, temp)
    }
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg2 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model2 > 0){
    fit_message$msg2 = F
    model_Vals$num = model_Vals$num - 1
  }
  })
})

## fit two-way anova
observeEvent(input$fit_model3, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if(input$fit_design == 1 && input$model_design == 3 && !is.null(input$mm_twoway_nb_vari1) && !is.null(input$mm_twoway_nb_vari2) &&
       !is.null(input$mm_twoway_nb_vari3) && !is.null(input$fit_model3) && input$fit_model3 > 0){
      mix.model.name = paste0("Model_", model_Vals$num)
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_twoway_nb_vari1
      temp$x = c(input$mm_twoway_nb_vari2, input$mm_twoway_nb_vari3)
      temp.model <- do.call(anova.fit, temp)
      temp.aov <- do.call(aov.fit, temp)
    }
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg3 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model3 > 0){
    fit_message$msg3 = F
    model_Vals$num = model_Vals$num - 1
  }
  })
})

## fit two-way anova with blocking
observeEvent(input$fit_model4, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if(input$fit_design == 1 && input$model_design == 4 && !is.null(input$mm_twoway_rb_vari1) && !is.null(input$mm_twoway_rb_vari2) &&
       !is.null(input$mm_twoway_rb_vari3) && !is.null(input$mm_twoway_rb_vari4) && !is.null(input$fit_model4) && input$fit_model4 > 0){
      mix.model.name = paste0("Model_", model_Vals$num)
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_twoway_rb_vari1
      temp$x = c(input$mm_twoway_rb_vari2, input$mm_twoway_rb_vari3)
      temp$blocking = input$mm_twoway_rb_vari4
      temp.model <- do.call(anova.fit, temp)
      temp.aov <- do.call(aov.fit, temp)
    }
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg4 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model4 > 0){
    fit_message$msg4 = F
    model_Vals$num = model_Vals$num - 1
  }
  })
})

## fit three way anova
observeEvent(input$fit_model5, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if(input$fit_design == 1 && input$model_design == 5 && !is.null(input$mm_threeway_nb_vari1) && !is.null(input$mm_threeway_nb_vari2) &&
       !is.null(input$mm_threeway_nb_vari3) && !is.null(input$mm_threeway_nb_vari4) && !is.null(input$fit_model5) && input$fit_model5 > 0){
      mix.model.name = paste0("Model_", model_Vals$num)
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_threeway_nb_vari1
      temp$x = c(input$mm_threeway_nb_vari2, input$mm_threeway_nb_vari3, input$mm_threeway_nb_vari4)
      temp.model <- do.call(anova.fit, temp)
      temp.aov <- do.call(aov.fit, temp)
    } 
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg5 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model5 > 0){
    fit_message$msg5 = F
    model_Vals$num = model_Vals$num - 1
  }
  })
})

## fit three way anova with blocking
observeEvent(input$fit_model6, {
  isolate({mix.model.name = ""
  temp.model = NULL
  ## fit model
  ## set parameters
  tryCatch({
    temp = mix.model.list.par()
    temp$data = mix.data()
    ## generate model name
    model_Vals$num = model_Vals$num + 1
    if(input$fit_design == 1 && input$model_design == 6 && !is.null(input$mm_threeway_rb_vari1) && !is.null(input$mm_threeway_rb_vari2) &&
       !is.null(input$mm_threeway_rb_vari3) && !is.null(input$mm_threeway_rb_vari4) && !is.null(input$mm_threeway_rb_vari5) &&
       !is.null(input$fit_model6) && input$fit_model6 > 0){
      mix.model.name = paste0("Model_", model_Vals$num)
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_threeway_rb_vari1
      temp$x = c(input$mm_threeway_rb_vari2, input$mm_threeway_rb_vari3, input$mm_threeway_rb_vari4)
      temp$blocking = input$mm_threeway_rb_vari5
      temp.model <- do.call(anova.fit, temp)
      temp.aov <- do.call(aov.fit, temp)
    }
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg6 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model6 > 0){
    fit_message$msg6 = F
    model_Vals$num = model_Vals$num - 1
  }
  })
})

## fit customized model
observeEvent(input$fit_model7, {
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
    if(input$fit_design == 2 && !is.null(input$fit_model7) && input$fit_model7 > 0 && 
       !is.null(values$create.fix.expression.text) && values$create.fix.expression.text != "" &&
       !is.null(values$create.random.expression.text) && values$create.fix.expression.text != ""){
      ## fit model
      temp$name = mix.model.name
      temp$y = input$mm_own_model_vari1
      temp$x = values$create.fix.expression.text
      temp$blocking = values$create.random.expression.text
      temp.model <- do.call(fit.own, temp)
      temp.aov <- do.call(aov.own, temp)
    }
  }, error = function(e){}, finally = {})
  if(!is.null(temp.model)){
    model_Vals$model[[mix.model.name]] = temp.model
    model_Vals$aov[[mix.model.name]] = temp.aov
    updateSelectInput(session, "model_select", choices = names(model_Vals$model),                          
                      selected = mix.model.name)
    fit_message$msg7 = T
    save_success_msg$aov = F
    save_success_msg$resip = F
    save_success_msg$tabmi = F
    save_success_msg$pht = F
    save_success_msg$meanLSD = F
  } else if(is.null(temp.model) && input$fit_model7 > 0){
    fit_message$msg7 = F
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
#output$model.code = renderPrint({
#  input$model_select
#  isolate({
#    if(!is.null(input$model_select) && !input$model_select %in% "") {
#      cat(attr(model_Vals$model[[input$model_select]], 'code'))
#    } else {
#      cat("No model code to show!")
#    }
#  })
#})

#output$model.summary = renderPrint({
#  input$model_select
#  isolate({
#    if (length(model_Vals$model)>0&&
#       (!is.null(model_Vals$model[[input$model_select]])&&
#          !is.null(input$model_select))){
#      summary(model_Vals$model[[input$model_select]])
#    } else {
#      cat("No model to show!")
#    }
#  })
#})



lapply(
  X = 1:7,
  FUN = function(i){
    observeEvent(input[[paste0("fit_model", i)]], {
      if (!is.null(fit_message[[paste0("msg", i)]]) && length(fit_message[[paste0("msg", i)]] > 0) && 
          fit_message[[paste0("msg", i)]] == T) {
        #        output$comment.save.summary <- renderUI({
        #          div(id = 'summary_doe_cca',
        #              checkboxInput(inputId = "comment_summary", label = "Add comments", value = FALSE),
        #              conditionalPanel(condition = "input.comment_summary == 1", 
        #                               column(12, textAreaInput(inputId = "comment_summary_text", label = "", value = "", resize = "both", width ="500px", height = "100px",
        #                                                        placeholder = "Box can be resized by dragging the arrow at bottom right"),
        #                                      style = "margin-top: -25px")),
        #              actionButton("refresh_summary", label = "Save", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        #        })
        output$comment.save.aov <- renderUI({
          div(id = 'aov_doe_cca',
              checkboxInput(inputId = "comment_aov", label = "Add comments", value = FALSE),
              conditionalPanel(condition = "input.comment_aov == 1", 
                               column(12, textAreaInput(inputId = "comment_aov_text", label = "", value = "", resize = "both", width ="500px", height = "100px",
                                                        placeholder = "Box can be resized by dragging the arrow at bottom right"),
                                      style = "margin-top: -25px")),
              actionButton("refresh_aov", label = "Save", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        })
        output$comment.save.resip <- renderUI({
          div(id = 'resip_doe_cca',
              checkboxInput(inputId = "comment_resip", label = "Add comments", value = FALSE),
              conditionalPanel(condition = "input.comment_resip == 1", 
                               column(12, textAreaInput(inputId = "comment_resip_text", label = "", value = "", resize = "both", width ="500px", height = "100px",
                                                        placeholder = "Box can be resized by dragging the arrow at bottom right"),
                                      style = "margin-top: -25px")),
              actionButton("refresh_resip", label = "Save", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        })
      }
    })
  }
)

## find out which button is last clicked
lapply(
  X = 1:7,
  FUN = function(i){
    observeEvent(input[[paste0("fit_model", i)]], {
      if (input[[paste0("fit_model", i)]] > 0) {
        lbc$lastBtn = paste0(i)    
      }
    })
  }
)

## hide mainPanel if the model cannot be fitted
observe({
  toggle(id = "doe_show", condition = fit_message[[paste0("msg", lbc$lastBtn)]])
})

## give error message if the model is invalid
lapply(1:7, function(x) {
  output[[paste0("error_msg_d", x)]] <- renderUI({
    if (!is.null(fit_message[[paste0("msg", x)]]) && length(fit_message[[paste0("msg", x)]]) > 0 && fit_message[[paste0("msg", x)]] == FALSE) {
      shiny::tags$div(shiny::code("The model can't be processed."), style = "margin-top: 10px")    
    }
  })
})

## if the save button is clicked, save the code.
## then widgets are refreshed and a successful message is shown
#observe({
#  input$refresh_summary
#  isolate({
#    if(!is.null(input$refresh_summary) && input$refresh_summary > 0){
#      reset("summary_doe_cca")
#      save_success_msg$summary = T
#      if (input$comment_summary == 1 && input$comment_summary_text != "" && 
#          length(input$comment_summary_text) > 0) {
#        ## provide succuss message
#        output$success_msg_summry_doe1 <- renderUI({
#          div(
#            br(),
#            strong("The model and comments are successfully saved"),
#            br(),
#            hr())
#        })
#        ## save code
#        code.save$model[[input$model_select]][["summary"]] = c("```{r}", attr(model_Vals$model[[input$model_select]], "code"),
#                                                               sprintf("summary(%s)", input$model_select), "```\n", paste0(input$comment_summary_text, "\n"))
#      } else if (input$comment_summary == 0 && input$comment_summary_text == ""){
#        ## provide success message
#        output$success_msg_summry_doe1 <- renderUI({
#          div(
#            br(),
#            strong("The model is successfully saved"),
#            br(),
#            hr())
#        })
#        ## save code
#        code.save$model[[input$model_select]][["summary"]] = c("```{r}", attr(model_Vals$model[[input$model_select]], "code"),
#                                                               sprintf("summary(%s)", input$model_select), "```\n")
#      }
#    }
#  })
#})


###--------------------###
###       ANOVA        ###
###--------------------###

## print code
output$aov.code = renderPrint({
  input$model_select
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "") {
      cat(attr(model_Vals$aov[[input$model_select]], 'code'))
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

## if the save button is clicked, code for anova and mixed model is saved
## the widgets are refreshed and a successful message is shown
observe({
  input$refresh_aov
  isolate({
    if(!is.null(input$refresh_aov) && input$refresh_aov > 0){
      reset("aov_doe_cca")
      save_success_msg$aov = T
      if (input$comment_aov == 1 && input$comment_aov_text != "" && 
          length(input$comment_aov_text) > 0) {
        ## provide succuss message
        output$success_msg_aov_doe1 <- renderUI({
          div(
            br(),
            strong("The ANOVA summary and comments are successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["aov"]] = c("```{r}", attr(model_Vals$aov[[input$model_select]], "code.save"), "```\n",
                                                           paste0(input$comment_aov_text, "\n"))
        code.save$model[[input$model_select]][["summary"]] = c("```{r}", attr(model_Vals$model[[input$model_select]], "code"),
                                                               "```\n")
      } else if (input$comment_aov == 0 && input$comment_aov_text == ""){
        ## provide success message
        output$success_msg_aov_doe1 <- renderUI({
          div(
            br(),
            strong("The ANOVA summary is successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["aov"]] = c("```{r}", attr(model_Vals$aov[[input$model_select]], "code.save"), "```\n")
        code.save$model[[input$model_select]][["summary"]] = c("```{r}", attr(model_Vals$model[[input$model_select]], "code"),
                                                               "```\n")
      }
    }
  })
})


###--------------------------###
###       Residual Plot      ###
###--------------------------###

output$resi.ep = renderPlot({
  input$model_select
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "") {
      predictmeans::residplot(model_Vals$model[[input$model_select]])
    } else {
      plot.new()
      text(0.5,0.5,"Please fit model first.",cex=2)
    }
  })
})

## if the save button is clicked, code for residual plot is saved
## the widgets are refreshed and a successful message is shown
observe({
  input$refresh_resip
  isolate({
    if(!is.null(input$refresh_resip) && input$refresh_resip > 0){
      reset("resip_doe_cca")
      save_success_msg$resip = T
      if (input$comment_resip == 1 && input$comment_resip_text != "" && 
          length(input$comment_resip_text) > 0) {
        ## provide succuss message
        output$success_msg_resip_doe1 <- renderUI({
          div(
            br(),
            strong("The residual plots and comments are successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["resip"]] = c("```{r}", sprintf("predictmeans::residplot(%s)", input$model_select), "```\n",
                                                             paste0(input$comment_resip_text, "\n"))
      } else if (input$comment_resip == 0 && input$comment_resip_text == ""){
        ## provide success message
        output$success_msg_resip_doe1 <- renderUI({
          div(
            br(),
            strong("The residual plots are successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["resip"]] = c("```{r}", sprintf("predictmeans::residplot(%s)", input$model_select), "```\n")
      }
    }
  })
})


###------------------###
###    mean table    ###
###------------------###


## select input for model terms in anova
output$select.mi <- renderUI({
  input$model_select
  isolate({    
    if(!is.null(input$model_select) && !input$model_select %in% ""){
      #sel = eval(parse(text = gsub('"', "",  sprintf("attributes(terms(~%s))$term.labels", char))))
      sel = c(" ", attr(model_Vals$model[[input$model_select]]$terms, "term.labels"))
      selectInput(inputId = "select.mi.var",
                  label = "Select fixed effect model term",
                  choices = sel,
                  selected = sel[length(sel)],
                  selectize = F)
    }else{
      plotOutput("mm_table.mi_nomodel")
    }
  })
})


output$mm_table.mi_nomodel <- renderPlot({
  plot.new()
  text(0.5,0.5,"Please fit model first.",cex=2)
})


## ui for mse table and post-hoc test
output$select.table.mi <- renderUI({
  if(input$select.mi.var != " " && !is.null(input$model_select) && length(input$model_select) > 0 
     && !is.null(input$select.mi.var)){  
      tabsetPanel(
        type = "pills",
        tabPanel("Group means and standard errors",
                 tableOutput("mm_table.mi_meanse"),
                 uiOutput("comment.save.tabmi"),
                 uiOutput("success_msg_tabmi_doe1")),
        tabPanel("Post-hoc test results",
                 uiOutput("mm_pval.mi_pht"),
                 tableOutput("mm_table.mi_pht"),
                 uiOutput("comment.save.pht"),
                 uiOutput("success_msg_pht_doe1")),
        tabPanel("Means and LSD plots",
                 uiOutput("select.mi.singlevar"),
                 plotOutput("mm_plot.mi_MeanLSD"),
                 uiOutput("comment.save.mLSD"),
                 uiOutput("success_msg_meanLSD_doe1")))
  } 
})


##mse table
output$mm_table.mi_meanse <- renderTable({
  input$model_select
  input$select.mi.var
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "" && input$select.mi.var != " "
       && !is.null(input$select.mi.var)) {
      ch = predictmeans::predictmeans(model_Vals$model[[input$model_select]], 
                                      modelterm = input$select.mi.var, pairwise = TRUE, plot = FALSE)
      meantable::means2df(ch)
    }
  })
})

## ui for comment box
output$comment.save.tabmi <- renderUI({
  if(input$select.mi.var != " " && !is.null(input$model_select) && length(input$model_select) > 0 
     && !is.null(input$select.mi.var)){  
      div(id = 'tabmi_doe_cca',
          checkboxInput(inputId = "comment_tabmi", label = "Add comments", value = FALSE),
          conditionalPanel(condition = "input.comment_tabmi == 1", 
                           column(12, textAreaInput(inputId = "comment_tabmi_text", label = "", value = "", resize = "both", width ="500px", height = "100px",
                                                    placeholder = "Box can be resized by dragging the arrow at bottom right"),
                                  style = "margin-top: -25px")),
          actionButton("refresh_tabmi", label = "Save", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  }
})


## if the save button is clicked, code for mean and se table is saved
## the widgets are refreshed and a successful message is shown
observe({
  input$refresh_tabmi
  isolate({
    if(!is.null(input$refresh_tabmi) && input$refresh_tabmi > 0){
      reset("tabmi_doe_cca")
      save_success_msg$tabmi = T
      if (input$comment_tabmi == 1 && input$comment_tabmi_text != "" && 
          length(input$comment_tabmi_text) > 0) {
        ## provide succuss message
        output$success_msg_tabmi_doe1 <- renderUI({
          div(
            br(),
            strong("The table and comments are successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["tabmi"]] = c(code.save$model[[input$model_select]][["tabmi"]], 
                                                             "```{r}", sprintf("means2df(predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE))", input$model_select, input$select.mi.var), "```\n",
                                                             paste0(input$comment_tabmi_text, "\n"))
      } else if (input$comment_tabmi == 0 && input$comment_tabmi_text == ""){
        ## provide success message
        output$success_msg_tabmi_doe1 <- renderUI({
          div(
            br(),
            strong("The table is successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["tabmi"]] = c(code.save$model[[input$model_select]][["tabmi"]], 
                                                             "```{r}", sprintf("means2df(predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE))", input$model_select, input$select.mi.var), "```\n")
      }
    }
  })
})


###-------------------###
###   post-hoc test   ###
###-------------------###

output$mm_pval.mi_pht <- renderUI({
  input$model_select
  input$select.mi.var
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "" && input$select.mi.var != " "
       && !is.null(input$select.mi.var)) {
      pvalvec <- summary(model_Vals$aov[[input$model_select]])[[1]][[5]][-length(summary(model_Vals$aov[[input$model_select]])[[1]][[5]])]
      pval <- pvalvec[match(input$select.mi.var, attr(model_Vals$aov[[input$model_select]]$terms, "term.labels"))]
      if(pval > 0.05){
        tags$div(br(),
                 fluidRow(column(12, shiny::code("Note: p-value of the selected variable is not significant"))),
                 br())
      }
     }
  })
})

output$mm_table.mi_pht <- renderTable({
  input$model_select
  input$select.mi.var
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "" && input$select.mi.var != " "
       && !is.null(input$select.mi.var)) {
      ch = predictmeans::predictmeans(model_Vals$model[[input$model_select]], 
                                      modelterm = input$select.mi.var, pairwise = TRUE, plot = FALSE)
      tab1 = makeSummaryTable_mm(ch)
      keepRows <- getFixedLevelComparisons(tab1[,1], sepChar = ":")
      tab1[keepRows, ]
    }
  })
})


## ui for comment box
output$comment.save.pht <- renderUI({
  if(input$select.mi.var != " " && !is.null(input$model_select) && length(input$model_select) > 0 
     && !is.null(input$select.mi.var)){  
      div(id = 'pht_doe_cca',
          checkboxInput(inputId = "comment_pht", label = "Add comments", value = FALSE),
          conditionalPanel(condition = "input.comment_pht == 1", 
                           column(12, textAreaInput(inputId = "comment_pht_text", label = "", value = "", resize = "both", width ="500px", height = "100px",
                                                    placeholder = "Box can be resized by dragging the arrow at bottom right"),
                                                    style = "margin-top: -25px")),
          actionButton("refresh_pht", label = "Save", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  }
})


## if the save button is clicked, code is saved
## the widgets are refreshed and a successful message is shown
observe({
  input$refresh_pht
  isolate({
    if(!is.null(input$refresh_pht) && input$refresh_pht > 0){
      reset("pht_doe_cca")
      save_success_msg$pht = T
      if (input$comment_pht == 1 && input$comment_pht_text != "" && 
          length(input$comment_pht_text) > 0) {
        ## provide succuss message
        output$success_msg_pht_doe1 <- renderUI({
          div(
            br(),
            strong("The table and comments are successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["pht"]] = c(code.save$model[[input$model_select]][["pht"]], 
                                                           "```{r}", paste0("ch", input$refresh_pht, " = ", sprintf("means2df(predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE))", input$model_select, input$select.mi.var)),
                                                           paste0("tab", input$refresh_pht, " = ", sprintf("makeSummaryTable(%s)", paste0("ch", input$refresh_pht))), 
                                                           paste0("keepRows", input$refresh_pht, " = ", sprintf("getFixedLevelComparisons(tab%s[,1], sepChar = \":\")", input$refresh_pht)),
                                                           paste0("tab", input$refresh_pht, "[keepRows", input$refresh_pht, ",]"), "```\n",
                                                           paste0(input$comment_pht_text, "\n"))
      } else if (input$comment_pht == 0 && input$comment_pht_text == ""){
        ## provide success message
        output$success_msg_pht_doe1 <- renderUI({
          div(
            br(),
            strong("The table is successfully saved"),
            br(),
            hr())
        })
        ## save code
        code.save$model[[input$model_select]][["pht"]] = c(code.save$model[[input$model_select]][["pht"]], 
                                                           "```{r}", paste0("ch", input$refresh_pht, " = ", sprintf("means2df(predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE))", input$model_select, input$select.mi.var)),
                                                           paste0("tab", input$refresh_pht, " = ", sprintf("makeSummaryTable(%s)", paste0("ch", input$refresh_pht))), 
                                                           paste0("keepRows", input$refresh_pht, " = ", sprintf("getFixedLevelComparisons(tab%s[,1], sepChar = \":\")", input$refresh_pht)),
                                                           paste0("tab", input$refresh_pht, "[keepRows", input$refresh_pht, ",]"), "```\n")
      }
    }
  })
})




###-----------------------###
###   Means and LSD plot  ###
###-----------------------###
output$select.mi.singlevar <- renderUI({
    input$model_select
    input$select.mi.var
    isolate({    
      if(!is.null(input$model_select) && !input$model_select %in% "" && input$select.mi.var != " "
        && !is.null(input$select.mi.var)){
        if(grepl(":", input$select.mi.var)){
        sel = unlist(strsplit(input$select.mi.var, "\\:"))
        } else {
          sel = input$select.mi.var
        }
       # sel = eval(parse(text = gsub('"', "",  sprintf("attributes(terms(~%s))$term.labels", input$select.mi.var))))
       # sel = attributes(terms(~input$select.mi.var))$term.labels[!grepl(":", attributes(terms(~input$select.mi.var))$term.labels)]
       # sel =  c(" ", attr(model_Vals$model[[input$model_select]]$terms, "term.labels"))
        tags$div(fluidRow(column(6, selectInput(inputId = "select.mi.LSDvar",
                                                label = "Select variable to be plotted on the x-axis",
                                                choices = c(" ", sel),
                                                selectize = F)),
                          column(6, selectInput(inputId = "select.mi.LSDvar.shape",
                                                label = "Select shape variable",
                                                choices = c(" ", sel),
                                                selectize = F))
                          ),
                 fluidRow(column(6, textInput(inputId = "select.mi.LSDvar.xlab",
                                              label = "Label of x-axis:",
                                              value = "")),
                          
                          column(6, textInput(inputId = "select.mi.LSDvar.ylab",
                                              label = "Label of y-axis:",
                                              value = ""))),
                 fluidRow(column(4, selectInput(inputId = "select.mi.LSDvar.facets",
                                                label = "Select facets variable",
                                                choices = c(" ", sel),
                                                selectize = F)),
                          column(4, selectInput(inputId = "select.mi.LSDvar.colour",
                                                label = "Select colour variable",
                                                choices = c(" ", sel),
                                                selectize = F)),
                          column(4, selectInput(inputId = "select.mi.LSDvar.lty",
                                                label = "Select lineType variable",
                                                choices = c(" ", sel),
                                                selectize = F))),
                 fluidRow(column(3, sliderInput("slider.mi.LSDvar.ptsize", label = "Point size", min = 0.1, 
                                                max = 25, value = 2.5)),
                          column(3, sliderInput("slider.mi.LSDvar.lwdsize", label = "Line width", min = 0.1, 
                                                max = 3, value = 0.9)),
                          column(3, sliderInput("slider.mi.LSDvar.axLSize", label = "Axis label size", min = 0.5, 
                                                max = 50, value = 14)),
                          column(3, sliderInput("slider.mi.LSDvar.tMLSize", label = "Tick label size", min = 0.5, 
                                                max = 50, value = 12))),
                 fluidRow(column(3, radioButtons("rab.mi.LSDvar.xAngle", label = "angle of x-axis tick",
                                                 choices = list("0" = 0, "45" = 45, "90" = 90), inline = T,  
                                                 selected = 0)),
                          column(3, sliderInput("slider.mi.LSDvar.xStripTS", label = "x Strip size", min = 0.5, 
                                                max = 50, value = 12)),
                          column(3, sliderInput("slider.mi.LSDvar.yStripTS", label = "y Strip size", min = 0.5, 
                                                max = 50, value = 12)),
                          column(3, sliderInput("slider.mi.LSDvar.lgdWd", label = "Legend key width", min = 0.1, 
                                                max = 5, value = 1)))
                 
                 )
        }
      })
})    

## as selectInput does not accept the NULL value for choices, so we transform them to reactive value which contains NULL

select.mi.LSDvar.shape <- reactive({
  if (input$select.mi.LSDvar.shape == " ")
    NULL
  else 
    input$select.mi.LSDvar.shape
})

select.mi.LSDvar.shape.code <- reactive({
  if (input$select.mi.LSDvar.shape == " ")
    "NULL"
  else 
    paste0("\"", input$select.mi.LSDvar.shape, "\"")
})

select.mi.LSDvar.colour <- reactive({
  if (input$select.mi.LSDvar.colour == " ")
    NULL
  else 
    input$select.mi.LSDvar.colour
})

select.mi.LSDvar.colour.code <- reactive({
  if (input$select.mi.LSDvar.colour == " ")
    "NULL"
  else 
    paste0("\"", input$select.mi.LSDvar.colour, "\"")
})

select.mi.LSDvar.lty <- reactive({
  if (input$select.mi.LSDvar.lty == " ")
    NULL
  else 
    input$select.mi.LSDvar.lty
})

select.mi.LSDvar.lty.code <- reactive({
  if (input$select.mi.LSDvar.lty == " ")
    "NULL"
  else 
    paste0("\"", input$select.mi.LSDvar.lty, "\"")
})


## means and LSD plot
output$mm_plot.mi_MeanLSD <- renderPlot({
  input$model_select
  input$select.mi.var
  input$select.mi.LSDvar
  input$select.mi.LSDvar.shape
  input$select.mi.LSDvar.colour
  input$select.mi.LSDvar.facets
  input$select.mi.LSDvar.lty
  input$select.mi.LSDvar.ylab
  input$select.mi.LSDvar.xlab
  input$slider.mi.LSDvar.ptsize
  input$slider.mi.LSDvar.lwdsize
  input$slider.mi.LSDvar.axLSize
  input$slider.mi.LSDvar.tMLSize
  input$rab.mi.LSDvar.xAngle
  input$slider.mi.LSDvar.xStripTS
  input$slider.mi.LSDvar.yStripTS
  input$slider.mi.LSDvar.lgdWd
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "" && input$select.mi.var != " "
       && !is.null(input$select.mi.var) && !is.null(input$select.mi.LSDvar) && input$select.mi.LSDvar != " ") {
      ch = predictmeans::predictmeans(model_Vals$model[[input$model_select]], 
                                      modelterm = input$select.mi.var, pairwise = TRUE, plot = FALSE)
      pm = meantable::means2df(ch)
      tab = makeSummaryTable_mm(ch)
      keepRows <- getFixedLevelComparisons(tab[,1], sepChar = ":")
      tab <- tab[keepRows, ]
      tab.list <- makeLSDlist(meansData = pm, y = "Mean", x = input$select.mi.LSDvar, 
                              LSD = mean(tab$LSD), 
                              edgeWidth = 0.05)
      if(!is.null(input$select.mi.LSDvar.facets) && input$select.mi.LSDvar.facets != " "){
        meansLSDplot(lsdList = tab.list, x = input$select.mi.LSDvar, y = "Mean", shape = select.mi.LSDvar.shape(),
                     colour = select.mi.LSDvar.colour(), facets = c(input$select.mi.LSDvar.facets, "."),
                     lineType = select.mi.LSDvar.lty(), yLabel = input$select.mi.LSDvar.ylab, xLabel = input$select.mi.LSDvar.xlab,
                     pointSize = input$slider.mi.LSDvar.ptsize, lineWidth = input$slider.mi.LSDvar.lwdsize, 
                     axisLabelSize = input$slider.mi.LSDvar.axLSize, tickMarkLabelSize = input$slider.mi.LSDvar.tMLSize,
                     xTickLabelAngle = as.numeric(input$rab.mi.LSDvar.xAngle), xStripTextSize = input$slider.mi.LSDvar.xStripTS,
                     yStripTextSize = input$slider.mi.LSDvar.yStripTS, legendKeyWidth = input$slider.mi.LSDvar.lgdWd)
      } else if(!is.null(input$select.mi.LSDvar.facets) && input$select.mi.LSDvar.facets == " "){
        meansLSDplot(lsdList = tab.list, x = input$select.mi.LSDvar, y = "Mean", shape = select.mi.LSDvar.shape(),
                     colour = select.mi.LSDvar.colour(),
                     lineType = select.mi.LSDvar.lty(), yLabel = input$select.mi.LSDvar.ylab, xLabel = input$select.mi.LSDvar.xlab,
                     pointSize = input$slider.mi.LSDvar.ptsize, lineWidth = input$slider.mi.LSDvar.lwdsize, 
                     axisLabelSize = input$slider.mi.LSDvar.axLSize, tickMarkLabelSize = input$slider.mi.LSDvar.tMLSize,
                     xTickLabelAngle = as.numeric(input$rab.mi.LSDvar.xAngle), xStripTextSize = input$slider.mi.LSDvar.xStripTS,
                     yStripTextSize = input$slider.mi.LSDvar.yStripTS, legendKeyWidth = input$slider.mi.LSDvar.lgdWd)
        }
    }
  })
})


## ui for comment box
output$comment.save.mLSD <- renderUI({
  if(input$select.mi.var != " " && !is.null(input$model_select) && length(input$model_select) > 0 
     && !is.null(input$select.mi.var) && !is.null(input$select.mi.LSDvar) && input$select.mi.LSDvar != " "){  
    div(id = 'meanLSD_doe_cca',
        checkboxInput(inputId = "comment_meanLSD", label = "Add comments", value = FALSE),
        conditionalPanel(condition = "input.comment_meanLSD == 1", 
                         column(12, textAreaInput(inputId = "comment_meanLSD_text", label = "", value = "", resize = "both", width ="500px", height = "100px",
                                                  placeholder = "Box can be resized by dragging the arrow at bottom right"),
                                style = "margin-top: -25px")),
        actionButton("refresh_meanLSD", label = "Save", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  }
})


## if the save button is clicked, code is saved
## the widgets are refreshed and a successful message is shown
observe({
  input$refresh_meanLSD
  isolate({
    if(!is.null(input$refresh_meanLSD) && input$refresh_meanLSD > 0){
      reset("meanLSD_doe_cca")
      save_success_msg$meanLSD = T
      if (input$comment_meanLSD == 1 && input$comment_meanLSD_text != "" && 
          length(input$comment_meanLSD_text) > 0) {
        ## provide succuss message
        output$success_msg_meanLSD_doe1 <- renderUI({
          div(
            br(),
            strong("The table and comments are successfully saved"),
            br(),
            hr())
        })
        ## save code
        if(!is.null(input$select.mi.LSDvar.facets) && input$select.mi.LSDvar.facets != " "){
          code.save$model[[input$model_select]][["meanLSD"]] = c(code.save$model[[input$model_select]][["meanLSD"]], 
                                                                 "```{r}", paste0("ch", input$refresh_meanLSD, " = ", sprintf("predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE)", input$model_select, input$select.mi.var)),
                                                                 paste0("tab", input$refresh_meanLSD, " = ", sprintf("makeSummaryTable(%s)", paste0("ch", input$refresh_meanLSD))), 
                                                                 paste0("keepRows", input$refresh_meanLSD, " = ", sprintf("getFixedLevelComparisons(tab%s[,1], sepChar = \":\")", input$refresh_meanLSD)),
                                                                 paste0("tab", input$refresh_meanLSD, " = ", "tab", input$refresh_meanLSD, "[keepRows", input$refresh_meanLSD, ",]"), 
                                                                 paste0("mse", input$refresh_meanLSD, " = ", sprintf("meantable::means2df(ch%s)", input$refresh_meanLSD)), 
                                                                 paste0("tab.list", input$refresh_meanLSD, " = ", sprintf("makeLSDlist(meansData = mse%s, y = \"Mean\",", input$refresh_meanLSD), " x = \"", input$select.mi.LSDvar,
                                                                        "\", LSD = mean(tab", input$refresh_meanLSD, "$LSD), edgeWidth = 0.05)"), 
                                                                 paste0("meansLSDplot(lsdList = tab.list", input$refresh_meanLSD,  ", x = \"", input$select.mi.LSDvar, "\", y = \"Mean\", 
                                                                        shape = ", select.mi.LSDvar.shape.code(), ", colour = ", select.mi.LSDvar.colour.code(), ", 
                                                                        lineType = ", select.mi.LSDvar.lty.code(), ", facet =  c(\"", input$select.mi.LSDvar.facets, "\", \".\"),
                                                                        lineWidth = ", input$slider.mi.LSDvar.lwdsize, ", axisLabelSize = ", input$slider.mi.LSDvar.axLSize, ", 
                                                                        tickMarkLabelSize = ", input$slider.mi.LSDvar.tMLSize, ", xTickLabelAngle = ", input$rab.mi.LSDvar.xAngle, ",
                                                                        xStripTextSize = ", input$slider.mi.LSDvar.xStripTS, ", yStripTextSize = ", input$slider.mi.LSDvar.yStripTS, ",
                                                                        legendKeyWidth = ", input$slider.mi.LSDvar.lgdWd, ")"
                                                                 ), "```\n",
                                                                 paste0(input$comment_meanLSD_text, "\n"))
        } else if(!is.null(input$select.mi.LSDvar.facets) && input$select.mi.LSDvar.facets == " "){
          code.save$model[[input$model_select]][["meanLSD"]] = c(code.save$model[[input$model_select]][["meanLSD"]], 
                                                                 "```{r}", paste0("ch", input$refresh_meanLSD, " = ", sprintf("predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE)", input$model_select, input$select.mi.var)),
                                                                 paste0("tab", input$refresh_meanLSD, " = ", sprintf("makeSummaryTable(%s)", paste0("ch", input$refresh_meanLSD))), 
                                                                 paste0("keepRows", input$refresh_meanLSD, " = ", sprintf("getFixedLevelComparisons(tab%s[,1], sepChar = \":\")", input$refresh_meanLSD)),
                                                                 paste0("tab", input$refresh_meanLSD, " = ", "tab", input$refresh_meanLSD, "[keepRows", input$refresh_meanLSD, ",]"), 
                                                                 paste0("mse", input$refresh_meanLSD, " = ", sprintf("meantable::means2df(ch%s)", input$refresh_meanLSD)), 
                                                                 paste0("tab.list", input$refresh_meanLSD, " = ", sprintf("makeLSDlist(meansData = mse%s, y = \"Mean\",", input$refresh_meanLSD), " x = \"", input$select.mi.LSDvar,
                                                                        "\", LSD = mean(tab", input$refresh_meanLSD, "$LSD), edgeWidth = 0.05)"), 
                                                                 paste0("meansLSDplot(lsdList = tab.list", input$refresh_meanLSD,  ", x = \"", input$select.mi.LSDvar, "\", y = \"Mean\", 
                                                                        shape = ", select.mi.LSDvar.shape.code(), ", colour = ", select.mi.LSDvar.colour.code(), ", 
                                                                        lineType = ", select.mi.LSDvar.lty.code(), ", 
                                                                        lineWidth = ", input$slider.mi.LSDvar.lwdsize, ", axisLabelSize = ", input$slider.mi.LSDvar.axLSize, ", 
                                                                        tickMarkLabelSize = ", input$slider.mi.LSDvar.tMLSize, ", xTickLabelAngle = ", input$rab.mi.LSDvar.xAngle, ",
                                                                        xStripTextSize = ", input$slider.mi.LSDvar.xStripTS, ", yStripTextSize = ", input$slider.mi.LSDvar.yStripTS, ",
                                                                        legendKeyWidth = ", input$slider.mi.LSDvar.lgdWd, ")"
                                                                 ), "```\n",
                                                                 paste0(input$comment_meanLSD_text, "\n"))
            
          }
      } else if (input$comment_meanLSD == 0 && input$comment_meanLSD_text == ""){
        ## provide success message
        output$success_msg_meanLSD_doe1 <- renderUI({
          div(
            br(),
            strong("The table is successfully saved"),
            br(),
            hr())
        })
        ## save code
        if(!is.null(input$select.mi.LSDvar.facets) && input$select.mi.LSDvar.facets != " "){
          code.save$model[[input$model_select]][["meanLSD"]] = c(code.save$model[[input$model_select]][["meanLSD"]], 
                                                                 "```{r}", paste0("ch", input$refresh_meanLSD, " = ", sprintf("predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE)", input$model_select, input$select.mi.var)),
                                                                 paste0("tab", input$refresh_meanLSD, " = ", sprintf("makeSummaryTable(%s)", paste0("ch", input$refresh_meanLSD))), 
                                                                 paste0("keepRows", input$refresh_meanLSD, " = ", sprintf("getFixedLevelComparisons(tab%s[,1], sepChar = \":\")", input$refresh_meanLSD)),
                                                                 paste0("tab", input$refresh_meanLSD, " = ", "tab", input$refresh_meanLSD, "[keepRows", input$refresh_meanLSD, ",]"), 
                                                                 paste0("mse", input$refresh_meanLSD, " = ", sprintf("meantable::means2df(ch%s)", input$refresh_meanLSD)), 
                                                                 paste0("tab.list", input$refresh_meanLSD, " = ", sprintf("makeLSDlist(meansData = mse%s, y = \"Mean\",", input$refresh_meanLSD), " x = \"", input$select.mi.LSDvar,
                                                                        "\", LSD = mean(tab", input$refresh_meanLSD, "$LSD), edgeWidth = 0.05)"), 
                                                                 paste0("meansLSDplot(lsdList = tab.list", input$refresh_meanLSD,  ", x = \"", input$select.mi.LSDvar, "\", y = \"Mean\", 
                                                                        shape = ", select.mi.LSDvar.shape.code(), ", colour = ", select.mi.LSDvar.colour.code(), ", 
                                                                        lineType = ", select.mi.LSDvar.lty.code(), ", facet =  c(\"", input$select.mi.LSDvar.facets, "\", \".\"),
                                                                        lineWidth = ", input$slider.mi.LSDvar.lwdsize, ", axisLabelSize = ", input$slider.mi.LSDvar.axLSize, ", 
                                                                        tickMarkLabelSize = ", input$slider.mi.LSDvar.tMLSize, ", xTickLabelAngle = ", input$rab.mi.LSDvar.xAngle, ",
                                                                        xStripTextSize = ", input$slider.mi.LSDvar.xStripTS, ", yStripTextSize = ", input$slider.mi.LSDvar.yStripTS, ",
                                                                        legendKeyWidth = ", input$slider.mi.LSDvar.lgdWd, ")"
                                                                 ), "```\n")
        } else if(!is.null(input$select.mi.LSDvar.facets) && input$select.mi.LSDvar.facets == " "){
            code.save$model[[input$model_select]][["meanLSD"]] = c(code.save$model[[input$model_select]][["meanLSD"]], 
                                                                   "```{r}", paste0("ch", input$refresh_meanLSD, " = ", sprintf("predictmeans(%s, modelterm = \"%s\", pairwise = TRUE, plot = FALSE)", input$model_select, input$select.mi.var)),
                                                                   paste0("tab", input$refresh_meanLSD, " = ", sprintf("makeSummaryTable(%s)", paste0("ch", input$refresh_meanLSD))), 
                                                                   paste0("keepRows", input$refresh_meanLSD, " = ", sprintf("getFixedLevelComparisons(tab%s[,1], sepChar = \":\")", input$refresh_meanLSD)),
                                                                   paste0("tab", input$refresh_meanLSD, " = ", "tab", input$refresh_meanLSD, "[keepRows", input$refresh_meanLSD, ",]"), 
                                                                   paste0("mse", input$refresh_meanLSD, " = ", sprintf("meantable::means2df(ch%s)", input$refresh_meanLSD)), 
                                                                   paste0("tab.list", input$refresh_meanLSD, " = ", sprintf("makeLSDlist(meansData = mse%s, y = \"Mean\",", input$refresh_meanLSD), " x = \"", input$select.mi.LSDvar,
                                                                          "\", LSD = mean(tab", input$refresh_meanLSD, "$LSD), edgeWidth = 0.05)"), 
                                                                   paste0("meansLSDplot(lsdList = tab.list", input$refresh_meanLSD,  ", x = \"", input$select.mi.LSDvar, "\", y = \"Mean\", 
                                                                          shape = ", select.mi.LSDvar.shape.code(), ", colour = ", select.mi.LSDvar.colour.code(), ", 
                                                                          lineType = ", select.mi.LSDvar.lty.code(), ", 
                                                                          lineWidth = ", input$slider.mi.LSDvar.lwdsize, ", axisLabelSize = ", input$slider.mi.LSDvar.axLSize, ", 
                                                                          tickMarkLabelSize = ", input$slider.mi.LSDvar.tMLSize, ", xTickLabelAngle = ", input$rab.mi.LSDvar.xAngle, ",
                                                                          xStripTextSize = ", input$slider.mi.LSDvar.xStripTS, ", yStripTextSize = ", input$slider.mi.LSDvar.yStripTS, ",
                                                                          legendKeyWidth = ", input$slider.mi.LSDvar.lgdWd, ")"
                                                                   ), "```\n")
          }
      }
    }
  })
})



###-----------------------------------###
###       Hide success message        ###
###-----------------------------------###


## if fit button is clicked or we change the model, then we are going to hide the success message

observe({
  input$model_select
  input$mm.tabs
  isolate({
    if(!is.null(input$model_select) && !input$model_select %in% "") {
      save_success_msg$aov = F
      save_success_msg$resip = F
      save_success_msg$tabmi = F
      save_success_msg$pht = F
      save_success_msg$meanLSD = F
    }
  })
})

observe({
  toggle(id = "success_msg_aov_doe1", condition = save_success_msg$aov)
  toggle(id = "success_msg_resip_doe1", condition = save_success_msg$resip)
  toggle(id = "success_msg_tabmi_doe1", condition = save_success_msg$tabmi)
  toggle(id = "success_msg_pht_doe1", condition = save_success_msg$pht)
  toggle(id = "success_msg_meanLSD_doe1", condition = save_success_msg$meanLSD)
})

###---------------------------------------###
###           Generate report             ###
###---------------------------------------###


code.list <- reactive({
  ## code for model
  #save_list = lapply(code.save$model, 
  #                   function(x) if(length(x) == 3){
  #                     paste0(Rchunk(paste0(x[c(1,2)], collapse = "\n")),
  #                         "\n", x[3], "\n")} else{
  #                              paste0(Rchunk(paste0(x[c(1,2)], collapse = "\n")), "\n")
  #                            })
  
  save_list = paste0(do.call(c, do.call(c, code.save$model)), collapse = "\n")
  c(rmd_YAML(input$name_rmd, input$title_rmd, input$format),
    Rchunk(c("## set working directory\nsetwd(\"~/Documents\")\n## read data\nmydata <- read.csv(\"Plant Vigour.csv\", header = T)")),
    Rchunk(code.save$variable), #the first is null
    save_list)
})


code.list1 <- reactive({
  ## code for model
  #save_list = lapply(code.save$model, 
  #                   function(x) if(length(x) == 3){
  #                     paste0(Rchunk(paste0(x[c(1,2)], collapse = "\n")),
  #                         "\n", x[3], "\n")} else{
  #                              paste0(Rchunk(paste0(x[c(1,2)], collapse = "\n")), "\n")
  #                            })
  
  save_list = paste0(do.call(c, do.call(c, code.save$model)), collapse = "\n")
  c(rmd_YAML(input$name_rmd, input$title_rmd, input$format),
    Rchunk("mydata <- read.csv(\"mydata.csv\", header = T)"),
    Rchunk(code.save$variable), #the first is null
    save_list)
})



output$report <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, pdf_document = 'pdf', html_document = 'html', word_document = 'docx'
    ))
  },
  
  content = function(file) {
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    write.csv(data.ori$data, file = 'mydata.csv')
    write(code.list1(), 'report.Rmd')
    library(rmarkdown)
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)


output$rmd <- downloadHandler(
  filename = function() {
    "my-report.rmd"
  },
  
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    write(code.list(), 'report.Rmd')
    file.rename('report.Rmd', file)
  }
)


output$report.pre <- renderUI({
  owd <- setwd(tempdir())
  write.csv(data.ori$data, file = 'mydata.csv')
  write(code.list1(), 'report.Rmd')
  div(tags$head(tags$style(HTML("
                           body {
                            width: 100% !important;
                            max-width: 100% !important;
                            }
                            "))),
      HTML(markdown::markdownToHTML(knitr::knit('report.Rmd', quiet = FALSE))))
})



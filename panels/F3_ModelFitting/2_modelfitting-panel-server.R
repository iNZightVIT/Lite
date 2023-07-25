###---------------------------------------------------###
###  Server Functions for the "Model Fitting" Module  ###
###---------------------------------------------------###
###
###  Date Created   :   June 23, 2015
###
###
###  * Note: This is to be sourced within "server.R" *

# reactive values to store the fitted models
modelValues = reactiveValues(models=list(),
                             code=list(),
                             code.history="# To make this code work outside of iNZight, read in your data first\n# In iNZight Lite, this has been done\ for you, just run the\n# following code in your R console:\nlibrary(iNZightRegression)\nlibrary(GGally)\nlibrary(survey) # only needed if a complex survey model was fitted\n",
                             transformation.log=c(),
                             interaction.log=list(),
                             independent.vars=list())

output$modelfitting.panel <- renderUI({
  get.data.set()
  isolate({
    model.fitting.panel.ui(get.data.set())
  })
})

# print the summary of the fitted model.
output$fit.summary = renderPrint({
  input$model.select
  isolate({
    if(length(modelValues$models)>0&&
       (!is.null(modelValues$models[[input$model.select]])&&
        !is.null(input$model.select))){
      confounds = input$confounding_variables

      # filter counfounds to those in names of get.data.set()
      confounds <- confounds[confounds %in% names(get.data.set())]

      op <- options(width = 200)
      on.exit(options(op))
      smry <- try(
        capture.output(
          iNZightSummary(
            modelValues$models[[input$model.select]],
            exclude = if (length(confounds)) confounds else NULL
          )
        ),
        silent = TRUE
      )
      if (inherits(smry, "try-error")) {
        smry <- try(
          capture.output(summary(modelValues$models[[input$model.select]])),
          silent = TRUE
        )
      }
      if (inherits(smry, "try-error"))
        smry <- "Unable to obtain summary information for model."

      cat(smry, sep = "\n")
    }else{
      cat("No model to show")
    }
  })
})

# panel for fitting a model
output$model_fit = renderUI({
  input$select_Y
  isolate({
    is.numeric.column = NULL
    sel = NULL
    if(!is.null(input$select_Y)&&
       input$select_Y%in%colnames(get.data.set())){
      sel = input$select_Y
    }
    predict.sel = input$independent_variables
    confound.sel = input$confounding_variables
    if(updatePanel$first){
      #       updatePanel$first = F
      get.vars = parseQueryString(session$clientData$url_search)
      if(!is.null(get.vars$url)) {
        temp = session$clientData$url_search
        get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
      }
      if(length(get.vars)>0&&
         (any(names(get.vars)%in%"url")||
          any(names(get.vars)%in%"example"))&&
         (any(names(get.vars)%in%"Y")&&
          !get.vars$Y%in%"")){
        sel = get.vars$Y
      }
    }
    select_Y = selectInput("select_Y",
                           label="Select Y variable",
                           choices=colnames(get.data.set()),
                           selected=sel)
    if(updatePanel$first){
      if(length(get.vars)>0&&
         (any(names(get.vars)%in%"url")||
          any(names(get.vars)%in%"example"))&&
         (any(names(get.vars)%in%"predict")&&
          !get.vars$predict%in%"")){
        predict.sel = strsplit(get.vars$predict,",")[[1]]
      }
    }
    if(updatePanel$first){
      if(length(get.vars)>0&&
         (any(names(get.vars)%in%"url")||
          any(names(get.vars)%in%"example"))&&
         (any(names(get.vars)%in%"confound")&&
          !get.vars$confound%in%"")){
        confound.sel = strsplit(get.vars$confound,",")[[1]]
      }
    }
    if(!is.null(input$select_Y)){
      updatePanel$first = F
    }
    #if(!is.null(sel)&&
    #   !sel%in%""&&
    #   sel%in%colnames(get.data.set())){
    #  is.numeric.column = toJSON(get.data.set()[,sel])
    #}
    list(br(),
         fixedRow(column(11,h4("Choose Model Settings")),
                  column(1,checkboxInput("toggle_check4",
                                         label="",
                                         value=T))),
         conditionalPanel("input.toggle_check4",
                          fixedRow(column(6,select_Y),
                                   column(6, uiOutput("mf.trans.y"))),
                          fixedRow(column(6,selectInput("model_framework",label="Model Framework",
                                                        choices=c("Least Squares",
                                                                  "Logistic Regression (Y binary)",
                                                                  "Poisson Regression (Y counts)"),
                                                        selected=input$model_framework))
                          ),
                          conditionalPanel("input.model_framework=='Logistic Regression (Y binary)'||
                                           input.model_framework=='Poisson Regression (Y counts)'",
                                           h4("Extra Arguments"),
                                           fixedRow(column(6,selectInput("quasi",
                                                                         label="quasi",
                                                                         choices=c(no=F,
                                                                                   yes=T),
                                                                         selected=input$quasi)),
                                                    column(6,textInput("offset",label="offset")))
                          )
         ),
         fixedRow(column(11,h4("Choose predictor Variables")),
                  column(1,checkboxInput("toggle_check3",
                                         label="",
                                         value=T))),
         conditionalPanel("input.toggle_check3",
                          fixedRow(column(6,selectInput("independent_variables",
                                                        label="Variables of interest",
                                                        choices=colnames(get.data.set())[-which(colnames(get.data.set())%in%sel)],
                                                        selected=predict.sel,
                                                        selectize=T,
                                                        multiple=T)),
                                   column(6,selectInput("confounding_variables",
                                                        label="Confounder Variables",
                                                        choices=colnames(get.data.set())[-which(colnames(get.data.set())%in%sel)],
                                                        selected=confound.sel,
                                                        selectize=T,
                                                        multiple=T))),
                          checkboxInput("modelfit_inc_int", label = "Include intercept", value = T)),
         fixedRow(column(11,h4("Add Interactions")),
                  column(1,checkboxInput("toggle_check1",
                                         label="",
                                         value=input$toggle_check1))),
         conditionalPanel("input.toggle_check1",
                          fixedRow(column(4,selectInput("intersaction_select",
                                                        label="Interaction",
                                                        choices=c("none","all","by degree",
                                                                  "by variables"),
                                                        selected="none")),
                                   conditionalPanel("input.intersaction_select=='by degree'",
                                                    column(5,offset=3,
                                                           textInput("arg2",label="degree"))),
                                   conditionalPanel("input.intersaction_select=='by variables'",
                                                    column(5,offset=3,
                                                           selectInput("interaction.vars.select",
                                                                       label="Select interaction",
                                                                       choices=c(input$independent_variables,
                                                                                 input$confounding_variables),
                                                                       multiple=T)))),
                          conditionalPanel("input.intersaction_select=='by variables'",
                                           fixedRow(column(3,
                                                           actionButton("submit.interaction",
                                                                        label="Submit")))),
                          conditionalPanel("input.intersaction_select=='by variables'",
                                           fixedRow(column(7,h5("Remove interaction")),
                                                    column(5,
                                                           selectInput("interaction_remove",
                                                                       label="",
                                                                       choices="none",
                                                                       selected=input$transformation_remove))))),
         fixedRow(column(11,h4("Transform x-variables")),
                  column(1,checkboxInput("toggle_check2",
                                         label="",
                                         value=input$toggle_check2))),
         conditionalPanel("input.toggle_check2",
                          fixedRow(column(4,selectInput("transform_select",
                                                        label="Transform",
                                                        choices=c("none","log","sqrt",
                                                                  "by degree",
                                                                  "polynomial of degree"),
                                                        selected="none")),
                                   column(4,
                                          conditionalPanel("input.transform_select!='none'",
                                                           selectInput("transform_variable_select",
                                                                       label="Variable",
                                                                       choices=c(" ",
                                                                                 input$independent_variables),
                                                                       selected=" "))),
                                   column(2,
                                          conditionalPanel("input.transform_select=='by degree'||
                                                           input.transform_select=='polynomial of degree'",
                                                           textInput("arg3",label="degree",
                                                                     value=input$arg3)))),
                          fixedRow(column(3,
                                          conditionalPanel("input.transform_select!='none'",
                                                           actionButton("transformation_submit",
                                                                        label="Submit")))),
                          br(),
                          fixedRow(column(8,h5("Remove transformation")),
                                   column(4,
                                          conditionalPanel("input.transform_select!='none'",
                                                           selectInput("transformation_remove",
                                                                       label="",
                                                                       choices="none",
                                                                       selected=input$transformation_remove))))),
         fixedRow(column(11,h4("Display model formula")),
                  column(1,checkboxInput("toggle_check5",
                                         label="",
                                         value=input$toggle_check5))),
         conditionalPanel("input.toggle_check5",
                          helpText("The code used for the next model fit. Note,
                                   this code might not be executable."),
                          verbatimTextOutput("current.code")),
         br(),
         actionButton("fit_model_button","Fit Model")
    )
  })
})

output$mf.trans.y <- renderUI({
  ret = NULL
  input$select_Y
  isolate({
    if(!is.null(input$select_Y)){
      num.y = is.numeric(get.data.set()[[input$select_Y]])
      if(num.y) {
        ret = list(fixedRow(column(6,
                                   selectInput("transform_Y",
                                               label="Transform Y",
                                               choices=c('None',
                                                         "log",
                                                         "sqrt",
                                                         "^argument"),
                                               selected=ifelse(is.null(input$transform_Y), 'None', input$transform_Y))),
                            column(6,conditionalPanel("input.transform_Y=='^argument'",
                                                      textInput("arg1",
                                                                label="argument",
                                                                value=input$arg1)))))
      }
    }
  })
  ret
})




# update the numericInput for the degree of selected variables
observe({
  input$interaction.vars.select
  isolate({
    max=2
    if(length(input$interaction.vars.select)>2){
      max = length(input$interaction.vars.select)
    }
    updateNumericInput(session,"degree.interaction.numeric",
                       max = max)
  })
})

# submit a custom interaction
observe({
  input$submit.interaction
  isolate({
    if(!is.null(input$submit.interaction)&&
       input$submit.interaction>0){
      if(!is.null(input$interaction.vars.select)&&
         !is.null(input$intersaction_select)&&
         !input$intersaction_select%in%"none"&&
         all(unlist(lapply(input$interaction.vars.select,
                           function(x,y,z){
                             x%in%z||(x%in%names(y)&&
                                      y[x]%in%z)
                           },modelValues$transformation.log,
                           colnames(get.data.set()))))&&
         length(input$interaction.vars.select)>1){
        dup = F
        if(length(modelValues$interaction.log)>0){
          dup = any(unlist(lapply(modelValues$interaction.log,function(x,y){
            length(which(y%in%x))==length(x)
          },input$interaction.vars.select)))
        }
        if(!dup){
          interaction.string = paste(input$interaction.vars.select,collapse=":")
          modelValues$interaction.log[[interaction.string]] = input$interaction.vars.select
          updateSelectInput(session,"interaction_remove",
                            choices=c("none",names(modelValues$interaction.log)))
        }
      }
    }
  })
})

observe({
  input$interaction_remove
  isolate({
    if(!is.null(input$interaction_remove)&&
       input$interaction_remove%in%names(modelValues$interaction.log)){
      modelValues$interaction.log[[input$interaction_remove]] = NULL
      updateSelectInput(session,"interaction_remove",
                        choices=c("none",names(modelValues$interaction.log)))
    }
  })
})

# submit a transformation of a variable
observe({
  input$transformation_submit
  isolate({
    if(!is.null(input$transformation_submit)&&
       input$transformation_submit>0){
      if(!is.null(input$transform_select)&&
         !input$transform_select%in%"none"&&
         !is.null(input$transform_variable_select)&&
         input$transform_variable_select%in%colnames(get.data.set())){
        transformation.string = get.transformation.string(input$transform_select,
                                                          input$transform_variable_select,
                                                          input$arg3)
        if(!transformation.string%in%""&&
           !transformation.string%in%names(modelValues$transformation.log)){
          modelValues$transformation.log[transformation.string] = input$transform_variable_select
          updateSelectInput(session,"transformation_remove",
                            choices=c("none",names(modelValues$transformation.log)),
                            selected="none")
          ch = c(input$independent_variables,input$confounding_variables)
          ch[which(ch%in%modelValues$transformation.log)] =
            names(modelValues$transformation.log)[which(modelValues$transformation.log%in%ch)]
          updateSelectInput(session,"interaction.vars.select",
                            choices=ch)
        }
      }
    }
  })
})

# delete transform variables if the
# independent variable is removed
# from model
observe({
  input$independent_variables
  input$confounding_variables
  isolate({
    updateSelectInput(session,"interaction.vars.select",
                      choices=c(" ",
                                input$independent_variables,
                                input$confounding_variables,
                                selected=input$interaction.vars.select))
    if((!is.null(input$independent_variables)||
        !is.null(input$confounding_variables))&&
       !is.null(modelValues$transformation.log)){
      if(length(which(!modelValues$transformation.log%in%input$independent_variables||
                      !modelValues$transformation.log%in%input$confounding_variables))>0){
        modelValues$transformation.log = modelValues$transformation.log[-which(!modelValues$transformation.log%in%input$independent_variables||
                                                                                 !modelValues$transformation.log%in%input$confounding_variables)]
        updateSelectInput(session,"transformation_remove",
                          choices=c("none",names(modelValues$transformation.log)),
                          selected="none")
        ch = c(input$independent_variables,input$confounding_variables)
        ch[which(ch%in%modelValues$transformation.log)] =
          names(modelValues$transformation.log)[which(modelValues$transformation.log%in%ch)]
        updateSelectInput(session,"interaction.vars.select",
                          choices=ch)
      }
    }
  })
})

# remove a transformation
observe({
  input$transformation_remove
  isolate({
    if(!is.null(input$transformation_remove)&&
       !is.null(names(modelValues$transformation.log))&&
       input$transformation_remove%in%names(modelValues$transformation.log)){
      temp = modelValues$transformation.log
      temp = temp[-which(names(modelValues$transformation.log)%in%input$transformation_remove)]
      modelValues$transformation.log = temp
      updateSelectInput(session,"transformation_remove",
                        choices=c("none",names(modelValues$transformation.log)),
                        selected="none")
      ch = c(input$independent_variables,input$confounding_variables)
      ch[which(ch%in%modelValues$transformation.log)] =
        names(modelValues$transformation.log)[which(modelValues$transformation.log%in%ch)]
      updateSelectInput(session,"interaction.vars.select",
                        choices=ch)
    }
  })
})

# update the variable selector for transformation
observe({
  input$independent_variables
  input$confounding_variables
  isolate({
    ch = c(input$independent_variables,
           input$confounding_variables)
    if(!is.null(ch)){
      ch = ch[ch%in%get.numeric.column.names(get.data.set())]
      updateSelectInput(session,"transform_variable_select",
                        choices = c(" ",ch),
                        selected=input$transform_variable_select)
    }
  })
})

# check whether arg2 (degree of transformation/interaction)
# has numeric input
observe({
  input$arg2
  isolate({
    if(!is.null(input$arg2)&&suppressWarnings(is.na(as.numeric(input$arg2)))){
      updateTextInput(session,"arg2",
                      value="")
    }
  })
})

# Submit the current model
observe({
  input$fit_model_button
  isolate({
    is_survey <- FALSE
    if (!is.null(design_params$design$dataDesign)) {
      design.obj <- createSurveyObject()
      is_survey <- TRUE
    }
    model.name = ""
    if(!is.null(input$fit_model_button)&&
       input$fit_model_button>0){
      temp.model = NULL
      temp.code = NULL
      tryCatch({
        model.name = "model_"
        if(length(modelValues$models)>0){
          numbers = strsplit(names(modelValues$models),"_",fixed=T)
          max = 1
          for(m.name in numbers){
            if(length(m.name)==2&&
               !is.na(suppressWarnings(as.numeric(m.name[2])))&&
               suppressWarnings(as.numeric(m.name[2]))>max){
              max = suppressWarnings(as.numeric(m.name[2]))
            }
          }
          max = max + 1
          model.name = paste0(model.name,max)
        }else{
          model.name = paste0(model.name,1)
        }
        formu = ""
        col = " + "
        int.deg = F
        int.specific = F
        # interaction is used
        if(!is.null(input$intersaction_select)&&
           input$intersaction_select%in%"all"){
          col  = " * "
        }else if(!is.null(input$intersaction_select)&&
                 input$intersaction_select%in%"by degree"){
          int.deg = T
        }else if(!is.null(input$intersaction_select)&&
                 input$intersaction_select%in%"by variables"&&
                 length(modelValues$interaction.log)>0){
          int.specific = T
        }
        if(!is.null(input$transform_Y) && input$transform_Y != "None") {
          if(input$transform_Y%in%"log"){
            formu = paste("log(",input$select_Y,") ~ ",
                          paste(input$independent_variables
                                ,collapse=col),sep="")
            if(length(input$confounding_variables)>0){
              formu = paste("log(",input$select_Y,") ~ ",
                            paste(input$independent_variables
                                  ,collapse=col),col,
                            paste(input$confounding_variables,
                                  collapse=col),sep="")
            }
          } else if(input$transform_Y%in%"sqrt"){
            formu = paste("sqrt(",input$select_Y,") ~ ",
                          paste(input$independent_variables
                                ,collapse=col),sep="")
            if(length(input$confounding_variables)>0){
              formu = paste("sqrt(",input$select_Y,") ~ ",
                            paste(input$independent_variables
                                  ,collapse=col),col,
                            paste(input$confounding_variables,
                                  collapse=col),sep="")
            }
          } else if(input$transform_Y%in%"^argument"&&
                    !input$arg1%in%""){
            formu = paste(input$select_Y,"^",input$arg1," ~ ",
                          paste(input$independent_variables
                                ,collapse=col),sep="")
            if(length(input$confounding_variables)>0){
              formu = paste(input$select_Y,"^",input$arg1," ~ ",
                            paste(input$independent_variables
                                  ,collapse=col),col,
                            paste(input$confounding_variables,
                                  collapse=col),sep="")
            }
          }
        } else {
          formu = paste(input$select_Y," ~ ",
                        paste(input$independent_variables
                              ,collapse=col),sep="")
          if(length(input$confounding_variables)>0){
            formu = paste(input$select_Y," ~ ",
                          paste(input$independent_variables
                                ,collapse=col),col,
                          paste(input$confounding_variables,
                                collapse=col),sep="")
          }
        }

        #if(length(input$confounding_variables)>0){
        #  formu = paste(input$select_Y," ~ ",
        #                paste(input$independent_variables
        #                      ,collapse=col),col,
        #                paste(input$confounding_variables,
        #                      collapse=col),sep="")
        #}else{
        #  formu = paste(input$select_Y," ~ ",
        #                paste(input$independent_variables
        #                      ,collapse=col),sep="")
        #}
        if(int.deg){
          formu = strsplit(formu," ~ ")[[1]]
          formu[2] = paste0("(",formu[2],")^",input$arg2)
          formu = paste(formu,collapse=" ~ ")
        }
        for(trans in names(modelValues$transformation.log)){
          if(!grepl(trans,formu,fixed=T)){
            formu = strsplit(formu,modelValues$transformation.log[trans])[[1]]
            if(length(formu)==1){
              formu = paste0(formu,trans)
            }else if(length(formu)==2){
              formu = paste0(formu,collapse=trans)
            }
          }
        }
        if(int.specific){
          if(any(unlist(lapply(modelValues$interaction.log,
                               function(x,y){
                                 any(x%in%y)
                               },
                               modelValues$transformation.log)))){
            new.interaction.log = lapply(modelValues$interaction.log,
                                         function(x,y){
                                           x[which(x%in%y)] = names(y)[which(y%in%x)]
                                         },modelValues$transformation.log)
            names(new.interaction.log) = unlist(lapply(new.interaction.log,function(x){
              paste(x,collapse=":")
            }))
            formu = paste(formu,paste(names(new.interaction.log),collapse=" + "),sep=" + ")
          }else{
            formu = paste(formu,paste(names(modelValues$interaction.log),collapse=" + "),sep=" + ")
          }
        }
        if(input$modelfit_inc_int != T){
          formu = paste0(formu, " - 1")
        }
        #  print(formu)
        dafr = get.data.set()
        if(is_survey){
          design0 = design.obj
          design.text = design.model.fit$code
          family0="gaussian"
          offset0=NULL
          if(!is.null(input$offset)&&
             !input$offset%in%""){
            offset0=input$offset
          }
          if(input$model_framework%in%"Poisson Regression (Y counts)"){
            if(input$quasi){
              family0 = "quasipoisson"
            }else{
              family0 = "poisson"
            }
          }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
            if(input$quasi){
              family0="quasibinomial"
            }else{
              family0="binomial"
            }
          }
          if(family0%in%"gaussian"){
            temp.model = svyglm(formula(formu),design=design0)
            temp.code = paste0(design.text,"\n",model.name,
                               " = svyglm(",formu,", design = ", design_params$design$dataDesignName,")")
          }else{
            temp.model = svyglm(formula(formu),design=design0,family=family0,offset=offset0)
            temp.code = paste0(design.text,"\n", model.name,
                               " = svyglm(",formu)
            if(!is.null(offset0)){
              temp.code = paste0(temp.code,", offset=", offset0)
            }
            temp.code = paste0(temp.code,", family = '", family0, "'")
            temp.code = paste0(temp.code,", design = ", design_params$design$dataDesignName,")")
          }
        }else{
          if(input$model_framework%in%"Least Squares"){
            temp.model = lm(formula(formu),data=dafr)
            temp.code = paste0(model.name," = lm(",formu,",data=", values$data.name, ")")
          }else if(input$model_framework%in%"Poisson Regression (Y counts)"){
            if(input$quasi){
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=dafr,family='quasipoisson')
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='quasipoisson')")
              }else{
                temp.model = glm(formula(formu),data=dafr,family='quasipoisson',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='quasipoisson',offset=",input$offset,")")
              }
            }else{
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=dafr,family='poisson')
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='poisson'")
              }else{
                temp.model = glm(formula(formu),data=dafr,family='poisson',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='poisson',offset=",input$offset,")")
              }
            }
          }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
            if(input$quasi){
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=dafr,family='quasibinomial')
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='quasibinomial')")
              }else{
                temp.model = glm(formula(formu),data=dafr,family='quasibinomial',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='quasibinomial',offset=",input$offset,")")
              }
            }else{
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=dafr,family='binomial')
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='binomial')")
              }else{
                temp.model = glm(formula(formu),data=dafr,family='binomial',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=", values$data.name, ",family='binomial',offset=",input$offset,")")
              }
            }
          }
        }
      }, error = function(e) {
        print(e)
      }, finally = {})
      modelValues$interaction.log = list()
      modelValues$transformation.log = c()
      if(!is.null(temp.model)){
        modelValues$models[[model.name]] = temp.model
        updateSelectInput(session,"model.select",
                          choices=names(modelValues$models),
                          selected=model.name)
        modelValues$code[[model.name]] = temp.code
        modelValues$independent.vars[[model.name]] = c(input$independent_variables,
                                                       input$confounding_variables)
        confounds = input$confounding_variables
        if(" "%in%confounds){
          confounds = confounds[-which(confounds%in%" ")]
        }
        if(!is.null(confounds)&&
           length(confounds)>0&&
           all(confounds%in%colnames(get.data.set()))){
          modelValues$code.history = paste0(modelValues$code.history,
                                            temp.code,
                                            "\niNZightSummary(",
                                            model.name,",exclude=c(",
                                            paste(confounds,collapse=","),"))\n")
        }else{
          modelValues$code.history = paste0(modelValues$code.history,
                                            temp.code,
                                            "\niNZightSummary(",
                                            model.name,")\n")
        }
      }
    }
  })
})

# Remove the posibility to select a variable in
# confounders and independent variables. This
# checks the independent variables.
observe({
  input$independent_variables
  input$select_Y
  isolate({
    independent_variables = input$independent_variables
    ch = colnames(get.data.set())[-which(colnames(get.data.set())%in%input$select_Y)]
    if(length(independent_variables)>0){
      ch = ch[-which(ch%in%independent_variables)]
    }
    updateSelectInput(session,"confounding_variables",
                      choices=c(" ",ch),
                      selected=input$confounding_variables)
  })
})

# Remove the posibility to select a variable in
# confounders and independent variables. This
# checks the confounding variables.
observe({
  input$confounding_variables
  input$select_Y
  isolate({
    confounding_variables = input$confounding_variables
    ch = colnames(get.data.set())[-which(colnames(get.data.set())%in%input$select_Y)]
    if(length(confounding_variables)>0){
      ch = ch[-which(ch%in%confounding_variables)]
    }
    updateSelectInput(session,"independent_variables",
                      choices=c(" ",ch),
                      selected=input$independent_variables)
  })
})

# Test whether the selected Y variable is
# numeric, binary or count data and set
# the model framework accordingly
observe({
  input$select_Y
  isolate({
    if(!is.null(input$select_Y)&&
       input$select_Y%in%colnames(get.data.set())&&
       !input$select_Y%in%""){
      if(is.numeric(get.data.set()[,input$select_Y])||
         length(levels(as.factor(na.omit(get.data.set()[,input$select_Y]))))!=2){
        updateSelectInput(session,"model_framework",
                          selected="Least Squares")
      }else{
        if(length(levels(as.factor(na.omit(get.data.set()[,input$select_Y]))))==2){
          updateSelectInput(session,"model_framework",
                            selected="Logistic Regression (Y binary)")
        }
      }
    }
  })
})

# Test whether input in the numeric text field for
# transforming Y is numeric
observe({
  input$arg1
  isolate({
    if(!is.null(input$arg1)&&
       !is.convertable.numeric(input$arg1)){
      updateTextInput(session,"arg1",value='')
    }
  })
})


# print the current code used for the next model
output$current.code = renderPrint({
  input$select_Y
  input$model_framework
  input$quasi
  input$offset
  design_params$design$dataDesign
  input$independent_variables
  input$confounding_variables
  input$transform_Y
  input$arg1
  input$intersaction_select
  input$transformation_select
  input$arg2
  modelValues$transformation.log
  modelValues$interaction.log
  input$interaction.vars.select
  input$degree.interaction.numeric
  isolate({
    is_survey <- FALSE
    if (!is.null(design_params$design$dataDesign)) {
      design.obj <- createSurveyObject()
      is_survey <- TRUE
    }
    # test whether y is not NULL or not "" and
    # a framework is selected
    if(!is.null(input$select_Y)&&
       !input$select_Y%in%""&&
       !is.null(input$model_framework)&&
       !input$model_framework%in%""){
      col = " + "
      int.all = F
      int.deg = F
      int.specific = F
      # interaction is used
      if(!is.null(input$intersaction_select)&&
         input$intersaction_select%in%"all"){
        int.all = T
        col  = " * "
      }else if(!is.null(input$intersaction_select)&&
               input$intersaction_select%in%"by degree"){
        int.deg = T
      }else if(!is.null(input$intersaction_select)&&
               input$intersaction_select%in%"by variables"&&
               length(modelValues$interaction.log)>0){
        int.specific = T
      }
      if(!is.null(input$transform_Y) && input$transform_Y != 'None') {
        if(input$transform_Y%in%"log"){
          func = paste("log(",input$select_Y,") ~ ",
                       paste(input$independent_variables
                             ,collapse=col),sep="")
          if(length(input$confounding_variables)>0){
            func = paste("log(",input$select_Y,") ~ ",
                         paste(input$independent_variables
                               ,collapse=col),col,
                         paste(input$confounding_variables,
                               collapse=col),sep="")
          }
        }else if(input$transform_Y%in%"sqrt"){
          func = paste("sqrt(",input$select_Y,") ~ ",
                       paste(input$independent_variables
                             ,collapse=col),sep="")
          if(length(input$confounding_variables)>0){
            func = paste("sqrt(",input$select_Y,") ~ ",
                         paste(input$independent_variables
                               ,collapse=col),col,
                         paste(input$confounding_variables,
                               collapse=col),sep="")
          }
        }else if(input$transform_Y%in%"^argument"&&
                 !input$arg1%in%""){
          func = paste(input$select_Y,"^",input$arg1," ~ ",
                       paste(input$independent_variables
                             ,collapse=col),sep="")
          if(length(input$confounding_variables)>0){
            func = paste(input$select_Y,"^",input$arg1," ~ ",
                         paste(input$independent_variables
                               ,collapse=col),col,
                         paste(input$confounding_variables,
                               collapse=col),sep="")
          }
        }
      } else {
        func = paste(input$select_Y," ~ ",
                     paste(input$independent_variables
                           ,collapse=col),sep="")
        if(length(input$confounding_variables)>0){
          func = paste(input$select_Y," ~ ",
                       paste(input$independent_variables
                             ,collapse=col),col,
                       paste(input$confounding_variables,
                             collapse=col),sep="")
        }
      }

      if(int.deg){
        func = strsplit(func," ~ ")[[1]]
        func[2] = paste0("(",func[2],")^",input$arg2)
        func = paste(func,collapse=" ~ ")
      }
      for(trans in names(modelValues$transformation.log)){
        if(!grepl(trans,func,fixed=T)){
          func = strsplit(func,modelValues$transformation.log[trans])[[1]]
          if(length(func)==1){
            func = paste0(func,trans)
          }else if(length(func)==2){
            func = paste0(func,collapse=trans)
          }
        }
      }
      if(int.specific){
        if(any(unlist(lapply(modelValues$interaction.log,
                             function(x,y){
                               any(x%in%y)
                             },
                             modelValues$transformation.log)))){
          new.interaction.log = lapply(modelValues$interaction.log,
                                       function(x,y){
                                         x[which(x%in%y)] = names(y)[which(y%in%x)]
                                       },modelValues$transformation.log)
          names(new.interaction.log) = unlist(lapply(new.interaction.log,function(x){
            paste(x,collapse=":")
          }))
          func = paste(func,paste(names(new.interaction.log),collapse=" + "),sep=" + ")
        }else{
          func = paste(func,paste(names(modelValues$interaction.log),collapse=" + "),sep=" + ")
        }
      }
      if(is_survey){
        design = paste0(design.model.fit$code, "\n")
        glmcode = paste0("svyglm(",func)
        if(input$model_framework%in%"Poisson Regression (Y counts)"){
          if(input$quasi){
            if(input$offset%in%""){
              glmcode = paste0(glmcode,",family='quasipoisson'")
            }else{
              glmcode = paste0(glmcode,",family='quasipoisson',offset=",input$offset)
            }
          }else{
            if(input$offset%in%""){
              glmcode = paste0(glmcode,",family='poisson'")
            }else{
              glmcode = paste0(glmcode,",family='poisson',offset=",input$offset)
            }
          }
        }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
          if(input$quasi){
            if(input$offset%in%""){
              glmcode = paste0(glmcode,",family='quasibinomial'")
            }else{
              glmcode = paste0(glmcode,",family='quasibinomial',offset=",input$offset)
            }
          }else{
            if(input$offset%in%""){
              glmcode = paste0(glmcode,",family='binomial'")
            }else{
              glmcode = paste0(glmcode,",family='binomial',offset=",input$offset)
            }
          }
        }
        glmcode = paste0(glmcode,", design = ", design_params$design$dataDesignName,")")
        cat(design,glmcode,sep="")
      }else{
        if(input$model_framework%in%"Least Squares"){
          cat("lm(",func,",data=", values$data.name, ")")
        }else if(input$model_framework%in%"Poisson Regression (Y counts)"){
          if(input$quasi){
            if(input$offset%in%""){
              cat("glm(",func,",data=", values$data.name, ",family='quasipoisson')")
            }else{
              cat("glm(",func,",data=", values$data.name, ",family='quasipoisson',offset=",input$offset,")")
            }
          }else{
            if(input$offset%in%""){
              cat("glm(",func,",data=", values$data.name, ",family='poisson')")
            }else{
              cat("glm(",func,",data=", values$data.name, ",family='poisson',offset=",input$offset,")")
            }
          }
        }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
          if(input$quasi){
            if(input$offset%in%""){
              cat("glm(",func,",data=", values$data.name, ",family='quasibinomial')")
            }else{
              cat("glm(",func,",data=", values$data.name, ",family='quasibinomial',offset=",input$offset,")")
            }
          }else{
            if(input$offset%in%""){
              cat("glm(",func,",data=", values$data.name, ",family='binomial')")
            }else{
              cat("glm(",func,",data=", values$data.name, ",family='binomial',offset=",input$offset,")")
            }
          }
        }else{
          cat("No code to present")
        }
      }
    }
  })
})

# rename a model
observe({
  input$rename_model
  isolate({
    if(!is.null(input$rename_model)&&
       input$rename_model>0){
      if(!is.null(input$new_model_name)&&
         !trim(input$new_model_name)%in%""&&
         !is.null(input$model.select)&&
         !input$new_model_name%in% names(modelValues$models)){
        names(modelValues$models)[which(names(modelValues$models)%in%input$model.select)] = input$new_model_name
        names(modelValues$code)[which(names(modelValues$code)%in%input$model.select)] = input$new_model_name
        names(modelValues$independent.vars)[which(names(modelValues$independent.vars)%in%input$model.select)] = input$new_model_name
        updateSelectInput(session,"model.select",
                          choices=names(modelValues$models),
                          selected=input$new_model_name)
      }
    }
  })
})

# remove a model
observe({
  input$remove.model
  isolate({
    tryCatch({
      if(!is.null(input$remove.model)&&
         input$remove.model>0){
        modelValues$models[[input$model.select]] = NULL
        modelValues$code[[input$model.select]] = NULL
        modelValues$independent.vars[[input$model.select]] = NULL
        ch = names(modelValues$models)
        updateSelectInput(session,"model.select",
                          choices=names(modelValues$models),
                          selected=names(modelValues$models)[1])
      }
    }, error = function(e) {})
  })
})

# show code history in main panel
output$code_history = renderUI({
  verbatimTextOutput("code.history.text")
})

# update code history
output$code.history.text = renderPrint({
  cat(modelValues$code.history)
})

# panel for the R code download
output$code_download = renderUI({
  list(helpText("Press button below to download the R
                code to rerun the work you have done."),
       downloadButton('downloadScript', 'Download R script'))
})

# download handler for retrieving the R code
output$downloadScript <- downloadHandler(

  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    "codeHistory.R"
  },

  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    sep = "\n"

    # Write to a file specified by the 'file' argument
    write(modelValues$code.history, file, sep = sep,)
  }
)

# display the code for the selected model.
output$r.code.fit = renderPrint({
  input$model.select
  #   modelValues$models
  isolate({
    if(!is.null(input$model.select)&&
       !input$model.select%in%""){
      cat(modelValues$code[[input$model.select]])
    }else{
      cat("No model code to show!")
    }
  })
})

output$show.model = renderUI({
  list(verbatimTextOutput("r.code.fit"),
       br(),
       verbatimTextOutput("fit.summary"))
})

output$model_main = renderUI({
  list(conditionalPanel("input.model_plot_selector=='Model'",
                        uiOutput("show.model")),
       conditionalPanel("input.model_plot_selector=='Plots'",
                        uiOutput("plots.main")),
       conditionalPanel("input.model_plot_selector=='Code History'",
                        uiOutput("code_history")))
})

# reset the panel if the data set has changed
observe({
  get.data.set()
  isolate({
    modelValues$models=list()
    modelValues$code=list()
    modelValues$code.history="# To make this code work outside of iNZight, read in your data first\n# In iNZight Lite, this has been done\ for you, just run the\n# following code in your R console:\nlibrary(iNZightRegression)\nlibrary(GGally)\nlibrary(survey) # only needed if a complex survey model was fitted\n"
    updateSelectInput(session,"select_Y",
                      choices=colnames(get.data.set()),
                      selected=colnames(get.data.set())[1])
    updateSelectInput(session,"model.select",
                      choices=c(""))
  })
})

# side bar for model plots
output$model_plots = renderUI({
  navlistPanel(id="plot_selector",
               "Select Plot Type",
               tabPanel("Graphical Diagnostics"),
               tabPanel("Normality Checks"),
               tabPanel("Factor level comparison"),
               widths=c(8,4)
  )
})

# UI for the main window when the plot tab is selected
output$plots.main = renderUI({
  input$model.select
  input$plot_selector
  isolate({
    if(is.null(input$model.select)||
       input$model.select%in%""){
      h2("Please fit a model first")
    }else{
      if(!any(modelValues$independent.vars[[input$model.select]]%in%
              get.categorical.column.names(get.data.set()))&&
         input$plot_selector%in%'Factor level comparison'){
        h2("No factor variables are fit in this model.")
      }else{
        ch1 = modelValues$independent.vars[[input$model.select]][which(modelValues$independent.vars[[input$model.select]]%in%
                                                                         get.categorical.column.names(get.data.set()))]
        ch2 = get.numeric.column.names(modelValues$models[[input$model.select]]$model)
        ch2 = ch2[which(ch2%in%modelValues$independent.vars[[input$model.select]])]
        list(conditionalPanel("input.plot_selector=='Factor level comparison'",
                              fixedRow(column(3,selectInput("factor.comp.select",
                                                            label="Select Factor to plot",
                                                            choices=ch1))),
                              plotOutput("factor.comparison.plot"),br(),
                              h4("Comparison matrix of selected factor"),
                              verbatimTextOutput("factor_comparison_matrix")),
             conditionalPanel("input.plot_selector=='Graphical Diagnostics'",
                              tabsetPanel(id="navlist_basic_plot",
                                          tabPanel("Basic Plots",
                                                   selectInput("plotlm6.selected",
                                                               label="Select Plot type",
                                                               choices=c("Residuals vs Fitted",
                                                                         "Scale-location",
                                                                         "Residuals vs Leverage",
                                                                         "Cooks Distance",
                                                                         "QQ-Plot",
                                                                         "Histogram",
                                                                         "All Plots"),
                                                               selected = input$plotlm6.select),
                                                   plotOutput("plotlm6")),
                                          tabPanel("Partial Residual Plot",
                                                   selectInput("partial.residual.plot.select",
                                                               label="Select a variable",
                                                               choices=ch2),
                                                   plotOutput("partial.residual.plot")),
                                          tabPanel("Scatter Plot Matrix",
                                                   plotOutput("scatter.plot.matrix")),
                                          type="pills")),
             conditionalPanel("input.plot_selector=='Normality Checks'",
                              tabsetPanel(id="navlist_basic_plot",
                                          tabPanel("Normal QQ-Plot",
                                                   plotOutput("normal.qq.plot")),
                                          tabPanel("Histogram",
                                                   plotOutput("histogram.plot")),
                                          tabPanel("Histogram Array",
                                                   plotOutput("histogram.array")),
                                          tabPanel("QQ-Plot inference",
                                                   plotOutput("qq.inference")),
                                          type="pills"))
        )
      }
    }
  })
})

# generates the qq-inference array (test for normality)
output$qq.inference = renderPlot({
  input$model.select
  isolate({
    modelValues$code.history = paste0(modelValues$code.history,
                                      "iNZightQQplot(",
                                      input$model.select,
                                      ")\n")
    iNZightQQplot(modelValues$models[[input$model.select]])
  })
})

# generates the histogram array (test for normality)
output$histogram.array = renderPlot({
  input$model.select
  isolate({
    modelValues$code.history = paste0(modelValues$code.history,
                                      "histogramArray(",
                                      input$model.select,
                                      ")\n")
    histogramArray(modelValues$models[[input$model.select]])
  })
})

# generates just the qq-plot from the plotlm6 function (test for normality)
output$normal.qq.plot = renderPlot({
  input$model.select
  isolate({
    modelValues$code.history = paste0(modelValues$code.history,
                                      "plotlm6(",
                                      input$model.select,
                                      ",which=",
                                      5,
                                      ")\n")
    invisible(plotlm6(modelValues$models[[input$model.select]],which=5))
  })
})

# generates just the histogram from the plotlm6 function (test for normality)
output$histogram.plot = renderPlot({
  input$model.select
  isolate({
    modelValues$code.history = paste0(modelValues$code.history,
                                      "plotlm6(",
                                      input$model.select,
                                      ",which=",
                                      6,
                                      ")\n")
    invisible(plotlm6(modelValues$models[[input$model.select]],which=6))
  })
})

# partial residual plots
output$partial.residual.plot = renderPlot({
  input$model.select
  input$partial.residual.plot.select
  isolate({
    partialResPlot(modelValues$models[[input$model.select]],
                   input$partial.residual.plot.select)
    modelValues$code.history = paste0(modelValues$code.history,
                                      "partialResPlot(",
                                      input$model.select,
                                      ",'",
                                      input$partial.residual.plot.select,
                                      "')\n")
  })
})

# generates the factor comparison plot
output$factor.comparison.plot = renderPlot({
  input$model.select
  input$factor.comp.select
  isolate({
    if(!is.null(modelValues$models)&&
       !is.null(input$model.select)&&
       !is.null(input$factor.comp.select)){
      suppressWarnings(plot(moecalc(modelValues$models[[input$model.select]],input$factor.comp.select)))
      modelValues$code.history = paste0(modelValues$code.history,
                                        paste0("plot(moecalc(",
                                               input$model.select,
                                               ",'",
                                               input$factor.comp.select,
                                               "'))\n"))
    }
  })
})

# prints the comparison matrix of the comparison plot
output$factor_comparison_matrix = renderPrint({
  input$model.select
  input$factor.comp.select
  isolate({
    print(suppressWarnings(iNZightRegression::factorComp(modelValues$models[[input$model.select]],input$factor.comp.select)))
    modelValues$code.history = paste0(modelValues$code.history,
                                      paste0("iNZightRegression::factorComp(",
                                             input$model.select,
                                             ",'",
                                             input$factor.comp.select,
                                             "')\n"))
  })
})

# generates the scatter plot matrix for the selected model
output$scatter.plot.matrix = renderPlot({
  input$model.select
  isolate({
    if(!is.null(modelValues$models[[input$model.select]])){
      temp = modelValues$models[[input$model.select]]$model
      modelValues$code.history = paste0(modelValues$code.history,
                                        paste0("ggpairs(",input$model.select,"$model,lower=list(continous='density',combo='box'),upper=list(continous='points',combo='dot'))\n"))
      suppressWarnings(ggpairs(temp,
                               lower=list(continous="density",
                                          combo="box"),
                               upper=list(continous="points",
                                          combo="dot")))
    }
  })
},width=800,height=800)

# generates the plots produced by plotlm6 (iNZightRegression) function
output$plotlm6 = renderPlot({
  input$model.select
  input$plotlm6.selected
  isolate({
    plotindex=7
    if(!is.null(input$plotlm6.selected)){
      if(input$plotlm6.selected%in%"Residuals vs Fitted"){
        plotindex=1
      }else if(input$plotlm6.selected%in%"Scale-location"){
        plotindex=2
      }else if(input$plotlm6.selected%in%"Residuals vs Leverage"){
        plotindex=3
      }else if(input$plotlm6.selected%in%"Cooks Distance"){
        plotindex=4
      }else if(input$plotlm6.selected%in%"QQ-Plot"){
        plotindex=5
      }else if(input$plotlm6.selected%in%"Histogram"){
        plotindex=6
      }
      # needs to have a global variable as the plotlm6 function reevaluates the
      # model formula which includes an object of name dafr (data set)
      dafr<<-get.data.set()
      # plotting call
      modelValues$code.history = paste0(modelValues$code.history,
                                        "plotlm6(",
                                        input$model.select,
                                        ",which=",
                                        plotindex,
                                        ")\n")
      invisible(plotlm6(modelValues$models[[input$model.select]],which=plotindex))
    }
  })
})

###-------------------------------------------------###
###  Server Functions for the "Model Fitting" Module  ###
###-------------------------------------------------###
###
###  Date Created   :   June 23, 2015
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *

# reactive values to store the fitted models
modelValues = reactiveValues(models=list(),
                             code=list(),
                             code.history="# To make this code work outside of iNZight, read in your data like so:\n# mydata = read.table(file.choose(), header = TRUE)\n# or \n# mydata = read.csv(file.choose())\n# if it is a comma seperated file.\n# In iNZight Lite, this has been done\ for you, just run the\n# following code in your R console:\nlibrary(iNZightRegression)\nlibrary(survey) # only needed if a complex survey model was fitted\n",
                             transformation.log=c(),
                             independent.vars=list())

# print the summary of the fitted model.
output$fit.summary = renderPrint({
  #   print("fit.summary")
  #   getModel()
  input$model.select
  isolate({
    if(!is.null(modelValues$models[[input$model.select]])&&
         !is.null(input$model.select)){
      confounds = input$confounding_variables
      if(" "%in%confounds){
        confounds = confounds[-which(confounds%in%" ")]
      }
      if(!is.null(confounds)&&
           length(confounds)>0&&
           all(confounds%in%colnames(get.data.set()))){
        iNZightSummary(modelValues$models[[input$model.select]],
                       exclude=confounds)
      }else{
        iNZightSummary(modelValues$models[[input$model.select]])
      }
    }else{
      cat("No model to show")
    }
  })
})

# panel for fitting a model
output$model_fit = renderUI({
  #   print("model fit side ui")
  #   get.data.set()
  input$select_Y
  isolate({
    is.numeric.column = NULL
    if(!is.null(input$select_Y)&&
         input$select_Y%in%colnames(get.data.set())){
      sel = input$select_Y
    }else{
      sel = NULL
    }
    if(!is.null(input$select_Y)&&
         !input$select_Y%in%""&&
         input$select_Y%in%colnames(get.data.set())){
      is.numeric.column = toJSON(get.data.set()[,input$select_Y])
    }
    list(br(),
         fixedRow(column(11,h4("Choose Model parameters")),
                  column(1,checkboxInput("toggle_check4",
                                         label="",
                                         value=T))),
         conditionalPanel("input.toggle_check4",
                          fixedRow(column(6,
                                          selectInput("select_Y",
                                                      label="Select Y variable",
                                                      choices=colnames(get.data.set()),
                                                      selected=sel)
                          ),
                          column(6, 
                                 conditionalPanel(paste0("testNumeric(",
                                                         is.numeric.column,
                                                         ")"),
                                                  fixedRow(column(6,
                                                                  selectInput("transform_Y",
                                                                              label="Transform Y",
                                                                              choices=c(" ",
                                                                                        "log",
                                                                                        "sqrt",
                                                                                        "^argument"),
                                                                              selected=input$transform_Y)),
                                                           column(6,conditionalPanel("input.transform_Y=='^argument'",
                                                                                     textInput("arg1",
                                                                                               label="argument",
                                                                                               value=input$arg1)))
                                                  )))),
                          fixedRow(column(6,selectInput("model_framework",label="Model Framework",
                                                        choices=c("Least Squares",
                                                                  "Logistic Regression (Y binary)",
                                                                  "Poisson Regression (Y counts)"),
                                                        selected=input$model_framework)),
                                   column(6,selectInput("data_structure",label="Data Structure",
                                                        choices=c("Standard",
                                                                  "Complex Survey"),
                                                        selected=input$data_structure))
                          ),
                          conditionalPanel("input.model_framework=='Logistic Regression (Y binary)'||
                                           input.model_framework=='Poisson Regression (Y counts)'",
                                           h4("Extra Arguments"),
                                           fixedRow(column(6,selectInput("quasi",
                                                                         label="quasi",
                                                                         choices=c(no=F,
                                                                                   yes=T),
                                                                         selected=input$quasi)),
                                                    column(6,textInput("offset",label="offset")))),
                          conditionalPanel("input.data_structure=='Complex Survey'",
                                           h4("Survey Design"),
                                           fixedRow(column(4,selectInput("cluster",label="Cluster",
                                                                         choices=c(" ","1",colnames(get.data.set())),
                                                                         selected=input$cluster)),
                                                    column(4,selectInput("strata",label="Strata",
                                                                         choices=c(" ",colnames(get.data.set())),
                                                                         selected=input$strata)),
                                                    column(4,selectInput("weights",label="Weights",
                                                                         choices=c(" ",colnames(get.data.set())),
                                                                         selected=input$weights))
                                           ),
                                           fixedRow(column(4,selectInput("fpc",label="fpc",
                                                                         choices=c(" ",colnames(get.data.set())),
                                                                         selected=input$fpc)),
                                                    column(4,checkboxInput("nest",label="Nest",
                                                                           value=input$nest)))
                          )),
         fixedRow(column(11,h4("Variables to fit")),
                  column(1,checkboxInput("toggle_check3",
                                         label="",
                                         value=T))),
         conditionalPanel("input.toggle_check3",
                          fixedRow(column(6,selectInput("independent_variables",
                                                        label="Independent Variables",
                                                        choices=colnames(get.data.set())[-which(input$select_Y%in%colnames(get.data.set()))],
                                                        selected=input$independent_variables,
                                                        selectize=T,
                                                        multiple=T)),
                                   column(6,selectInput("confounding_variables",
                                                        label="Confounder Variables",
                                                        choices=colnames(get.data.set())[-which(input$select_Y%in%colnames(get.data.set()))],
                                                        selected=input$confounding_variables,
                                                        selectize=T,
                                                        multiple=T)))),
         fixedRow(column(11,h4("Interaction")),
                  column(1,checkboxInput("toggle_check1",
                                         label="",
                                         value=input$toggle_check1))),
         conditionalPanel("input.toggle_check1",
                          fixedRow(column(4,selectInput("intersaction_select",
                                                        label="Interaction",
                                                        choices=c("none","all","by degree"),
                                                        selected="none")),
                                   column(4,helpText("")),
                                   column(4,textInput("arg2",
                                                      label="degree",
                                                      value=input$arg2)))),
         fixedRow(column(11,h4("Transformation")),
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
         fixedRow(column(11,h4("Code preview")),
                  column(1,checkboxInput("toggle_check5",
                                         label="",
                                         value=T))),
         conditionalPanel("input.toggle_check5",
                          helpText("The code used for the next model fit. Note,
                                   this code might not be executable."),
                          verbatimTextOutput("current.code")),
         br(),
         actionButton("fit.model.button","Fit Model")
         )
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
        transformation.string = ""
        if(input$transform_select%in%"log"){
          transformation.string = paste0("log(",
                                         input$transform_variable_select,
                                         ")")
        }else if(input$transform_select%in%"sqrt"){
          transformation.string = paste0("sqrt(",
                                         input$transform_variable_select,
                                         ")")
        }else if(input$transform_select%in%"by degree"&&
                   !input$arg3%in%""){
          transformation.string = paste0(input$transform_variable_select,
                                         " + I(",
                                         input$transform_variable_select,
                                         ",^",
                                         input$arg3,
                                         ")")
        }else if(input$transform_select%in%"polynomial of degree"&&
                   !input$arg3%in%""){
          transformation.string = paste0("poly(",
                                         input$transform_variable_select,
                                         ",",input$arg3,")")
        }
        if(!transformation.string%in%""&&
             !transformation.string%in%names(modelValues$transformation.log)){
          modelValues$transformation.log[transformation.string] = input$transform_variable_select
          updateSelectInput(session,"transformation_remove",
                            choices=c("none",names(modelValues$transformation.log)),
                            selected="none")
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
  #   print("submit model")
  input$fit.model.button
  isolate({
    if(!is.null(input$fit.model.button)&&
         input$fit.model.button>0){
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
        if(!is.null(input$intersaction_select)&&
             input$intersaction_select%in%"all"){
          col  = " * "
        }else if(!is.null(input$intersaction_select)&&
                   input$intersaction_select%in%"by degree"){
          int.deg = T
        }
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
        }else if(input$transform_Y%in%"sqrt"){
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
        }else if(input$transform_Y%in%"^argument"&&
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
        }else{
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
        if(length(input$confounding_variables)>0){
          formu = paste(input$select_Y," ~ ",
                        paste(input$independent_variables
                              ,collapse=col),col,
                        paste(input$confounding_variables,
                              collapse=col),sep="")
        }else{
          formu = paste(input$select_Y," ~ ",
                        paste(input$independent_variables
                              ,collapse=col),sep="")
        }
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
        design0=NULL
        if(input$data_structure%in%"Complex Survey"){
          id0 = formula(paste0("~",input$cluster))
          if(!input$strata%in%" "){
            strata0 = formula(paste0("~",input$strata))
          }else{
            strata0 = NULL
          }
          if(!input$weights%in%" "){
            weights0=eval(bquote(formula(paste0("~",input$weights))))
          }else{
            weights0=NULL
          }
          if(!input$fpc%in%" "){
            fpc0 = formula(paste0("~",input$fpc))
          }else{
            fpc0 = NULL
          }
          nest0 = input$nest
          dafr = get.data.set()
          design0 = svydesign(id=id0,
                              strata=strata0,
                              weights=weights0,
                              fpc=fpc0,
                              nest=nest0,
                              data=dafr,
                              variables=dafr)
          design.text = paste0("mydesign = svydesign(id=~",input$cluster)
          if(!is.null(strata0)){
            design.text = paste0(design.text,",strata=~",input$strata)
          }
          if(!is.null(weights0)){
            design.text = paste0(design.text,",weights=~",input$weights)
          }
          if(!is.null(fpc0)){
            design.text = paste0(design.text,",fpc=~",input$fpc)
          }
          design.text = paste0(design.text,",nest=",input$nest)
          design.text = paste0(design.text,")")
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
                               " = svyglm(",formu,",design=mydesign)")
          }else{
            temp.model = svyglm(formula(formu),design=design0,family=family0,offset=offset0)
            temp.code = paste0(design.text,"\nsvyglm(",formu)
            if(!is.null(offset0)){
              temp.code = paste0(temp.code,",offset=",offset0)
            }
            temp.code = paste0(temp.code,"family=",family0)
            temp.code = paste0(temp.code,",design=mydesign)")
          }
        }else{
          if(input$model_framework%in%"Least Squares"){
            temp.model = lm(formula(formu),data=get.data.set())
            temp.code = paste0(model.name," = lm(",formu,",data=mydata)")
          }else if(input$model_framework%in%"Poisson Regression (Y counts)"){
            if(input$quasi){
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=get.data.set(),family='quasipoisson')
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='quasipoisson')")
              }else{
                temp.model = glm(formula(formu),data=get.data.set(),family='quasipoisson',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='quasipoisson',offset=",input$offset,")")
              }
            }else{
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=get.data.set(),family='poisson')
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='poisson'")
              }else{
                temp.model = glm(formula(formu),data=get.data.set(),family='poisson',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='poisson',offset=",input$offset,")")
              }
            }
          }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
            if(input$quasi){
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=get.data.set(),family='quasibinomial')
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='quasibinomial')")
              }else{
                temp.model = glm(formula(formu),data=get.data.set(),family='quasibinomial',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='quasibinomial',offset=",input$offset,")")
              }
            }else{
              if(input$offset%in%""){
                temp.model = glm(formula(formu),data=get.data.set(),family='binomial')
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='binomial')")
              }else{
                temp.model = glm(formula(formu),data=get.data.set(),family='binomial',offset=input$offset)
                temp.code = paste0(model.name," = glm(",formu,",data=mydata,family='binomial',offset=",input$offset,")")
              }
            }
          }
        }
      }, warning = function(w) {
        print(w)
      }, error = function(e) {
        print(e)
      }, finally = {})
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
  #   print("check independent")
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
  #   print("check confounders")
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
  #   print("check numeric")
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
  #   print("check transform text")
  input$arg1
  isolate({
    if(!is.null(input$arg1)&&
         !is.convertable.numeric(input$arg1)){
      updateTextInput(session,"arg1",value='')
    }
  })
})

# update input when complex survey is selected (not implemented yet)
observe({
  input$data_structure
  isolate({
    #     if(){
    #       updateSelectInput(session,"quasi",selected="yes")
    #     }
  })
})

# print the current code used for the next model
output$current.code = renderPrint({
  #   print("print current code")
  input$select_Y
  input$model_framework
  input$quasi
  input$offset
  input$independent_variables
  input$confounding_variables
  input$data_structure
  input$cluster
  input$strata
  input$weights
  input$fpc
  input$nest
  input$transform_Y
  input$arg1
  input$intersaction_select
  input$transformation_select
  input$arg2
  modelValues$transformation.log
  isolate({
    if(!is.null(input$select_Y)&&
         !input$select_Y%in%""&&
         !is.null(input$model_framework)&&
         !input$model_framework%in%""){
      col = " + "
      int.deg = F
      if(!is.null(input$intersaction_select)&&
           input$intersaction_select%in%"all"){
        col  = " * "
      }else if(!is.null(input$intersaction_select)&&
                 input$intersaction_select%in%"by degree"){
        int.deg = T
      }
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
      }else{
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
          print(func)
          print(trans)
          if(length(func)==1){
            func = paste0(func,trans)
          }else if(length(func)==2){
            func = paste0(func,collapse=trans)
          }
          print(func)
        }
      }
      if(input$data_structure%in%"Complex Survey"){
        design=paste0("svyDesign = svydesign(id=~",input$cluster)
        if(!input$strata%in%" "){
          design = paste0(design,",strata=~",input$strata)
        }
        #         print("input$weights")
        #         print(input$weights)
        #         print(!input$weights%in%"")
        if(!input$weights%in%" "){
          design = paste0(design,",weights=~",input$weights)
        }
        if(!input$fpc%in%" "){
          design = paste0(design,",fpc=~",input$fpc)
        }
        if(input$nest){
          design = paste0(design,",nest=T,")
        }else{
          design = paste0(design,",nest=F,")
        }
        design = paste0(design,"data=mydata)\n")
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
        glmcode = paste0(glmcode,",design=svyDesign)")
        cat(design,glmcode,sep="")
      }else{
        if(input$model_framework%in%"Least Squares"){
          cat("lm(",func,",data=mydata)")
        }else if(input$model_framework%in%"Poisson Regression (Y counts)"){
          if(input$quasi){
            if(input$offset%in%""){
              cat("glm(",func,",data=mydata,family='quasipoisson')")
            }else{
              cat("glm(",func,",data=mydata,family='quasipoisson',offset=",input$offset,")")
            }
          }else{
            if(input$offset%in%""){
              cat("glm(",func,",data=mydata,family='poisson')")
            }else{
              cat("glm(",func,",data=mydata,family='poisson',offset=",input$offset,")")
            }
          }
        }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
          if(input$quasi){
            if(input$offset%in%""){
              cat("glm(",func,",data=mydata,family='quasibinomial')")
            }else{
              cat("glm(",func,",data=mydata,family='quasibinomial',offset=",input$offset,")")
            }
          }else{
            if(input$offset%in%""){
              cat("glm(",func,",data=mydata,family='binomial')")
            }else{
              cat("glm(",func,",data=mydata,family='binomial',offset=",input$offset,")")
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
  #   print("rename button")
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
  #   print("remove button")
  input$remove.model
  isolate({
    if(!is.null(input$remove.model)&&
         input$remove.model>0){
      modelValues$models[[input$model.select]] = NULL
      modelValues$code[[input$model.select]] = NULL
      modelValues$independent.vars[[input$model.select]] = NULL
      updateSelectInput(session,"model.select",
                        choices=names(modelValues$models),
                        selected=names(modelValues$models)[1])
    }
  })
})

# show code history in main panel
output$code_history = renderUI({
  #   print("code history ui")
  verbatimTextOutput("code.history.text")
})

# update code history
output$code.history.text = renderPrint({
  #   print("code history print")
  cat(modelValues$code.history)
})

# panel for the R code download
output$code_download = renderUI({
  #   print("code history side")
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
  #   print("code selected model")
  input$model.select
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
  #   print("model ui")
  list(verbatimTextOutput("r.code.fit"),
       br(),
       verbatimTextOutput("fit.summary"))
})

output$model_main = renderUI({
  #   print("main panels")
  #   print(input$model_plot_selector)
  list(conditionalPanel("input.model_plot_selector=='Fit Model'",
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
    #     print(get.data.name())
    #     print("modelValues reset")
    modelValues$models=list()
    modelValues$code=list()
    modelValues$code.history="# To make this code work outside of iNZight, read in your data like so:\n# mydata = read.table(file.choose(), header = TRUE)\n# or \n# mydata = read.csv(file.choose())\n# if it is a comma seperated file.\n# In iNZight Lite, this has been done\ for you, just run the\n# following code in your R console:\nlibrary(iNZightRegression)\nlibrary(survey) # only needed if a complex survey model was fitted\n"
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
    tabPanel("Factor level comparison"),
    navbarMenu("Graphical Diagnostics",
               id="graphical_diagnostics",
               tabPanel("Scatter plot matrix"),
               tabPanel("tester")),
    tabPanel("Third"),
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
           input.plot_selector%in%'Factor level comparison'){
        h2("No factor variables are fit in this model.")
      }else{
        ch = modelValues$independent.vars[[input$model.select]][which(modelValues$independent.vars[[input$model.select]]%in%
                                                                        get.categorical.column.names(get.data.set()))]
        list(conditionalPanel("input.plot_selector=='Factor level comparison'",
                              fixedRow(column(3,selectInput("factor.comp.select",
                                                            label="Select Factor to plot",
                                                            choices=ch))),
                              plotOutput("factor.comparison.plot"),br(),
                              h4("Comparison matrix of selected factor"),
                              verbatimTextOutput("factor_comparison_matrix")),
             conditionalPanel("input.plot_selector=='Scatter plot matrix'",
                              plotOutput("scatter.plot.matrix")))
      }
    }
  })
})

# generates the facto comparison plot
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
    suppressWarnings(multicomp(moecalc(modelValues$models[[input$model.select]],input$factor.comp.select)))
    modelValues$code.history = paste0(modelValues$code.history,
                                      paste0("multicomp(moecalc(",
                                             input$model.select,
                                             ",'",
                                             input$factor.comp.select,
                                             "'))\n"))
  })
})

# generates the scatter plot matrix for the selected model
output$scatter.plot.matrix = renderPlot({
  input$model.select
  isolate({
    if(!is.null(modelValues$models[[input$model.select]])){
      temp = modelValues$models[[input$model.select]]$model
      modelValues$code.history = paste0(modelValues$code.history,
                                        paste0("gpairs(",input$model.select,"$model)\n"))
  #     temp = temp[,c(get.numeric.column.names(temp),get.categorical.column.names(temp))]
      gpairs(temp)
    }
  })
},width=800,height=800)
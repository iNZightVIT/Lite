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
                             code.history="# To make this code work outside of iNZight, read in your data like so:\n# mydata = read.table(file.choose(), header = TRUE)\n# iNZight has done this step for you, just run the\n# following code in your R console:\n")

# print the summary of the fitted model.
output$fit.summary = renderPrint({
#   print("fit.summary")
#   getModel()
  input$model.select
  isolate({
    if(!is.null(modelValues$models[[input$model.select]])&&
         !is.null(input$model.select)){
      iNZightSummary(modelValues$models[[input$model.select]])
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
                                                                      selected="")),
                                                   column(6,conditionalPanel("input.transform_Y=='^argument'",
                                                                             textInput("arg1",
                                                                                       label="argument",
                                                                                       value=input$arg1)))
                                                   )
                                         ) 
                         )
                  ),
         fixedRow(column(6,selectInput("model_framework",label="Model Framework",
                                       choices=c("Least Squares",
                                                 "Logistic Regression (Y binary)",
                                                 "Poisson Regression (Y counts)"),
                                       selected=input$model_framework)),
                  column(6,selectInput("data_structure",label="Data Structure",
                                       choices=c("Standard",
                                                 "Complex Survey"),
                                       selected="Standard"))
         ),
         conditionalPanel("input.model_framework=='Logistic Regression (Y binary)'||
                          input.model_framework=='Poisson Regression (Y counts)'",
                          h4("Extra Arguments"),
                          fixedRow(column(6,selectInput("quasi",
                                                        label="quasi",
                                                        choices=c(yes=T,
                                                                  no=F),
                                                        selected=F)),
                                   column(6,textInput("offset",label="offset")))),
         conditionalPanel("input.data_structure=='Complex Survey'",
                          h4("Survey Design"),
                          fixedRow(column(4,selectInput("cluster",label="Cluster",
                                                        choices=c(" ","1",colnames(get.data.set())),
                                                        selected=" ")),
                                   column(4,selectInput("strata",label="Strata",
                                                        choices=c(" ",colnames(get.data.set())),
                                                        selected=" ")),
                                   column(4,selectInput("weights",label="Weights",
                                                        choices=c(" ",colnames(get.data.set())),
                                                        selected=" "))
                                   ),
                          fixedRow(column(4,selectInput("fpc",label="fpc",
                                                        choices=c(" ",colnames(get.data.set())),
                                                        selected=" ")),
                                   column(4,checkboxInput("nest",label="Nest",
                                                          value=F)))
                          ),
         h4("Variables to fit"),
         fixedRow(column(6,selectInput("independent_variables",
                                       label="Independent Variables",
                                       choices=c(" ",colnames(get.data.set())[-which(input$select_Y%in%colnames(get.data.set()))]),
                                       selected=" ",
                                       selectize=T,
                                       multiple=T)),
                  column(6,selectInput("confounding_variables",
                                       label="Confounder Variables",
                                       choices=c(" ",colnames(get.data.set())[-which(input$select_Y%in%colnames(get.data.set()))]),
                                       selected=" ",
                                       selectize=T,
                                       multiple=T))),
         helpText("The code used for the next model fit. Note, 
                  this code might lead to error messages if 
                  not done properly."),
         textOutput("current.code"),
         br(),
         actionButton("fit.model.button","Fit Model")
         )
  })
})

# Submit the current model
observe({
  print("submit model")
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
        formu = paste(input$select_Y," ~ ",
                      paste(input$independent_variables[-which(input$independent_variables%in%" ")],
                            collapse=" + "),sep="")
        if(input$model_framework%in%"Least Squares"){
          temp.model = lm(formula(formu),data=get.data.set())
          temp.code = paste0("lm(",formu,",data=mydata.new)")
        }else if(input$model_framework%in%"Poisson Regression (Y counts)"){
          if(input$quasi){
            if(input$offset%in%""){
              temp.model = glm(formula(formu),data=get.data.set(),family='quasipoisson')
              temp.code = paste0("glm(",formu,",data=mydata.new,family='quasipoisson')")
            }else{
              temp.model = glm(formula(formu),data=get.data.set(),family='quasipoisson',offset=input$offset)
              temp.code = paste0("glm(",formu,",data=mydata.new,family='quasipoisson',offset=",input$offset,")")
            }
          }else{
            if(input$offset%in%""){
              temp.model = glm(formula(formu),data=get.data.set(),family='poisson')
              temp.code = paste0("glm(",formu,",data=mydata.new,family='poisson'")
            }else{
              temp.model = glm(formula(formu),data=get.data.set(),family='poisson',offset=input$offset)
              temp.code = paste0("glm(",formu,",data=mydata.new,family='poisson',offset=",input$offset,")")
            }
          }
        }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
          if(input$quasi){
            if(input$offset%in%""){
              temp.model = glm(formula(formu),data=get.data.set(),family='quasibinomial')
              temp.code = paste0("glm(",formu,",data=mydata.new,family='quasibinomial')")
            }else{
              temp.model = glm(formula(formu),data=get.data.set(),family='quasibinomial',offset=input$offset)
              temp.code = paste0("glm(",formu,",data=mydata.new,family='quasibinomial',offset=",input$offset,")")
            }
          }else{
            if(input$offset%in%""){
              temp.model = glm(formula(formu),data=get.data.set(),family='binomial')
              temp.code = paste0("glm(",formu,",data=mydata.new,family='binomial')")
            }else{
              temp.model = glm(formula(formu),data=get.data.set(),family='binomial',offset=input$offset)
              temp.code = paste0("glm(",formu,",data=mydata.new,family='binomial',offset=",input$offset,")")
            }
          }
        }
      }, warning = function(w) {
        print(w)
      }, error = function(e) {
        print(e)
      }, finally = {})
      if(!is.null(temp.model)){
        print("model submitted")
        modelValues$models[[model.name]] = temp.model
        updateSelectInput(session,"model.select",
                          choices=names(modelValues$models),
                          selected=model.name)
        modelValues$code[[model.name]] = temp.code
        modelValues$code.history = paste0(modelValues$code.history,
                                          temp.code,"\niNZightSummary(",
                                          model.name,
                                          ")\n")
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
    independent_variables = input$independent_variables[-which(input$independent_variables%in%" ")]
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
    confounding_variables = input$confounding_variables[-which(input$confounding_variables%in%" ")]
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

# print the current code used for the next model
output$current.code = renderPrint({
#   print("print current code")
  input$select_Y
  input$model_framework
  input$quasi
  input$offset
  input$independent_variables
  isolate({
    if(!is.null(input$select_Y)&&
         !input$select_Y%in%""&&
         !is.null(input$model_framework)&&
         !input$model_framework%in%""){
      func = paste(input$select_Y," ~ ",
                   paste(input$independent_variables[-which(input$independent_variables%in%" ")],
                         collapse=" + "),sep="")
      if(input$model_framework%in%"Least Squares"){
        cat("lm(",func,",data=mydata.new)")
      }else if(input$model_framework%in%"Poisson Regression (Y counts)"){
        if(input$quasi){
          if(input$offset%in%""){
            cat("glm(",func,",data=mydata.new,family='quasipoisson')")
          }else{
            cat("glm(",func,",data=mydata.new,family='quasipoisson',offset=",input$offset,")")
          }
        }else{
          if(input$offset%in%""){
            cat("glm(",func,",data=mydata.new,family='poisson')")
          }else{
            cat("glm(",func,",data=mydata.new,family='poisson',offset=",input$offset,")")
          }
        }
      }else if(input$model_framework%in%"Logistic Regression (Y binary)"){
        if(input$quasi){
          if(input$offset%in%""){
            cat("glm(",func,",data=mydata.new,family='quasibinomial')")
          }else{
            cat("glm(",func,",data=mydata.new,family='quasibinomial',offset=",input$offset,")")
          }
        }else{
          if(input$offset%in%""){
            cat("glm(",func,",data=mydata.new,family='binomial')")
          }else{
            cat("glm(",func,",data=mydata.new,family='binomial',offset=",input$offset,")")
          }
        }
      }else{
        cat("No code to present")
      }
    }
  })
})

# rename a model
observe({
#   print("rename button")
  input$rename.model
  isolate({
    if(!is.null(input$rename.model)&&
         input$rename.model>0){
      if(!trim(input$new_model_name)%in%""){
        names(modelValues$models)[which(names(modelValues$models)%in%input$model.select)] = input$new_model_name
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
output$code.download = renderUI({
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

observe({
  get.data.set()
  isolate({
    print(get.data.name())
    print("modelValues reset")
    modelValues$models=list()
    modelValues$code=list()
    modelValues$code.history="# To make this code work outside of iNZight, read in your data like so:\n# mydata = read.table(file.choose(), header = TRUE)\n# iNZight has done this step for you, just run the\n# following code in your R console:\n"
    updateSelectInput(session,"select_Y",
                      choices=colnames(get.data.set()),
                      selected=colnames(get.data.set())[1])
#     updateSelectInput(session,"independent_variables",
#                       choices=c(" ",colnames(get.data.set())[-which(input$select_Y%in%colnames(get.data.set()))]),
#                       selected=" ")
#     updateSelectInput(session,"confounding_variables",
#                       choices=c(" ",colnames(get.data.set())[-which(input$select_Y%in%colnames(get.data.set()))]),
#                       selected=" ")
    updateSelectInput(session,"model.select",
                      choices=c())
  })
})

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
                             code=list())

# retrieve the selected model reactivly
getModel = reactive({
  if(!is.null(input$model.select)){
    modelValues$models[[input$model.select]]
  }else{
    NULL
  }
})

getCode = reactive({
  if(!is.null(input$model.select)){
    modelValues$code[[input$model.select]]
  }else{
    NULL
  }
})

# Display the code to produvce the module.
output$r.code.fit = renderPrint({
  if(!is.null(getCode())){
    cat(getCode())
  }else{
    cat("No code to show.")
  }
})

# print the summary of the fitted model.
output$fit.Summary = renderPrint({
  if(!is.null(getModel())){
    iNZightSummary(getModel())
  }else{
    cat("No model to show")
  }
})

# panel for fitting a model
output$model.fit = renderUI({
  get.data.set()
  input$select_Y
  isolate({
    list(br(),fixedRow(column(6,
                              selectInput("select_Y",
                                          label="Select Y variable",
                                          choices=colnames(get.data.set()))),
                       column(3,
                              selectInput("transform_Y",
                                          label="Transform Y",
                                          choices=c(" ",
                                                    "log",
                                                    "sqrt",
                                                    "^argument"),
                                          selected="")),
                       column(3,conditionalPanel("input.transform_Y=='^argument'",
                                                 textInput("arg1",
                                                           label="argument",
                                                           value=input$arg1)))
                       ))
  })
})

observe({
  input$arg1
  isolate({
    if(!is.null(input$arg1)&&
         !is.convertable.numeric(input$arg1)){
      updateTextInput(session,"arg1",value='')
    }
  })
})

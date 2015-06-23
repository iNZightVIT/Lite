###-------------------------------------------------###
###  UI Functions for the "Model Fitting" Module  ###
###-------------------------------------------------###
###
###  Date Created   :   June 23, 2015
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *


model.fitting.panel.ui = function(data.set){
  if(is.null(data.set)){
    fluidRow(
      includeMarkdown(
        "panels/8_ModelFitting/4_modelfitting-panel-null.md")
    )
  }else{
    sidebarLayout(
      sidebarPanel(
        selectInput("model.select",
                    label="Select Fitted Model",
                    choices = names(modelValues$models)),
        tabsetPanel(id="model_plot_selector",
                    tabPanel("Fit Model",
                             uiOutput("model.fit")),
                    tabPanel("Plots",
                             uiOutput("model.plots")),
                    type="pills"),br(),
        modelfitting.help()
      ), 
      mainPanel(conditionalPanel("input.model_plot_selector=='Fit Model'",
                                 textOutput("r.code.fit"),br(),
                                 textOutput("fit.Summary")),
                conditionalPanel("input.model_plot_selector=='Plots'",
                                 uiOutput("plots.main"))    
      )
    )
  }
}

# check constantly whether the input is numeric
modelfitting.help = function() {
  help.display(
    title = "Model Fitting Module",
    id = "Model_Fitting",
    file = "panels/8_ModelFitting/3_modelfitting-panel-help.md")
}
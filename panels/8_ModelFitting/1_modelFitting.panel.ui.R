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


model.fitting.panel.ui = function(data.set,has.model=F){
  if(is.null(data.set)){
    fluidRow(
      includeMarkdown(
        "panels/8_ModelFitting/4_modelfitting-panel-null.md")
    )
  }else{
    if(has.model){
      sidebarLayout(
        sidebarPanel(fixedRow(
          column(4,
                 selectInput("model.select",
                             label="Select Fitted Model",
                             choices = c())),
          column(4,textInput("new_model_name",label="Rename Model")),
          column(2,br(),actionButton("rename_model",label="Rename")),
          column(2,br(),actionButton("remove.model",label="Remove"))),
          tabsetPanel(id="model_plot_selector",
                      tabPanel("Model",
                               uiOutput("model_fit")),
                      tabPanel("Plots",
                               uiOutput("model_plots")),
                      tabPanel("Code History",
                               uiOutput("code_download")),
                      type="pills"),br(),
          modelfitting.help()
        ), 
        mainPanel(uiOutput("model_main"))
      )
    }else{
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(id="model_plot_selector",
                      tabPanel("Model",
                               uiOutput("model_fit")),
                      tabPanel("Plots",
                               uiOutput("model_plots")),
                      tabPanel("Code History",
                               uiOutput("code_download")),
                      type="pills"),br(),
          modelfitting.help()
        ), 
        mainPanel(uiOutput("model_main"))
      )
    }
  }
}

# check constantly whether the input is numeric
modelfitting.help = function() {
  help.display(
    title = "Model Fitting Module",
    id = "Model_Fitting",
    file = "panels/8_ModelFitting/3_modelfitting-panel-help.md")
}
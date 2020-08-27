###-------------------------------------------------------###
###  User Interface for the "Experimental Design" Module  ###
###-------------------------------------------------------###
###
###  Date Created  : March 25, 2018.
###
###  The UI is divided into two parts:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: This is to be sourced within "server.R" *

###----------------###
###  Sidebar Panel ###
###----------------###

###  First, we set up the "help" functionality for this module.


###  Next, we set up the sidebar panel with "exploratory.analysis.panel.ui".
mixedModel.sidebarPanel <- function(){
  sidebarPanelUI = list(
    useShinyalert(),
    ## select fitted models 
    
    fluidRow(column(6, selectInput("model_select",
                                   label = "Select Fitted Model",
                                   choices = c())),
             tags$div(style = "margin-top: 24px", column(2, offset = 1, actionButton(inputId = "remove.model",
                                                                                     label = "Remove",
                                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
    tabsetPanel(type = "pills",
                id = "Doe_type",
                tabPanel('ANOVA',
                         fluidRow(column(12, hr()),
                                  
                                  
                                  tags$div(column(12, radioButtons(inputId = "fit_design", label = "Select design", 
                                                                   choices = c("Standard design" = 1, 
                                                                               "Customized design" = 2), inline = T,
                                                                   selected = 1)))),
                         
                         ## select model to fit
                         conditionalPanel(condition = "input.fit_design == 1", 
                                          fluidRow(column(12, selectInput("model_design",
                                                                          label = "",
                                                                          choices = c("One-way anova (no Blocking)" = 1,
                                                                                      "One-way anova (in randomized Blocks)" = 2,
                                                                                      "Two-way anova (no Blocking)" = 3,
                                                                                      "Two-way anova (in randomized Blocks)" = 4,
                                                                                      "Three-way anova (no Blocking)" = 5,
                                                                                      "Three-way anova (in randomized Blocks)" = 6)))),
                                          fluidRow(column(12, hr())),
                                          uiOutput("ep_anova"),
                                          
                                          fluidRow(column(12, hr())),
                                          
                                          actionButton(inputId = "fit_model_aov",
                                                       label = "Fit Model",
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                          
                                          fluidRow(column(12, hr()))
                                          
                         ),
                         ## fit own model
                         conditionalPanel(condition = "input.fit_design == 2",
                                          
                                          ## select response variable
                                          fixedRow(column(12, uiOutput("own_model_var1_panel"))),
                                          
                                          hr(),
                                          
                                          ## select fixed effect
                                          checkboxInput(inputId = "select_fix_effect", 
                                                        label = "Explanatory component (fixed effect)", value = FALSE),
                                          
                                          ## select Explanatory component
                                          conditionalPanel(condition = "input.select_fix_effect == 1",
                                                           
                                                           ## select variables and operations
                                                           fluidRow(column(12, uiOutput("own_model_fixed_panel"))),
                                                           
                                                           ## display results
                                                           fluidRow(column(12,textInput("fixed_effect", label = "Fixed effect:", value = "")),
                                                                    
                                                                    hr())
                                                           
                                                           ## fluidRow(column(10, verbatimTextOutput("fixed.expression.message")))
                                          ),
                                          ## select random effect
                                          checkboxInput(inputId = "select_random_effect", 
                                                        label = "Structural component (random effect)", value = FALSE),
                                          
                                          conditionalPanel(condition = "input.select_random_effect == 1",
                                                           
                                                           ## select variables and operations
                                                           fluidRow(column(6, uiOutput("own_model_random_panel"))),
                                                           
                                                           fluidRow(column(12,textInput("random_effect", label = "Random effect:", value = "")))),
                                          
                                          fluidRow(column(12, radioButtons(inputId = "mm_trt.mean1",
                                                                           label = "Treatment means",
                                                                           choices = c("No blocking or complete blocks", "balanced incomplete blocks"),
                                                                           selected = "No blocking or complete blocks"))),
                                          
                                          fluidRow(column(12, hr())),
                                          
                                          actionButton(inputId = "fit_model_own",
                                                       label = "Fit Model",
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                          
                                          fluidRow(column(12, hr()))
                         )## end of fiting own model
                         ),
                tabPanel("Interaction Plot",
                         fluidRow(column(12, hr())),
                         uiOutput("Doe.int.var")))
    
  )## end of list
}

### now, we set up the main panel
mixedModel.mainPanel = function(){
  mainPanelUI = list(
    div(id = "doe_show",
        conditionalPanel("input.Doe_type == 'ANOVA'",
                         tabsetPanel(type = "pills",
                                     tabPanel(
                                       "ANOVA",
                                       br(),
                                       verbatimTextOutput("aov.code"),
                                       br(),
                                       conditionalPanel(
                                         condition = "input.model_select.length > 0",
                                         helpText("AOV summary")
                                       ),
                                       verbatimTextOutput("aov.summary"),
                                       conditionalPanel(
                                         condition = "input.model_select.length > 0",
                                         helpText("Tables of treatment means")
                                       ),
                                       verbatimTextOutput("aov.trtmean"),
                                       uiOutput("Doe.smy")
                                     ))),
        conditionalPanel("input.Doe_type == 'Interaction Plot'",
                         tabsetPanel(type = "pills",
                                     tabPanel("Interaction Plot",
                                              plotOutput("Doe.interaction.plot"),
                                              uiOutput("Doe.interaction.plot.save")
                                              )))
        
    )
  )## end of mainPanelUI
}

###------------------###
###  Mixed Model UI  ###
###------------------###
###
###  We combine the 2 sidebarPanel() and 2 mainPanel() functions to
###  complete the UI for the Mixed Model module.

mixedModel.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/F5_DesignofExperiment/4_mixedModel-panel-null.md")
      )
    } else {
      fluidRow(
        column(4, mixedModel.sidebarPanel()),
        column(8, mixedModel.mainPanel())
      )
    }
  )
}






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
    ## select fitted models 
    
    fluidRow(column(6, selectInput("model_select",
                                   label = "Select Fitted Model",
                                   choices = c())),
             tags$div(style = "margin-top: 24px", column(2, offset = 1, actionButton(inputId = "remove.model",
                                                                                     label = "Remove",
                                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
    
    column(12, hr()),
    
    
    tags$div(column(12, radioButtons(inputId = "fit_design", label = "Select design", 
                                     choices = c("Existed design" = 1, 
                                                 "Customized design" = 2), inline = T,
                                     selected = 1))),
    
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
                     ## one-way anova model no blocking
                     conditionalPanel(condition = "input.model_design == 1",
                                      
                                      ## select response variable(numeric)
                                      fixedRow(column(12, uiOutput("oneway_nb_var1_panel"))),
                                      
                                      ## select treatment varibale(factor)
                                      fixedRow(column(12, uiOutput("oneway_nb_var2_panel"))),
                                      
                                      fluidRow(column(12, hr())),
                                      
                                      actionButton(inputId = "fit_model1",
                                                   label = "Fit Model",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      uiOutput("error_msg_d1"),
                                      
                                      fluidRow(column(12, hr()))
                     ),
                     
                     ## One-way anova (in randomized Blocks)
                     conditionalPanel(condition = "input.model_design == 2",
                                      ## select response variable(numeric)
                                      fixedRow(column(12, uiOutput("oneway_rb_var1_panel"))),
                                      
                                      ## select treatment varibale(factor)
                                      fixedRow(column(12, uiOutput("oneway_rb_var2_panel"))),
                                      
                                      ## select block factor variable
                                      fixedRow(column(12, uiOutput("oneway_rb_var3_panel"))),
                                      
                                      fluidRow(column(12, hr())),
                                      
                                      actionButton(inputId = "fit_model2",
                                                   label = "Fit Model",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      uiOutput("error_msg_d2"),
                                      
                                      fluidRow(column(12, hr()))
                                      
                     ),
                     
                     ## two-way anova (no Blocking)
                     conditionalPanel(condition = "input.model_design == 3",
                                      ## select response variable(numeric)
                                      fixedRow(column(12, uiOutput("twoway_nb_var1_panel"))),
                                      
                                      ## select first treatment varibale
                                      fixedRow(column(12, uiOutput("twoway_nb_var2_panel"))),
                                      
                                      ## select second treatment varibale
                                      fixedRow(column(12, uiOutput("twoway_nb_var3_panel"))),
                                      
                                      fluidRow(column(12, hr())),
                                      
                                      actionButton(inputId = "fit_model3",
                                                   label = "Fit Model",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      uiOutput("error_msg_d3"),
                                      
                                      fluidRow(column(12, hr()))
                     ),
                     
                     ## Two-way anova (in randomized Blocks)
                     conditionalPanel(condition = "input.model_design == 4",
                                      ## select response variable(numeric)
                                      fixedRow(column(12, uiOutput("twoway_rb_var1_panel"))),
                                      
                                      ## select first treatment variable(factor)
                                      fixedRow(column(12, uiOutput("twoway_rb_var2_panel"))),
                                      
                                      ## select second treatment variable
                                      fixedRow(column(12, uiOutput("twoway_rb_var3_panel"))),
                                      
                                      ## select blocking variable
                                      fixedRow(column(12, uiOutput("twoway_rb_var4_panel"))),
                                      
                                      fluidRow(column(12, hr())),
                                      
                                      actionButton(inputId = "fit_model4",
                                                   label = "Fit Model",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      uiOutput("error_msg_d4"),
                                      
                                      fluidRow(column(12, hr()))
                     ),
                     
                     ## Three-way anova (no Blocking)
                     conditionalPanel(condition = "input.model_design == 5",
                                      ## select response variable(numeric)
                                      fixedRow(column(12, uiOutput("threeway_nb_var1_panel"))),
                                      
                                      ## select first treatment varibale
                                      fixedRow(column(12, uiOutput("threeway_nb_var2_panel"))),
                                      
                                      ## select second treatment varibale
                                      fixedRow(column(12, uiOutput("threeway_nb_var3_panel"))),
                                      
                                      ## select third treatment variable
                                      fixedRow(column(12, uiOutput("threeway_nb_var4_panel"))),
                                      
                                      fluidRow(column(12, hr())),
                                      
                                      actionButton(inputId = "fit_model5",
                                                   label = "Fit Model",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      uiOutput("error_msg_d5"),
                                      
                                      fluidRow(column(12, hr()))
                     ),
                     
                     ## Three-way anova (in randomized Blocks)
                     conditionalPanel(condition = "input.model_design == 6",
                                      ## select response variable(numeric)
                                      fixedRow(column(12, uiOutput("threeway_rb_var1_panel"))),
                                      
                                      ## select first treatment variable(factor)
                                      fixedRow(column(12, uiOutput("threeway_rb_var2_panel"))),
                                      
                                      ## select second treatment variable
                                      fixedRow(column(12, uiOutput("threeway_rb_var3_panel"))),
                                      
                                      ## select third variable
                                      fixedRow(column(12, uiOutput("threeway_rb_var4_panel"))),
                                      
                                      ## select blocking variable
                                      fixedRow(column(12, uiOutput("threeway_rb_var5_panel"))),
                                      
                                      fluidRow(column(12, hr())),
                                      
                                      actionButton(inputId = "fit_model6",
                                                   label = "Fit Model",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      uiOutput("error_msg_d6"),
                                      
                                      fluidRow(column(12, hr()))
                     )
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
                                      fluidRow(column(6, uiOutput("own_model_fixed_panel")),
                                               
                                               column(6, selectInput(inputId="select_operation",
                                                                     label="Select an operation",
                                                                     choices=c(" ", "+", "*", 
                                                                               "(", ")", ":", "."),
                                                                     selected=" ",
                                                                     size=7,
                                                                     selectize=F))
                                      ),
                                      
                                      tags$div(style = "margin-left: -1.1em;", column(12, p("Fixed effect:"))),
                                      
                                      ## display results
                                      fluidRow(column(10, verbatimTextOutput("fixed_expr", placeholder = T)),
                                               column(2, actionButton(inputId = "delete_mm_fix",
                                                                      label = "Del",
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                      
                                      ## fluidRow(column(10, verbatimTextOutput("fixed.expression.message")))
                                      hr()
                     ),
                     ## select random effect
                     checkboxInput(inputId = "select_random_effect", 
                                   label = "Structural component (random effect)", value = FALSE),
                     
                     ## select Explanatory component
                     conditionalPanel(condition = "input.select_random_effect == 1",
                                      
                                      ## select variables and operations
                                      fluidRow(column(6, uiOutput("own_model_random_panel")),
                                               
                                               column(6, selectInput(inputId="select_operation_random",
                                                                     label="Select an operation",
                                                                     choices=c(" ", "+", "*", 
                                                                               "(", ")", ":", ".", "~", "|", "1", "/"),
                                                                     selected=" ",
                                                                     size=11,
                                                                     selectize=F))
                                      ),
                                      
                                      tags$div(style = "margin-left: -1.1em;", column(12, p("Random effect:"))),
                                      
                                      ## display results
                                      fluidRow(column(10, verbatimTextOutput("random_expr", placeholder = T)),
                                               column(2, actionButton(inputId = "delete_mm_random",
                                                                      label = "Del",
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
                     
                     fluidRow(column(12, hr())),
                     
                     actionButton(inputId = "fit_model7",
                                  label = "Fit Model",
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     
                     uiOutput("error_msg_d7"),
                     
                     fluidRow(column(12, hr()))
    )## end of fiting own model
  )## end of list
}

### now, we set up the main panel
mixedModel.mainPanel = function(){
  mainPanelUI = list(
    div(id = "doe_show",
        tabsetPanel(type = "pills",
                    id = "mm.tabs",
                    tabPanel(
                      "ANOVA",
                      br(),
                      verbatimTextOutput("aov.code"),
                      br(),
                      verbatimTextOutput("aov.summary"),
                      uiOutput("comment.save.aov"),
                      uiOutput("success_msg_aov_doe1")
                    ),
                    #                                   tabPanel(
                    #                                     "Summary",
                    #                                     br(),
                    #                                     verbatimTextOutput("model.code"),
                    #                                     br(),
                    #                                     verbatimTextOutput("model.summary"),
                    #                                     uiOutput("comment.save.summary"),
                    #                                     uiOutput("success_msg_summry_doe1")
                    #                                     ),## end of tabPanel for Model summary
                    tabPanel(
                      "Residual Plot",
                      br(),
                      plotOutput("resi.ep", height = "500px"),
                      uiOutput("comment.save.resip"),
                      uiOutput("success_msg_resip_doe1")),
                    tabPanel(
                      "Group means and comparisons",
                      br(),
                      uiOutput("select.mi"),
                      uiOutput("select.table.mi")),
                    tabPanel(
                      "Preview report",
                      br(),
                      uiOutput("report.pre"))
        )
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






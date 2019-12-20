survey.design.sidebar =  function(){
  list(
    selectInput("svytype", label = "Select survey design",
                c = list("Specify design" = "survey",
                         "Specify replicate design" = "replicate",
                         "Post stratify" = "frequency")),
    conditionalPanel("input.svytype == 'survey'",
                     helpText("The design generated is always used with the current data set. 
             Please make sure the right data set is selected first. In case 
             the data is changed, the current design object is lost."),
                     fluidRow(column(12,selectInput("stratVar",
                                                    label="Strata variable",
                                                    choices=c(" ",
                                                              colnames(get.data.set())),
                                                    selected = " ",
                                                    selectize = F))),
                    
                     fluidRow(column(12,selectInput("clus1Var",
                                                    label="1st stage clustering variable",
                                                    choices=c(" ",colnames(get.data.set())),
                                                    selected = " ",
                                                    selectize = F)),
                              column(12, selectInput("clus2Var",
                                                     label="2nd stage clustering variable",
                                                     choices=c(" ",colnames(get.data.set())),
                                                     selected = " ",
                                                     selectize = F)),
                              column(12,checkboxInput("nestChk",
                                                      label="Use nested sampling",
                                                      value=F)),
                              column(12,selectInput("wtVar",
                                                    label="Weighting variable",
                                                    choices=c(" ",
                                                              colnames(get.data.set())),
                                                    selected = " ",
                                                    selectize = F)),
                              
                              column(12,selectInput("fpcVar",
                                                    label="Finite population correction",
                                                    choices= c(" ", colnames(get.data.set())),
                                                    selected = " ",
                                                    selectize = F))
                              ),
                     fluidRow(column(6,actionButton("create.design","Create design",
                                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                              column(6,actionButton("remove.design","Remove design",
                                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))),br(),
                     help.display('Create design','create_design_help',
                                  "panels/D6_SurveyDesign/3_survey.design.help.md"),
                     br())
  )
  
}

create.design.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Create design','create_design_help',
                                "panels/D6_SurveyDesign/3_survey.design.help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(survey.design.sidebar()),
      mainPanel(verbatimTextOutput("create.design.summary"))
    )
  }
}

survey.design.sidebar =  function(){
  list(
    helpText("The design generated is always used with the current data set. 
             Please make sure the right data set is selected first. In case 
             the data is changed, the current design object is lost."),
    fixedRow(column(12,selectInput("strata.select",
                                   label="Strata variable",
                                   choices=c("none",
                                             colnames(get.data.set()))))),
    helpText("Select the clustering variables. In case of multiple stage 
             clustering, select more than one."),
    fixedRow(column(12,selectInput("clustering.select",
                                   label="Clustering variables",
                                   choices=c("none","1",colnames(get.data.set())),
                                   multiple=T,
                                   selectize=T,
                                   selected="none"))),
    fixedRow(column(12,selectInput("weights.select",
                                   label="Weighting variable",
                                   choices=c("none",
                                             colnames(get.data.set()))))),
    fixedRow(column(12,checkboxInput("nest.check",
                                     label="Use nested sampling",
                                     value=F))),
    helpText("Select finite population correction variables. In case of 
             multiple stage clustering, select more than one."),
    fixedRow(column(12,selectInput("fpc.select",
                                   label="Finite population correction",
                                   choices=colnames(get.data.set()),
                                   multiple=T,
                                   selectize=T,
                                   selected="none"))),
    fixedRow(column(6,actionButton("create.design","Create design")),
             column(6,actionButton("remove.design","Remove design"))),br(),
    verbatimTextOutput("design.success.text"),br(),
    help.display('Create design','create_design_help',
                 "panels/D6_SurveyDesign/3_survey.design.help.md"),
    br())
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

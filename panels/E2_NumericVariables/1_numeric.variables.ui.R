numeric.variables.panel = function(data.set){
  temp = NULL
  if(is.null(data.set)){
    temp = list(list(help.display('Numeric variables','numeric_variables',
                                  "panels/E2_NumericVariables/3_numeric.variables.help.md"),
                     br(),HTML("")),
                h1("Please select or import a data set."))
  }else{
    choices=c("Transform variables",
              "Form Class interval",
              "Rank numeric")
    temp = list(list(selectInput(inputId="numeric_variables_select",
                                 label="Numeric variables",
                                 choices=choices,
                                 selectize=F),
                     conditionalPanel("input.numeric_variables_select=='Transform variables'",
                                      uiOutput("transform.columns.side")),
                     conditionalPanel("input.numeric_variables_select=='Form Class interval'",
                                      uiOutput("form.class.interval.side")),
                     conditionalPanel("input.numeric_variables_select=='Rank numeric'",
                                      uiOutput("rank.numeric.side"))
                     ),
                list(conditionalPanel("input.numeric_variables_select=='Transform variables'",
                                      uiOutput("transform.columns.main")),
                     conditionalPanel("input.numeric_variables_select=='Form Class interval'",
                                      uiOutput("form.class.interval.main")),
                     conditionalPanel("input.numeric_variables_select=='Rank numeric'",
                                      uiOutput("rank.numeric.main"))))
  }
  if(!is.null(temp)){
    sidebarLayout(
      sidebarPanel(temp[[1]]),
      mainPanel(temp[[2]])
    )  
  }
}

get.transform.sidebar =  function(data.set){
  list(selectInput("select.columns.transform", "Select Columns", choices = c("",colnames(data.set)),multiple=T,selectize=T),br(),
       selectInput("select.transform", "Select Transformation", 
                   choices = c("", "change to factor","add","subtract","multiply","divide","log","root","square","abs","center",
                               "standardize","median split","reverse-coding","copy","change sign"),
                   multiple=F,selectize=F),br(),
       actionButton("transform","Transform"),br(),br(),textOutput("status"),br(),br(),
       help.display('Modify data','transform_columns',"panels/E2_NumericVariables/4_transform.variables.help.md"),br())
}

get.transform.main = function(){
  dataTableOutput(outputId="table_part")
}

get.form.class.interval.side = function(data.set){
  list(helpText("Please select a column and specify the number of intervals and the 
                method of forming class intervals. The output can be poduced in two 
                different formats. See help for more information."),
       selectInput(inputId="form.class.interval.column.select",
                   label="Form Class interval",
                   choices=get.numeric.column.names(data.set),
                   selected=1),
       textInput(inputId="form_class_interval_number",
                 label="Number of intervals",
                 value = 2),
       checkboxInput("form.class.interval.format",
                     label="Uncheck to change the format from (open left, closed right]
                     to [closed left, open right)",
                     value =T),
       selectInput(inputId="form_class_interval_method_select",
                   label="Select method to form Class intervals",
                   choices=c(c("equal.width",
                               "equal.count",
                               "specified")),
                   selected=1),
       conditionalPanel("input.form_class_interval_method_select=='specified'",
                        uiOutput("specified.range")),
       checkboxInput("form_class_interval_labels_provide",label="Provide custom labels for the intervals."),
       conditionalPanel("input.form_class_interval_labels_provide==true",
                        uiOutput("labels.provide")),
       actionButton(inputId="form.class.interval.submit",
                    label="Form Class interval"),br(),br(),
       help.display('Form Class interval','form_class_interval',
                    "panels/E2_NumericVariables/5_form.class.interval.help.md")
  )
}

get.form.class.interval.main = function(){
  dataTableOutput("form.class.interval.table")
}

rank.numeric.sidebar = function(data.set){
  list(selectInput("rank.numeric.select.column",
                   label="Select one or more columns",
                   selected=NULL,multiple=T,
                   choices=get.numeric.column.names(data.set)),
       br(),
       actionButton("rank.numeric.submit",label="Rank Numeric"),
       br(),br(),
       help.display('Rank Numeric','rank_numeric',
                    "panels/E2_NumericVariables/6_rank.numeric.help.md"))
}

rank.numeric.main = function(){
  dataTableOutput("rank.numeric.table")
}
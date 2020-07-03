numeric.variables.panel = function(data.set){
  temp = NULL
  if(is.null(data.set)){
    temp = list(list(help.display('Numeric variables','numeric_variables',
                                  "panels/E2_NumericVariables/3_numeric.variables.help.md"),
                     br(),HTML("")),
                h1("Please select or import a data set."))
  }else{
    choices=c("Transform variables",
              "Standardise variables",
              "Form class intervals",
              "Rank numeric",
              "Convert to categorical type")
    temp = list(list(h5(strong("Numeric variables")),
                     selectInput(inputId="numeric_variables_select1",
                                 label=NULL,
                                 choices=choices,
                                 selectize=F),
                     conditionalPanel("input.numeric_variables_select1=='Transform variables'",
                                      uiOutput("transform.columns.side")),
                     conditionalPanel("input.numeric_variables_select1=='Standardise variables'",
                                      uiOutput("standardise.variables.side")),
                     conditionalPanel("input.numeric_variables_select1=='Form class intervals'",
                                      uiOutput("form.class.interval.side")),
                     conditionalPanel("input.numeric_variables_select1=='Rank numeric'",
                                      uiOutput("rank.numeric.side")),
                     conditionalPanel("input.numeric_variables_select1=='Convert to categorical type'",
                                      uiOutput("convert.to.cate.side"))
    ),
    list(conditionalPanel("input.numeric_variables_select1=='Transform variables'",
                          uiOutput("transform.columns.main")),
         conditionalPanel("input.numeric_variables_select1=='Standardise variables'",
                          DTOutput("standardise.variables.table")),
         conditionalPanel("input.numeric_variables_select1=='Form class intervals'",
                          uiOutput("form.class.interval.main")),
         conditionalPanel("input.numeric_variables_select1=='Rank numeric'",
                          uiOutput("rank.numeric.main")),
         conditionalPanel("input.numeric_variables_select1=='Convert to categorical type'",
                          DTOutput("convert.to.cate.table"))))
  }
  if(!is.null(temp)){
    sidebarLayout(
      sidebarPanel(temp[[1]]),
      mainPanel(temp[[2]])
    )  
  }
}

get.transform.sidebar =  function(data.set){
  choice1 = colnames(data.set)
  type = unname(sapply(data.set, class))
  list(selectInput("select.columns.transform", "Select Columns", choices = choice1[type %in% c("numeric", 'integer')], multiple=F,selectize=F),
       selectInput("select.transform", "Select Transformation", 
                   choices = c("LOG (e)" = "log",  "LOG (10)" = "log10",
                               "EXPONENTIAL" = "exp", "SQUARE (X^2)" = "square",
                               "SQUARE ROOT" = "sqrt", "RECIPROCAL (1/X)" = "reciprocal"),
                   multiple=F),
       actionButton("transform","Transform",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),br(),br(),textOutput("status"),br(),br(),
       help.display('Modify data','transform_columns',"panels/E2_NumericVariables/4_transform.variables.help.md"),br())
}

get.transform.main = function(){
  DTOutput(outputId="table_part")
}

get.form.class.interval.side = function(data.set){
  list(helpText("Please select a column and specify the number of intervals and the 
                method of forming class intervals. The output can be poduced in two 
                different formats. See help for more information."),
       selectInput(inputId="form.class.interval.column.select",
                   label="Choose variable",
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
                    label="Proceed",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),br(),br(),
       help.display('Form Class interval','form_class_interval',
                    "panels/E2_NumericVariables/5_form.class.interval.help.md")
  )
}

get.form.class.interval.main = function(){
  DTOutput("form.class.interval.table")
}



rank.numeric.sidebar = function(data.set){
  list(selectInput("rank.numeric.select.column",
                   label="Rank the numerical variables X (vector, matrix)",
                   multiple=T,
                   choices=get.numeric.column.names(data.set),
                   selectize = F,
                   size = 7),
       actionButton("rank.numeric.submit",label="Rank",
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
       br(),br(),
       help.display('Rank Numeric','rank_numeric',
                    "panels/E2_NumericVariables/6_rank.numeric.help.md"))
}

rank.numeric.main = function(){
  DTOutput("rank.numeric.table")
}
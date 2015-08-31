num.select.panel = function(num.vars,data.set){
  if(!all(is.convertable.numeric(num.vars))){
    num.vars = 1
  }
  if(as.numeric(num.vars)>ncol(data.set)){
    num.vars=ncol(data.set)
  }
  lapply(1:num.vars,function(index){
    fixedRow(
      column(8,selectInput(inputId=paste0("sort",index),
                            choices=c("",colnames(data.set)),
                            selected=1,
                            label=paste("Variable",index))),
      column(3,checkboxInput(inputId=paste0("increasing",index),
                             label="Increasing"))
    )
  })
}

sort.variables.sidebar =  function(){
  c(list(textInput(inputId="num_columns_sort",label="How many variables to sort on?",value="1")),
    list(uiOutput("num.select")),
    list(actionButton("sort_vars","Sort variables"),br(),br(),
         help.display('Sort data by variables','sort_vars_help',"panels/D2_SortDataByVariables/3_sort.variables.help.md"),
         br()))
}

sort.variables =function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Sort data by variables','sort_vars_help',"D2_SortDataByVariables/3_sort.variables.help.md")),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(sort.variables.sidebar()),
      mainPanel(dataTableOutput("sort.table"))
    )
  }
}
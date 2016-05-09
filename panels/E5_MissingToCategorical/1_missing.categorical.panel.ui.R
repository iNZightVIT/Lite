missing.categorical.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Missing to categorical','missing_categorical',
                                "panels/E5_MissingToCategorical/3_missing.categorical.help.md")),
      mainPanel(h1("Please select or import a data set.")))
  }else{
    sidebarLayout(
      sidebarPanel(helpText("Select one or more columns to get an idea on how 
                            missing values are distributed throughout the 
                            selected data set. If the \"SUBMIT DATA\" button is 
                            pressed, additional columns will be added for all 
                            selected columns. The added columns will be binary 
                            factor variables. NA values are replace with 
                            \"missing\" and all others are set to \"observed\"."),
                   selectInput("missing.categorical.column.select",
                               label="Select columns",
                               choices=colnames(data.set),
                               selected=1,
                               multiple=T),
                   actionButton("missing.categorical.submit",
                                label="Create the new variables"), br(), br(),
                   verbatimTextOutput("message.newvariablesadded"), br(),br(),
                   help.display('Missing to categorical','missing_categorical',
                                "panels/E5_MissingToCategorical/3_missing.categorical.help.md")),
      mainPanel(helpText("The table shows the distribution of missing values in 
                         the data. All possible combiantions of NA (missing) and 
                         not NA (observed) are shown. The row count of how often 
                         the row combination is seen in the data is in the last 
                         column of the table" ),
                dataTableOutput("missing.categorical.table")))
  }
}
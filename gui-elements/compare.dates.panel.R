help.compare.dates = function(){
  helpModal('Compare dates','help_compare_dates',inclMD("gui-elements/notes/compare.dates.md"))
}

get.sidebar.comp.dates = function(){
  is.dates = test.for.dates()
  if(any(is.dates)){
    list(
      helpText("Select columns to remove. The table on the left shows what the data will look like when the 
             \"Remove columns\" button is pressed. If the selection is empty, there is no columns to be 
             deleted anymore."),
      selectInput("sel.compare.dates","Compare dates",choices=c("",colnames(data)[is.dates]),multiple=T,selectize=T,selected=""),br(),
      actionButton(inputId="compare_dates",label="Add compare dates column",),
      br(),br(),help.compare.dates(),"HELP"
    )
  }else{
    list(
      helpText("No dates detected in the current data set."),
      br(),br(),help.compare.dates(),"HELP"
    )
  }
}

compare.dates.panel = function(){
  sidebarLayout(
    sidebarPanel(get.sidebar.comp.dates()),
    mainPanel( dataTableOutput("comp.dates.table")
    )
  )
}
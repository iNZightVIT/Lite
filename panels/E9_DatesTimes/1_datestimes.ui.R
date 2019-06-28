
dates.times.sidebar = function(){
  list(h5(strong("Select dates/times manipulation options")),
       
       selectInput(inputId = "select_datestimes",
                   label = NULL,
                   choices = c("Convert to a Date-Time variable", "Aggregate datetimes to monthly or quarterly"),
                   selectize = FALSE,
                   multiple = FALSE),
       
       conditionalPanel("input.select_datestimes == 'Convert to a Date-Time variable'",
                        uiOutput("convert_datestimes_panel")),
       
       conditionalPanel("input.select_datestimes == 'Aggregate datetimes to monthly or quarterly'",
                        uiOutput("aggregate_datestimes_panel")))
  
}



dates.times.panel =function(){
  if(is.null(get.data.set())){
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(dates.times.sidebar()),
      mainPanel(conditionalPanel("input.select_datestimes == 'Convert to a Date-Time variable'",
                                 dataTableOutput("convert.datestimes.table")),
                                
                conditionalPanel("input.select_datestimes == 'Aggregate datetimes to monthly or quarterly'",
                                 h5(strong("Original dataset"))))
    )
  }
}
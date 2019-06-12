
unite.columns.sidebar = function(){
  list(h5(strong("Unite columns")),
       
       selectInput(inputId="select_unite_columns",
                label = "Select columns to unite",
                choices = colnames(get.data.set()),
                selectize = FALSE,
                multiple = T,
                size = 10),
       
       textInput("name_unite_columns",
                 label = NULL,
                 value = ""),
       textInput("sep_unite_columns",
                 label = "Enter the separator to use between values",
                 value = "_"),
       
       
       fixedRow(column(3, actionButton("preview_unitecolumns_button", "Preview",
                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                column(3, actionButton("unitecolumns_dataset_button", "Unite",
                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")))
       )
}



unite.columns.panel =function(){
  if(is.null(get.data.set())){
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(unite.columns.sidebar()),
      mainPanel(h5(strong("Original dataset")),
                dataTableOutput("unitecolumns.table"),
                h5(strong("New dataset")),
                dataTableOutput("previewunitecolumns.table"))
    )
  }
}
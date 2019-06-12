
reshape.dataset.sidebar = function(){
  list(h5(strong("Reshape Dataset")),
       
       selectInput(inputId="select_reshape_mode",
                label = "Select reshape mode",
                choices = c("", "Wide to long", "Long to wide"),
                selectize = FALSE,
                multiple = F),
       
       uiOutput("reshape_main_panel"),
       
       fixedRow(column(3, actionButton("preview_dataset_button", "Preview",
                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                column(3, actionButton("reshape_dataset_button", "Reshape",
                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")))
       )
}



reshape.dataset.panel =function(){
  if(is.null(get.data.set())){
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("Please select or import a data set.")
      )
    )
  }else{
    sidebarLayout(
      sidebarPanel(reshape.dataset.sidebar()),
      mainPanel(h5(strong("Original dataset")),
                dataTableOutput("reshape.table"),
                h5(strong("New dataset")),
                dataTableOutput("preview.table"))
    )
  }
}
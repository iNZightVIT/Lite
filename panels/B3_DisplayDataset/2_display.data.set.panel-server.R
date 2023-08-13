## "Current data" - presents currently selected data to user.
output$current.text <- renderText({
  input$selector
  if (!is.null(get.data.set())) {
    paste0("Current selected data: ", get.data.name())
  } else {
    "No data selected!"
  }
})

if(LITE2) {
  output$data.sample.info <- renderText({
    if (!is.null(get.data.set()) && !is.null(get.data.name())) {
      paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
    }
  })
}

output$current.data <- renderUI({
  current.data()
})

# TODO: check
output$current <- renderDT({
  input$selector
  if(!is.null(values$data.set)){
    values$data.sample
  }
}, options =
  list(lengthMenu = c(10, 30, 50), pageLength = 10,
       columns.defaultContent = "NA", scrollX = TRUE))
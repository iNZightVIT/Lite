## "Current data" - presents currently selected data to user.
output$current.text <- renderText({
  input$selector
  if (!is.null(get.data.set())) {
    paste0("Current selected data: ", get.data.name())
  } else {
    "No data selected!"
  }
})

output$data.sample.info <- renderText({
  sample_info_cas()
})

output$current.data <- renderUI({
  current.data()
})

output$current <- renderDT(
  {
    input$selector
    get.data.set.display()
  },
  options =
    list(
      lengthMenu = c(10, 30, 50), pageLength = 10,
      columns.defaultContent = "NA", scrollX = TRUE
    )
)

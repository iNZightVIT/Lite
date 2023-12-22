output$VIT.panel <- renderUI({
  shinyjs::runjs(paste0('window.location.href = "https://vit.inzight.nz";'))

  x <- list(
    "Click",
    a("here", href = "https://vit.inzight.nz"),
    "if you are not redirected automatically."
  )
  h4(x)
})

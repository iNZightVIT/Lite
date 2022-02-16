output$vit.panel <- renderUI({
  VIT.panel.ui(get.data.set())
})

## get data set
sv.data <- reactive({
  #   values$data.set
  get.data.set()
})

output$web <- renderUI({
  df <- as.data.frame(sv.data())
  json_output <- jsonlite::toJSON(df, na = "null") # need this for VITonline
  
  form_html <-  htmltools::div(
    htmltools::tags$form(method = "post", 
                         action = "https://vit-test123.herokuapp.com/randomisationTest/RVar.php?file=JSON",
                         target="_blank",
                         htmltools::tags$input(type = "hidden", 
                                               name = "p_data", 
                                               value = json_output),
                         htmltools::tags$input(type = "submit", 
                                               value="Jump to VIT online (test) randomisation test")
    )
  )
  
  htmltools::tagList(form_html)
})
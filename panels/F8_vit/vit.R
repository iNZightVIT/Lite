# output$VIT.panel <- renderUI({
# 	verticalLayout(
# 		actionButton("to_vit_randomisation_test", label = "VIT Randomisation Test"),
# 		actionButton("to_vit_sampling_variation", label = "VIT Sampling Variation"),
# 		actionButton("to_vit_bootstrap", label = "VIT Bootstrap"),
# 		actionButton("to_vit_randomisation_variation", label = "VIT Randomisation Variation")
# 		)
# })
# 
# sv.data <- reactive(get.data.set())
# 
# observeEvent(input$to_vit_randomisation_test, {
# 	df <- as.data.frame(sv.data())
# 	json_output <- jsonlite::toJSON(df, na = "null")
# 	session$sendCustomMessage("to_vit_randomisation_test", json_output)
# })
# observeEvent(input$to_vit_sampling_variation, {
# 	df <- as.data.frame(sv.data())
# 	json_output <- jsonlite::toJSON(df, na = "null")
# 	session$sendCustomMessage("to_vit_sampling_variation", json_output)
# })
# observeEvent(input$to_vit_bootstrap, {
# 	df <- as.data.frame(sv.data())
# 	json_output <- jsonlite::toJSON(df, na = "null")
# 	session$sendCustomMessage("to_vit_bootstrap", json_output)
# })
# observeEvent(input$to_vit_randomisation_variation, {
# 	df <- as.data.frame(sv.data())
# 	json_output <- jsonlite::toJSON(df, na = "null")
# 	session$sendCustomMessage("to_vit_randomisation_variation", json_output)
# })


output$VIT.panel = renderUI({
  shinyjs::runjs(paste0('window.location.href = "https://vit.inzight.nz";'))
  
  x = list(
    "Click",
    a("here", href = "https://vit.inzight.nz"),
    "if you are not redirected automatically."
  )
  h4(x)
})












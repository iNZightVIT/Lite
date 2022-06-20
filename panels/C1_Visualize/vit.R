output$`vit-frame` <- renderUI({
	tags$iframe(name="vit-frame", title="VIT Frame", class="fill-frame")
})

observe({if (length(input$`vit-type`)) {
	test <- switch(input$`vit-type`,
		       bootstrap = "bootstrap/bootstrap",
		       random = "randomisationTest/RVar")
	col1 <- switch(input$vari1, none=NULL, input$vari1)
	col2 <- switch(input$vari2, none=NULL, input$vari2)
	var1 <- if (!is.null(col1)) paste0(col1, if (is.numeric(col1)) ",n" else ",c") else col1
	var2 <- if (!is.null(col2)) paste0(col1, if (is.numeric(col2)) ",n" else ",c") else col2
	data <- get.data.set()[,c(col1, col2), drop=FALSE]
	out <- list(data = data, test = test, vars = c(var1, var2))
	json_output <- jsonlite::toJSON(out, na = "null")
	session$sendCustomMessage("send_to_vit_frame", json_output)
}})

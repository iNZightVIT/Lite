output$`vit-frame` <- renderUI({
	tags$iframe(name="vit-frame", title="VIT Frame", class="fill-frame")
})

observeEvent(input$`vit-type`, {
	data <- get.data.set()
	json_output <- jsonlite::toJSON(data, na = "null")
	# capture.output(str(json_output), file=stderr())
	session$sendCustomMessage("data_for_randomisation", json_output)
})

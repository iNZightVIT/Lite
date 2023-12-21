observe({
    if (identical(input$plot_selector, "vit")) {
        test <- switch(input$`vit-type`,
            bootstrap = "bootstrap/bootstrap",
            random = "randomisationTest/RVar"
        )
        col1 <- if (identical(input$vari1, "none")) NULL else input$vari1
        col2 <- if (identical(input$vari2, "none")) NULL else input$vari2
        var1 <- if (!is.null(col1)) paste0(col1, if (is.numeric(col1)) ",n" else ",c") else col1
        var2 <- if (!is.null(col2)) paste0(col1, if (is.numeric(col2)) ",n" else ",c") else col2
        data <- get.data.set()
        subset <- if (any(c(col1, col2) %in% colnames(data))) data[, c(col1, col2), drop = FALSE] else NULL
        out <- list(data = subset, test = test, vars = c(var1, var2))
        json_output <- jsonlite::toJSON(out, na = "null")
        session$sendCustomMessage("send_to_vit_frame", json_output)
    }
})

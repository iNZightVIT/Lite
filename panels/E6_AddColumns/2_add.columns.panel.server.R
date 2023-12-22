##  Manipulate variables -> add columns : paste in data to add as additional column.

observe({
  input$add_column
  isolate({
    temp <- get.data.set()
    if (!is.null(get.data.set()) && !is.null(input$new.column) &&
      input$add_column > 0) {
      colu <- strsplit(input$new.column, "\n", fixed = T)[[1]]
      if (length(colu) == 1) {
        colu <- strsplit(input$new.column, ",", fixed = T)[[1]]
      }
      if (length(colu) < nrow(get.data.set())) {
        colu <- rep(colu, length.out = nrow(get.data.set()))
      }
      if (length(colu) > nrow(get.data.set())) {
        colu <- colu[1:nrow(get.data.set())]
      }
      if (input$convert.numeric &&
        !any(suppressWarnings(is.na(as.numeric(colu))))) {
        colu <- as.numeric(colu)
      }
      count <- 1
      name <- "add.column1"
      while (name %in% colnames(get.data.set())) {
        count <- count + 1
        name <- paste0("add.column", count)
      }
      temp <- cbind(get.data.set(), temp.column = colu)
      colnames(temp)[which(colnames(temp) %in% "temp.column")] <- name
      #               temp
    }
    if (!is.null(temp)) {
      updatePanel$datachanged <- updatePanel$datachanged + 1
      values$data.set <- temp
      updateTextInput(session, inputId = "new.column", value = "")
    }
  })
})

output$add.table <- renderDT(
  {
    temp <- get.data.set()
    input$new.column
    input$add_column
    isolate({
      if (!is.null(get.data.set()) && !is.null(input$new.column)) {
        colu <- strsplit(input$new.column, "\n", fixed = T)[[1]]
        if (length(colu) == 1) {
          colu <- strsplit(input$new.column, ",", fixed = T)[[1]]
        }
        if (length(colu) < nrow(get.data.set())) {
          colu <- rep(colu, length.out = nrow(get.data.set()))
        }
        if (length(colu) > nrow(get.data.set())) {
          colu <- colu[1:nrow(get.data.set())]
        }
        NAs <- which(is.na(colu))
        if (length(NAs) > 0 && length(colu[-NAs]) > 0) {
          temp.colu <- as.numeric(colu[-NAs])
          if (!any(is.na(temp.colu))) {
            colu <- as.numeric(colu)
          }
        }
        count <- 1
        name <- "add.column1"
        while (name %in% colnames(get.data.set())) {
          count <- count + 1
          name <- paste0("add.column", count)
        }
        temp <- cbind(get.data.set(), temp.column = colu)
        colnames(temp)[which(colnames(temp) %in% "temp.column")] <- name
        temp
      }
      temp
    })
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)

output$add.columns <- renderUI({
  add.columns.panel(get.data.set())
})

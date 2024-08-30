rename_variables <- reactiveValues(
  num.cols = 0
)



output$rename_variables_two_columns <- renderUI({
  get.data.set()
  ret <- list()
  isolate({
    old.variable.names <- colnames(get.data.set())
    num.variables <- length(old.variable.names)
    rename_variables$num.cols <- num.variables
    ret <- vector(mode = "list", length = num.variables + 1)
    ret[[1]] <- fixedRow(
      column(6, h5("Old Variables")),
      column(6, h5("New Variables"))
    )
    for (i in 1:num.variables) {
      ret[[i + 1]] <- fixedRow(
        column(6, h5(old.variable.names[i])),
        column(6, textInput(paste("variablenames", i, sep = ""),
          label = NULL,
          value = old.variable.names[i]
        ))
      )
    }
  })
  ret
})


observe({
  input$rename_variables_two_columns_but
  isolate({
    tryCatch({
      if (rename_variables$num.cols > 0 &&
        !is.null(input$rename_variables_two_columns_but) &&
        input$rename_variables_two_columns_but > 0) {
        old.variable.names <- colnames(get.data.set())
        indexes1 <- grep("^variablenames[0-9]+$", names(input))
        indexes1 <- names(input)[indexes1]
        indexes1 <- indexes1[indexes1 %in% paste0(
          "variablenames", 1:length(old.variable.names)
        )]
        idxmch <- as.numeric(gsub("variablenames", "", indexes1))
        namelist <- list()
        for (i in 1:rename_variables$num.cols) {
          if (!is.null(eval(parse(text = paste0("input$variablenames", i)))) &&
            length(eval(parse(text = paste0("input$variablenames", i)))) > 0 &&
            !grepl("^\\s*$", eval(parse(
              text =
                paste0("input$variablenames", i)
            )))) {
            namelist[i] <- colnames(get.data.set())[idxmch[i]]
            names(namelist)[i] <- make_names(input[[indexes1[i]]])
          }
        }
        changed <- sapply(seq_along(namelist), function(i) {
          names(namelist)[i] != namelist[i]
        })
        namelist <- namelist[changed]
        if (!is.null(namelist)) {
          temp <- iNZightTools::rename_vars(get.data.set(), namelist)
        }
        if (!is.null(temp)) {
          updatePanel$datachanged <- updatePanel$datachanged + 1
          values$data.set <- as.data.frame(temp)
          values <- sample_if_cas(
            rvalues = values, d = temp,
            new_sample = FALSE
          )
          ## code history
          code <- tidy_assign_pipe(gsub(
            "get.data.set\\()",
            code.save$name, iNZightTools::code(values$data.set)
          ))
          code.save$variable <- c(code.save$variable, list(c("\n", code, "\n")))
        }
      }
    }, error = function(e) {
      print(e)
    }, finally = {})
  })
})

output$rename.variables.table <- renderDT(
  {
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = T
  )
)


output$rename.variables <- renderUI({
  rename.variables.panel()
})

output$rename.var.data.sample.info <- renderText({
  sample_info_cas()
})

##  Row operations (Perform row operations) --> Sort data by variables

output$sort1_panel <- renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "sort1",
      label = NULL,
      choices = c("", colnames(get.data.set())),
      selected = input$sort1,
      selectize = F
    )
  })
})

output$sort2_panel <- renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "sort2",
      label = NULL,
      choices = c("", colnames(get.data.set())),
      selected = input$sort2,
      selectize = F
    )
  })
})

output$sort3_panel <- renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "sort3",
      label = NULL,
      choices = c("", colnames(get.data.set())),
      selected = input$sort3,
      selectize = F
    )
  })
})

output$sort4_panel <- renderUI({
  get.data.set()
  isolate({
    selectInput(
      inputId = "sort4",
      label = NULL,
      choices = c("", colnames(get.data.set())),
      selected = input$sort4,
      selectize = F
    )
  })
})



observe({
  input$sort_vars
  isolate({
    vars <- NULL
    sort.type <- NULL
    if (!is.null(input$sort1) && input$sort1 != "") {
      vars <- c(vars, input$sort1)
      sort.type <- c(sort.type, ifelse(input$sort1_order == 1, TRUE, FALSE))
    }
    if (!is.null(input$sort2) && input$sort2 != "") {
      if (!input$sort2 %in% vars) {
        vars <- c(vars, input$sort2)
        sort.type <- c(sort.type, ifelse(input$sort2_order == 1, TRUE, FALSE))
      }
    }
    if (!is.null(input$sort3) && input$sort3 != "") {
      if (!input$sort3 %in% vars) {
        vars <- c(vars, input$sort3)
        sort.type <- c(sort.type, ifelse(input$sort3_order == 1, TRUE, FALSE))
      }
    }
    if (!is.null(input$sort4) && input$sort4 != "") {
      if (!input$sort4 %in% vars) {
        vars <- c(vars, input$sort4)
        sort.type <- c(sort.type, ifelse(input$sort4_order == 1, TRUE, FALSE))
      }
    }
    if (!is.null(vars)) {
      temp <- iNZightTools::sort_vars(get.data.set(), vars, sort.type)
      if (!is.null(temp)) {
        ## save code
        code.save$dataname <- paste(code.save$name, "sorted", sep = ".")
        code <- code.data.modify(code.save$dataname, temp)
        code.save$variable <- c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged <- updatePanel$datachanged + 1

        values$data.set <- as.data.frame(temp)

        values <- sample_if_cas(rvalues = values, d = values$data.set)

        code.save$name <- code.save$dataname
        values$data.name <- code.save$dataname
        updateSelectInput(session,
          "sort1",
          selected = 0,
          choices = colnames(get.data.set())
        )
        updateSelectInput(session,
          "sort2",
          selected = 0,
          choices = colnames(get.data.set())
        )
        updateSelectInput(session,
          "sort3",
          selected = 0,
          choices = colnames(get.data.set())
        )
        updateSelectInput(session,
          "sort4",
          selected = 0,
          choices = colnames(get.data.set())
        )
      }
    }
  })
})

output$sort.table <- renderDT(
  {
    input$sort_vars
    get.data.set.display()
  },
  options = list(
    lengthMenu = c(5, 30, 50),
    pageLength = 5,
    columns.defaultContent = "NA",
    scrollX = T
  )
)

output$sort.variables <- renderUI({
  sort.variables.panel()
})

output$sort.table.data.sample.info <- renderText({
  sample_info_cas()
})

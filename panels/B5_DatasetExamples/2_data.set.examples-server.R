## "Switch data" - 'switches' to a different data set.

## loads and updates the switch data table Panel
output$switch.data.panel <- renderUI({
  updatePanel$datachanged
  input$remove_set
  isolate({
    switch.data.panel(
      get.data.set(), get.data.dir.global(),
      get.data.dir.imported()
    )
  })
})

output$temp_table <- renderDT(
  {
    if (!is.null(input[[input$data_select]])) {
      if ("Imported" %in% input$data_select) {
        as.data.frame(
          load.data(
            get.data.dir.imported(),
            strsplit(input[[input$data_select]],
              "==>",
              fixed = T
            )[[1]][length(strsplit(input[[input$data_select]],
              "==>",
              fixed = T
            )[[1]])]
          )[[2]],
          stringsAsFactors = TRUE
        )
      } else {
        as.data.frame(
          load.data(
            get.data.dir.global(),
            strsplit(input[[input$data_select]],
              "==>",
              fixed = T
            )[[1]][length(strsplit(input[[input$data_select]],
              "==>",
              fixed = T
            )[[1]])]
          )[[2]],
          stringsAsFactors = TRUE
        )
      }
    } else {
      NULL
    }
  },
  options = list(
    lengthMenu = c(5, 30, 50), pageLength = 5,
    columns.defaultContent = "NA", scrollX = TRUE,
    columnDefs = list(list(className = "dt-center", targets = "_all")), filter = "bottom"
  )
)

set_to_change_reac <- reactive({
  if (is.null(input[[input$data_select]])) {
    "No data to select!"
  } else {
    temp <- NULL
    if ("Imported" %in% input$data_select) {
      if (!file.exists(get.data.dir.imported()) &&
        file.writable(dirname(get.data.dir.imported()))) {
        dir.create(get.data.dir.imported())
      } else if (!file.exists(get.data.dir.imported()) &&
        !file.writable(dirname(get.data.dir.imported()))) {
        warning(paste("Directory : ", get.data.dir.imported(),
          " : is not writable. Reset Imported dir
                      to global dir",
          sep = ""
        ))
      }
      temp <- load.data(get.data.dir.imported(), strsplit(input[[input$data_select]], "==>", fixed = T)[[1]]
      [length(strsplit(input[[input$data_select]], "==>", fixed = T)[[1]])])[[2]]
    } else {
      temp <- load.data(get.data.dir.global(), strsplit(input[[input$data_select]], "==>", fixed = T)[[1]]
      [length(strsplit(input[[input$data_select]], "==>", fixed = T)[[1]])])[[2]]
    }
    if (is.null(temp[[1]]) & is.null(temp[[2]])) {
      "No data to select!"
    } else {
      paste0("Data: ", input[[input$data_select]])
    }
  }
})

output$set_to_change <- renderText({
  input[[input$data_select]]
  set_to_change_reac()
})

col_names_show_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Column names: ", paste(colnames(get.data.set()), collapse = ", "))
  } else {
    ""
  }
})

output$col_names_show <- renderText({
  input$change_set
  input$selector
  col_names_show_reac()
})

change_col_dim_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Selected data number of columns is: ", dim(get.data.set())[2])
  } else {
    ""
  }
})

output$col_dimension_show <- renderText({
  input$change_set
  input$selector
  input$selector
  change_col_dim_reac()
})

# change_row_dim_reac <- reactive({
#  input$change_set
#  input$selector
#  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
#    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
#  } else {
#    ""
#  }
# })

# output$row_dimension_show <- renderText({
#  input$change_set
#  input$selector
#  change_row_dim_reac()
# })

change_data_name_reac <- reactive({
  input$change_set
  input$selector
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("Selected data set: ", get.data.name())
  } else {
    "No data selected!"
  }
})

output$data_name_show <- renderText({
  input$change_set
  input$selector
  change_data_name_reac()
})


pkgname <- reactive({
  pkgsL <- list(
    "iNZight" = "Examples",
    "iNZightMR" = "Multiple-Response",
    "iNZightTS" = "Time-Series",
    "iNZightMaps" = "Maps",
    "survey" = "Surveys",
    "FutureLearnData" = "Future-Learn"
  )
  named.pkg <- unlist(pkgsL)
  names(named.pkg[named.pkg %in% input$data_select])
})


observe({
  if (!is.null(input$change_set)) {
    isolate({
      if (!is.null(input[[input$data_select]]) && input$change_set > 0) {
        new.data <- NULL
        if ("Imported" %in% input$data_select) {
          new.data <-
            load.data(get.data.dir.imported(), strsplit(input[[input$data_select]],
              "==>",
              fixed = TRUE
            )[[1]]
            [length(strsplit(input[[input$data_select]], "==>", fixed = T)[[1]])])
        } else {
          new.data <-
            load.data(get.data.dir.global(), strsplit(input[[input$data_select]],
              "==>",
              fixed = TRUE
            )[[1]]
            [length(strsplit(input[[input$data_select]], "==>", fixed = T)[[1]])])
        }
        plot.par$design <- NULL
        values$data.name <- new.data[[1]]
        values$data.set <- new.data[[2]]

        values <- sample_if_cas(rvalues = values, d = new.data[[2]])

        updatePanel$doit <- updatePanel$doit + 1
        values$data.restore <- get.data.set()
        ## code history
        ## survey package
        if (req(pkgname()) == "survey") {
          if (grepl("\\(.+\\)", values$data.name)) {
            values$data.name <- gsub(" \\(.+", "", values$data.name)
          }
        }

        code.save$name <- sprintf("%s_ex", values$data.name)
        code.save$datacode <- sprintf(".dataset <- %s", values$data.name)
        code.save$variable <- c(code.save$variable, list(c(sprintf(
          "## Load example data set\ndata(%s, package = '%s')",
          values$data.name, pkgname()
        ), "\n")))
        code.save$variable <- c(code.save$variable, list(c("\n", sep(), "\n", paste0(
          sprintf("## Exploring the '%s' dataset", code.save$name),
          "\n"
        ))))
        code.save$variable <- c(code.save$variable, list(c("\n", sprintf("%s <- %s", code.save$name, values$data.name), "\n")))
        values$data.name <- code.save$name
        values$name.restore <- code.save$name
        updateSelectInput(session, "subs2", selected = "none")
        updateSelectInput(session, "subs1", selected = "none")
        updateSelectInput(session, "vari2", selected = "none")
        updateSelectInput(session, "vari1", selected = "none")
        plot.par$design <- NULL
        design_params$design <- NULL
      }
    })
  }
})

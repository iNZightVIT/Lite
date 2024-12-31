## Manipulate variables --> Categorical variables

output$categorical.variables <- renderUI({
  get.data.set()
  isolate({
    if (input$selector %in% "Categorical variables") {
      categorical.variables.panel(get.data.set())
    }
  })
})

output$categorical.main.panel <- renderUI({
  input$categorical_variables_select1
  isolate({
    if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Reorder levels") {
      reorder.main.panel()
    } else if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Collapse levels") {
      collapse.main.panel()
    } else if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Rename levels") {
      rename.levels.main.panel()
    } else if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Combine categorical") {
      combine.main.panel()
    }
  })
})

output$categorical.side.panel <- renderUI({
  input$categorical_variables_select1
  get.data.set()
  isolate({
    if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Reorder levels") {
      reorder.sidebar.panel(get.data.set())
    } else if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Collapse levels") {
      collapse.sidebar.panel(get.data.set())
    } else if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Rename levels") {
      rename.levels.sidebar.panel(get.data.set())
    } else if (!is.null(input$categorical_variables_select1) &&
      input$categorical_variables_select1 %in% "Combine categorical") {
      combine.sidebar.panel(get.data.set())
    }
  })
})

## Manipulate variables --> Categorical variables --> Reorder levels
observe({
  input$select.reorder.column
  isolate({
    if (!is.null(input$select.reorder.column) &&
      input$select.reorder.column %in% colnames(get.data.set())) {
      updateTextInput(session, "recorder_variable_newname",
        value = paste0(input$select.reorder.column, ".reord", sep = "")
      )
    }
  })
})


observe({
  input$reorder
  isolate({
    if (!is.null(input$select.reorder.column) &&
      input$select.reorder.column %in% colnames(get.data.set())) {
      var <- input$select.reorder.column
      if (!is.null(input$recorder_variable_newname) &&
        !grepl("^\\s*$", input$recorder_variable_newname)) {
        name <- input$recorder_variable_newname
        if (!is.null(input$recorder_sort_levels) &&
          input$recorder_sort_levels == "manually") {
          rm.na.name <- unique(get.data.set()[[
            input$select.reorder.column
          ]])[!is.na(unique(get.data.set()[[
            input$select.reorder.column
          ]]))]

          if (!is.null(input$select.reorder.item) &&
            length(input$select.reorder.item) == length(rm.na.name)) {
            levels <- as.character(input$select.reorder.item)
            temp <- iNZightTools::reorder_levels(
              get.data.set(), var, levels,
              name = make_names(name)
            )
            updatePanel$datachanged <- updatePanel$datachanged + 1
            values$data.set <- temp
            values <- sample_if_cas(
              rvalues = values, d = temp,
              new_sample = FALSE
            )
            ## code history
            code <- tidy_assign_pipe(gsub(
              "get.data.set\\()",
              code.save$name,
              iNZightTools::code(values$data.set)
            ))
            code.save$variable <- c(
              code.save$variable, list(c("\n", code, "\n"))
            )
          }
        } else {
          data <- iNZightTools::reorder_levels(
            get.data.set(), var,
            auto = "freq", name = make_names(name)
          )
          updatePanel$datachanged <- updatePanel$datachanged + 1
          values$data.set <- data
          values <- sample_if_cas(
            rvalues = values, d = data,
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
    }
  })
})



output$text_reorder <- renderPrint({
  if (!is.null(input$select.reorder.column) &&
    !"" %in% input$select.reorder.column) {
    print(table(get.data.set()[, input$select.reorder.column]))
  } else {
    cat("Select a column!")
  }
})

observe({
  if (!is.null(input$select.reorder.column)) {
    choices <- ""
    if (!"" %in% input$select.reorder.column) {
      if (is.factor(get.data.set()[, input$select.reorder.column])) {
        choices <- levels(get.data.set()[, input$select.reorder.column])
      } else {
        choices <- levels(as.factor(
          get.data.set()[, input$select.reorder.column]
        ))
      }
    }
    updateSelectInput(
      session = session,
      inputId = "select.reorder.item",
      selected = "",
      choices = choices
    )
  }
})

## Manipulate variables --> Categorical variables --> Collapse levels

observe({
  input$select.collapse.column
  isolate({
    if (!is.null(input$select.collapse.column) &&
      input$select.collapse.column %in% colnames(get.data.set())) {
      updateTextInput(session, "collapse_variable_newname",
        value = paste0(input$select.collapse.column, ".coll", sep = "")
      )
    }
  })
})


observe({
  if (!is.null(input$select.collapse.column)) {
    choices <- ""
    if (!"" %in% input$select.collapse.column) {
      if (is.factor(get.data.set()[, input$select.collapse.column])) {
        choices <- levels(get.data.set()[, input$select.collapse.column])
      } else {
        choices <- levels(as.factor(
          get.data.set()[, input$select.collapse.column]
        ))
      }
    }
    updateSelectInput(
      session = session, inputId = "select.collapse.item",
      selected = "", choices = choices
    )
  }
})


observe({
  input$select.collapse.item
  isolate({
    if (!is.null(input$select.collapse.item) &&
      length(input$select.collapse.item) > 0) {
      if (length(input$select.collapse.item) == 1) {
        updateTextInput(session, "collapse_level_newname",
          value = input$select.collapse.item
        )
      } else if (length(input$select.collapse.item) > 1) {
        updateTextInput(session, "collapse_level_newname",
          value = paste0(input$select.collapse.item, collapse = "_")
        )
      }
    }
  })
})


output$text_collapse_1st <- renderPrint({
  input$collapse
  if (!is.null(input$select.collapse.column) &&
    !"" %in% input$select.collapse.column) {
    print(table(get.data.set()[, input$select.collapse.column]))
  } else {
    cat("Select a column!")
  }
})

output$text_collapse_2nd <- renderPrint({
  input$collapse
  if (!is.null(input$select.collapse.column) &&
    !"" %in% input$select.collapse.column &&
    !is.null(input$select.collapse.item) &&
    !"" %in% input$select.collapse.item) {
    print(table(get.collapsed.column(
      get.data.set()[, input$select.collapse.column], input$select.collapse.item
    )))
  } else {
    cat("")
  }
})



observe({
  input$collapse
  isolate({
    if (!is.null(input$select.collapse.column) &&
      input$select.collapse.column %in% colnames(get.data.set())) {
      var <- input$select.collapse.column
      if (!is.null(input$select.collapse.item) &&
        length(input$select.collapse.item) > 1) {
        lvls <- input$select.collapse.item
        if (!is.null(input$collapse_variable_newname) &&
          !grepl("^\\s*$", input$collapse_variable_newname) &&
          !is.null(input$collapse_level_newname) &&
          !grepl("^\\s*$", input$collapse_level_newname)) {
          name <- input$collapse_variable_newname
          lvlname <- input$collapse_level_newname
          temp <- iNZightTools::collapse_cat(
            get.data.set(), var, lvls, lvlname, name
          )
          updatePanel$datachanged <- updatePanel$datachanged + 1
          values$data.set <- temp
          values <- sample_if_cas(
            rvalues = values, d = temp,
            new_sample = FALSE
          )
          ## code history
          code <- tidy_assign_pipe(gsub(
            "get.data.set\\()",
            code.save$name, iNZightTools::code(values$data.set)
          ))
          code.save$variable <- c(
            code.save$variable,
            list(c("\n", code, "\n"))
          )
        }
      }
    }
  })
})


## Manipulate variables --> Categorical variables --> Rename levels

rename.levels.main.panel <- function() {
  verbatimTextOutput("text_rename")
}

rename.factors.textfields <- function(factors) {
  ret <- list()
  for (fac in 1:length(factors)) {
    ret[[fac]] <- textInput(
      inputId = paste0("factor", fac),
      label = factors[fac], value = factors[fac]
    )
  }
  ret
}


output$rename.factors.inputs <- renderUI({
  input$select.rename.column
  get.data.set()
  isolate({
    if (!is.null(input$select.rename.column) &&
      !input$select.rename.column == "") {
      rename.factors.textfields(levels(
        get.data.set()[, input$select.rename.column]
      ))
    }
  })
})


output$text_rename <- renderPrint({
  input$select.rename.column
  get.data.set()
  isolate({
    if (!is.null(input$select.rename.column) &&
      !input$select.rename.column %in% "") {
      print(summary(get.data.set()[, input$select.rename.column]))
    } else {
      cat("")
    }
  })
})

observe({
  input$rename.levs
  input$select.rename.column
  isolate({
    tryCatch({
      if (!is.null(input$rename.levs) && input$rename.levs > 0) {
        num <- levels(get.data.set()[, input$select.rename.column])
        indexes1 <- grep("^factor[0-9]+$", names(input))
        indexes1 <- names(input)[indexes1]
        indexes1 <- indexes1[indexes1 %in% paste0("factor", 1:length(num))]
        idxmch <- as.numeric(gsub("factor", "", indexes1))
        new.levels <- list()
        for (i in 1:length(indexes1)) {
          if (is.null(input[[indexes1[i]]]) || input[[indexes1[i]]] %in% "") {
            new.levels[i] <- num[idxmch[i]]
            names(new.levels)[i] <- num[idxmch[i]]
          } else {
            new.levels[i] <- num[idxmch[i]]
            names(new.levels)[i] <- input[[indexes1[i]]]
          }
        }
        changed <- sapply(seq_along(new.levels), function(i) {
          names(new.levels)[i] != new.levels[i]
        })
        new.levels <- new.levels[changed]
        if (!is.null(new.levels)) {
          temp <- iNZightTools::rename_levels(
            get.data.set(),
            input$select.rename.column,
            new.levels
          )
        }
        if (!is.null(temp)) {
          updatePanel$datachanged <- updatePanel$datachanged + 1
          values$data.set <- temp
          values <- sample_if_cas(
            rvalues = values, d = temp,
            new_sample = FALSE
          )
          updateSelectInput(session, "select.rename.column", selected = 0)
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



## Manipulate variables --> Categorical variables --> Combine variables

output$text_combine <- renderPrint({
  if (length(input$select.combine.columns) > 0) {
    temp <- iNZightTools::combineCatVars(
      get.data.set(), input$select.combine.columns
    )
    print(table(temp[, ncol(temp)]))
  } else {
    cat("Please select a set of columns")
  }
})

observe({
  input$combine
  isolate({
    if (!is.null(input$combine) && input$combine > 0) {
      temp <- iNZightTools::combineCatVars(
        get.data.set(), input$select.combine.columns
      )
      if (!is.null(temp)) {
        updatePanel$datachanged <- updatePanel$datachanged + 1
        values$data.set <- temp
        values <- sample_if_cas(rvalues = values, d = temp, new_sample = FALSE)
        ## code history
        code <- tidy_assign_pipe(
          gsub(
            "get.data.set\\()", code.save$name,
            iNZightTools::code(values$data.set)
          )
        )
        code.save$variable <- c(code.save$variable, list(c("\n", code, "\n")))
      }
    }
  })
})

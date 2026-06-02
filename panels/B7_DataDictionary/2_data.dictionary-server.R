data_dict_dt_options <- list(
  lengthMenu = c(5, 30, 50), pageLength = 5,
  columns.defaultContent = "NA", scrollX = TRUE,
  columnDefs = list(
    list(className = "dt-center", targets = "_all")
  )
  # filter = "bottom"
)

data_dict_state <- reactiveValues(
  import_status = NULL,
  file = NULL,
  apply_status = NULL
)

data_dict_success_ui <- function(message) {
  tags$p(style = "color: green; font-weight: bold;", message)
}

data_dict_error_ui <- function(message) {
  tags$p(style = "color: red; font-weight: bold;", message)
}

data_dict_reset_apply <- function() {
  data_dict_state$apply_status <- NULL
}

output$data.dictionary.panel <- renderUI({
  data.dictionary.panel()
})

output$data_dict_import_status <- renderUI({
  input$change_data_dict
  status <- data_dict_state$import_status
  if (is.null(status)) {
    return(NULL)
  }
  if (identical(status, "success")) {
    data_dict_success_ui("Data dictionary imported successfully.")
  } else {
    data_dict_error_ui(status)
  }
})

output$data_dict_import_button <- renderUI({
  updatePanel$datachanged
  has_dataset <- !is.null(get.data.set())
  hint <- if (has_dataset) {
    helpText("Finalise your choice by pressing the button below.")
  } else {
    tags$p(tags$strong("Dataset required to import a dictionary."))
  }
  tagList(
    hint,
    actionButton(
      inputId = "change_data_dict",
      label = "Import Data Dictionary",
      disabled = !has_dataset
    )
  )
})

output$data_dict_apply_button <- renderUI({
  updatePanel$datachanged
  input$change_data_dict
  input$data_dict_var_name
  tagList(
    helpText("Apply data dictionary to current dataset."),
    actionButton(
      inputId = "apply_data_dict",
      label = "Apply",
      disabled = is.null(get.data.set()) || is.null(get.data.dict())
    )
  )
})

output$data_dict_apply_status <- renderUI({
  input$apply_data_dict
  status <- data_dict_state$apply_status
  if (is.null(status)) {
    return(NULL)
  }
  if (identical(status, "success")) {
    data_dict_success_ui("Data dictionary applied successfully.")
  } else {
    data_dict_error_ui(status)
  }
})

output$data_dict_preview_section <- renderUI({
  input$change_data_dict
  input$data_dict_var_name
  input$data_dict_var_type
  input$data_dict_friendly_name
  input$data_dict_description
  input$data_dict_units
  input$data_dict_factor_codes
  input$data_dict_factor_labels
  input$data_dict_code_separator
  if (is.null(get.data.dict())) {
    return(NULL)
  }
  tagList(
    h1("Data Dictionary"),
    dataTableOutput("data_dict_table")
  )
})

output$data_dict_applied_section <- renderUI({
  input$apply_data_dict
  if (!identical(data_dict_state$apply_status, "success")) {
    return(NULL)
  }
  tagList(
    br(),
    h1("Applied Dataset"),
    dataTableOutput("data_dict_applied_table")
  )
})

data_dict_val_or_null <- function(x) {
  if (is.null(x) || length(x) == 0 || any(is.na(x))) {
    return(NULL)
  }
  x <- as.character(x)
  if (!any(nzchar(x))) {
    return(NULL)
  }
  x
}

dict_col_if <- function(col, file_cols) {
  if (col %in% file_cols) {
    col
  } else {
    ""
  }
}

data_dict_guess_mapping <- function(file_cols) {
  list(
    name = ifelse(length(file_cols), file_cols[[1]], NULL),
    type = data_dict_val_or_null(dict_col_if("type", file_cols)),
    title = data_dict_val_or_null(dict_col_if("title", file_cols)),
    description = data_dict_val_or_null(dict_col_if("description", file_cols)),
    units = data_dict_val_or_null(dict_col_if("units", file_cols)),
    codes = data_dict_val_or_null(dict_col_if("codes", file_cols)),
    values = data_dict_val_or_null(dict_col_if("values", file_cols))
  )
}

data_dict_read <- function(file, mapping, separator = "|") {
  arglist <- list(
    file = file,
    name = data_dict_val_or_null(mapping$name),
    type = data_dict_val_or_null(mapping$type),
    title = data_dict_val_or_null(mapping$title),
    description = data_dict_val_or_null(mapping$description),
    units = data_dict_val_or_null(mapping$units),
    codes = data_dict_val_or_null(mapping$codes),
    values = data_dict_val_or_null(mapping$values),
    level_separator = separator
  )
  arglist <- arglist[!sapply(arglist, is.null)]
  dict <- try(do.call(iNZightTools::read_dictionary, arglist), silent = TRUE)
  if (inherits(dict, "try-error")) {
    return(NULL)
  }
  dict
}

data_dict_mapping_from_input <- function() {
  list(
    name = input$data_dict_var_name,
    type = input$data_dict_var_type,
    title = input$data_dict_friendly_name,
    description = input$data_dict_description,
    units = input$data_dict_units,
    codes = input$data_dict_factor_codes,
    values = input$data_dict_factor_labels
  )
}

data_dict_load <- function() {
  file <- data_dict_state$file
  if (is.null(file) || !nzchar(file) || !file.exists(file)) {
    return(invisible(NULL))
  }
  dict <- data_dict_read(
    file,
    data_dict_mapping_from_input(),
    input$data_dict_code_separator
  )
  if (is.null(dict)) {
    values$data.dict <- NULL
    return(invisible(NULL))
  }
  values$data.dict <- dict
  invisible(dict)
}

data_dict_display_df <- function(dict, n = 20) {
  if (is.null(dict)) {
    return(NULL)
  }
  as.data.frame(
    iNZightTools::as_tibble(dict, n = n, code_sep = "\n"),
    stringsAsFactors = TRUE
  )
}

data_dict_preview_df <- function(dict, n = 20) {
  data_dict_display_df(dict, n = n)
}

update_data_dict_mapping <- function(session, file_cols) {
  cols_opt <- c("", file_cols)
  updateSelectInput(session, "data_dict_var_name",
    choices = file_cols,
    selected = ifelse(length(file_cols), file_cols[[1]], character(0))
  )
  updateSelectInput(session, "data_dict_var_type",
    choices = cols_opt,
    selected = dict_col_if("type", file_cols)
  )
  updateSelectInput(session, "data_dict_friendly_name",
    choices = cols_opt,
    selected = dict_col_if("title", file_cols)
  )
  updateSelectInput(session, "data_dict_description",
    choices = cols_opt,
    selected = dict_col_if("description", file_cols)
  )
  updateSelectInput(session, "data_dict_units",
    choices = cols_opt,
    selected = dict_col_if("units", file_cols)
  )
  updateSelectInput(session, "data_dict_factor_codes",
    choices = cols_opt,
    selected = dict_col_if("codes", file_cols)
  )
  updateSelectInput(session, "data_dict_factor_labels",
    choices = cols_opt,
    selected = dict_col_if("values", file_cols)
  )
}

output$data_dict_table <- renderDT(
  {
    input$change_data_dict
    input$data_dict_var_name
    input$data_dict_var_type
    input$data_dict_friendly_name
    input$data_dict_description
    input$data_dict_units
    input$data_dict_factor_codes
    input$data_dict_factor_labels
    input$data_dict_code_separator
    data_dict_preview_df(get.data.dict())
  },
  options = data_dict_dt_options
)
outputOptions(output, "data_dict_table", suspendWhenHidden = FALSE)

output$data_dict_applied_table <- renderDT(
  {
    input$apply_data_dict
    if (!identical(data_dict_state$apply_status, "success")) {
      return(NULL)
    }
    get.data.set()
  },
  options = data_dict_dt_options
)
outputOptions(output, "data_dict_applied_table", suspendWhenHidden = FALSE)

observeEvent(input$change_data_dict, {
  data_dict_reset_apply()
  data_dict_state$import_status <- NULL
  if (is.null(input$data_dict_file) || nrow(input$data_dict_file) == 0) {
    return()
  }
  datapath <- input$data_dict_file$datapath[1]
  if (!file.exists(datapath)) {
    return()
  }
  raw <- try(as.data.frame(iNZightTools::smart_read(datapath)), silent = TRUE)
  if (inherits(raw, "try-error")) {
    values$data.dict <- NULL
    data_dict_state$file <- NULL
    data_dict_state$import_status <- "Import failed."
    return()
  }
  
  data_dict_state$file <- datapath
  update_data_dict_mapping(session, colnames(raw))
  dict <- data_dict_read(
    datapath,
    data_dict_guess_mapping(colnames(raw)),
    input$data_dict_code_separator
  )
  if (is.null(dict)) {
    values$data.dict <- NULL
    data_dict_state$import_status <- "Could not read data dictionary."
    return()
  }

  data_dict_state$import_status <- "success"
  values$data.dict <- dict
})

observeEvent(input$apply_data_dict, {
  isolate({
    if (is.null(input$apply_data_dict) || is.null(input$apply_data_dict)) {
      return()
    }
    if (is.null(get.data.set()) || is.null(get.data.dict())) {
      data_dict_state$apply_status <- "Could not apply data dictionary."
      return()
    }
    dict <- data_dict_load()
    # https://github.com/iNZightVIT/iNZight/blob/dev/R/iNZDocument.R#L271
    if (is.null(dict)) {
      data_dict_state$apply_status <- "Could not apply data dictionary."
      return()
    }

    temp <- try(
      get.data.set() %>% iNZightTools::apply_dictionary(dict),
      silent = TRUE
    )
    if (inherits(temp, "try-error")) {
      data_dict_state$apply_status <- "Could not apply data dictionary."
      return()
    }

    data_dict_state$apply_status <- "success"
    values$data.set <- as.data.frame(temp)

  })
})

observe({
  if (is.null(data_dict_state$file)) {
    return()
  }
  input$data_dict_var_name
  input$data_dict_var_type
  input$data_dict_friendly_name
  input$data_dict_description
  input$data_dict_units
  input$data_dict_factor_codes
  input$data_dict_factor_labels
  input$data_dict_code_separator
  data_dict_load()
})

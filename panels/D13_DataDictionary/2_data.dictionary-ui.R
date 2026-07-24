get.data.dictionary.main <- function() {
  list(
    div(class = "page-divider"),
    uiOutput("data_dict_preview_section"),
    uiOutput("data_dict_applied_section")
  )
}

data_dict_mapping_inputs <- function() {
  tagList(
    fixedRow(
      column(6, selectInput(
        inputId = "data_dict_var_name",
        label = "Variable name",
        choices = NULL
      )),
      column(6, selectInput(
        inputId = "data_dict_var_type",
        label = "Variable type",
        choices = NULL
      ))
    ),
    fixedRow(
      column(6, selectInput(
        inputId = "data_dict_friendly_name",
        label = "Friendly name/title",
        choices = NULL
      )),
      column(6, selectInput(
        inputId = "data_dict_description",
        label = "Description",
        choices = NULL
      ))
    ),
    fixedRow(
      column(6, selectInput(
        inputId = "data_dict_units",
        label = "Units",
        choices = NULL
      )),
      column(6, selectInput(
        inputId = "data_dict_factor_codes",
        label = "Factor codes",
        choices = NULL
      ))
    ),
    fixedRow(
      column(6, selectInput(
        inputId = "data_dict_factor_labels",
        label = "Factor labels",
        choices = NULL
      )),
      column(6, textInput(
        inputId = "data_dict_code_separator",
        label = "Code/level separator",
        value = "|"
      ))
    )
  )
}

data.dictionary.panel <- function() {
  sidebarLayout(
    sidebarPanel(
      HTML("Please let us know If you have difficulty importing data. If you can include information about the operating system, browser and a copy of the data that would be extremely helpful. <br/> Email: inzightlite_support@stat.auckland.ac.nz<br/>"),
      br(), br(),
      helpText("Select a file (Size Limit: 5MB)"),
      fileInput("data_dict_file", label = "", multiple = FALSE),
      data_dict_mapping_inputs(),
      uiOutput("data_dict_import_button"),
      uiOutput("data_dict_import_status"),
      br(),
      uiOutput("data_dict_apply_button"),
      uiOutput("data_dict_apply_status"),
      br(),
      tags$a(
        href = "https://inzight.nz/docs/reference/data-dictionary/",
        "Help",
        class = "btn btn-xs btn-success",
        target = "_blank",
        rel = "noopener noreferrer"
      ),
      br()
    ),
    mainPanel(get.data.dictionary.main())
  )
}

### -------------------------------------###
### Server Functions for iNZight Lite   ###
### -------------------------------------###
###
### Date Created : January 10, 2015
###
### Please consult the comments before editing any code.
### This file sources the ui files for each panel separately.

###  We load the packages we require. This is done only ONCE per instance.
message("Starting iNZight Lite Server...")

suppressPackageStartupMessages(library(iNZightPlots))
suppressPackageStartupMessages(library(iNZightTS))
suppressPackageStartupMessages(library(iNZightMR))
suppressPackageStartupMessages(library(markdown))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(iNZightRegression))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(survey))
suppressPackageStartupMessages(library(iNZightMaps))
suppressPackageStartupMessages(library(colorspace))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(sas7bdat))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyalert))
suppressPackageStartupMessages(library(rjson))
# suppressPackageStartupMessages(library(shinylogs))

## read in all the functions used in iNZight Lite
source("functions.R")

### We write the server function.
shinyServer(function(input, output, session) {
  observe({
    params <- parseQueryString(session$clientData$url_search)
    if (!is.null(params$lite_config) &&
      is.null(session$userData$LITE_VERSION)) {
      # only read in config if the "lite_config" param is present
      if ("lite_config" %in% names(params)) {
        config <- read_config()
        if (!is.null(config)) {
          session$userData$LITE_VERSION <- toupper(params$lite_config)
          if (session$userData$LITE_VERSION %in% names(config)) {
            session$userData$LITE_CONFIG <-
              config[[session$userData$LITE_VERSION]]
          }
        }
      }
    }
    cat("Version: ", session$userData$LITE_VERSION, "\n")
  })

  session$userData$BUILD_INFO <- ""
  if (!is.null(session$clientData$hostname)) {
    session$userData$BUILD_INFO <- switch(session$clientData$hostname,
      "lite-staging.inzight.nz" = " [Development build]",
      "lite-prod.inzight.nz" = " [Back-up build]",
      ""
    )
  }

  # init_lite_logs()
  # updateQueryString(
  #   queryString = paste0("?v=lite&sessionId=", LITE_SESSION_ID_)
  # )

  desc <- read.dcf("DESCRIPTION")

  session$allowReconnect(TRUE)

  ## Specify all the reactive values
  values <- reactiveValues()
  values$data.name <- NULL
  values$data.dir.global <- "data"
  values$data.dir.imported <- "data"
  values$data.set <- NULL
  values$data.type <- NULL # file type (e.g., csv, xlsx, rda ... etc)
  values$data.available.dnames <- NULL # available data/sheet names
  values$data.current.dname <- NULL # current data/sheet names
  values$data.restore <- NULL
  values$name.restore <- NULL
  values$lite.version <- desc[, "Version"][[1]]
  values$lite.update <- ""
  values$button <- F
  values$transform.text <- ""
  values$create.variables.expression.text <- ""

  # TODO: generalise this / does it work?
  observe({
    if (!is.null(session$userData$LITE_VERSION) &&
      session$userData$LITE_VERSION == "CAS") {
      values$data.sample <- NULL
      values$sample.row <- NULL
      values$sample.num <- NULL
    }
  })
  # dummy variable to update whole panels when the data
  # set is switched but not updated when data is changed.
  updatePanel <- reactiveValues()
  updatePanel$doit <- 0
  updatePanel$datachanged <- 0
  updatePanel$first <- T

  if (!"vars.path" %in% ls()) {
    vars.path <<- NULL
  }

  vars <- get.vars(vars.path)
  if (!is.null(vars)) {
    if ("data.dir.global" %in% names(vars) &&
      file.exists(vars$data.dir.global)) {
      values$data.dir.global <- vars[["data.dir.global"]]
    } else if (!file.writable(vars$data.dir.global)) {
      warning(paste(
        "The directory : ", vars$data.dir.global,
        " : is not writable."
      ))
    }
    if ("data.dir.imported" %in% names(vars) &&
      dir.create.logical(paste0(vars$data.dir.imported, "/Imported"),
        recursive = T
      )) {
      if (file.writable(vars$data.dir.imported)) {
        values$data.dir.imported <- vars[["data.dir.imported"]]
      }
    } else {
      values$data.dir.imported <- values$data.dir.global
      if (!file.exists(paste0(values$data.dir.imported, "/Imported"))) {
        dir.create(paste0(values$data.dir.imported, "/Imported"),
          recursive = T
        )
      }
    }
    if ("lite.version" %in% names(vars)) {
      values$lite.version <- vars$lite.version
    }
    if ("lite.update" %in% names(vars)) {
      values$lite.update <- vars$lite.update
    }
  }

  # -- LITE2 --
  sample_if_cas <- function(rvalues, d, new_sample = TRUE) {
    if (!(!is.null(session$userData$LITE_VERSION) &&
      session$userData$LITE_VERSION == "CAS")) {
      return(rvalues)
    }

    if (new_sample) {
      rvalues$sample.num <- ifelse(nrow(d) > 2000, 500, round(nrow(d) / 4))
      rvalues$sample.row <- sort(sample(1:nrow(d), rvalues$sample.num))
    }
    new_data <- as.data.frame(d[rvalues$sample.row, ])
    row.names(new_data) <- 1:nrow(new_data)
    colnames(new_data) <- colnames(d)
    rvalues$data.sample <- new_data

    return(rvalues)
  }

  sample_info_cas <- function() {
    if ((!is.null(session$userData$LITE_VERSION) &&
      session$userData$LITE_VERSION == "CAS") &&
      !is.null(values$data.sample) &&
      !is.null(get.data.set()) && !is.null(get.data.name())) {
      return(paste(
        "The displayed data is a random sample of",
        nrow(values$data.sample), "rows from the original data"
      ))
    }
  }
  # ----------

  get.data.name <- reactive({
    values$data.name
  })

  get.data.dir.global <- reactive({
    values$data.dir.global
  })

  get.data.dir.imported <- reactive({
    values$data.dir.imported
  })

  get.data.set <- reactive({
    values$data.set
  })

  get.data.set.display <- reactive({
    if (!is.null(session$userData$LITE_VERSION) &&
      session$userData$LITE_VERSION == "CAS") {
      values$data.sample
    } else {
      values$data.set
    }
  })

  get.data.restore <- reactive({
    values$data.restore
  })

  get.name.restore <- reactive({
    values$name.restore
  })

  get.lite.version <- reactive({
    values$lite.version
  })

  get.lite.update <- reactive({
    values$lite.update
  })

  get.button <- reactive({
    values$button
  })

  get.transform.text <- reactive({
    values$transform.text
  })

  get.create.variables.expression.text <- reactive({
    values$create.variables.expression.text
  })

  ## code list
  code.save <- reactiveValues(
    variable = "\n",
    name = "",
    datacode = ""
  )

  #################################

  ##  Turn errors and warnings off
  ## options(warn = -1, show.error.messages = FALSE)

  ##  Delete all imported files that are older than 1 day.
  ## Load all panels into memory.
  filepaths <- list.files(
    pattern = "[.]R$",
    path = "gui-elements/",
    full.names = TRUE
  )
  sapply(filepaths, source)

  ## ----------------------##
  ##  A1. "About" Module  ##
  ## ----------------------##
  source("panels/A1_About/1_about-panel-ui.R", local = T)
  source("panels/A1_About/2_about-panel-server.R", local = T)

  ## ---------------------------------------##
  ##  B1. "File -> Import Dataset" Module  ##
  ## ---------------------------------------##
  source("panels/B1_ImportDataset/1_import.data.set.panel-ui.R", local = TRUE)
  source("panels/B1_ImportDataset/2_import.data.set.panel-server.R",
    local = TRUE
  )

  ## ---------------------------------------##
  ##  B2. "File -> Import Dataset" Module  ##
  ## ---------------------------------------##
  source("panels/B2_ExportDataset/1_export.dataset.panel-ui.R", local = TRUE)
  source("panels/B2_ExportDataset/2_export.dataset.panel-server.R", local = TRUE)

  ## ---------------------------------------##
  ##  B3. "File -> Export Dataset" Module  ##
  ## ---------------------------------------##
  source("panels/B3_DisplayDataset/1_display.data.set.panel-ui.R", local = TRUE)
  source("panels/B3_DisplayDataset/2_display.data.set.panel-server.R",
    local = TRUE
  )

  ## ---------------------------------------##
  ##  B4. "File -> Remove Dataset" Module  ##
  ## ---------------------------------------##
  # source("panels/B4_RemoveDataset/1_remove.data.set.panel-ui.R", local = TRUE)
  # source("panels/B4_RemoveDataset/2_remove.data.set.panel-server.R",
  # local = TRUE)

  ## -----------------------------------------##
  ##  B5. "File -> Dataset Examples" Module  ##
  ## -----------------------------------------##
  source("panels/B5_DatasetExamples/1_data.set.examples-ui.R", local = TRUE)
  source("panels/B5_DatasetExamples/2_data.set.examples-server.R",
    local = TRUE
  )


  ## -----------------------------------------##
  ##  B6. "File -> Paste Dataset" Module  ##
  ## -----------------------------------------##
  source("panels/B6_PasteDataset/1_paste.data.set-ui.R", local = TRUE)
  source("panels/B6_PasteDataset/2_paste.data.set-server.R", local = TRUE)

  ## ------------------------##
  ##  C1. Visualize Module  ##
  ## ------------------------##
  source("panels/C1_Visualize/1_visualize-panel-ui.R", local = TRUE)
  source("panels/C1_Visualize/2_visualize-panel-server.R", local = TRUE)

  ## -----------------------------##
  ##  D1. Filter Dataset Module  ##
  ## -----------------------------##
  source("panels/D1_FilterDataset/1_filter.dataset-ui.R", local = TRUE)
  source("panels/D1_FilterDataset/2_filter.dataset-server.R", local = TRUE)

  ## -----------------------------##
  ##  D2. Sort data by variables ##
  ## -----------------------------##
  source("panels/D2_SortDataByVariables/1_sort.variables.ui.R", local = TRUE)
  source("panels/D2_SortDataByVariables/2_sort.variables.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  D3. Aggregate data         ##
  ## -----------------------------##
  source("panels/D3_AggregateData/1_aggregate.data.ui.R", local = TRUE)
  source("panels/D3_AggregateData/2_aggregate.data.server.R", local = TRUE)

  ## -----------------------------##
  ##  D4. Stack variables        ##
  ## -----------------------------##
  source("panels/D4_StackVariables/1_stack.variables.ui.R", local = TRUE)
  source("panels/D4_StackVariables/2_stack.variables.server.R", local = TRUE)

  ## -----------------------------##
  ##  D8. Reshape data           ##
  ## -----------------------------##
  source("panels/D8_ReshapeData/1_reshape.dataset.ui.R", local = TRUE)
  source("panels/D8_ReshapeData/2_reshape.dataset.server.R", local = TRUE)

  ## -----------------------------##
  ##  D9.  Separate columns      ##
  ## -----------------------------##
  source("panels/D9_SeparateColumns/1_separate.columns.ui.R", local = TRUE)
  source("panels/D9_SeparateColumns/2_separate.columns.server.R", local = TRUE)

  ## -----------------------------##
  ##  D10. Unite columns         ##
  ## -----------------------------##
  source("panels/D10_UniteColumns/1_unite.columns.ui.R", local = TRUE)
  source("panels/D10_UniteColumns/2_unite.columns.server.R", local = TRUE)

  ## -----------------------------##
  ##  D11. Merge/Join Datasets   ##
  ## -----------------------------##
  source("panels/D11_MergeJoinDatasets/1_mergejoin.datasets.ui.R", local = TRUE)
  source("panels/D11_MergeJoinDatasets/2_mergejoin.datasets.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  D5. Restore data           ##
  ## -----------------------------##
  source("panels/D5_RestoreData/1_restore.data.ui.R", local = TRUE)
  source("panels/D5_RestoreData/2_restore.data.server.R", local = TRUE)

  ## -----------------------------##
  ##  D6. Create design (beta)   ##
  ## -----------------------------##
  source("panels/D6_SurveyDesign/1_survey.design.ui.R", local = TRUE)
  source("panels/D6_SurveyDesign/2_survey.design.server.R", local = TRUE)

  ## -----------------------------##
  ##  D7. Alphabetise Variables  ##
  ## -----------------------------##
  source("panels/D7_AlphabetiseVariables/1_alphabetise.variables-ui.R",
    local = TRUE
  )
  source("panels/D7_AlphabetiseVariables/2_alphabetise.variables-server.R",
    local = TRUE
  )

  ## ---------------------------------##
  ##  D12. Frequency tables (beta)   ##
  ## ---------------------------------##
  source("panels/D12_FrequencyTables/1_frequency.tables.ui.R", local = TRUE)
  source("panels/D12_FrequencyTables/2_frequency.tables.server.R", local = TRUE)

  ## -----------------------------##
  ##  E0. Convert to Categorical ##
  ## -----------------------------##
  source("panels/E0_ConvertToCategorical/1_convert.to.categorical.ui.R",
    local = TRUE
  )
  source("panels/E0_ConvertToCategorical/2_convert.to.categorical.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  E1. Categorical Variables  ##
  ## -----------------------------##
  source("panels/E1_CategoricalVariables/1_categorical.variables.ui.R",
    local = TRUE
  )
  source("panels/E1_CategoricalVariables/2_categorical.variables.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  E2. Numeric Variables      ##
  ## -----------------------------##
  source("panels/E2_NumericVariables/1_numeric.variables.ui.R", local = TRUE)
  source("panels/E2_NumericVariables/2_numeric.variables.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  E3. Rename Variables       ##
  ## -----------------------------##
  source("panels/E3_RenameVariables/1_rename.variables.panel.ui.R",
    local = TRUE
  )
  source("panels/E3_RenameVariables/2_rename.variables.server.R", local = TRUE)

  ## -----------------------------##
  ##  E4. Create Variables       ##
  ## -----------------------------##
  if (!(!is.null(session$userData$LITE_VERSION) && session$userData$LITE_VERSION == "CAS")) {
    source("panels/E4_CreateVariables/1_create.variables.panel.ui.R",
      local = TRUE
    )
    source("panels/E4_CreateVariables/2_create.variables.panel.server.R",
      local = TRUE
    )
  }

  ## -----------------------------##
  ##  E5. Missing to categorical ##
  ## -----------------------------##
  source("panels/E5_MissingToCategorical/1_missing.categorical.panel.ui.R",
    local = TRUE
  )
  source("panels/E5_MissingToCategorical/2_missing.categorical.panel.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  E8. Remove columns         ##
  ## -----------------------------##
  source("panels/E8_RemoveColumns/1_remove.columns.panel.ui.R", local = TRUE)
  source("panels/E8_RemoveColumns/2_remove.columns.panel.server.R",
    local = TRUE
  )

  ## -----------------------------##
  ##  E9. Dates and Times        ##
  ## -----------------------------##
  source("panels/E9_DatesTimes/1_datestimes.ui.R", local = TRUE)
  source("panels/E9_DatesTimes/2_datestimes.server.R", local = TRUE)

  ## -----------------------------##
  ##  F1. Quick explore           ##
  ## -----------------------------##
  if (!(!is.null(session$userData$LITE_VERSION) &&
    session$userData$LITE_VERSION == "CAS")) {
    source("panels/F1_QuickExplore/1_quick.explore.ui.R", local = TRUE)
    source("panels/F1_QuickExplore/2_quick.explore.server.R", local = TRUE)
  }

  ## ----------------------##
  ##  Time Series Module  ##
  ## ----------------------##
  if (!(!is.null(session$userData$LITE_VERSION) &&
    session$userData$LITE_VERSION == "CAS")) {
    source("panels/F2_TimeSeries/1_timeseries-panel-ui.R", local = TRUE)
    source("panels/F2_TimeSeries/2_timeseries-panel-server.R", local = TRUE)
  }

  #   Advanced --> Model Fitting

  ## ------------------------##
  ##  Model Fitting Module  ##
  ## ------------------------##
  if (!(!is.null(session$userData$LITE_VERSION) &&
    session$userData$LITE_VERSION == "CAS")) {
    source("panels/F3_ModelFitting//1_modelFitting.panel.ui.R", local = TRUE)
    source("panels/F3_ModelFitting//2_modelfitting-panel-server.R",
      local = TRUE
    )
  }

  #   Advanced --> Maps

  ## ---------------##
  ##  Maps Module  ##
  ## ---------------##
  if (!(!is.null(session$userData$LITE_VERSION) &&
    session$userData$LITE_VERSION == "CAS")) {
    source("panels/F4_Maps//1_maps.panel-ui.R", local = TRUE)
    source("panels/F4_Maps//2_maps.panel-server.R", local = TRUE)
  }

  #   Advanced --> Design of Experiment

  ## ------------------------------##
  ##  Experimental Design Module  ##
  ## ------------------------------##
  if (!(!is.null(session$userData$LITE_VERSION) &&
    session$userData$LITE_VERSION == "CAS")) {
    source("panels/F5_DesignofExperiment//1_DesignofExperiment.panel-ui.R",
      local = TRUE
    )
    source("panels/F5_DesignofExperiment//2_DesignofExperiment.panel-server.R",
      local = TRUE
    )
  }
  #   Advanced --> Multiple Response

  ## ----------------------------##
  ##  Multiple Response Module  ##
  ## ----------------------------##
  source("panels/F6_MultipleResponse//1_MultipleResponse.panel-ui.R",
    local = TRUE
  )
  source("panels/F6_MultipleResponse//2_MultipleResponse.panel-server.R",
    local = TRUE
  )

  #   Advanced --> Multivariate

  ## ----------------##
  ##  Multivariate  ##
  ## ----------------##
  if (
    (is.null(session$userData$LITE_VERSION) ||
      session$userData$LITE_VERSION != "CAS"
    ) && requireNamespace("iNZightMultivariate", quietly = TRUE)) {
    source("panels/F7_Multivariate//1_Multivariate.panel-ui.R", local = TRUE)
    source("panels/F7_Multivariate//2_Multivariate.panel-server.R",
      local = TRUE
    )
  }

  ## -------##
  ##  VIT  ##
  ## -------##
  if (!(!is.null(session$userData$LITE_VERSION) &&
    session$userData$LITE_VERSION == "CAS")) {
    source("panels/F8_vit/vit.R", local = TRUE)
  }

  #   Show code history

  ## ---------------##
  ##  Code Module  ##
  ## ---------------##
  source("panels/G1_Code//1_code.panel-ui.R", local = TRUE)
  source("panels/G1_Code//2_code.panel-server.R", local = TRUE)

  generate_tabs <- function(version = NULL) {
    import_tabs <- list(
      import = tabPanel("Import Dataset", uiOutput("load.data.panel")),
      paste = tabPanel("Paste Dataset", uiOutput("paste.data.panel")),
      export = tabPanel("Export Dataset", uiOutput("save.data.panel")),
      display = tabPanel("Display Dataset", uiOutput("current.data")),
      examples = tabPanel("Dataset Examples", uiOutput("switch.data.panel"))
    )
    row_ops_tabs <- list(
      filter = tabPanel("Filter Dataset", uiOutput("filter.dataset")),
      sort = tabPanel("Sort data by variables", uiOutput("sort.variables")),
      aggregate = tabPanel("Aggregate data", uiOutput("aggregate.variable")),
      stack = tabPanel("Stack variables", uiOutput("stack.variables")),
      reshape = tabPanel("Reshape data", uiOutput("reshape.dataset")),
      seperate = tabPanel("Separate columns", uiOutput("separate.columns")),
      unite = tabPanel("Unite columns", uiOutput("unite.columns")),
      merge = tabPanel("Merge/Join datasets", uiOutput("mergejoin.datasets")),
      alphabetise = tabPanel(
        "Alphabetise Variables",
        uiOutput("alphabetise.variables")
      ),
      restore = tabPanel("Restore data", uiOutput("restore.data")),
      survey = tabPanel("Survey design", uiOutput("survey.design")),
      frequency = tabPanel("Frequency tables", uiOutput("frequency.tables"))
    )
    manipulate_tabs <- list(
      convert = tabPanel(
        "Convert to categorical",
        uiOutput("convert.to.categorical")
      ),
      categorical = tabPanel(
        "Categorical variables",
        uiOutput("categorical.variables")
      ),
      numeric = tabPanel("Numeric variables", uiOutput("numeric.variables")),
      dates = tabPanel("Dates and Times", uiOutput("dates.times")),
      rename = tabPanel("Rename Variables", uiOutput("rename.variables")),
      create = tabPanel("Create Variables", uiOutput("create.variables")),
      missing = tabPanel(
        "Missing to category",
        uiOutput("missing.categorical")
      ),
      delete = tabPanel("Delete variables", uiOutput("remove.columns"))
    )
    advance_tabs <- list(
      quick = tabPanel("Quick explore", uiOutput("quick.explore")),
      time_series = tabPanel("Time Series",
        value = "timeSeries",
        uiOutput("timeseries.panel")
      ),
      model = tabPanel("Model Fitting",
        value = "regression",
        uiOutput("modelfitting.panel")
      ),
      maps = tabPanel("Maps", uiOutput("newmaps.panel")),
      design_exp = tabPanel(
        "Design of Experiments",
        uiOutput("mixedmodel.panel")
      ),
      multiple = tabPanel(
        "Multiple Response",
        uiOutput("multiple.response")
      ),
      multivariate = tabPanel(
        "Multivariate",
        uiOutput("multivariate.panel")
      ),
      vit = tabPanel("VIT", uiOutput("VIT.panel"))
    )

    visualize_tabs <- tabPanel("Visualize",
      value = "visualize",
      uiOutput("visualize.panel")
    )
    row_ops_tabs <- do.call("navbarMenu", c("Dataset", unname(row_ops_tabs)))
    history_tabs <- tabPanel("R code history",
      value = "rhistory",
      uiOutput("code.panel")
    )

    if (!is.null(session$userData$LITE_VERSION) &&
      session$userData$LITE_VERSION == "CAS") {
      import_tabs <- import_tabs[names(import_tabs) != "export"]
      manipulate_tabs <- manipulate_tabs[names(manipulate_tabs) != "create"]
      advance_tabs <- advance_tabs[names(advance_tabs) == "multiple"]
      row_ops_tabs <- NULL
    }

    if (!requireNamespace("iNZightMultivariate", quietly = TRUE)) {
      advance_tabs <- advance_tabs[names(advance_tabs) != "multivariate"]
    }

    import_tabs <- do.call("navbarMenu", c("File", unname(import_tabs)))
    manipulate_tabs <- do.call(
      "navbarMenu",
      c("Manipulate variables", unname(manipulate_tabs))
    )
    advance_tabs <- do.call("navbarMenu", c("Advanced", unname(advance_tabs)))

    list(
      import_tabs,
      visualize_tabs,
      row_ops_tabs,
      manipulate_tabs,
      advance_tabs,
      history_tabs
    )
  }

  observe({
    all_tabs <- generate_tabs(session$userData$LITE_VERSION)

    for (tab in all_tabs) {
      if (!is.null(tab)) {
        insertTab(inputId = "selector", tab)
      }
    }
  })
})

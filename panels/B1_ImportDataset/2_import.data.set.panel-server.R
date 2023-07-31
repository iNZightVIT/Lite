options(shiny.maxRequestSize = 5 * 1024^2)


import_reactives <- reactiveValues(
  success = NULL
)

observe({
  input$filter_data_perform
  isolate({

  })
})

ext_choices <- list(
  "Comma Seperated Values (.csv)" = "csv",
  "Tab-delimited Text Files (.txt, .tsv)" = "tsv",
  "Tab-delimited Text Files (.txt, .tsv)" = "txt",
  "SPSS Files (.sav)" = "sav",
  "SAS Data Files (.sas7bdat)" = "sas7dbat",
  "SAS XPORT Files (.xpt)" = "xpt",
  "97-2003 Excel Files (.xls)" = "xls",
  "2007 Excel Files (.xlsx)" = "xlsx",
  "SATA Files (.dta)" = "dta",
  "JSON (.json)" = "json",
  "R Object (.rds)" = "rds",
  "RData Files (.RData, .rda)" = "RData",
  "RData Files (.RData, .rda)" = "rda"
  # "Survey Design Files (.svydesign)" = "svydesign",
  # "Linked Data (.inzlnk)" = "inzlnk"
)
delim_choices <- list(
  "Detected automatically" = NULL,
  "Comma (,)" = ",",
  "Semi-colon (;)" = ";",
  "Tab" = "\t"
)

reset_preview_data <- function() {
  preview_data <<- reactiveValues(
    fpath = NULL,
    data = NULL,
    preview_data = NULL,
    delimiter = NULL,
    ext = NULL,
    comment_symbol = "#",
    available_dnames = NULL,
    current_dname = NULL,
    # use count to prevent rendering multiple times
    state = NULL
  )
  output$preview_data <<- renderDataTable({
    datatable(
      preview_data$preview_data,
      selection = "single",
      options = list(dom = "t")
    )
  })
}
reset_preview_data()

# output$preview_data <- renderDataTable({
#   datatable(
#     preview_data$preview_data,
#     selection = "single",
#     options = list(dom = "t")
#   )
# })

# smart_delimiter = function(fpath) {
#   ext = tolower(tools::file_ext(fpath[1]))
#   switch(
#     ext,
#     "csv" = ",",
#     "tsv" = "\t",
#     # default txt file delim to tab
#     "txt" = "\t",
#     NULL
#   )
# }

options(inzighttools.comment = "#")
lite_read <- function(fpath, delimiter = NULL, ext = NULL, sheet = NULL) {
  # ensure correct type
  delimiter <- unlist(delimiter)
  ext <- unlist(ext)
  # if no fpath given, get it from preview_data
  if (is.null(fpath)) {
    fpath <- preview_data$fpath
  }
  # if ext not given then guess by its file type
  if (is.null(ext)) {
    ext <- tolower(tools::file_ext(fpath[1]))
    # tools::file_ext might fail if its a googledoc which uses =csv
    # instead of .csv
    if (ext == "") {
      ext <- tolower(tools::file_ext(gsub("=", ".", fpath[1])))
    }
  }
  # treat Rdata as rda
  if (tolower(ext) == "rdata") {
    ext <- "rda"
  }
  # if delimiter not given then guess by its file type
  if (is.null(delimiter)) {
    delimiter <- "auto"
    # delimiter = smart_delimiter(fpath)
  }

  d <- tryCatch(
    if (any(grepl("pdf|docx?|odt|rtf", ext))) {
      readtext::readtext(fpath)
    } else if (ext == "txt") {
      readtext::readtext(fpath)
    } else if (ext == "rdta" | ext == "rda" | ext == "rdata") {
      rda_data <- iNZightTools::load_rda(fpath)

      if (is.null(preview_data$current_dname)) {
        # store available names
        values$data.available.dnames <- names(rda_data)
        # by default the first data is read in
        values$data.current.dname <- values$data.available.dnames[1]
        rda_data <- rda_data[values$data.current.dname]
      } else {
        rda_data <- rda_data[preview_data$current_dname]
      }

      as.data.frame(rda_data)
    } else if (ext == "tsv" | ext == "csv") {
      as.data.frame(iNZightTools::smart_read(
        fpath,
        delimiter = unlist(delimiter),
        ext = ext
      ))
    } else if (ext == "numbers") {
      stop("Not a valid file extension: ", ext)
    } else {
      # if its an excel file
      if (ext == "xls" | ext == "xlsx") {
        if (is.null(preview_data$current_dname)) {
          d <- as.data.frame(iNZightTools::smart_read(fpath))
          # get available sheets
          sheet_names <- iNZightTools::sheets(d)
          # store available sheets
          values$data.available.dnames <- sheet_names
          # by default the first sheet is read in
          values$data.current.dname <- sheet_names[1]
        } else {
          d <- as.data.frame(
            iNZightTools::smart_read(
              fpath,
              sheet = preview_data$current_dname
            )
          )
        }
      }
      d
    },
    error = identity
  )

  preview_data$preview_data <- NULL
  if (is.data.frame(d)) {
    if(LITE2){
      # values$data.set = d
      # in preview lite2 should also show the sampled data only
      values$sample.num = ifelse(nrow(d) > 2000, 500, round(nrow(d) / 4))
      preview_rows = sample(1:nrow(d), values$sample.num)
      values$sample.row = preview_rows
    } else {
      preview_rows <- 1:min(nrow(d), 5)
    }
    # show 5 columns no matter lite/lite2
    preview_cols <- 1:min(ncol(d), 5)

    try({
      data_ext <- tolower(tools::file_ext(fpath[1]))
      preview_data$fpath <- fpath
      preview_data$data <- d
      # ensure its a df
      preview_data$preview_data <- as.data.frame(d[preview_rows,preview_cols])
      preview_data$ext <- ext
      preview_data$delimiter <- delimiter
      # preview_data$state = 0,
      if (is.null(preview_data$current_dname)) {
        preview_data$available_dnames <- values$data.available.dnames
        preview_data$current_dname <- values$data.current.dname
      }
      
      row.names(preview_data$preview_data) = 1:nrow(preview_data$preview_data)

    })
  }
}

show_preview_modal <- function() {
  ext <- preview_data$ext
  is_excel <- ext %in% c("xls", "xlsx")
  is_rda <- ext %in% c("rdta", "rda", "rdata")
  delimiter <- preview_data$delimiter
  imported_preview_data <- preview_data$preview_data

  h3_title <- ifelse(is.null(imported_preview_data), "Failed to load data", "Preview")
  if (is.null(imported_preview_data)) {
    table_output <- NULL
  } else {
    table_output <- DT::dataTableOutput("preview_data")
  }

  ext_selected <- ifelse(is.null(ext), "", names(which(unlist(ext_choices) == ext)))
  select_inputs <- list(
    column(
      width = 5,
      selectInput(
        inputId = "preview.filetype",
        label = "File type",
        selected = ext_selected,
        choices = c("", unique(names(ext_choices)))
      )
    )
  )
  if (!is.null(delimiter) && !(delimiter %in% c("txt", "tsv", "csv", "json"))) {
    if (delimiter == "auto") {
      delim_selected <- "Detected automatically"
    } else {
      delim_selected <- names(delimiter)
    }

    if (is_excel | is_rda) {
      select_inputs2 <- list(
        column(
          width = 5,
          selectInput(
            inputId = "preview.sheets",
            label = ifelse(is_excel, "Sheets", "Data"),
            selected = preview_data$current_dname,
            choices = preview_data$available_dnames
          )
        )
      )
    } else {
      select_inputs2 <- list(
        column(
          width = 5,
          selectInput(
            inputId = "preview.delim",
            label = "Delimiter",
            selected = delim_selected,
            choices = names(delim_choices)
          )
        ),
        column(
          width = 2,
          textInput(
            inputId = "preview.comment",
            label = "Comment symbol",
            value = preview_data$comment_symbol
            # placeholder = "#"
          )
        )
      )
    }

    select_inputs <- append(select_inputs, select_inputs2)
  }
  select_inputs <- do.call("fluidRow", select_inputs)

  m <- modalDialog(
    title = "Import file",
    h3(h3_title),
    table_output,
    hr(),
    select_inputs,
    footer = tagList(
      actionButton(
        session$ns("cancel_import"),
        style = "background-color: #eeeeee; border-color: #e2e2e2;", "Cancel"
      ),
      actionButton(session$ns("confirm_import"), "Confirm"),
    ),
    size = "l"
  )
  showModal(m)
}

observeEvent(c(
  input$preview.filetype,
  input$preview.delim,
  input$preview.comment,
  input$preview.sheets
), {
  # check if file type is excel
  ext <- tolower(ext_choices[input$preview.filetype])
  is_excel <- ext %in% c("xls", "xlsx")
  is_rda <- ext %in% c("rdta", "rda", "rdata")
  # if first import failed, manually set the state
  if (is.null(preview_data$state)) {
    preview_data$state <- 1
  }
  # work around for preventing shiny rendering multiple times
  # `ignoreInit` dont seem to work
  if (preview_data$state == 0) {
    preview_data$state <- preview_data$state + 1
  } else {
    if (!is.null(input$preview.comment)) {
      preview_data$comment_symbol <- input$preview.comment
      options(inzighttools.comment = input$preview.comment)
    }

    delimiter <- NULL
    if (is_excel | is_rda) {
      sheet_name <- input$preview.sheets
      preview_data$current_dname <- sheet_name
    } else {
      delimiter <- input$preview.delim
      if (!is.null(input$preview.delim) && input$preview.delim == "Detected automatically") {
        delimiter <- preview_data$delimiter
      } else {
        delimiter <- delim_choices[names(delim_choices) == input$preview.delim][1]
      }
    }

    ext <- ext_choices[names(ext_choices) == input$preview.filetype][1]
    # ext choices have 2 "RData Files (.RData, .rda)"
    # default to rda
    lite_read(
      fpath = preview_data$fpath,
      delimiter = delimiter,
      ext = ext,
      sheet = sheet_name
    )
    show_preview_modal()
  }
})

# when user uploads a file
observeEvent(input$files, {
  if (file.exists(input$files[1, "datapath"])) {
    # isolate({
    preview_data$fpath <- input$files$datapath
    lite_read(fpath = preview_data$fpath)
    show_preview_modal()
    # })
  }
})

# when user clicks cancel in preview
observeEvent(input$cancel_import, {
  reset_preview_data()
  removeModal()
})

# when user confirms the data in preview
observeEvent(input$confirm_import, {
  if (!is.null(preview_data$data)) {
    if(LITE2) {
      values$data.set <- preview_data$data
      values$data.sample <- preview_data$preview_data
    } else {
      values$data.set <- preview_data$data
    }
    plot.par$design <- NULL
    
    values$data.type <- preview_data$ext
    updatePanel$doit <- updatePanel$doit + 1
    values$data.restore <<- get.data.set()
    temp.name <- make.names(tools::file_path_sans_ext(input$files[1, "name"]))

    if (length(temp.name) > 1) {
      temp.name <- temp.name[1:(length(temp.name) - 1)]
    }
    values$data.name <- temp.name

    # setting success status to show "Import sucessful" text
    import_reactives$success <- !inherits(preview_data$data, "condition")

    if (!(preview_data$ext %in% c("RData", "rda", "Rda"))) {
      code.save$name <- temp.name
      code.save$variable <- c(code.save$variable, list(c(sep(), "\n", paste0(
        sprintf("## Exploring the '%s' dataset", code.save$name),
        "\n"
      ))))
      code <- c(paste0(code.save$name, " <- "), gsub(paste0("\".*(?=.", preview_data$ext, ")"), paste0("\"", values$data.name), iNZightTools::code(preview_data$data), perl = T))
      code <- do.call(c, lapply(code, function(x) {
        y <- try(
          {
            formatR::tidy_source(text = x, width.cutoff = 80, output = F, indent = 4)$text.tidy
          },
          silent = TRUE
        )
        if (inherits(y, "try-error")) x else c(y, "\n")
      }))
      code <- gsub("(.*)\\).*", "\\1)", paste0(code, collapse = "\n"))
      code.save$variable <- c(code.save$variable, list(c("\n", code, "\n")))
    }
    values$name.restore <- temp.name
    updateSelectInput(session, "subs2", selected = "none")
    updateSelectInput(session, "subs1", selected = "none")
    updateSelectInput(session, "vari2", selected = "none")
    updateSelectInput(session, "vari1", selected = "none")
    plot.par$design <- NULL
    design_params$design <- NULL
  }
  removeModal()
  reset_preview_data()
  updateTabsetPanel(session, "selector", "visualize")
})


# "Import file from url" button
observeEvent(input$import_set, {
  if (!is.null(input$URLtext) && !input$URLtext %in% "") {
    input_url <- input$URLtext
    input_url <- trimws(input_url)

    isolate({
      if (!is.null(input$files) && file.exists(input$files[1, "datapath"])) {
        unlink(input$files[1, "datapath"])
      }

      preview_data$fpath <- input_url
      import_success <- tryCatch(
        {
          lite_read(fpath = input_url)
          import_reactives$success <- TRUE
          TRUE
        },
        error = function(e) {
          import_reactives$success <- FALSE
          FALSE
        }
      )
      show_preview_modal()
    })
  }
})

# whole data import panel
output$load.data.panel <- renderUI({
  input$selector
  isolate({
    # looks for get requests to pass in an URL for a dataset
    if (grepl("docs.google.com", session$clientData$url_search)) {
      URL <- session$clientData$url_search
      url.index1 <- gregexpr("url=", URL)
      url.index1 <- unlist(url.index1)
      url.index2 <- gregexpr("&land=", URL)
      url.index2 <- unlist(url.index2)
      temp <- list()
      temp$url <- substr(URL, url.index1 + 4, url.index2 - 1)
      temp$land <- substr(URL, url.index2 + 6, nchar(URL))
      load.data.panel(temp[1])
    } else {
      load.data.panel(parseQueryString(session$clientData$url_search)[1])
    }
  })
})

# output of the imported file
output$filedisplay <- renderUI({
  if (is.data.frame(get.data.set())) {
    DTOutput("filetable")
  } else if (inherits(get.data.set(), "condition")) {
    textOutput("fileError")
  } else if (!is.null(get.data.set())) {
    verbatimTextOutput("fileprint")
  } else {
    NULL
  }
})

output$fileError <- renderText(safeError(message(get.data.set.display())))

output$fileprint <- renderPrint(get.data.set.display())

output$filetable <- renderDT(get.data.set.display(),
  options = list(
    lengthMenu = c(5, 30, 50),
    pageLength = 5,
    columns.defaultContent = "NA",
    scrollX = TRUE,
    columnDefs = list(list(
      targets = "_all",
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data != null && data.length > 30 ?",
        "'<span>' + data.substr(0, 300) + '...</span>' : data;",
        "}"
      )
    ))
  )
)

output$import.data.sample.info <- renderText({
  if (!is.null(get.data.set()) && !is.null(get.data.name())) {
    paste("The displayed data is a random sample of", nrow(values$data.sample), "rows from the original data")
  }
})


observe({
  input$remove_set
  isolate({
    if (!is.null(input$remove_set) && input$remove_set > 0) {
      files <- list.files(
        path = paste0(get.data.dir.imported(), "/Imported"),
        pattern = input$Importedremove,
        full.names = TRUE
      )
      if (!is.null(input$files) && file.exists(input$files[1, "datapath"]) &&
        grepl(get.data.name(), input$files[1, "name"])) {
        unlink(input$files[1, "datapath"])
      }
      for (f in files) {
        if (file.exists(f)) {
          unlink(f)
        }
      }
    }
  })
})

output$message.success <- renderText({
  input$import_set
  input$files
  isolate({
    if (isTRUE(import_reactives$success)) {
      import_reactives$success <- F
      "Import was successful"
    } else if (isFALSE(import_reactives$success)) {
      "Import failed, check URL"
    }
  })
})

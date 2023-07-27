### -------------------------------------------###
###  Server Functions for the "About" Module  ###
### -------------------------------------------###
###
###  Date Created   :   February 10, 2015
###  Last Modified  :   February 25, 2015
###
###  Please consult the comments before editing any code.
###
library(openssl)
# library(wkb)

output$about.panel <- renderUI({
  get.vars = parseQueryString(session$clientData$url_search)
  # TODO: whats this?
  # if (!is.null(get.vars$showLogs)) {
  #   logs <- list.files(tempdir())
  #   return(logs.panel.ui(logs))
  # }
  if (length(get.vars) > 0 && (any(names(get.vars) %in% "url") || any(names(get.vars) %in% "example"))) {
    data.vals = NULL
    if (!is.null(get.vars$url) && get.vars$url != "") {
      data.vals = get.data.from.URL(get.vars$url, get.data.dir.imported())
    }
    if (!is.null(get.vars$example) && get.vars$example != "") {
      data.vals = load.data("data", get.vars$example)
    }
    if (!is.null(data.vals)) {
      values$data.set = data.vals$data.set
      values$data.restore = get.data.set()
      values$data.name = data.vals$data.name
      
      if(LITE2) {
        values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set) / 4))
        values$sample.row = sample(1:nrow(values$data.set), values$sample.num)
        values$data.sample = as.data.frame(values$data.set[values$sample.row, ])
      }
      
      if (!is.null(get.data.set()) && "land" %in% names(get.vars) && get.vars$land != "" && get.vars$land %in% "visualize") {
        updateTabsetPanel(session, "selector", "visualize")
      } else if (!is.null(get.data.set()) && "land" %in% names(get.vars) && get.vars$land != "" && get.vars$land %in% "timeSeries") {
        updateTabsetPanel(session, "selector", "timeSeries")
      } else if (!is.null(get.data.set()) && "land" %in% names(get.vars) && get.vars$land != "" && get.vars$land %in% "regression") {
        updateTabsetPanel(session, "selector", "regression")
      }
    }
  } else if (length(get.vars)>0&&
             (any(names(get.vars)%in%"filename")||
              any(names(get.vars)%in%"iv"))){
    data.vals = NULL

    f.name = rawToChar(aes_cbc_decrypt(base64_decode(get.vars$filename),
                                       hex2raw(Sys.getenv("LITE_KEY"))),
                                       hex2raw(get.vars$iv))

    get.vars$url = paste0(Sys.getenv("LITE_URL"), f.name)
    data.vals = get.data.from.URL(get.vars$url,get.data.dir.imported())
    if(!is.null(data.vals)){
      values$data.set = as.data.frame(data.vals$data.set)
      values$data.restore = get.data.set()
      values$data.name = "data"

      values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set)/4))
      values$sample.row = sample(1:nrow(values$data.set), values$sample.num)
      values$data.sample = as.data.frame(values$data.set[values$sample.row,])

      if(!is.null(get.data.set())&&
         "land"%in%names(get.vars)&&
         get.vars$land!=""&&
         get.vars$land%in%"visualize"){
        updateTabsetPanel(session,"selector","visualize")
      }else if(!is.null(get.data.set())&&
               "land"%in%names(get.vars)&&
               get.vars$land!=""&&
               get.vars$land%in%"timeSeries"){
        updateTabsetPanel(session,"selector","timeSeries")
      }else if(!is.null(get.data.set())&&
               "land"%in%names(get.vars)&&
               get.vars$land!=""&&
               get.vars$land%in%"regression"){
        updateTabsetPanel(session,"selector","regression")
      }
    }
  }
  about.panel.ui(get.lite.version(), get.lite.update())
})

# pop up for change log
observeEvent(input$change_log_link, {
  change_log <- includeMarkdown("NEWS.md")

  showModal(modalDialog(
    title = "Change Log",
    renderUI(change_log),
    easyClose = TRUE
  ))
})

observeEvent(input$disconnect, {
  session$close()
})

observeEvent(input$log_file, {
  log_f <- file.path(tempdir(), input$log_file)
  print(log_f)
  if (!file.exists(log_f)) {
    return()
  }

  print("reading log file")
  log <- shinylogs::read_json_logs(log_f)

  output$log_session <- renderTable({
    log$session |>
      tibble::as_tibble() |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.list),
          ~ paste0(., collapse = ", ")
        )
      )
  })
  output$log_inputs <- renderTable({
    if (length(log$inputs) == 0) {
      return(NULL)
    }
    log$inputs |>
      tibble::as_tibble() |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.list),
          ~ paste0(., collapse = ", ")
        )
      ) |>
      dplyr::arrange(dplyr::desc(timestamp)) |>
      dplyr::filter(!grepl("^.clientdata", name))
  })
  output$log_errors <- renderTable({
    if (length(log$errors) == 0) {
      return(NULL)
    }
    log$errors |>
      tibble::as_tibble() |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.list),
          ~ paste0(., collapse = ", ")
        )
      ) |>
      dplyr::arrange(dplyr::desc(timestamp))
  })
  output$log_outputs <- renderTable({
    if (length(log$outputs) == 0) {
      return(NULL)
    }
    log$outputs |>
      tibble::as_tibble() |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.list),
          ~ paste0(., collapse = ", ")
        )
      ) |>
      dplyr::arrange(dplyr::desc(timestamp))
  })
})

# observe({
#   if(!is.null(values$sample.row) && LITE2){
#     row.names(values$data.sample) = 1:length(values$sample.row)
#     colnames(values$data.sample) = colnames(values$data.set)
#   }
# })

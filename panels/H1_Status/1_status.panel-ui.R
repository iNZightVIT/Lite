## -----------------------------------------###
###  UI Functions for the "Status" Module  ###
### ----------------------------------------###
###
###  Note: This file is sourced locally within "server.R" *

status.metric <- function(label, value) {
  div(
    class = "well",
    style = "margin-bottom: 0.75em;",
    div(style = "font-size: 0.9em; color: #666;", label),
    div(style = "font-size: 1.4em; font-weight: 600;", value)
  )
}

status.panel.ui <- function(instance_rows, summary_rows, collector_url, crowding_banner) {
  fixedPage(
    fluidRow(
      column(
        width = 12,
        h2("Instance Status"),
        p(
          style = "color: #666;",
          "Live status for your current iNZight Lite instance."
        )
      )
    ),
    fluidRow(
      column(width = 12, crowding_banner)
    ),
    fluidRow(
      column(width = 6, instance_rows),
      column(width = 6, summary_rows)
    ),
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-top: 0.75em;",
          tags$a(
            href = "#",
            class = "btn btn-warning btn-lg",
            onclick = "performReconnect(); return false;",
            "Force reconnect to new instance"
          )
        ),
        if (!is.null(collector_url) && nzchar(collector_url)) {
          p(
            style = "margin-top: 0.75em; color: #666;",
            "Cluster summary source: ",
            code(collector_url)
          )
        } else {
          p(
            style = "margin-top: 0.75em; color: #666;",
            "Cluster summary unavailable (STATUS_REPORT_URL not configured)."
          )
        }
      )
    )
  )
}

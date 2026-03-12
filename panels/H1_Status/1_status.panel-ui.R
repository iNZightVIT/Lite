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

status.bar.metric <- function(label, value, percent) {
  pct <- suppressWarnings(as.numeric(percent))
  if (is.na(pct)) pct <- 0
  pct <- max(0, min(100, pct))

  bar_class <- if (pct >= 85) {
    "progress-bar-danger"
  } else if (pct >= 65) {
    "progress-bar-warning"
  } else {
    "progress-bar-success"
  }

  div(
    class = "well",
    style = "margin-bottom: 0.75em;",
    div(style = "font-size: 0.9em; color: #666;", label),
    div(class = "progress", style = "margin: 0.35em 0 0.2em 0;",
      div(
        class = paste("progress-bar", bar_class),
        role = "progressbar",
        style = paste0("width: ", round(pct), "%;"),
        `aria-valuenow` = round(pct),
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    ),
    div(style = "font-size: 1.1em; font-weight: 600;", value)
  )
}

status.panel.ui <- function(instance_rows, summary_rows, crowding_banner) {
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
        )
      )
    )
  )
}

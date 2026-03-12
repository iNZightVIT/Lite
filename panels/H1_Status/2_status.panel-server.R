## ---------------------------------------------###
###  Server Functions for the "Status" Module  ###
### --------------------------------------------###

safe_fetch_json <- function(url) {
  tryCatch(
    {
      payload <- readLines(url, warn = FALSE)
      if (length(payload) == 0) return(NULL)
      jsonlite::fromJSON(paste(payload, collapse = "\n"))
    },
    error = function(e) NULL
  )
}

fmt_status_time <- function(x) {
  if (is.null(x) || !nzchar(x)) return("\u2013")
  t <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (is.na(t)) return(as.character(x))
  format(t, "%Y-%m-%d %H:%M:%S UTC")
}

fmt_uptime <- function(seconds) {
  if (is.null(seconds) || !is.finite(seconds)) return("\u2013")
  s <- as.integer(seconds)
  d <- s %/% 86400
  h <- (s %% 86400) %/% 3600
  m <- (s %% 3600) %/% 60
  if (d > 0) return(sprintf("%dd %dh %dm", d, h, m))
  if (h > 0) return(sprintf("%dh %dm", h, m))
  sprintf("%dm %ds", m, s %% 60)
}

pick_first <- function(...) {
  vals <- list(...)
  for (v in vals) {
    if (!is.null(v) && !(is.character(v) && length(v) == 1 && !nzchar(v))) {
      return(v)
    }
  }
  NULL
}

as_num <- function(x) suppressWarnings(as.numeric(x))

build_crowding_banner <- function(local_status, tasks) {
  local_task_id <- local_status$task_id
  local_sessions <- as_num(local_status$sessions$active)

  if (is.null(local_task_id) || !nzchar(local_task_id)) {
    return(div(
      class = "alert alert-info",
      style = "margin-top: 0.5em;",
      strong("Crowding check unavailable."),
      " Local task ID is not available yet."
    ))
  }

  if (is.null(tasks) || length(tasks) == 0) {
    return(div(
      class = "alert alert-info",
      style = "margin-top: 0.5em;",
      strong("Crowding check unavailable."),
      " Waiting for cluster task stats from the collector."
    ))
  }

  task_df <- tryCatch(as.data.frame(tasks), error = function(e) NULL)
  if (is.null(task_df) || nrow(task_df) == 0 || !("task_id" %in% names(task_df))) {
    return(div(
      class = "alert alert-info",
      style = "margin-top: 0.5em;",
      strong("Crowding check unavailable."),
      " Collector task data format is not ready."
    ))
  }

  if (!("active_sessions" %in% names(task_df))) {
    task_df$active_sessions <- NA_real_
  }

  task_df$active_sessions <- as_num(task_df$active_sessions)
  task_df <- task_df[!is.na(task_df$active_sessions), , drop = FALSE]
  if (nrow(task_df) == 0) {
    return(div(
      class = "alert alert-info",
      style = "margin-top: 0.5em;",
      strong("Crowding check unavailable."),
      " No active session counts are available yet."
    ))
  }

  local_row <- task_df[task_df$task_id == local_task_id, , drop = FALSE]
  if (nrow(local_row) == 0) {
    return(div(
      class = "alert alert-info",
      style = "margin-top: 0.5em;",
      strong("Crowding check unavailable."),
      " This instance is not present in the latest cluster report yet."
    ))
  }

  if (is.na(local_sessions)) {
    local_sessions <- local_row$active_sessions[[1]]
  }

  all_sessions <- task_df$active_sessions
  min_sessions <- min(all_sessions)
  median_sessions <- stats::median(all_sessions)
  max_sessions <- max(all_sessions)
  total_instances <- length(all_sessions)

  # Robust spread estimate (MAD) so we can detect multiple crowded instances.
  mad_sessions <- stats::mad(all_sessions, center = median_sessions, constant = 1)
  crowded_cutoff <- median_sessions + max(3, 2 * mad_sessions)
  crowded_gap_cutoff <- max(5, round(0.5 * max(1, median_sessions)))

  crowded_mask <- all_sessions >= crowded_cutoff &
    (all_sessions - min_sessions) >= crowded_gap_cutoff
  crowded_count <- sum(crowded_mask)

  is_crowded <- total_instances > 1 &&
    crowded_count > 0 &&
    local_sessions >= crowded_cutoff &&
    (local_sessions - min_sessions) >= crowded_gap_cutoff

  if (is_crowded) {
    return(div(
      class = "alert alert-warning",
      style = "margin-top: 0.5em;",
      strong("This instance looks crowded."),
      sprintf(
        " You are at %s active sessions.",
        local_sessions
      ),
      sprintf(
        " Crowded threshold is %.1f; %s/%s instances are currently above it.",
        crowded_cutoff,
        crowded_count,
        total_instances
      ),
      sprintf(
        " Lowest instance has %s sessions; median is %s.",
        min_sessions,
        round(median_sessions, 1)
      ),
      " Reconnect may move you to a less busy instance."
    ))
  }

  div(
    class = "alert alert-success",
    style = "margin-top: 0.5em;",
    strong("This instance is not unusually crowded."),
    sprintf(
      " Current sessions: %s (range across instances: %s to %s).",
      local_sessions,
      min_sessions,
      max_sessions
    ),
    " You can still reconnect if performance is poor."
  )
}

output$status.panel <- renderUI({
  invalidateLater(15000, session)

  local_status <- safe_fetch_json("http://127.0.0.1:3099/__status")

  collector_base <- Sys.getenv("STATUS_REPORT_URL", "")
  collector_base <- sub("/$", "", collector_base)
  summary <- NULL
  tasks <- NULL
  if (nzchar(collector_base)) {
    summary <- safe_fetch_json(paste0(collector_base, "/api/summary"))
    tasks <- safe_fetch_json(paste0(collector_base, "/api/tasks"))
  }

  instance_rows <- tagList(
    status.metric("Hostname", pick_first(local_status$hostname, session$clientData$url_hostname, "\u2013")),
    status.metric("Task ID", pick_first(local_status$task_id, "\u2013")),
    status.metric("Version", pick_first(local_status$version, values$lite.version, "\u2013")),
    status.metric("Uptime", fmt_uptime(local_status$uptime_seconds)),
    status.metric("Active sessions (this instance)", pick_first(local_status$sessions$active, "\u2013")),
    status.metric("CPU (%)", pick_first(local_status$cpu$percent, "\u2013")),
    status.metric("Memory used (%)", pick_first(local_status$memory$percent_used, "\u2013")),
    status.metric("Status timestamp", fmt_status_time(local_status$timestamp))
  )

  summary_rows <- if (is.null(summary)) {
    tagList(
      h3("General Stats"),
      p(style = "color: #666;", "Collector summary currently unavailable.")
    )
  } else {
    tagList(
      h3("General Stats"),
      status.metric("Instances", pick_first(summary$task_count, "\u2013")),
      status.metric("Active sessions (all instances)", pick_first(summary$active_sessions, "\u2013")),
      status.metric("Requests (latest interval)", pick_first(summary$request_total, "\u2013")),
      status.metric("Latest report", fmt_status_time(summary$reported_at))
    )
  }

  crowding_banner <- build_crowding_banner(local_status, tasks)

  status.panel.ui(instance_rows, summary_rows, collector_base, crowding_banner)
})

library(httpuv)
library(jsonlite)

BOOT_TIME <- Sys.time()

task_id <- tryCatch(
  {
    meta_uri <- Sys.getenv("ECS_CONTAINER_METADATA_URI_V4", "")
    if (nzchar(meta_uri)) {
      meta <- jsonlite::fromJSON(paste0(meta_uri, "/task"))
      arn <- meta$TaskARN
      substr(arn, nchar(arn) - 7L, nchar(arn))
    } else {
      "local"
    }
  },
  error = function(e) "local"
)

parse_meminfo <- function() {
  lines <- readLines("/proc/meminfo", warn = FALSE)
  vals <- setNames(
    as.numeric(sub(".*:\\s+(\\d+).*", "\\1", lines)),
    sub(":.*", "", lines)
  )
  total <- vals[["MemTotal"]] / 1024
  available <- vals[["MemAvailable"]] / 1024
  list(
    total_mb = round(total, 1),
    used_mb = round(total - available, 1),
    available_mb = round(available, 1),
    percent_used = round((total - available) / total * 100, 1)
  )
}

parse_loadavg <- function() {
  parts <- strsplit(readLines("/proc/loadavg", warn = FALSE), " ")[[1]]
  list(
    load_1m = as.numeric(parts[1]),
    load_5m = as.numeric(parts[2]),
    load_15m = as.numeric(parts[3])
  )
}

count_shiny_processes <- function() {
  instances <- as.integer(Sys.getenv("SHINY_INSTANCES", "3"))
  base_port <- 3000L
  running <- 0L
  for (i in seq_len(instances)) {
    port <- base_port + i
    rc <- system2("curl", c("-sf", "-o", "/dev/null", "--max-time", "1",
      sprintf("http://127.0.0.1:%d", port)), stdout = FALSE, stderr = FALSE)
    if (rc == 0L) running <- running + 1L
  }
  list(
    configured = instances,
    running = running
  )
}

build_status <- function() {
  list(
    task_id = task_id,
    uptime_seconds = round(as.numeric(difftime(Sys.time(), BOOT_TIME, units = "secs"))),
    cpu = parse_loadavg(),
    memory = parse_meminfo(),
    shiny = count_shiny_processes(),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

app <- list(
  call = function(req) {
    status <- tryCatch(
      build_status(),
      error = function(e) list(error = conditionMessage(e))
    )
    list(
      status = 200L,
      headers = list(
        "Content-Type" = "application/json",
        "Cache-Control" = "no-cache"
      ),
      body = toJSON(status, auto_unbox = TRUE, pretty = TRUE)
    )
  }
)

cat("Status server starting on port 3099\n")
runServer("127.0.0.1", 3099, app)

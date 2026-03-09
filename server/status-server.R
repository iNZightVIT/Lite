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

count_connections <- function() {
  tryCatch(
    {
      instances <- as.integer(Sys.getenv("SHINY_INSTANCES", "3"))
      base_port <- 3000L

      # Read all TCP connections (both IPv4 and IPv6)
      read_tcp <- function(path) {
        lines <- readLines(path, warn = FALSE)[-1]
        if (length(lines) == 0L) return(list())
        strsplit(trimws(lines), "\\s+")
      }
      all_conns <- c(read_tcp("/proc/net/tcp"), read_tcp("/proc/net/tcp6"))

      # Exclude internal remotes: loopback (127.x) and private ranges (10.x, 172.16–31.x, 169.254.x)
      # /proc/net/tcp stores IPv4 as 8 hex chars, little-endian: AABBCCDD -> bytes DD CC BB AA -> octets (7:8)(5:6)(3:4)(1:2)
      is_internal_remote <- function(rem_hex) {
        if (is.na(rem_hex) || nchar(rem_hex) != 8) return(FALSE)
        o1 <- strtoi(substr(rem_hex, 7, 8), 16L)
        o2 <- strtoi(substr(rem_hex, 5, 6), 16L)
        (o1 == 127) || (o1 == 10) || (o1 == 172 && o2 >= 16 && o2 <= 31) || (o1 == 169 && o2 == 254)
      }
      external <- sum(vapply(all_conns, function(p) {
        if (length(p) < 4) return(FALSE)
        if (!grepl(":0EFE$", p[2]) || p[4] != "01") return(FALSE)
        rem <- strsplit(p[3], ":", fixed = TRUE)[[1]][1]
        !is_internal_remote(rem)
      }, logical(1)))

      # Per-instance: ESTABLISHED connections to each Shiny backend port
      per_instance <- list()
      for (i in seq_len(instances)) {
        port_hex <- toupper(sprintf("%04X", base_port + i))
        n <- sum(vapply(all_conns, function(p) {
          grepl(paste0(":", port_hex, "$"), p[2]) && p[4] == "01"
        }, logical(1)))
        per_instance[[paste0("instance_", i)]] <- n
      }

      c(list(total = external), per_instance)
    },
    error = function(e) list(total = "unavailable")
  )
}

scrape_traefik_metrics <- function() {
  tryCatch(
    {
      raw <- system2("curl", c("-sf", "--max-time", "2",
        "http://127.0.0.1:8080/metrics"),
        stdout = TRUE, stderr = FALSE)
      if (length(raw) == 0L) return(list(error = "unavailable"))

      # Parse Prometheus text format: metric_name{labels} value
      parse_metric <- function(lines, name) {
        pattern <- paste0("^", name, "(\\{[^}]+\\})?\\s+")
        matched <- grep(pattern, lines, value = TRUE)
        if (length(matched) == 0L) return(NULL)
        lapply(matched, function(line) {
          # Extract labels
          labels <- list()
          label_match <- regmatches(line, regexpr("\\{[^}]+\\}", line))
          if (length(label_match) == 1L) {
            pairs <- strsplit(gsub("[{}]", "", label_match), ",")[[1]]
            for (pair in pairs) {
              kv <- strsplit(pair, "=")[[1]]
              if (length(kv) == 2L) {
                labels[[kv[1]]] <- gsub('"', '', kv[2])
              }
            }
          }
          # Extract value
          val <- as.numeric(sub(".*\\s+", "", line))
          list(labels = labels, value = val)
        })
      }

      # Per-service request totals
      req_metrics <- parse_metric(raw, "traefik_service_requests_total")
      services <- list()
      for (m in req_metrics) {
        svc <- m$labels$service %||% "unknown"
        code <- m$labels$code %||% "unknown"
        if (is.null(services[[svc]])) services[[svc]] <- list()
        services[[svc]][[paste0("http_", code)]] <-
          (services[[svc]][[paste0("http_", code)]] %||% 0) + m$value
      }

      # Open connections (global, by entrypoint; sum if multiple protocols per entrypoint)
      conn_metrics <- parse_metric(raw, "traefik_open_connections")
      open_conns <- list()
      for (m in conn_metrics) {
        ep <- m$labels$entrypoint %||% "unknown"
        open_conns[[ep]] <- (open_conns[[ep]] %||% 0) + m$value
      }

      list(
        open_connections = open_conns,
        requests_by_service = services
      )
    },
    error = function(e) list(error = conditionMessage(e))
  )
}

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

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
  traefik <- scrape_traefik_metrics()
  conns <- count_connections()
  # Prefer Traefik's open-connection count for entrypoint "web" (app port 3838) when available
  total <- if (is.numeric(traefik$open_connections$web)) {
    as.integer(traefik$open_connections$web)
  } else {
    conns$total
  }
  connections <- c(list(total = total), conns[names(conns) != "total"])
  list(
    task_id = task_id,
    uptime_seconds = round(as.numeric(difftime(Sys.time(), BOOT_TIME, units = "secs"))),
    cpu = parse_loadavg(),
    memory = parse_meminfo(),
    shiny = count_shiny_processes(),
    connections = connections,
    services = traefik,
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

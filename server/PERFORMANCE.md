# Stack performance and timeout investigation

## Architecture (production)

- **Traefik** (port 3838): reverse proxy; routes `/` to Shiny backends (sticky cookie), `/__status` to status-server.
- **Shiny R processes** (3001..3010): one R process per instance; single-threaded per process.
- **status-server** (Node, 127.0.0.1:3099): `/` and `/__status` return JSON; `/session` accepts session events. Single event loop.

## Bottlenecks identified and fixes

### 1. Status-server blocking on every `/__status` (fixed)

**Issue:** `buildStatus()` called `countShinyProcesses()`, which ran `execSync(curl ...)` once per Shiny instance (e.g. 10 × 1s). Every GET `/__status` could block the Node event loop for up to N seconds, so the status endpoint became slow and any Shiny session calling it (Status panel) could block the R process.

**Fix:** Shiny health is now cached (`cachedShinyHealth`). It is updated in the existing 30s interval (reporter/reaper) and at startup. GET `/__status` no longer runs curl.

### 2. Traefik waiting forever for backends (fixed)

**Issue:** No backend timeouts were set. If a Shiny instance or status-server was stuck, Traefik kept the connection open and the client saw “never loads” / response timeout.

**Fix:** Added `serversTransports` in `generate-traefik-configs.sh`:
- **shiny-transport:** `responseHeaderTimeout: 90s`, `dialTimeout: 10s` for shiny-service.
- **status-transport:** `responseHeaderTimeout: 15s`, `dialTimeout: 5s` for status-service.

After the timeout, Traefik fails the request (e.g. 504) instead of hanging.

### 3. R status panel blocking without timeout (fixed)

**Issue:** `safe_fetch_json()` used `readLines(url)` with R’s default timeout (60s). If status-server or the collector was slow, the Shiny process blocked for up to 60s and locked all sessions on that instance.

**Fix:** `safe_fetch_json()` now sets `options(timeout = 5)` for the duration of the fetch so status/collector calls abort after 5s and return NULL instead of blocking the process.

### 4. Status panel polling for all sessions (fixed earlier)

**Issue:** The Status panel’s 15s polling ran for every Shiny session, not only when the Status tab was visible, multiplying load on status-server and the collector.

**Fix:** The observer that fetches status and reschedules only runs when `input$selector == "status"`.

### 5. Request duration (latency) tracking

**Added:** status-server now scrapes Traefik’s Prometheus histogram metrics (`traefik_entrypoint_request_duration_seconds_sum` / `_count` and `traefik_service_request_duration_seconds_*`) and computes the **average request duration over the last 30s** (per reporting interval). Exposed as `requests.in_duration_avg_sec` and `requests.out_duration_avg_sec` in `/__status` and in the ingest payload. Use these to spot when latency spikes (e.g. > 5–10s) and correlate with stuck sessions.

**Durations stay null?** Traefik only exposes histogram `_sum`/`_count` after at least one request. You need: (1) some traffic (open the app in a browser, click around), (2) wait **two or three** 30s reporting intervals (60–90s) so a baseline is stored and the next scrape can compute an average over the interval. If still null, list what Traefik exposes: `docker exec <container> curl -s http://127.0.0.1:8080/metrics | grep -E 'duration|request'` and confirm lines like `traefik_entrypoint_request_duration_seconds_sum{...,entrypoint="web",...}` exist.

## Further investigation if issues persist

- **Traefik access logs:** Enable `accessLog.filePath` in `traefik.yml` and log request duration (and status code) per request to see which URLs or backends are slow.
- **Which Shiny instance:** When users report “stuck”, check which instance they’re on (sticky cookie or load balancer logs). Correlate with that instance’s CPU/memory and request duration for that task.
- **R event loop:** Shiny is single-threaded; any long-running reactive or `readLines`/HTTP call blocks the whole process. Profile with `profvis` or add logging around known slow paths (e.g. status fetch, heavy visualizations).
- **Status collector:** If the dashboard or ingest is slow, check SQLite lock/contention and add indexes or batch deletes; consider request duration metrics in the collector DB for trend views.

## Remaining recommendations

- **Health checks:** Traefik can use health checks to stop sending traffic to a backend that is down or repeatedly timing out. This would require a small health endpoint and Traefik service health-check configuration.
- **Monitor timeouts:** If 504s or “gateway timeout” appear in logs after these changes, a backend is exceeding the new timeouts; investigate that instance (e.g. R busy, status-server overloaded).
- **status-server startup:** `refreshShinyHealthCache()` runs synchronously at startup (once per Shiny instance curl). If `SHINY_INSTANCES` is large, consider deferring the first refresh with `setImmediate` so the server listens first, then populates the cache.

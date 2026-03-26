# Stack performance and timeout investigation

## Architecture (production)

- **Traefik** (port 3838): reverse proxy; routes `/` to Shiny backends (sticky cookie), `/__status` and `/healthz` to status-server.
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

**How it’s computed:** For each 30s interval we take the **delta** of Traefik’s cumulative `_sum` and `_count` (current scrape minus previous). Average = delta_sum / delta_count → **average response time per request in seconds** (not a total). The collector then averages these per-report values across instances/buckets for the dashboard.

**Durations stay null?** Traefik only exposes histogram `_sum`/`_count` after at least one request. You need: (1) some traffic (open the app in a browser, click around), (2) wait **two or three** 30s reporting intervals (60–90s) so a baseline is stored and the next scrape can compute an average over the interval. If still null, list what Traefik exposes: `docker exec <container> curl -s http://127.0.0.1:8080/metrics | grep -E 'duration|request'` and confirm lines like `traefik_entrypoint_request_duration_seconds_sum{...,entrypoint="web",...}` exist.

**Values like ~40s?** Typical response times are under 1s. If you see tens of seconds, the metric may be in **milliseconds** (some exporters use ms despite `_seconds_` in the name). status-server converts only when the computed average is > 60 (treats as ms and divides by 1000). For values in the 1–60 range we do not auto-convert, so 40 could mean 40s (real slow requests) or 40ms (unit mismatch); check Traefik’s metric docs or raw `/metrics` output to confirm unit.

## Further investigation if issues persist

- **Traefik access logs:** Enable `accessLog.filePath` in `traefik.yml` and log request duration (and status code) per request to see which URLs or backends are slow.
- **Which Shiny instance:** When users report “stuck”, check which instance they’re on (sticky cookie or load balancer logs). Correlate with that instance’s CPU/memory and request duration for that task.
- **R event loop:** Shiny is single-threaded; any long-running reactive or `readLines`/HTTP call blocks the whole process. Profile with `profvis` or add logging around known slow paths (e.g. status fetch, heavy visualizations).
- **Status collector:** If the dashboard or ingest is slow, check SQLite lock/contention and add indexes or batch deletes; consider request duration metrics in the collector DB for trend views.

## Remaining recommendations

- **Health checks:** Keep platform/container checks on `/healthz` (lightweight) and, if needed, add Traefik backend health checks for Shiny services.
- **Monitor timeouts:** If 504s or “gateway timeout” appear in logs after these changes, a backend is exceeding the new timeouts; investigate that instance (e.g. R busy, status-server overloaded).
- **status-server startup:** `refreshShinyHealthCache()` runs synchronously at startup (once per Shiny instance curl). If `SHINY_INSTANCES` is large, consider deferring the first refresh with `setImmediate` so the server listens first, then populates the cache.

## Scale-down idle hardening (minor changes)

- **Dedicated health endpoint:** status-server now exposes `GET /healthz` (lightweight `200 ok`) and Traefik routes `/healthz` to status-service. This lets container/ALB health checks avoid `/` (Shiny) when desired.
- **Session cleanup decoupled from ingest success:** ended/stale sessions are removed every interval regardless of whether collector ingest succeeds, preventing memory growth during collector outages.
- **Idle throttle (opt-in):** set `STATUS_IDLE_THROTTLE_ENABLED=1` to run expensive periodic work less often when `active_sessions == 0`. Tune `STATUS_IDLE_WORK_INTERVALS` (default `4`) to run heavy work every N intervals while idle.

## Scale-down verification checklist

Use the same low-traffic window before and after enabling idle hardening:

1. **Status collector baseline:** compare `/api/summary`, `/api/tasks`, and `/api/history?range=1h` for request floor (`request_total`) and session floor (`active_sessions`).
2. **CloudWatch correlation:** overlay ECS `CPUUtilization` and `MemoryUtilization` with ALB request count per target and ECS scale events.
3. **Success criteria:**
   - lower idle request baseline per task,
   - no monotonic growth in session-map memory footprint when collector ingest is unavailable,
   - faster and more consistent scale-in after traffic drops.

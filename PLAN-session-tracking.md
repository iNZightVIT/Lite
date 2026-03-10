# Plan: Session-level user tracking via Shiny events

## Architecture diagram

```
┌──────────────────────── AWS ECS Task (one container) ────────────────────────┐
│                                                                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                          │
│  │  Shiny #1   │  │  Shiny #2   │  │  Shiny #N   │   Shiny instances        │
│  │  :3001      │  │  :3002      │  │  :300N      │   (server.R)             │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘                          │
│         │ start/ping/end │                │                                  │
│         │  (curl async)  │                │                                  │
│         └────────────────┼────────────────┘                                  │
│                          ▼                                                   │
│             ┌────────────────────────┐                                       │
│             │  status-server.js      │   Node.js: HTTP server (:3099)        │
│             │                        │   + in-memory session store           │
│             │  GET  /  → full JSON   │   + /proc metrics (mem, cpu%)        │
│             │  POST /session → event │   + CPU % via /proc/stat delta       │
│             │                        │   + built-in reporter (setInterval)   │
│             │  Every 30s: POST to    │   + stale session reaper             │
│             │  collector /ingest     │   + version & hostname               │
│             └────────────┬───────────┘                                       │
│                          │                                                   │
└──────────────────────────┼───────────────────────────────────────────────────┘
                           │ HTTPS POST /ingest
                           │ (Bearer token auth)
                           ▼
              ┌────────────────────────┐
              │  status-collector      │   Central Node.js server
              │  (index.js)            │
              │                        │   SQLite: status_reports
              │  POST /ingest          │
              │  GET  /api/summary     │   Stores: active_sessions count,
              │  GET  /api/tasks       │   version, hostname, cpu_percent
              │  GET  /api/history     │
              │  GET  /  (dashboard)   │
              └────────────────────────┘
                           │
                           ▼
              ┌────────────────────────┐
              │  Dashboard (HTML/JS)   │
              │                        │
              │  Summary cards:        │   Instances | Active Sessions
              │  Time series chart:    │   sessions + instances over time
              │  Range: 1h/6h/1d/7d   │   X-axis: "minutes ago" / "hours ago"
              │  Task table:           │   sessions, health, mem/cpu sparklines
              └────────────────────────┘
```

## Goal

Replace the current connection-counting approach (polling `/proc/net/tcp` every 60s) with event-driven session tracking using Shiny's `onSessionEnded`. This gives near-realtime user counts and a log of individual session start/end times, including disconnect reasons (clean close, timeout, crash).

Additionally:
- **Rewrite status-server from R to Node.js** — simpler, better suited to the task, absorbs reporter
- Track **Lite version** and **hostname** per task
- Improve CPU metrics from load averages to actual **CPU utilization %**
- Redesign dashboard with **time range selector** and **"X ago" x-axis labels**
- Reduce reporting interval from 60s to **30s** for better granularity

## Current architecture

```
Shiny app (server.R)  →  no session reporting
status-server.R       →  polls /proc/net/tcp + Traefik metrics every request
status-reporter.sh    →  POSTs status JSON to collector every 60s
status-collector      →  stores in SQLite, serves dashboard
```

The connection count is a rough proxy — it counts ESTABLISHED TCP sockets, which doesn't map 1:1 to user sessions (websocket connections persist, HTTP/2 multiplexes, idle timeouts create gaps).

---

## Design

### 1. Rewrite status-server as Node.js (replacing status-server.R + status-reporter.sh)

**Why:** The status-server does no R-specific work — it reads `/proc` files, serves JSON, and manages in-memory state. Node.js is better suited for all of these, and by absorbing the reporter loop (`setInterval` + `fetch`) it eliminates `status-reporter.sh` entirely. That's two processes replaced by one.

**What the new `status-server.js` does:**

1. **HTTP server on :3099** (localhost only, via `http.createServer`)
   - `GET /` — returns full status JSON (metrics + sessions + version + hostname)
   - `POST /session` — accepts session events from Shiny instances

2. **Metrics collection** (on each GET, or cached for a few seconds):
   - `/proc/meminfo` → memory total/used/available/percent
   - `/proc/stat` → CPU utilization % (delta from previous reading)
   - Shiny instance health checks (curl to each port)

3. **In-memory session store:**
   - `Map<session_id, { instance, start_time, last_ping }>`
   - Updated by POST /session events
   - Stale reaper runs every 30s via `setInterval`
   - Ended sessions (closed or reaped) are removed from the map once they have been included in a report to the collector — no disk persistence needed, since a status-server restart implies the Shiny instances restarted too

4. **Built-in reporter** (replaces `status-reporter.sh`):
   - Every 30s: build status JSON, POST to `STATUS_REPORT_URL/ingest` with Bearer token
   - Uses `fetch()` (Node 18+ built-in) — no dependencies
   - After a successful report, remove ended sessions from memory (they've been delivered)
   - Logs success/failure to stdout

**Dependencies:** Zero. Uses only Node.js built-ins (`http`, `fs`, `os`, `child_process` for Shiny health checks). Node.js may need to be added to the container image (e.g., `apk add nodejs` on Alpine, or copy a static binary). Verify during Phase 1.

#### Example status JSON output

```json
{
  "task_id": "abc12345",
  "version": "2026.01.3",
  "hostname": "ip-10-0-1-42",
  "uptime_seconds": 3600,
  "cpu": {
    "percent": 42.3,
    "load_1m": 1.23,
    "load_5m": 0.98,
    "load_15m": 0.87
  },
  "memory": {
    "total_mb": 4096,
    "used_mb": 2048,
    "available_mb": 2048,
    "percent_used": 50.0
  },
  "shiny": {
    "configured": 3,
    "running": 3
  },
  "sessions": {
    "active": 5
  },
  "timestamp": "2026-03-10T12:00:00+0000"
}
```

#### CPU utilization % (replacing load averages)

**Problem:** Load averages (from `/proc/loadavg`) are not CPU utilization — they measure runnable processes over time and scale with core count, making them hard to interpret on varying instance sizes.

**Solution:** Compute actual CPU utilization % by reading `/proc/stat` twice with a delta. Store the previous reading in memory; on each status request, compute the delta since last read. The ~30s interval between reporter polls gives a meaningful average.

```js
// /proc/stat first line: cpu  user nice system idle iowait irq softirq steal
function readCpuJiffies() {
  const line = fs.readFileSync('/proc/stat', 'utf8').split('\n')[0];
  const vals = line.trim().split(/\s+/).slice(1).map(Number);
  const idle = vals[3] + vals[4]; // idle + iowait
  const total = vals.reduce((a, b) => a + b, 0);
  return { idle, total };
}

let prevCpu = readCpuJiffies();

function getCpuPercent() {
  const curr = readCpuJiffies();
  const dTotal = curr.total - prevCpu.total;
  const dIdle = curr.idle - prevCpu.idle;
  prevCpu = curr;
  if (dTotal === 0) return 0;
  return Math.round((1 - dIdle / dTotal) * 1000) / 10; // one decimal
}
```

#### Stale session reaping

A sweep runs every 30 seconds: any session whose `last_ping` is older than 2 minutes is auto-ended with `reason: "stale"` and an `end` event is emitted. This handles:
- Shiny instance crashes (no `onSessionEnded` fires)
- Network partitions
- Any edge case where events are lost

#### Version and hostname

Read once at startup. Version is parsed from the `DESCRIPTION` file (source of truth — e.g., `Version: 2026.01.3`):
```js
const version = (() => {
  try {
    const desc = fs.readFileSync('/app/DESCRIPTION', 'utf8');
    const match = desc.match(/^Version:\s*(.+)$/m);
    return match ? match[1].trim() : 'unknown';
  } catch { return 'unknown'; }
})();
const hostname = os.hostname();
```

### 2. Shiny app changes (server.R)

Use `system2("curl", ..., wait = FALSE)` — runs async, doesn't block the R process, no new dependencies needed.

```r
# Determine which instance this is
shiny_instance <- as.integer(Sys.getenv("SHINY_INSTANCE", "0"))
session_id <- substr(session$token, 1, 12)

# Helper: fire-and-forget POST to local status server (async, non-blocking)
report_session <- function(event, reason = NULL) {
  body <- toJSON(list(
    event = event,
    session_id = session_id,
    instance = shiny_instance,
    reason = reason,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), auto_unbox = TRUE)
  tryCatch(
    system2("curl", c(
      "-sf", "-X", "POST",
      "-H", "Content-Type: application/json",
      "-d", shQuote(body),
      "--max-time", "1",
      "http://127.0.0.1:3099/session"
    ), wait = FALSE),
    error = function(e) {} # fire-and-forget
  )
}

# Report session start
report_session("start")

# Heartbeat ping every 30s
ping_observer <- observe({
  invalidateLater(30000, session)
  report_session("ping")
})

# Report session end
onSessionEnded(function() {
  ping_observer$destroy()
  report_session("end", reason = "closed")
})
```

#### Disconnect reason detection

Shiny's `onSessionEnded` doesn't provide a reason, but we can infer one:

- **"closed"** — default when `onSessionEnded` fires normally (user navigated away or idle timeout)
- **"timeout"** — can be detected by comparing session duration to the idle timeout (currently ~15 min per the disconnect modal text). Track last input time via `session$input` observer and compare on end.
- **"error"** — reported from client-side JavaScript (see below)

Client-side enhancement (`www/js/disconnect.js` already listens for `shiny:disconnected`):

```javascript
// In disconnect.js, before loading the modal:
$(document).on("shiny:disconnected", function(e) {
  navigator.sendBeacon("/__session_event", JSON.stringify({
    event: "client_disconnect",
  }));
});
```

This `/__session_event` route through Traefik could forward to the status-server, or we simply rely on the stale-session reaper — if `onSessionEnded` never fires (true crash/segfault), the session goes stale after 2 minutes and gets reaped with `reason: "stale"`.

**Practical reason mapping:**
| Scenario | `onSessionEnded` fires? | Reason |
|----------|------------------------|--------|
| User closes tab / navigates away | Yes | `"closed"` |
| Idle timeout (15 min) | Yes | `"closed"` (could detect as `"timeout"` via last-input tracking) |
| R error / Shiny crash | Usually yes | `"closed"` (Shiny catches most errors) |
| Segfault / OOM kill | No | `"stale"` (reaped after 2 min) |
| Instance restart / deploy | No | `"stale"` (reaped after 2 min) |

### 3. Status-collector changes (index.js)

#### Database schema changes

1. **Update `status_reports` table:**

   Drop `connections_total` and `load_1m` (replaced by `active_sessions` and `cpu_percent`). For fresh installs, the CREATE TABLE becomes:
   ```sql
   CREATE TABLE IF NOT EXISTS status_reports (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     task_id TEXT NOT NULL,
     reported_at TEXT NOT NULL,
     uptime_seconds REAL,
     active_sessions INTEGER,
     cpu_percent REAL,
     memory_used_mb REAL,
     memory_percent_used REAL,
     shiny_configured INTEGER,
     shiny_running INTEGER,
     version TEXT,
     hostname TEXT,
     raw_json TEXT
   );
   ```

   For existing DBs, add the new columns via ALTER and stop writing to the old ones.

#### Endpoint changes

1. **Update `POST /ingest`:**
   - Extract `sessions.active` → store as `active_sessions` in `status_reports`
   - Extract `cpu.percent` → store as `cpu_percent`
   - Extract `version` and `hostname` → store in new columns

2. **Update `GET /api/summary`:** Replace `total_connections` with `active_sessions`.

3. **Update `GET /api/tasks`:**
   - Include `active_sessions`, `cpu_percent`, `version`, `hostname` per task
   - History should span **30 minutes** (for sparklines) — use a separate query window from the visibility window
   - Include `cpu_percent` and `active_sessions` in the history array alongside `mem`

4. **Update `GET /api/history`:**
   - Accept `range` parameter: `1h`, `6h`, `1d`, `7d` (instead of just `hours`)
   - Add `active_sessions` per time bucket (sum across tasks from `status_reports`)
   - Return timestamps as ISO strings (dashboard computes "X ago" labels)

   Active session counts are derived directly from `status_reports.active_sessions` — no separate sessions endpoint needed for Phases 1-2.

### 4. Dashboard changes (index.html)

#### Summary cards
```
┌──────────────┐  ┌──────────────┐
│  Instances   │  │   Sessions   │
│     12       │  │      47      │
└──────────────┘  └──────────────┘
```

- **Instances** = active task count (existing)
- **Sessions** = total active sessions (from `active_sessions`)

#### Time series chart

**Default view: Last 1 hour**, x-axis labelled "minutes ago" (e.g., "60", "45", "30", "15", "now")

Range selector buttons above the chart:
```
[ 1h ]  [ 6h ]  [ 1d ]  [ 7d ]
```

| Range | Bucket size | X-axis labels |
|-------|-------------|---------------|
| 1h    | 1 minute    | "60 min ago" → "now" |
| 6h    | 5 minutes   | "6h ago" → "now" |
| 1d    | 15 minutes  | "24h ago" → "now" |
| 7d    | 1 hour      | "7d ago" → "now" |

Two lines:
- **Sessions** (solid blue, left y-axis) — primary metric
- **Instances** (dashed orange, right y-axis) — same as current task_count

#### Task table

| Column | Source | Notes |
|--------|--------|-------|
| Task ID | `task_id` | Short hash, as today |
| Version | `version` | e.g., "2026.01.3" (from DESCRIPTION) |
| Hostname | `hostname` | Container hostname |
| Sessions | `active_sessions` | Current active count |
| Health | `last_seen` | Green dot if <90s ago, yellow if <3min, red if >3min |
| Memory % | sparkline (30 min) | Blue sparkline + current value |
| CPU % | sparkline (30 min) | Orange sparkline + current value (now real %) |

The task history endpoint returns the last 30 minutes of data for sparklines, giving ~60 data points at 30s reporting interval.

### 5. Supervisor / instance configuration

Each Shiny instance needs to know its instance number. The `generate-traefik-configs.sh` script already starts instances in a loop — add `SHINY_INSTANCE` as an environment variable to each instance's supervisor config.

```bash
environment=LITE_INSTANCE="$i",SHINY_INSTANCES="${SHINY_INSTANCES}"
```

Update supervisor to run `node /app/server/status-server.js` instead of `Rscript /app/server/status-server.R`. Remove the `status-reporter` program section entirely.

---

## Implementation phases

### Phase 1: Status-server rewrite (Node.js) + Shiny events — DONE ✓
1. ✓ Create `server/status-server.js`: HTTP server, /proc parsing (mem, cpu%), session store, stale reaper, built-in reporter, version/hostname
2. ✓ Delete `server/status-server.R` and `server/status-reporter.sh`
3. ✓ Update `server.R`: `report_session()` helper, start/end/ping events
4. ✓ Update `server/generate-traefik-configs.sh`: replace status-server/reporter supervisor entries with single `status-server.js` entry, add `SHINY_INSTANCES` env var
5. ✓ Update `Dockerfile`: install Node.js 20 via NodeSource (distro package was Node 12, too old for `fetch()`), remove `chmod +x` on deleted `status-reporter.sh`

**Variations from plan:**
- Used `LITE_INSTANCE` env var (already existed in codebase) instead of `SHINY_INSTANCE` as originally written in the plan
- Used `jsonlite::toJSON` in server.R (already loaded) instead of `rjson::toJSON` (not available)
- Added input validation: event type whitelist, session ID length cap (100), session store size cap (10,000), request body size limit (10KB)
- Combined reaper + reporter into a single sequential `setInterval` to avoid race conditions between independent timers
- Dockerfile change was not in the original plan — Node.js 20 needed to be installed via NodeSource since the base image (`rocker/r-ver:4.2`) only has Node 12 in its distro repos

### Phase 2: Collector + dashboard — DONE ✓
5. ✓ Update `server/status-collector/index.js`: schema changes (add `active_sessions`, `cpu_percent`, `version`, `hostname`; retain old columns in migrated DBs), update /ingest, /api/summary, /api/tasks, /api/history
6. ✓ Update `server/status-collector/www/index.html`: new summary cards, time range selector, "X ago" x-axis, redesigned task table with health/version/hostname

**Variations from plan:**
- Added WAL mode for better concurrent read/write performance
- Schema migration preserves old columns (`connections_total`, `load_1m`) in migrated DBs rather than dropping them — SQLite doesn't support DROP COLUMN cleanly
- Sparkline history uses a separate 30-minute query window (configurable via `SPARKLINE_MINUTES`) independent of the dashboard visibility window
- History bucketing uses epoch-based math (`Math.floor(ms / bucketMs) * bucketMs`) instead of hour/minute decomposition for correctness across day boundaries
- Session aggregation uses latest-per-task-per-bucket (not averaging) to accurately represent concurrent session counts
- Added `escapeHtml()` for XSS prevention on task_id, version, hostname in dashboard
- Range defaults to `1h` (plan said default view is last 1 hour)

### Phase 3: Disconnect reasons (enhancement)
7. Add last-input tracking in `server.R` to distinguish `"timeout"` from `"closed"`
8. Optionally enhance `disconnect.js` with `sendBeacon` for client-side crash detection
9. Dashboard breakdown of end reasons

---

## File change summary

| File | Change |
|------|--------|
| `server/status-server.js` | **NEW** — Node.js replacement for status-server.R + status-reporter.sh. HTTP server (:3099), /proc metrics (mem, cpu%), session store, stale reaper, built-in 30s reporter, version/hostname. Zero dependencies (Node built-ins only). |
| `server/status-server.R` | **DELETE** — replaced by status-server.js |
| `server/status-reporter.sh` | **DELETE** — reporter loop absorbed into status-server.js |
| `server.R` | `report_session()` helper with system2 async, start/end/ping events |
| `server/status-collector/index.js` | Schema: replace `connections_total`/`load_1m` with `active_sessions`/`cpu_percent`, add `version`/`hostname` columns. Update /ingest, /api/summary, /api/tasks, /api/history. |
| `server/status-collector/www/index.html` | Redesigned dashboard: sessions card, time range selector (1h/6h/1d/7d), "X ago" x-axis, task table with version/hostname/health/cpu% |
| `server/generate-traefik-configs.sh` | Replace status-server + reporter supervisor entries with single `node status-server.js` entry. Add `SHINY_INSTANCES` env var to Shiny instances. |
| `Dockerfile` | Install Node.js 20 via NodeSource, remove `chmod +x` on deleted `status-reporter.sh`. |
| `www/js/disconnect.js` | (Phase 3) Optional sendBeacon for client-side crash detection |

No new npm dependencies — `status-server.js` uses only Node.js built-ins (`http`, `fs`, `os`, `child_process`). Node.js 20 is installed via NodeSource in the Dockerfile.

---

## Considerations

- **Async & non-blocking:** `system2(..., wait = FALSE)` spawns curl in background — the Shiny process is never blocked by session reporting.
- **Graceful degradation:** If status-server is unreachable, all reports fail silently. Shiny app is completely unaffected.
- **Reconnects:** `session$allowReconnect(TRUE)` is enabled. A reconnect fires `onSessionEnded` on the old session and creates a new one. This accurately reflects the user's state.
- **No disk persistence needed:** If the status-server restarts, the Shiny instances restarted too — there's nothing to recover. Ended sessions are simply removed from memory once they've been successfully reported to the collector.
- **Data volume:** With ~100 concurrent users and 30s pings, that's ~200 events/min in memory on the status-server. The collector only stores one row per task per 30s report (the active count), keeping the DB lean.
- **No transition period needed:** Sessions fully replace connection counting — `/proc/net/tcp` parsing and Traefik metrics scraping are dropped entirely.
- **CPU accuracy:** `/proc/stat` delta gives true CPU utilization % regardless of core count, unlike load averages. The 30s interval between reads gives a meaningful average — not too noisy, not too smoothed.
- **Version tracking:** Read once at startup — the version won't change during a container's lifetime. Useful for verifying deployments and spotting mixed-version fleets.
- **Why Node.js:** The status-server does zero R-specific work. Node.js is a natural fit for "lightweight HTTP server with timers and in-memory state." Consolidating status-server + reporter into one process reduces moving parts. Node may need to be added to the container image.
- **Simplicity:** The status payload sends only `sessions.active` (a count), not individual session events. The collector stores this count per report — no event replay, no dedup, no ring buffers. Per-session event tracking (for disconnect reason analytics) can be added later in Phase 3 if needed.
- **SQLite under 30s writes from many tasks:** Each task writes once per 30s. With 50 tasks, that's ~100 writes/min — trivially within SQLite's capability. WAL mode recommended for concurrent reads.
- **Data retention:** All data is kept (no automatic pruning). At 30s intervals with 50 tasks, that's ~144K rows/day (~5MB/day). TODO: monitor disk usage on the collector and add configurable retention/pruning if space becomes a concern.

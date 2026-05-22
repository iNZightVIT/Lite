# iNZight Lite Status Collector

Central aggregation service and dashboard for iNZight Lite ECS task status. Each ECS task reports its status about every 30 seconds; this service stores the data and serves a dashboard.

## Deploy on VPS (Docker Compose)

Includes standalone Traefik with Let's Encrypt (HTTP→HTTPS redirect, auto SSL).

```bash
cd server/status-collector
cp .env.example .env
# Edit .env: set INGEST_TOKEN, DOMAIN (e.g. status-dev.inzight.example.com), ACME_EMAIL
docker compose up -d
```

Ensure ports 80 and 443 are open. Traefik handles TLS and routes traffic to the status-collector.

## Configure ECS Tasks

Set these environment variables in your ECS task definition:

- `STATUS_REPORT_URL` — base URL of this collector (e.g. `https://status.example.com`)
- `STATUS_REPORT_TOKEN` — same value as `INGEST_TOKEN` on the collector

Use Secrets Manager or SSM Parameter Store for the token. Add GitHub secret `STATUS_REPORT_TOKEN` if you inject it during deployment.

## API

| Endpoint       | Method | Description                                                   |
| -------------- | ------ | ------------------------------------------------------------- |
| `/ingest`      | POST   | Accept status JSON (requires `Authorization: Bearer <token>`) |
| `/api/health`  | GET    | Fleet health for monitors (`ok`, per-task issues); `?hostnames=` filter; always HTTP 200 |
| `/api/summary` | GET    | Task count, total connections, latest report time             |
| `/api/tasks`   | GET    | Per-task breakdown (tasks that reported in the active window) |
| `/api/history` | GET    | Time-series (`?range=1h\|6h\|1d\|7d`), for charts              |
| `/`            | GET    | Dashboard (static HTML)                                       |

### `/api/health`

Used by Instatus (or similar) instead of polling `/api/tasks`. Only tasks that reported within `HEALTH_WINDOW_MINUTES` (default 2, same as `/api/summary`) are evaluated, so instances removed by scale-in drop out quickly instead of appearing stale for five minutes.

Per-task status matches the dashboard dots: **healthy** if last report &lt; 90s and all Shiny workers up; **degraded** if 90–180s; **stale** if ≥ 180s; **shiny_down** if `shiny_running` &lt; `shiny_configured`.

**`ok`** is true when `tasks_healthy` ≥ `HEALTH_MIN_HEALTHY_TASKS` (default 1) and no task has **shiny_down** for longer than `SHINY_DOWN_FAIL_SEC` (default 120). A new task with Shiny still starting up is not treated as down until that window elapses. Degraded/stale counts are informational (`status` may be `degraded` while `ok` is still true).

For Instatus, assert **JSON path `ok` equals `true`** (response is always HTTP 200).

Summary and task list only include instances that reported in the last **2 minutes** by default (`ACTIVE_WINDOW_MINUTES`). So when testing locally with one instance, you’ll see at most one task once older reports age out; set a higher value (e.g. 5) in production if needed.

## Data retention and database size

- Reports older than **`RETENTION_DAYS`** (default **7**) are deleted on each successful ingest.
- **`MAX_DB_SIZE_MB`** (default **800**) sets SQLite **`max_page_count`** so the **main** `status.db` file cannot grow past roughly that size. Set **`0`** to disable the cap. The **`-wal`** file may still use a few extra MB until checkpoint.
- If the DB **already** exceeds the cap on startup (e.g. after lowering the limit), the collector deletes older data in steps and runs **`VACUUM`** (can take minutes), then applies the cap.
- **`STORE_RAW_JSON`**: omit full ingest JSON per row by default (`0`). Set **`1`** only if you need raw payloads in SQLite (they bloat the DB quickly).


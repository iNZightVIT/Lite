# iNZight Lite Status Collector

Central aggregation service and dashboard for iNZight Lite ECS task status. Each ECS task reports its status every minute; this service stores the data and serves a dashboard.

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
| `/api/summary` | GET    | Task count, total connections, latest report time             |
| `/api/tasks`   | GET    | Per-task breakdown (tasks that reported in the active window) |
| `/api/history` | GET    | Time-series (`?hours=6`), for charts                          |
| `/`            | GET    | Dashboard (static HTML)                                       |

Summary and task list only include instances that reported in the last **2 minutes** by default (`ACTIVE_WINDOW_MINUTES`). So when testing locally with one instance, you’ll see at most one task once older reports age out; set a higher value (e.g. 5) in production if needed.

## Data Retention

Reports older than 7 days are deleted automatically.

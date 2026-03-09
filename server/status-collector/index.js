const express = require("express");
const Database = require("better-sqlite3");
const path = require("path");

const PORT = process.env.PORT || 8787;
const INGEST_TOKEN = process.env.INGEST_TOKEN;
const DB_PATH = process.env.DB_PATH || path.join(__dirname, "data", "status.db");
// Only tasks that reported in this window count as "active" (summary + task list)
const ACTIVE_WINDOW_MINUTES = Math.max(1, parseInt(process.env.ACTIVE_WINDOW_MINUTES, 10) || 2);

const app = express();
app.use(express.json({ limit: "1mb" }));

// Ensure data dir exists and init DB
const dataDir = path.dirname(DB_PATH);
try {
  require("fs").mkdirSync(dataDir, { recursive: true });
} catch (e) {}

const db = new Database(DB_PATH);

db.exec(`
  CREATE TABLE IF NOT EXISTS status_reports (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    task_id TEXT NOT NULL,
    reported_at TEXT NOT NULL,
    uptime_seconds REAL,
    connections_total INTEGER,
    memory_used_mb REAL,
    memory_percent_used REAL,
    load_1m REAL,
    shiny_configured INTEGER,
    shiny_running INTEGER,
    raw_json TEXT
  );
  CREATE INDEX IF NOT EXISTS idx_status_reports_task_reported
    ON status_reports(task_id, reported_at);
`);

const insertStmt = db.prepare(`
  INSERT INTO status_reports (
    task_id, reported_at, uptime_seconds, connections_total,
    memory_used_mb, memory_percent_used, load_1m,
    shiny_configured, shiny_running, raw_json
  ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
`);

const RETENTION_DAYS = 7;
const deleteOldStmt = db.prepare(`
  DELETE FROM status_reports
  WHERE datetime(reported_at) < datetime('now', ?)
`);

function trimOldRows() {
  try {
    deleteOldStmt.run(`-${RETENTION_DAYS} days`);
  } catch (e) {}
}

function parsePayload(body) {
  const conns = body.connections;
  const total = conns && typeof conns.total === "number" ? conns.total : null;
  const mem = body.memory;
  const usedMb = mem && typeof mem.used_mb === "number" ? mem.used_mb : null;
  const pctUsed =
    mem && typeof mem.percent_used === "number" ? mem.percent_used : null;
  const cpu = body.cpu;
  const load1 = cpu && typeof cpu.load_1m === "number" ? cpu.load_1m : null;
  const shiny = body.shiny;
  const cfg =
    shiny && typeof shiny.configured === "number" ? shiny.configured : null;
  const run =
    shiny && typeof shiny.running === "number" ? shiny.running : null;

  return {
    task_id: body.task_id || "unknown",
    reported_at: body.timestamp || new Date().toISOString(),
    uptime_seconds: typeof body.uptime_seconds === "number" ? body.uptime_seconds : null,
    connections_total: total,
    memory_used_mb: usedMb,
    memory_percent_used: pctUsed,
    load_1m: load1,
    shiny_configured: cfg,
    shiny_running: run,
    raw_json: JSON.stringify(body),
  };
}

// POST /ingest — token-protected
app.post("/ingest", (req, res) => {
  const auth = req.headers.authorization;
  const token = auth && auth.startsWith("Bearer ") ? auth.slice(7) : null;

  if (INGEST_TOKEN) {
    if (!token || token !== INGEST_TOKEN) {
      res.status(401).json({ error: "Unauthorized" });
      return;
    }
  }

  let body;
  try {
    body = req.body;
    if (!body || typeof body !== "object") {
      res.status(400).json({ error: "Invalid JSON body" });
      return;
    }
  } catch (e) {
    res.status(400).json({ error: "Invalid JSON" });
    return;
  }

  const row = parsePayload(body);
  try {
    insertStmt.run(
      row.task_id,
      row.reported_at,
      row.uptime_seconds,
      row.connections_total,
      row.memory_used_mb,
      row.memory_percent_used,
      row.load_1m,
      row.shiny_configured,
      row.shiny_running,
      row.raw_json
    );
    trimOldRows();
  } catch (e) {
    console.error(e);
    res.status(500).json({ error: "Database error" });
    return;
  }

  res.status(204).send();
});

// GET /api/summary
app.get("/api/summary", (req, res) => {
  const cutoff = new Date(Date.now() - ACTIVE_WINDOW_MINUTES * 60 * 1000).toISOString();
  const rows = db
    .prepare(
      `SELECT task_id, connections_total, reported_at
       FROM status_reports
       WHERE reported_at >= ?
       ORDER BY reported_at DESC`
    )
    .all(cutoff);

  const byTask = new Map();
  for (const r of rows) {
    if (!byTask.has(r.task_id)) {
      byTask.set(r.task_id, r.connections_total ?? 0);
    }
  }
  const tasks = new Set(rows.map((r) => r.task_id));
  const totalConnections = Array.from(byTask.values()).reduce((sum, n) => sum + n, 0);
  const latest = rows[0]?.reported_at || null;

  res.json({
    task_count: tasks.size,
    total_connections: totalConnections,
    reported_at: latest,
  });
});

// GET /api/tasks
app.get("/api/tasks", (req, res) => {
  const cutoff = new Date(Date.now() - ACTIVE_WINDOW_MINUTES * 60 * 1000).toISOString();
  const rows = db
    .prepare(
      `SELECT task_id, reported_at, uptime_seconds, connections_total,
              memory_used_mb, memory_percent_used, load_1m,
              shiny_configured, shiny_running
       FROM status_reports
       WHERE reported_at >= ?
       ORDER BY task_id, reported_at DESC`
    )
    .all(cutoff);

  const byTask = new Map();
  for (const r of rows) {
    if (!byTask.has(r.task_id)) {
      byTask.set(r.task_id, { ...r });
    }
  }

  res.json(
    Array.from(byTask.values()).map((t) => ({
      task_id: t.task_id,
      last_seen: t.reported_at,
      uptime_seconds: t.uptime_seconds,
      connections_total: t.connections_total,
      memory_used_mb: t.memory_used_mb,
      memory_percent_used: t.memory_percent_used,
      load_1m: t.load_1m,
      shiny_configured: t.shiny_configured,
      shiny_running: t.shiny_running,
    }))
  );
});

// GET /api/history — time-series for charts (bucketed by minute)
app.get("/api/history", (req, res) => {
  const hours = Math.min(24, Math.max(1, parseInt(req.query.hours, 10) || 6));
  const cutoff = new Date(Date.now() - hours * 60 * 60 * 1000).toISOString();

  const rows = db
    .prepare(
      `SELECT reported_at, task_id, connections_total
       FROM status_reports
       WHERE reported_at >= ?
       ORDER BY reported_at ASC`
    )
    .all(cutoff);

  const byTime = new Map();
  for (const r of rows) {
    const bucket = r.reported_at.slice(0, 16);
    if (!byTime.has(bucket)) {
      byTime.set(bucket, { timestamp: bucket, tasks: new Set(), total: 0 });
    }
    const cur = byTime.get(bucket);
    cur.tasks.add(r.task_id);
    cur.total += r.connections_total ?? 0;
  }

  const points = Array.from(byTime.entries())
    .map(([t, v]) => ({
      timestamp: t,
      task_count: v.tasks.size,
      total_connections: v.total,
    }))
    .sort((a, b) => a.timestamp.localeCompare(b.timestamp));

  res.json({ history: points });
});

// Serve static dashboard
app.use(express.static(path.join(__dirname, "www")));

app.listen(PORT, () => {
  console.log(`Status collector listening on port ${PORT}`);
});

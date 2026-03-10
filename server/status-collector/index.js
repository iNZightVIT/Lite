const express = require("express");
const Database = require("better-sqlite3");
const path = require("path");

const PORT = process.env.PORT || 8787;
const INGEST_TOKEN = process.env.INGEST_TOKEN;
const DB_PATH = process.env.DB_PATH || path.join(__dirname, "data", "status.db");
// Only tasks that reported in this window count as "active" (summary + task list)
const ACTIVE_WINDOW_MINUTES = Math.max(1, parseInt(process.env.ACTIVE_WINDOW_MINUTES, 10) || 2);
// Dashboard task list: hide tasks not seen within this many minutes; sort by last seen
const DASHBOARD_VISIBLE_MINUTES = Math.max(1, parseInt(process.env.DASHBOARD_VISIBLE_MINUTES, 10) || 5);
// Sparkline history window for task table (minutes)
const SPARKLINE_MINUTES = 30;

const app = express();
app.use(express.json({ limit: "1mb" }));

// Ensure data dir exists and init DB
const dataDir = path.dirname(DB_PATH);
try {
  require("fs").mkdirSync(dataDir, { recursive: true });
} catch (e) {}

const db = new Database(DB_PATH);
db.pragma("journal_mode = WAL");

// --- Schema: create table with new columns for fresh installs ---
db.exec(`
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
  CREATE INDEX IF NOT EXISTS idx_status_reports_task_reported
    ON status_reports(task_id, reported_at);
`);

// --- Schema migration for existing DBs ---
// Add new columns if they don't exist. Old columns (connections_total, load_1m)
// are retained in migrated DBs for historical data. When reading, use
// connections_total as fallback for active_sessions so old rows contribute.
const existingCols = new Set(
  db.prepare("PRAGMA table_info(status_reports)").all().map((c) => c.name)
);

const sessionCol =
  existingCols.has("connections_total")
    ? "COALESCE(active_sessions, connections_total)"
    : "active_sessions";

const migrations = [
  ["active_sessions", "INTEGER"],
  ["cpu_percent", "REAL"],
  ["version", "TEXT"],
  ["hostname", "TEXT"],
];
for (const [col, type] of migrations) {
  if (!existingCols.has(col)) {
    db.exec(`ALTER TABLE status_reports ADD COLUMN ${col} ${type}`);
  }
}

const insertStmt = db.prepare(`
  INSERT INTO status_reports (
    task_id, reported_at, uptime_seconds, active_sessions,
    cpu_percent, memory_used_mb, memory_percent_used,
    shiny_configured, shiny_running, version, hostname, raw_json
  ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
  const mem = body.memory;
  const usedMb = mem && typeof mem.used_mb === "number" ? mem.used_mb : null;
  const pctUsed =
    mem && typeof mem.percent_used === "number" ? mem.percent_used : null;

  const cpu = body.cpu;
  const cpuPercent =
    cpu && typeof cpu.percent === "number" ? cpu.percent : null;

  // New format: sessions.active. Old format: connections.total or sum of connections.instance_N
  let activeSessions = null;
  const sess = body.sessions;
  if (sess && typeof sess.active === "number") {
    activeSessions = sess.active;
  } else {
    const conns = body.connections;
    if (conns && typeof conns === "object") {
      const instanceKeys = Object.keys(conns).filter((k) => k.startsWith("instance_"));
      if (instanceKeys.length > 0) {
        activeSessions = instanceKeys.reduce(
          (sum, k) => sum + (typeof conns[k] === "number" ? conns[k] : 0),
          0
        );
      } else if (typeof conns.total === "number") {
        activeSessions = conns.total;
      }
    }
  }

  const shiny = body.shiny;
  const cfg =
    shiny && typeof shiny.configured === "number" ? shiny.configured : null;
  const run =
    shiny && typeof shiny.running === "number" ? shiny.running : null;

  return {
    task_id: body.task_id || "unknown",
    reported_at: body.timestamp || new Date().toISOString(),
    uptime_seconds: typeof body.uptime_seconds === "number" ? body.uptime_seconds : null,
    active_sessions: activeSessions,
    cpu_percent: cpuPercent,
    memory_used_mb: usedMb,
    memory_percent_used: pctUsed,
    shiny_configured: cfg,
    shiny_running: run,
    version: typeof body.version === "string" ? body.version : null,
    hostname: typeof body.hostname === "string" ? body.hostname : null,
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
      row.active_sessions,
      row.cpu_percent,
      row.memory_used_mb,
      row.memory_percent_used,
      row.shiny_configured,
      row.shiny_running,
      row.version,
      row.hostname,
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
app.get("/api/summary", (_req, res) => {
  const cutoff = new Date(Date.now() - ACTIVE_WINDOW_MINUTES * 60 * 1000).toISOString();
  const rows = db
    .prepare(
      `SELECT task_id, ${sessionCol} AS active_sessions, reported_at
       FROM status_reports
       WHERE reported_at >= ?
       ORDER BY reported_at DESC`
    )
    .all(cutoff);

  // Take the most recent report per task
  const byTask = new Map();
  for (const r of rows) {
    if (!byTask.has(r.task_id)) {
      byTask.set(r.task_id, r.active_sessions ?? 0);
    }
  }
  const totalSessions = Array.from(byTask.values()).reduce((sum, n) => sum + n, 0);
  const latest = rows[0]?.reported_at || null;

  res.json({
    task_count: byTask.size,
    active_sessions: totalSessions,
    reported_at: latest,
  });
});

// GET /api/tasks — only tasks seen within DASHBOARD_VISIBLE_MINUTES, sorted by uptime (longest first)
// Includes recent history for sparkline graphs (SPARKLINE_MINUTES window)
app.get("/api/tasks", (_req, res) => {
  const cutoff = new Date(Date.now() - DASHBOARD_VISIBLE_MINUTES * 60 * 1000).toISOString();
  const sparklineCutoff = new Date(Date.now() - SPARKLINE_MINUTES * 60 * 1000).toISOString();

  // Get latest report per task for current state
  const latestRows = db
    .prepare(
      `SELECT task_id, reported_at, uptime_seconds, ${sessionCol} AS active_sessions,
              cpu_percent, memory_used_mb, memory_percent_used,
              shiny_configured, shiny_running, version, hostname
       FROM status_reports
       WHERE reported_at >= ?
       ORDER BY task_id, reported_at ASC`
    )
    .all(cutoff);

  // Build set of visible tasks and their latest data
  const byTask = new Map();
  for (const r of latestRows) {
    if (!byTask.has(r.task_id)) {
      byTask.set(r.task_id, { latest: r });
    }
    byTask.get(r.task_id).latest = r; // last row (ASC) is most recent
  }

  // Get sparkline history for visible tasks
  // SAFETY: taskIds are from our own DB query results, never user input
  const taskIds = Array.from(byTask.keys());
  if (taskIds.length > 0) {
    const placeholders = taskIds.map(() => "?").join(",");
    const historyRows = db
      .prepare(
        `SELECT task_id, reported_at, cpu_percent, memory_percent_used, ${sessionCol} AS active_sessions
         FROM status_reports
         WHERE task_id IN (${placeholders}) AND reported_at >= ?
         ORDER BY task_id, reported_at ASC`
      )
      .all(...taskIds, sparklineCutoff);

    for (const r of historyRows) {
      const entry = byTask.get(r.task_id);
      if (entry) {
        if (!entry.history) entry.history = [];
        entry.history.push({
          t: r.reported_at,
          cpu: r.cpu_percent,
          mem: r.memory_percent_used,
          sessions: r.active_sessions,
        });
      }
    }
  }

  const tasks = Array.from(byTask.values())
    .map(({ latest: t, history }) => ({
      task_id: t.task_id,
      last_seen: t.reported_at,
      uptime_seconds: t.uptime_seconds,
      active_sessions: t.active_sessions,
      cpu_percent: t.cpu_percent,
      memory_used_mb: t.memory_used_mb,
      memory_percent_used: t.memory_percent_used,
      shiny_configured: t.shiny_configured,
      shiny_running: t.shiny_running,
      version: t.version,
      hostname: t.hostname,
      history: history || [],
    }))
    .sort((a, b) => (b.uptime_seconds ?? 0) - (a.uptime_seconds ?? 0));

  res.json(tasks);
});

// GET /api/history — time-series for charts
// Accepts ?range=1h|6h|1d|7d (default: 1h)
const RANGE_CONFIG = {
  "1h": { ms: 60 * 60 * 1000, bucketMinutes: 1 },
  "6h": { ms: 6 * 60 * 60 * 1000, bucketMinutes: 5 },
  "1d": { ms: 24 * 60 * 60 * 1000, bucketMinutes: 15 },
  "7d": { ms: 7 * 24 * 60 * 60 * 1000, bucketMinutes: 60 },
};

app.get("/api/history", (req, res) => {
  const range = RANGE_CONFIG[req.query.range] ? req.query.range : "1h";
  const { ms, bucketMinutes } = RANGE_CONFIG[range];
  const cutoff = new Date(Date.now() - ms).toISOString();

  const rows = db
    .prepare(
      `SELECT reported_at, task_id, ${sessionCol} AS active_sessions
       FROM status_reports
       WHERE reported_at >= ?
       ORDER BY reported_at ASC`
    )
    .all(cutoff);

  // Bucket by time intervals, using latest report per task per bucket
  const bucketMs = bucketMinutes * 60 * 1000;
  const byTime = new Map();
  for (const r of rows) {
    const ts = new Date(r.reported_at).getTime();
    const bucketedMs = Math.floor(ts / bucketMs) * bucketMs;
    const key = new Date(bucketedMs).toISOString();

    if (!byTime.has(key)) {
      byTime.set(key, { timestamp: key, taskLatest: new Map(), taskIds: new Set() });
    }
    const entry = byTime.get(key);
    entry.taskLatest.set(r.task_id, r.active_sessions ?? 0);
    entry.taskIds.add(r.task_id);
  }

  const buckets = Array.from(byTime.values()).sort((a, b) =>
    a.timestamp.localeCompare(b.timestamp)
  );
  const PERSIST_BUCKETS = 2; // Task counts if it reported in this or prev 2 buckets
  const points = buckets.map((v, i) => {
    const sessionValues = Array.from(v.taskLatest.values());
    const seenTasks = new Set();
    for (let j = Math.max(0, i - PERSIST_BUCKETS); j <= i; j++) {
      buckets[j].taskIds.forEach((id) => seenTasks.add(id));
    }
    return {
      timestamp: v.timestamp,
      task_count: seenTasks.size,
      active_sessions: sessionValues.reduce((sum, n) => sum + n, 0),
    };
  });

  res.json({ range, history: points });
});

// Serve static dashboard
app.use(express.static(path.join(__dirname, "www")));

app.listen(PORT, () => {
  console.log(`Status collector listening on port ${PORT}`);
});

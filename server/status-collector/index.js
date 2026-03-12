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
const HOSTNAME_LOOKBACK_HOURS = 48;

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
  ["request_in_total", "REAL"],
  ["request_out_total", "REAL"],
  ["request_in_interval", "REAL"],
  ["request_out_interval", "REAL"],
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
    shiny_configured, shiny_running, version, hostname,
    request_in_total, request_out_total, request_in_interval, request_out_interval,
    raw_json
  ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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

  const reqs = body.requests && typeof body.requests === "object" ? body.requests : {};
  const requestInTotal =
    typeof reqs.in_total === "number"
      ? reqs.in_total
      : typeof body.request_in_total === "number"
      ? body.request_in_total
      : null;
  const requestOutTotal =
    typeof reqs.out_total === "number"
      ? reqs.out_total
      : typeof body.request_out_total === "number"
      ? body.request_out_total
      : null;
  const requestInInterval =
    typeof reqs.in_interval === "number"
      ? reqs.in_interval
      : typeof body.request_in_interval === "number"
      ? body.request_in_interval
      : null;
  const requestOutInterval =
    typeof reqs.out_interval === "number"
      ? reqs.out_interval
      : typeof body.request_out_interval === "number"
      ? body.request_out_interval
      : null;

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
    request_in_total: requestInTotal,
    request_out_total: requestOutTotal,
    request_in_interval: requestInInterval,
    request_out_interval: requestOutInterval,
    raw_json: JSON.stringify(body),
  };
}

function parseHostnames(queryHostnames) {
  if (!queryHostnames) return [];
  const raw = Array.isArray(queryHostnames)
    ? queryHostnames
    : String(queryHostnames).split(",");
  return raw
    .map((h) => String(h).trim())
    .filter((h) => h.length > 0);
}

function buildHostnameWhere(hostnames) {
  if (!hostnames || hostnames.length === 0) {
    return { sql: "", params: [] };
  }
  const placeholders = hostnames.map(() => "?").join(",");
  return {
    sql: ` AND hostname IN (${placeholders})`,
    params: hostnames,
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
      row.request_in_total,
      row.request_out_total,
      row.request_in_interval,
      row.request_out_interval,
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
  const hostnames = parseHostnames(req.query.hostnames);
  const hostFilter = buildHostnameWhere(hostnames);
  const rows = db
    .prepare(
      `SELECT task_id, ${sessionCol} AS active_sessions, reported_at
              , request_in_interval, request_out_interval
       FROM status_reports
       WHERE reported_at >= ?${hostFilter.sql}
       ORDER BY reported_at DESC`
    )
    .all(cutoff, ...hostFilter.params);

  // Take the most recent report per task
  const byTask = new Map();
  let requestTotal = 0;
  for (const r of rows) {
    if (!byTask.has(r.task_id)) {
      byTask.set(r.task_id, r.active_sessions ?? 0);
      const reqVal =
        typeof r.request_in_interval === "number"
          ? r.request_in_interval
          : typeof r.request_out_interval === "number"
          ? r.request_out_interval
          : 0;
      requestTotal += Math.max(0, reqVal);
    }
  }
  const totalSessions = Array.from(byTask.values()).reduce((sum, n) => sum + n, 0);
  const latest = rows[0]?.reported_at || null;

  res.json({
    task_count: byTask.size,
    active_sessions: totalSessions,
    request_total: Math.max(0, Math.round(requestTotal)),
    // Backward compatibility for older frontend clients.
    request_in_total: Math.max(0, Math.round(requestTotal)),
    request_out_total: Math.max(0, Math.round(requestTotal)),
    reported_at: latest,
  });
});

// GET /api/tasks — only tasks seen within DASHBOARD_VISIBLE_MINUTES, sorted by uptime (longest first)
// Includes recent history for sparkline graphs (SPARKLINE_MINUTES window)
app.get("/api/tasks", (req, res) => {
  const cutoff = new Date(Date.now() - DASHBOARD_VISIBLE_MINUTES * 60 * 1000).toISOString();
  const sparklineCutoff = new Date(Date.now() - SPARKLINE_MINUTES * 60 * 1000).toISOString();
  const hostnames = parseHostnames(req.query.hostnames);
  const hostFilter = buildHostnameWhere(hostnames);

  // Get latest report per task for current state
  const latestRows = db
    .prepare(
      `SELECT task_id, reported_at, uptime_seconds, ${sessionCol} AS active_sessions,
              cpu_percent, memory_used_mb, memory_percent_used,
              shiny_configured, shiny_running, version, hostname,
              request_in_interval, request_out_interval
       FROM status_reports
       WHERE reported_at >= ?${hostFilter.sql}
       ORDER BY task_id, reported_at ASC`
    )
    .all(cutoff, ...hostFilter.params);

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
         WHERE task_id IN (${placeholders}) AND reported_at >= ?${hostFilter.sql}
         ORDER BY task_id, reported_at ASC`
      )
      .all(...taskIds, sparklineCutoff, ...hostFilter.params);

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
      request_in_interval: t.request_in_interval,
      request_out_interval: t.request_out_interval,
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
  const nowMs = Date.now();
  const cutoffMs = nowMs - ms;
  const cutoff = new Date(cutoffMs).toISOString();
  const hostnames = parseHostnames(req.query.hostnames);
  const hostFilter = buildHostnameWhere(hostnames);

  const rows = db
    .prepare(
      `SELECT reported_at, task_id, ${sessionCol} AS active_sessions, request_in_interval, request_out_interval
       FROM status_reports
       WHERE reported_at >= ?${hostFilter.sql}
       ORDER BY reported_at ASC`
    )
    .all(cutoff, ...hostFilter.params);

  // Bucket by time intervals.
  // For sessions/task_count: use latest report per task per bucket with short persistence.
  // For requests: smooth latest buckets similarly by carrying forward
  // per-task request values from recent buckets.
  const bucketMs = bucketMinutes * 60 * 1000;
  const byTime = new Map();
  const currentSessionsByTask = new Map();
  const currentRequestsByTask = new Map();
  let currentSessionsTotal = 0;
  let currentRequestsTotal = 0;

  function recordBucketSample(entry, sessionsVal, requestsVal, taskCountVal) {
    if (entry.sessions_min == null || sessionsVal < entry.sessions_min) {
      entry.sessions_min = sessionsVal;
    }
    if (entry.sessions_max == null || sessionsVal > entry.sessions_max) {
      entry.sessions_max = sessionsVal;
    }
    entry.sessions_sum += sessionsVal;
    entry.sessions_count += 1;

    if (entry.requests_min == null || requestsVal < entry.requests_min) {
      entry.requests_min = requestsVal;
    }
    if (entry.requests_max == null || requestsVal > entry.requests_max) {
      entry.requests_max = requestsVal;
    }
    entry.requests_sum += requestsVal;
    entry.requests_count += 1;

    if (entry.tasks_min == null || taskCountVal < entry.tasks_min) {
      entry.tasks_min = taskCountVal;
    }
    if (entry.tasks_max == null || taskCountVal > entry.tasks_max) {
      entry.tasks_max = taskCountVal;
    }
    entry.tasks_sum += taskCountVal;
    entry.tasks_count += 1;
  }

  for (const r of rows) {
    const ts = new Date(r.reported_at).getTime();
    const bucketedMs = Math.floor(ts / bucketMs) * bucketMs;
    const key = new Date(bucketedMs).toISOString();

    if (!byTime.has(key)) {
      byTime.set(key, {
        timestamp: key,
        taskLatest: new Map(),
        taskRequestLatest: new Map(),
        taskIds: new Set(),
        sessions_min: null,
        sessions_max: null,
        sessions_sum: 0,
        sessions_count: 0,
        requests_min: null,
        requests_max: null,
        requests_sum: 0,
        requests_count: 0,
        tasks_min: null,
        tasks_max: null,
        tasks_sum: 0,
        tasks_count: 0,
      });
    }
    const entry = byTime.get(key);
    entry.taskLatest.set(r.task_id, r.active_sessions ?? 0);
    entry.taskIds.add(r.task_id);
    const reqVal =
      typeof r.request_in_interval === "number"
        ? r.request_in_interval
        : typeof r.request_out_interval === "number"
        ? r.request_out_interval
        : 0;
    const normalizedReqVal = Math.max(0, reqVal);
    entry.taskRequestLatest.set(r.task_id, normalizedReqVal);

    const prevSess = currentSessionsByTask.get(r.task_id);
    const nextSess = r.active_sessions ?? 0;
    if (prevSess == null) {
      currentSessionsByTask.set(r.task_id, nextSess);
      currentSessionsTotal += nextSess;
    } else {
      currentSessionsByTask.set(r.task_id, nextSess);
      currentSessionsTotal += nextSess - prevSess;
    }

    const prevReq = currentRequestsByTask.get(r.task_id);
    if (prevReq == null) {
      currentRequestsByTask.set(r.task_id, normalizedReqVal);
      currentRequestsTotal += normalizedReqVal;
    } else {
      currentRequestsByTask.set(r.task_id, normalizedReqVal);
      currentRequestsTotal += normalizedReqVal - prevReq;
    }

    recordBucketSample(
      entry,
      currentSessionsTotal,
      currentRequestsTotal,
      currentSessionsByTask.size
    );
  }

  const bucketStartMs = Math.floor(cutoffMs / bucketMs) * bucketMs;
  const bucketEndMs = Math.floor(nowMs / bucketMs) * bucketMs;
  for (let t = bucketStartMs; t <= bucketEndMs; t += bucketMs) {
    const key = new Date(t).toISOString();
    if (!byTime.has(key)) {
      byTime.set(key, {
        timestamp: key,
        taskLatest: new Map(),
        taskRequestLatest: new Map(),
        taskIds: new Set(),
        sessions_min: null,
        sessions_max: null,
        sessions_sum: 0,
        sessions_count: 0,
        requests_min: null,
        requests_max: null,
        requests_sum: 0,
        requests_count: 0,
        tasks_min: null,
        tasks_max: null,
        tasks_sum: 0,
        tasks_count: 0,
      });
    }
  }

  const buckets = Array.from(byTime.values()).sort((a, b) =>
    a.timestamp.localeCompare(b.timestamp)
  );
  const PERSIST_BUCKETS = 2;
  const points = buckets.map((v, i) => {
    const mergedSessionsByTask = new Map(v.taskLatest);
    const mergedRequestByTask = new Map(v.taskRequestLatest);
    let usedCarryForward = false;
    let usedRequestCarryForward = false;

    // Smooth incomplete latest buckets by carrying task session values
    // from recent buckets (same persistence horizon as task_count).
    for (let j = Math.max(0, i - PERSIST_BUCKETS); j < i; j++) {
      for (const [taskId, sess] of buckets[j].taskLatest.entries()) {
        if (!mergedSessionsByTask.has(taskId)) {
          mergedSessionsByTask.set(taskId, sess);
          usedCarryForward = true;
        }
      }
      for (const [taskId, reqVal] of buckets[j].taskRequestLatest.entries()) {
        if (!mergedRequestByTask.has(taskId)) {
          mergedRequestByTask.set(taskId, reqVal);
          usedRequestCarryForward = true;
        }
      }
    }

    const sessionValues = Array.from(mergedSessionsByTask.values());
    const requestValues = Array.from(mergedRequestByTask.values());
    const seenTasks = new Set();
    for (let j = Math.max(0, i - PERSIST_BUCKETS); j <= i; j++) {
      buckets[j].taskIds.forEach((id) => seenTasks.add(id));
    }
    return {
      timestamp: v.timestamp,
      task_count:
        v.tasks_count > 0
          ? Math.round(v.tasks_sum / v.tasks_count)
          : seenTasks.size,
      task_count_min:
        v.tasks_count > 0 ? Math.round(v.tasks_min ?? 0) : seenTasks.size,
      task_count_max:
        v.tasks_count > 0 ? Math.round(v.tasks_max ?? 0) : seenTasks.size,
      active_sessions:
        v.sessions_count > 0
          ? Math.round(v.sessions_sum / v.sessions_count)
          : sessionValues.reduce((sum, n) => sum + n, 0),
      active_sessions_min:
        v.sessions_count > 0
          ? Math.round(v.sessions_min ?? 0)
          : sessionValues.reduce((sum, n) => sum + n, 0),
      active_sessions_max:
        v.sessions_count > 0
          ? Math.round(v.sessions_max ?? 0)
          : sessionValues.reduce((sum, n) => sum + n, 0),
      request_total:
        v.requests_count > 0
          ? Math.round(v.requests_sum / v.requests_count)
          : Math.round(requestValues.reduce((sum, n) => sum + n, 0)),
      request_total_min:
        v.requests_count > 0
          ? Math.round(v.requests_min ?? 0)
          : Math.round(requestValues.reduce((sum, n) => sum + n, 0)),
      request_total_max:
        v.requests_count > 0
          ? Math.round(v.requests_max ?? 0)
          : Math.round(requestValues.reduce((sum, n) => sum + n, 0)),
      // Backward compatibility for older frontend clients.
      request_in:
        v.requests_count > 0
          ? Math.round(v.requests_sum / v.requests_count)
          : Math.round(requestValues.reduce((sum, n) => sum + n, 0)),
      request_out:
        v.requests_count > 0
          ? Math.round(v.requests_sum / v.requests_count)
          : Math.round(requestValues.reduce((sum, n) => sum + n, 0)),
      activity_estimated: usedCarryForward,
      request_estimated: usedRequestCarryForward,
    };
  });

  res.json({ range, history: points });
});

// GET /api/hostnames — available hostnames for filter controls
app.get("/api/hostnames", (_req, res) => {
  const cutoff = new Date(
    Date.now() - HOSTNAME_LOOKBACK_HOURS * 60 * 60 * 1000
  ).toISOString();
  const rows = db
    .prepare(
      `SELECT DISTINCT hostname
       FROM status_reports
       WHERE reported_at >= ?
         AND hostname IS NOT NULL
         AND TRIM(hostname) <> ''
       ORDER BY hostname ASC`
    )
    .all(cutoff);
  res.json({ hostnames: rows.map((r) => r.hostname) });
});

// Serve static dashboard
app.use(express.static(path.join(__dirname, "www")));

app.listen(PORT, () => {
  console.log(`Status collector listening on port ${PORT}`);
});

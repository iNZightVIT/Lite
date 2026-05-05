#!/usr/bin/env node
"use strict";

const http = require("http");
const fs = require("fs");
const os = require("os");

// --- Configuration ---
const PORT = 3099;
const SHINY_INSTANCES = parseInt(process.env.SHINY_INSTANCES || "3", 10);
const SHINY_BASE_PORT = 3000;
const REPORT_INTERVAL_MS = 30_000;
const STALE_THRESHOLD_MS = 120_000; // 2 minutes
const STATUS_REPORT_URL = process.env.STATUS_REPORT_URL || "";
const STATUS_REPORT_TOKEN = process.env.STATUS_REPORT_TOKEN || "";
const TRAEFIK_METRICS_URL = process.env.TRAEFIK_METRICS_URL || "http://127.0.0.1:8080/metrics";

// --- Boot-time constants ---
const BOOT_TIME = Date.now();

// Public hostname from first user session (Host header); undefined until then
let observedPublicHostname = null;
let requestMetrics = {
  in_total: null,
  out_total: null,
  in_interval: 0,
  out_interval: 0,
  in_duration_avg_sec: null,
  out_duration_avg_sec: null,
  updated_at: null,
};
let previousRequestTotals = {
  in_total: null,
  out_total: null,
  in_sum: null,
  in_count: null,
  out_sum: null,
  out_count: null,
};

const version = (() => {
  try {
    const desc = fs.readFileSync("/app/DESCRIPTION", "utf8");
    const match = desc.match(/^Version:\s*(.+)$/m);
    return match ? match[1].trim() : "unknown";
  } catch {
    return "unknown";
  }
})();

const taskId = (() => {
  try {
    const metaUri = process.env.ECS_CONTAINER_METADATA_URI_V4;
    if (!metaUri) return "local";
    // Fetch ECS task metadata synchronously at startup
    const { execSync } = require("child_process");
    const raw = execSync(`curl -sf --max-time 2 "${metaUri}/task"`, {
      encoding: "utf8",
    });
    const meta = JSON.parse(raw);
    const arn = meta.TaskARN || "";
    return arn.slice(-8) || "local";
  } catch {
    return "local";
  }
})();

const hostName = os.hostname();

// --- CPU utilization via /proc/stat ---
function readCpuJiffies() {
  const line = fs.readFileSync("/proc/stat", "utf8").split("\n")[0];
  const vals = line.trim().split(/\s+/).slice(1).map(Number);
  const idle = vals[3] + vals[4]; // idle + iowait
  const total = vals.reduce((a, b) => a + b, 0);
  return { idle, total };
}

let prevCpu = null;
try {
  prevCpu = readCpuJiffies();
} catch {
  // /proc/stat not available (e.g., macOS dev)
}

function getCpuPercent() {
  if (!prevCpu) return 0;
  try {
    const curr = readCpuJiffies();
    const dTotal = curr.total - prevCpu.total;
    const dIdle = curr.idle - prevCpu.idle;
    prevCpu = curr;
    if (dTotal === 0) return 0;
    return Math.round((1 - dIdle / dTotal) * 1000) / 10;
  } catch {
    return 0;
  }
}

// --- Memory via /proc/meminfo ---
function getMemory() {
  try {
    const raw = fs.readFileSync("/proc/meminfo", "utf8");
    const vals = {};
    for (const line of raw.split("\n")) {
      const m = line.match(/^(\w+):\s+(\d+)/);
      if (m) vals[m[1]] = parseInt(m[2], 10);
    }
    const totalMb = (vals.MemTotal || 0) / 1024;
    const availMb = (vals.MemAvailable || 0) / 1024;
    const usedMb = totalMb - availMb;
    return {
      total_mb: Math.round(totalMb * 10) / 10,
      used_mb: Math.round(usedMb * 10) / 10,
      available_mb: Math.round(availMb * 10) / 10,
      percent_used:
        totalMb > 0 ? Math.round((usedMb / totalMb) * 1000) / 10 : 0,
    };
  } catch {
    return { total_mb: 0, used_mb: 0, available_mb: 0, percent_used: 0 };
  }
}

// --- Load averages (kept for backward compat in JSON, but cpu.percent is primary) ---
function getLoadAvg() {
  try {
    const parts = fs.readFileSync("/proc/loadavg", "utf8").trim().split(/\s+/);
    return {
      load_1m: parseFloat(parts[0]),
      load_5m: parseFloat(parts[1]),
      load_15m: parseFloat(parts[2]),
    };
  } catch {
    const [l1, l5, l15] = os.loadavg();
    return {
      load_1m: Math.round(l1 * 100) / 100,
      load_5m: Math.round(l5 * 100) / 100,
      load_15m: Math.round(l15 * 100) / 100,
    };
  }
}

// --- Shiny instance health checks (cached to avoid blocking /__status) ---
// Running curl synchronously per instance blocked the event loop and could
// make /__status take up to N seconds (N = SHINY_INSTANCES), causing Shiny
// sessions that call __status to block and the whole instance to appear stuck.
let cachedShinyHealth = { configured: SHINY_INSTANCES, running: 0 };

function countShinyProcesses() {
  const { execSync } = require("child_process");
  let running = 0;
  for (let i = 1; i <= SHINY_INSTANCES; i++) {
    try {
      execSync(
        `curl -sf -o /dev/null --max-time 1 http://127.0.0.1:${SHINY_BASE_PORT + i}`,
        {
          stdio: "ignore",
        },
      );
      running++;
    } catch {
      // not running
    }
  }
  return { configured: SHINY_INSTANCES, running };
}

function refreshShinyHealthCache() {
  try {
    cachedShinyHealth = countShinyProcesses();
  } catch (e) {
    // keep previous cache on error
  }
}

// --- Session store ---
// Map<session_id, { instance, start_time, last_ping, ended, end_reason }>
const sessions = new Map();

const MAX_SESSIONS = 10_000;

function handleSessionEvent(body) {
  const { event, session_id, instance, reason } = body;
  if (!event || !session_id) return;
  if (typeof session_id !== "string" || session_id.length > 100) return;
  if (!["start", "ping", "end"].includes(event)) return;

  if (typeof body.hostname === "string" && body.hostname.trim().length > 0) {
    observedPublicHostname = body.hostname.trim();
  }
  if (event === "start") {
    if (sessions.size >= MAX_SESSIONS) {
      console.warn(
        `[status-server] session store full (${sessions.size}), rejecting new session`,
      );
      return;
    }
    sessions.set(session_id, {
      instance: instance || 0,
      start_time: Date.now(),
      last_ping: Date.now(),
      ended: false,
      end_reason: null,
    });
  } else if (event === "ping") {
    const s = sessions.get(session_id);
    if (s && !s.ended) {
      s.last_ping = Date.now();
    }
  } else if (event === "end") {
    const s = sessions.get(session_id);
    if (s) {
      s.ended = true;
      s.end_reason = reason || "closed";
    }
  }
}

function reapStaleSessions() {
  const now = Date.now();
  for (const [id, s] of sessions) {
    if (!s.ended && now - s.last_ping > STALE_THRESHOLD_MS) {
      s.ended = true;
      s.end_reason = "stale";
    }
  }
}

function getActiveSessions() {
  let count = 0;
  for (const s of sessions.values()) {
    if (!s.ended) count++;
  }
  return count;
}

function parsePrometheusLabels(labelsRaw) {
  const labels = {};
  const re = /(\w+)="([^"]*)"/g;
  let m;
  while ((m = re.exec(labelsRaw)) !== null) {
    labels[m[1]] = m[2];
  }
  return labels;
}

function sumPrometheusCounter(metricsText, metricName, matcher) {
  const escaped = metricName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const lineRegex = new RegExp(`^${escaped}\\{([^}]*)\\}\\s+([0-9.eE+-]+)\\s*$`, "gm");
  let sum = 0;
  let found = false;
  let match;
  while ((match = lineRegex.exec(metricsText)) !== null) {
    const labels = parsePrometheusLabels(match[1]);
    if (!matcher(labels)) continue;
    const value = Number(String(match[2]).trim());
    if (!Number.isFinite(value)) continue;
    sum += value;
    found = true;
  }
  return found ? sum : null;
}

// Try several (metricName, matcher) pairs; return first non-null (for Traefik v2 vs v3).
function sumPrometheusCounterAny(metricsText, pairs) {
  for (const [name, matcher] of pairs) {
    const v = sumPrometheusCounter(metricsText, name, matcher);
    if (v != null) return v;
  }
  return null;
}

function getTraefikRequestTotals(metricsText) {
  const inTotal = sumPrometheusCounter(
    metricsText,
    "traefik_entrypoint_requests_total",
    (labels) => labels.entrypoint === "web",
  );

  let outTotal = sumPrometheusCounter(
    metricsText,
    "traefik_service_requests_total",
    (labels) => labels.service === "shiny-service@file",
  );
  if (outTotal == null) {
    outTotal = sumPrometheusCounter(
      metricsText,
      "traefik_router_requests_total",
      (labels) => labels.router === "shiny-router@file",
    );
  }

  // Duration: try known metric names (Traefik v2 vs v3). If still null, Traefik may not expose
  // duration in Prometheus; inspect with: curl -s http://127.0.0.1:8080/metrics | grep -E 'duration|request'
  const inSum = sumPrometheusCounterAny(metricsText, [
    ["traefik_entrypoint_request_duration_seconds_sum", (l) => l.entrypoint === "web"],
    ["http_server_request_duration_seconds_sum", () => true],
    ["http_server_request_duration_sum", () => true],
  ]);
  const inCount = sumPrometheusCounterAny(metricsText, [
    ["traefik_entrypoint_request_duration_seconds_count", (l) => l.entrypoint === "web"],
    ["http_server_request_duration_seconds_count", () => true],
    ["http_server_request_duration_count", () => true],
  ]);
  const outSum = sumPrometheusCounterAny(metricsText, [
    ["traefik_service_request_duration_seconds_sum", (l) => l.service === "shiny-service@file"],
    ["traefik_router_request_duration_seconds_sum", (l) => l.router === "shiny-router@file"],
    ["http_client_request_duration_seconds_sum", () => true],
    ["http_client_request_duration_sum", () => true],
  ]);
  const outCount = sumPrometheusCounterAny(metricsText, [
    ["traefik_service_request_duration_seconds_count", (l) => l.service === "shiny-service@file"],
    ["traefik_router_request_duration_seconds_count", (l) => l.router === "shiny-router@file"],
    ["http_client_request_duration_seconds_count", () => true],
    ["http_client_request_duration_count", () => true],
  ]);

  return {
    in_total: inTotal,
    out_total: outTotal,
    in_sum: inSum,
    in_count: inCount,
    out_sum: outSum,
    out_count: outCount,
  };
}

async function updateRequestMetrics() {
  requestMetrics.in_interval = 0;
  requestMetrics.out_interval = 0;
  requestMetrics.in_duration_avg_sec = null;
  requestMetrics.out_duration_avg_sec = null;
  try {
    const resp = await fetch(TRAEFIK_METRICS_URL, {
      signal: AbortSignal.timeout(2_000),
    });
    if (!resp.ok) {
      throw new Error(`HTTP ${resp.status}`);
    }
    const metricsText = await resp.text();
    const totals = getTraefikRequestTotals(metricsText);

    if (totals.in_total != null) {
      const prev = previousRequestTotals.in_total;
      const delta = prev == null ? 0 : totals.in_total - prev;
      requestMetrics.in_total = Math.round(totals.in_total);
      requestMetrics.in_interval = delta >= 0 ? Math.round(delta) : 0;
      previousRequestTotals.in_total = totals.in_total;
    }

    if (totals.out_total != null) {
      const prev = previousRequestTotals.out_total;
      const delta = prev == null ? 0 : totals.out_total - prev;
      requestMetrics.out_total = Math.round(totals.out_total);
      requestMetrics.out_interval = delta >= 0 ? Math.round(delta) : 0;
      previousRequestTotals.out_total = totals.out_total;
    }

    if (
      totals.in_sum != null &&
      totals.in_count != null &&
      previousRequestTotals.in_sum != null &&
      previousRequestTotals.in_count != null
    ) {
      const dSum = totals.in_sum - previousRequestTotals.in_sum;
      const dCount = totals.in_count - previousRequestTotals.in_count;
      if (dCount > 0 && dSum >= 0) {
        requestMetrics.in_duration_avg_sec = Math.round((dSum / dCount) * 1000) / 1000;
      }
    }
    previousRequestTotals.in_sum = totals.in_sum;
    previousRequestTotals.in_count = totals.in_count;

    if (
      totals.out_sum != null &&
      totals.out_count != null &&
      previousRequestTotals.out_sum != null &&
      previousRequestTotals.out_count != null
    ) {
      const dSum = totals.out_sum - previousRequestTotals.out_sum;
      const dCount = totals.out_count - previousRequestTotals.out_count;
      if (dCount > 0 && dSum >= 0) {
        requestMetrics.out_duration_avg_sec = Math.round((dSum / dCount) * 1000) / 1000;
      }
    }
    previousRequestTotals.out_sum = totals.out_sum;
    previousRequestTotals.out_count = totals.out_count;

    requestMetrics.updated_at = new Date().toISOString();
  } catch (err) {
    console.warn(`[status-server] traefik metrics scrape failed: ${err.message}`);
  }
}

function removeEndedSessions() {
  for (const [id, s] of sessions) {
    if (s.ended) sessions.delete(id);
  }
}

// --- Build status JSON ---
function buildStatus() {
  const load = getLoadAvg();
  return {
    task_id: taskId,
    version,
    hostname: observedPublicHostname ?? null,
    uptime_seconds: Math.round((Date.now() - BOOT_TIME) / 1000),
    cpu: {
      percent: getCpuPercent(),
      ...load,
    },
    memory: getMemory(),
    shiny: cachedShinyHealth,
    sessions: {
      active: getActiveSessions(),
    },
    requests: {
      in_total: requestMetrics.in_total,
      out_total: requestMetrics.out_total,
      in_interval: requestMetrics.in_interval,
      out_interval: requestMetrics.out_interval,
      in_duration_avg_sec: requestMetrics.in_duration_avg_sec,
      out_duration_avg_sec: requestMetrics.out_duration_avg_sec,
      updated_at: requestMetrics.updated_at,
    },
    timestamp: new Date().toISOString().replace(/\.\d{3}Z$/, "+0000"),
  };
}

// --- HTTP server ---
const server = http.createServer((req, res) => {
  if (req.method === "GET" && (req.url === "/" || req.url === "/__status")) {
    const status = buildStatus();
    const json = JSON.stringify(status, null, 2);
    res.writeHead(200, {
      "Content-Type": "application/json",
      "Cache-Control": "no-cache",
    });
    res.end(json);
  } else if (req.method === "POST" && req.url === "/session") {
    let body = "";
    let size = 0;
    const MAX_BODY = 10 * 1024;
    req.on("data", (chunk) => {
      size += chunk.length;
      if (size > MAX_BODY) {
        res.writeHead(413, { "Content-Type": "application/json" });
        res.end('{"error":"payload too large"}');
        req.destroy();
        return;
      }
      body += chunk;
    });
    req.on("end", () => {
      try {
        const parsed = JSON.parse(body);
        handleSessionEvent(parsed);
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end('{"ok":true}');
      } catch {
        res.writeHead(400, { "Content-Type": "application/json" });
        res.end('{"error":"invalid json"}');
      }
    });
  } else {
    res.writeHead(404);
    res.end("Not found");
  }
});

server.listen(PORT, "127.0.0.1", () => {
  console.log(`[status-server] listening on 127.0.0.1:${PORT}`);
  console.log(
    `[status-server] task_id=${taskId} version=${version} hostname=${hostName}`,
  );
  console.log(`[status-server] shiny_instances=${SHINY_INSTANCES}`);
});

// --- Combined reaper + reporter tick (every 30s) ---
// Runs sequentially: reap stale sessions first, then report (if configured).
// This avoids race conditions between independent timers.
if (STATUS_REPORT_URL && STATUS_REPORT_TOKEN) {
  console.log(
    `[status-server] reporter enabled, posting to ${STATUS_REPORT_URL}/ingest every ${REPORT_INTERVAL_MS / 1000}s`,
  );

  setInterval(async () => {
    reapStaleSessions();
    refreshShinyHealthCache();
    await updateRequestMetrics();

    try {
      const status = buildStatus();
      const payload = JSON.stringify(status);

      const resp = await fetch(new URL("/ingest", STATUS_REPORT_URL), {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${STATUS_REPORT_TOKEN}`,
        },
        body: payload,
        signal: AbortSignal.timeout(10_000),
      });

      if (resp.ok) {
        console.log(
          `[status-server] report OK (${getActiveSessions()} active sessions)`,
        );
        removeEndedSessions();
      } else {
        console.error(`[status-server] report failed: HTTP ${resp.status}`);
      }
    } catch (err) {
      console.error(`[status-server] report error: ${err.message}`);
    }
  }, REPORT_INTERVAL_MS);
} else {
  console.log(
    "[status-server] reporter disabled (STATUS_REPORT_URL or STATUS_REPORT_TOKEN not set)",
  );
  // Still reap stale sessions and refresh Shiny health cache
  setInterval(async () => {
    reapStaleSessions();
    refreshShinyHealthCache();
    await updateRequestMetrics();
  }, REPORT_INTERVAL_MS);
}

// Populate cache at startup so first /__status is fast
refreshShinyHealthCache();

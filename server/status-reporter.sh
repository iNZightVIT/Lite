#!/bin/bash
# Reports status from local status-server to a central collector every 60 seconds.
# Sleeps forever (no retries) if STATUS_REPORT_URL or STATUS_REPORT_TOKEN are unset.

if [ -z "${STATUS_REPORT_URL}" ] || [ -z "${STATUS_REPORT_TOKEN}" ]; then
  while true; do sleep 3600; done
fi

while true; do
  status=$(curl -sf --max-time 5 "http://127.0.0.1:3099/" 2>/dev/null)
  if [ -n "$status" ]; then
    echo "$status" | curl -sf -X POST \
      -H "Content-Type: application/json" \
      -H "Authorization: Bearer ${STATUS_REPORT_TOKEN}" \
      -d @- \
      --max-time 10 \
      "${STATUS_REPORT_URL}/ingest" 2>/dev/null
    rc=$?
    if [ $rc -eq 0 ]; then
      echo "[status-reporter] POST to ${STATUS_REPORT_URL}/ingest OK"
    else
      echo "[status-reporter] POST to ${STATUS_REPORT_URL}/ingest failed (curl exit $rc)"
    fi
  else
    echo "[status-reporter] no status from localhost:3099, skipping"
  fi
  sleep 60
done

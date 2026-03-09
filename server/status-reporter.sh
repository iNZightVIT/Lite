#!/bin/bash
# Reports status from local status-server to a central collector every 60 seconds.
# Exits immediately if STATUS_REPORT_URL or STATUS_REPORT_TOKEN are unset.

if [ -z "${STATUS_REPORT_URL}" ] || [ -z "${STATUS_REPORT_TOKEN}" ]; then
  exit 0
fi

while true; do
  status=$(curl -sf --max-time 5 "http://127.0.0.1:3099/" 2>/dev/null)
  if [ -n "$status" ]; then
    echo "$status" | curl -sf -X POST \
      -H "Content-Type: application/json" \
      -H "Authorization: Bearer ${STATUS_REPORT_TOKEN}" \
      -d @- \
      --max-time 10 \
      "${STATUS_REPORT_URL}/ingest" 2>/dev/null || true
  fi
  sleep 60
done

#!/bin/bash

# Default to 10 instances if not specified
INSTANCES=${SHINY_INSTANCES:-10}
BASE_PORT=3000

echo "Generating configuration for $INSTANCES Shiny instances with Traefik..."

# Generate Traefik dynamic configuration
# serversTransport sets backend timeouts so Traefik does not wait forever if a
# Shiny instance is stuck (e.g. blocked on status fetch). Without this, timeouts
# cause the site to "never load" for users.
cat > /etc/traefik/dynamic.yml << EOF
http:
  routers:
    status-router:
      rule: "PathPrefix(\`/__status\`)"
      service: status-service
      entryPoints:
        - web
      priority: 100
    shiny-router:
      rule: "PathPrefix(\`/\`)"
      service: shiny-service
      entryPoints:
        - web
      priority: 1

  services:
    status-service:
      loadBalancer:
        serversTransport: status-transport
        servers:
          - url: "http://127.0.0.1:3099"
    shiny-service:
      loadBalancer:
        serversTransport: shiny-transport
        sticky:
          cookie:
            name: INZLITESESSION
            secure: false
            httpOnly: false
            sameSite: lax
        servers:
EOF

# Add server entries for each Shiny instance
for i in $(seq 1 $INSTANCES); do
    PORT=$((BASE_PORT + i))
    cat >> /etc/traefik/dynamic.yml << EOF
          - url: "http://127.0.0.1:${PORT}"
EOF
done

# Backend timeouts: avoid indefinite wait when Shiny or status-server is stuck
cat >> /etc/traefik/dynamic.yml << EOF

  serversTransports:
    status-transport:
      forwardingTimeouts:
        dialTimeout: "5s"
        responseHeaderTimeout: "15s"
    shiny-transport:
      forwardingTimeouts:
        dialTimeout: "10s"
        responseHeaderTimeout: "90s"
EOF

# Generate supervisor config
cat > /etc/supervisor/conf.d/supervisord.conf << EOF
[supervisord]
nodaemon=true
user=root
logfile=/dev/stdout
logfile_maxbytes=0
loglevel=info
pidfile=/var/run/supervisor/supervisord.pid

[unix_http_server]
file=/var/run/supervisor/supervisor.sock
chmod=0700

[rpcinterface:supervisor]
supervisor.rpcinterface_factory = supervisor.rpcinterface:make_main_rpcinterface

[supervisorctl]
serverurl=unix:///var/run/supervisor/supervisor.sock

[program:traefik]
command=/usr/local/bin/traefik --configfile=/etc/traefik/traefik.yml
autostart=true
autorestart=true
stdout_logfile=/dev/stdout
stdout_logfile_maxbytes=0
stderr_logfile=/dev/stderr
stderr_logfile_maxbytes=0
priority=100
user=root

EOF

# Append Shiny instances to supervisor config
for i in $(seq 1 $INSTANCES); do
    PORT=$((BASE_PORT + i))
    cat >> /etc/supervisor/conf.d/supervisord.conf << EOF
[program:shiny-${i}]
command=/usr/local/bin/R --slave -e "shiny::runApp('/app', port=${PORT}, host='127.0.0.1')"
autostart=true
autorestart=true
stdout_logfile=/dev/stdout
stdout_logfile_maxbytes=0
stderr_logfile=/dev/stderr
stderr_logfile_maxbytes=0
user=shiny
environment=HOME="/home/shiny",USER="shiny",LITE_INSTANCE="${i}",SHINY_INSTANCES="${INSTANCES}",R_LIBS_USER="/usr/local/lib/R/site-library"
priority=200
stopasgroup=true
killasgroup=true
startsecs=10
startretries=3

EOF
done

# Append status server to supervisor config (Node.js — replaces status-server.R + status-reporter.sh)
STATUS_REPORT_URL_ESC=${STATUS_REPORT_URL//%/%%}
STATUS_REPORT_TOKEN_ESC=${STATUS_REPORT_TOKEN//%/%%}
cat >> /etc/supervisor/conf.d/supervisord.conf << EOF
[program:status-server]
command=/usr/bin/node /app/server/status-server.js
autostart=true
autorestart=true
stdout_logfile=/dev/stdout
stdout_logfile_maxbytes=0
stderr_logfile=/dev/stderr
stderr_logfile_maxbytes=0
user=shiny
environment=HOME="/home/shiny",USER="shiny",SHINY_INSTANCES="${INSTANCES}",STATUS_REPORT_URL="${STATUS_REPORT_URL_ESC}",STATUS_REPORT_TOKEN="${STATUS_REPORT_TOKEN_ESC}"
priority=150
startsecs=2
startretries=3

EOF

echo "Configuration generated successfully!"
echo "Traefik will use cookie-based sticky sessions with cookie name: INZLITESESSION"

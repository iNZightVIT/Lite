#!/bin/bash

# Default to 10 instances if not specified
INSTANCES=${SHINY_INSTANCES:-10}
BASE_PORT=3000

echo "Generating configuration for $INSTANCES Shiny instances with Traefik..."

# Generate Traefik dynamic configuration
cat > /etc/traefik/dynamic.yml << EOF
http:
  routers:
    shiny-router:
      rule: "PathPrefix(\`/\`)"
      service: shiny-service
      entryPoints:
        - web

  services:
    shiny-service:
      loadBalancer:
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
environment=HOME="/home/shiny",USER="shiny",LITE_INSTANCE="${i}",R_LIBS_USER="/usr/local/lib/R/site-library"
priority=200
stopasgroup=true
killasgroup=true
startsecs=10
startretries=3

EOF
done

echo "Configuration generated successfully!"
echo "Traefik will use cookie-based sticky sessions with cookie name: INZLITESESSION"

#!/bin/bash

# Default to 10 instances if not specified
INSTANCES=${SHINY_INSTANCES:-10}
BASE_PORT=3000

echo "Generating configuration for $INSTANCES Shiny instances..."

# Generate nginx upstream servers
NGINX_SERVERS=""
for i in $(seq 1 $INSTANCES); do
    PORT=$((BASE_PORT + i))
    NGINX_SERVERS="${NGINX_SERVERS}        server 127.0.0.1:${PORT};\n"
done

# Replace placeholders in nginx config using echo and sed
echo -e "$NGINX_SERVERS" | sed 's/\\n/\n/g' > /tmp/nginx_servers.txt
sed "/SHINY_SERVERS/r /tmp/nginx_servers.txt" /etc/nginx/nginx.conf.template | sed '/SHINY_SERVERS/d' > /etc/nginx/nginx.conf

# Generate supervisor config directly
cat > /etc/supervisor/conf.d/supervisord.conf << SUPER_EOF
[supervisord]
nodaemon=true
user=root
logfile=/var/log/supervisor/supervisord.log
pidfile=/var/run/supervisor/supervisord.pid
childlogdir=/var/log/supervisor
loglevel=info

[unix_http_server]
file=/var/run/supervisor/supervisor.sock
chmod=0700

[rpcinterface:supervisor]
supervisor.rpcinterface_factory = supervisor.rpcinterface:make_main_rpcinterface

[supervisorctl]
serverurl=unix:///var/run/supervisor/supervisor.sock

[program:nginx]
command=/usr/sbin/nginx -g "daemon off;"
autostart=true
autorestart=true
stdout_logfile=/var/log/supervisor/nginx.log
stderr_logfile=/var/log/supervisor/nginx_error.log
priority=100
user=root

SUPER_EOF

# Append Shiny instances to supervisor config
for i in $(seq 1 $INSTANCES); do
    PORT=$((BASE_PORT + i))
    cat >> /etc/supervisor/conf.d/supervisord.conf << SHINY_EOF
[program:shiny-${i}]
command=/usr/local/bin/R -e "shiny::runApp('/srv/shiny-server', port=${PORT})"
autostart=true
autorestart=true
stdout_logfile=/var/log/supervisor/shiny-${i}.log
stderr_logfile=/var/log/supervisor/shiny-${i}_error.log
user=shiny
environment=HOME="/home/shiny",USER="shiny"
priority=200
stopasgroup=true
killasgroup=true

SHINY_EOF
done

echo "Configuration generated successfully!"

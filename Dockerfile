FROM rocker/shiny-verse:4.2

# Number of Shiny instances (build argument)
ARG SHINY_INSTANCES=10

# Install required packages
RUN apt-get update && apt-get install -y \
    supervisor \
    cmake \
    libpoppler-cpp-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Traefik
RUN curl -L https://github.com/traefik/traefik/releases/download/v3.0.0/traefik_v3.0.0_linux_amd64.tar.gz \
    -o /tmp/traefik.tar.gz \
    && tar -xzf /tmp/traefik.tar.gz -C /usr/local/bin \
    && rm /tmp/traefik.tar.gz \
    && chmod +x /usr/local/bin/traefik

# Remove default Shiny Server content
RUN rm -rf /srv/shiny-server/index.html /srv/shiny-server/sample-apps

# Setup GitHub PAT for R package installation
ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}
ENV SHINY_INSTANCES=${SHINY_INSTANCES}

RUN echo "GITHUB_PAT=${GITHUB_PAT}" >> .Renviron

# Copy and run setup script
COPY setup.R /srv/shiny-server
RUN Rscript /srv/shiny-server/setup.R
RUN rm .Renviron

# Copy application files
COPY . /srv/shiny-server
RUN cp /srv/shiny-server/VARS.default /srv/shiny-server/VARS \
    && sed -i "s/^\(lite.update=\).*/\1$(TZ='Pacific/Auckland' date '+%d %B %Y %-I:%M:%S%p')/g" /srv/shiny-server/VARS

# Set R options for Shiny (binding to localhost)
RUN echo "options(\
shiny.host = '127.0.0.1',\
shiny.autoload.r = FALSE \
)" >> /usr/local/lib/R/etc/Rprofile.site

# Copy configuration files
COPY server/traefik.yml /etc/traefik/traefik.yml
COPY server/generate-traefik-configs.sh /usr/local/bin/generate-configs.sh

# Make generation script executable
RUN chmod +x /usr/local/bin/generate-configs.sh

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server \
    && mkdir -p /var/log/supervisor /var/run/supervisor /var/log/traefik \
    && chown -R shiny:shiny /var/log/supervisor

# Expose port 80
EXPOSE 80

# Generate configs and start supervisor
ENTRYPOINT ["/bin/bash", "-c", "/usr/local/bin/generate-configs.sh && exec /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf"]

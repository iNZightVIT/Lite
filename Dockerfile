FROM rocker/r-ver:4.2

# install shiny
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libcurl4-gnutls-dev \
        libcairo2-dev \
        libxt-dev \
        libssl-dev \
        libssh2-1-dev \
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

RUN echo "GITHUB_PAT=${GITHUB_PAT}" >> .Renviron
COPY setup.R .
RUN Rscript setup.R
RUN rm .Renviron

# copy files to app dir and set vars
COPY . /app
RUN cp /app/VARS.default /app/VARS \
    && sed -i "s/^\(lite.update=\).*/\1$(TZ='Pacific/Auckland' date '+%d %B %Y %-I:%M:%S%p')/g" /app/VARS

RUN useradd shiny
RUN chown -R shiny:shiny /app \
    && mkdir -p /var/log/supervisor /var/run/supervisor /var/log/traefik \
    && chown -R shiny:shiny /var/log/supervisor

# Number of Shiny instances (build argument)
ARG SHINY_INSTANCES=3
ENV SHINY_INSTANCES=${SHINY_INSTANCES}

# Copy configuration files
COPY server/traefik.yml /etc/traefik/traefik.yml
COPY server/generate-traefik-configs.sh /usr/local/bin/generate-configs.sh
RUN chmod +x /usr/local/bin/generate-configs.sh

EXPOSE 3838

# Generate configs and start supervisor
ENTRYPOINT ["/bin/bash", "-c", "/usr/local/bin/generate-configs.sh && exec /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf"]

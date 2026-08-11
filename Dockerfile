# Ubuntu 24.04 + R 4.2.3 from source (final 4.2.x; there is no 4.2.4).
# Newer than rocker/r-ver:4.2.3 (Ubuntu 22.04) so main/universe security
# updates are available without waiting on a frozen rocker tag.
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    TZ=Etc/UTC \
    R_VERSION=4.2.3 \
    R_HOME=/usr/local/lib/R \
    CRAN=https://p3m.dev/cran/__linux__/noble/latest

# Locale + toolchain + R runtime libs, then build R from source.
# Compilers stay installed so pak can compile packages that lack noble binaries.
RUN apt-get update \
    && apt-get upgrade -y --no-install-recommends \
    && apt-get install -y --no-install-recommends \
        bash-completion \
        build-essential \
        ca-certificates \
        curl \
        file \
        g++ \
        gfortran \
        gnupg \
        libblas-dev \
        libbz2-dev \
        libcairo2-dev \
        libcurl4 \
        libcurl4-openssl-dev \
        libicu-dev \
        libjpeg-dev \
        liblapack-dev \
        liblzma-dev \
        libopenblas-dev \
        libpangocairo-1.0-0 \
        libpango1.0-dev \
        libpcre2-dev \
        libpng-dev \
        libreadline-dev \
        libssl-dev \
        libtiff-dev \
        libxt-dev \
        locales \
        make \
        tzdata \
        unzip \
        wget \
        zip \
        zlib1g-dev \
    && locale-gen en_US.UTF-8 \
    && update-locale LANG=en_US.UTF-8 \
    && ARCH="$(uname -m)" \
    && update-alternatives --set "libblas.so.3-${ARCH}-linux-gnu" \
        "/usr/lib/${ARCH}-linux-gnu/openblas-pthread/libblas.so.3" \
    && curl -fsSL "https://cloud.r-project.org/src/base/R-4/R-${R_VERSION}.tar.gz" \
        -o /tmp/R.tar.gz \
    && mkdir -p /tmp/R-src \
    && tar -xzf /tmp/R.tar.gz -C /tmp/R-src --strip-components=1 \
    && cd /tmp/R-src \
    && perl -0777 -i -pe 's/(#if LIBCURL_VERSION_MAJOR > 7\s+)exit\(1\)/${1}exit(0)/' configure \
    && ./configure \
        --prefix=/usr/local \
        --enable-R-shlib \
        --enable-memory-profiling \
        --with-readline \
        --with-blas \
        --with-lapack \
        --without-tcltk \
        --with-recommended-packages \
    && make -j"$(nproc)" \
    && make install \
    && mkdir -p "${R_HOME}/site-library" \
    && echo "R_LIBS=\${R_LIBS-'${R_HOME}/site-library:${R_HOME}/library'}" \
        >> "${R_HOME}/etc/Renviron.site" \
    && echo "options(repos = c(CRAN = '${CRAN}'), download.file.method = 'libcurl')" \
        >> "${R_HOME}/etc/Rprofile.site" \
    && printf '%s\n' \
        'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))' \
        >> "${R_HOME}/etc/Rprofile.site" \
    && cd / \
    && rm -rf /tmp/R-src /tmp/R.tar.gz \
    && apt-get purge -y linux-libc-dev \
    && rm -rf /var/lib/apt/lists/*

# R 4.2 cannot use current CRAN (e.g. GGally needs R >= 4.3; ggmosaic/waffle
# are archived). Use the same date snapshot as rocker/r-ver:4.2.3, but
# source packages — jammy binaries (e.g. stringi) link ICU 70, noble has 74.
ENV CRAN=https://p3m.dev/cran/2023-04-20
RUN echo "options(repos = c(CRAN = '${CRAN}'), download.file.method = 'libcurl')" \
        >> "${R_HOME}/etc/Rprofile.site"

# App system deps + Node 20 (Ubuntu 24.04 repo Node is older)
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        cmake \
        gfortran \
        libpoppler-cpp-dev \
        libssh2-1-dev \
        supervisor \
    && ln -sfn /usr/bin/gfortran-13 /usr/bin/gfortran \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key \
       | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" \
       > /etc/apt/sources.list.d/nodesource.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

# Install Traefik (3.0.0 has multiple known CVEs; 3.7.10 is current patch)
RUN curl -L https://github.com/traefik/traefik/releases/download/v3.7.10/traefik_v3.7.10_linux_amd64.tar.gz \
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
ARG SHINY_INSTANCES=2
ENV SHINY_INSTANCES=${SHINY_INSTANCES}

# Status reporter (optional; passed at build from GitHub secrets)
ARG STATUS_REPORT_TOKEN
ARG STATUS_REPORT_URL
ENV STATUS_REPORT_TOKEN=${STATUS_REPORT_TOKEN}
ENV STATUS_REPORT_URL=${STATUS_REPORT_URL}

# Idle-throttle defaults for status-server periodic work while no sessions
ARG STATUS_IDLE_THROTTLE_ENABLED=1
ARG STATUS_IDLE_WORK_INTERVALS=4
ENV STATUS_IDLE_THROTTLE_ENABLED=${STATUS_IDLE_THROTTLE_ENABLED}
ENV STATUS_IDLE_WORK_INTERVALS=${STATUS_IDLE_WORK_INTERVALS}

# Copy configuration files
COPY server/traefik.yml /etc/traefik/traefik.yml
COPY server/generate-traefik-configs.sh /usr/local/bin/generate-configs.sh
RUN chmod +x /usr/local/bin/generate-configs.sh

EXPOSE 3838

# Generate configs and start supervisor
ENTRYPOINT ["/bin/bash", "-c", "/usr/local/bin/generate-configs.sh && exec /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf"]

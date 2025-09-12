FROM rocker/shiny-verse:4.2
RUN rm -rf /srv/shiny-server/index.html /srv/shiny-server/sample-apps
RUN apt-get update && apt-get install -y \
    cmake \
    libpoppler-cpp-dev

ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}
RUN echo "GITHUB_PAT=${GITHUB_PAT}" >> .Renviron

COPY setup.R /srv/shiny-server
RUN Rscript /srv/shiny-server/setup.R

RUN rm .Renviron
COPY . /srv/shiny-server
RUN cp /srv/shiny-server/VARS.default /srv/shiny-server/VARS \
  && sed -i "s/^\(lite.update=\).*/\1$(TZ='Pacific/Auckland' date '+%d %B %Y %-I:%M:%S%p')/g" /srv/shiny-server/VARS
RUN chown -R shiny:shiny /srv/shiny-server

RUN echo "options(\
  shiny.host = '0.0.0.0',\
  shiny.autoload.r = FALSE \
)" >> /usr/local/lib/R/etc/Rprofile.site

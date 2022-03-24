FROM rocker/shiny-verse:4.1.2
RUN rm -rf /srv/shiny-server/index.html /srv/shiny-server/sample-apps
RUN apt-get update && apt-get install -y \
    cmake \
    libpoppler-cpp-dev 
RUN install2.r --error --skipinstalled \
    colorspace \
    countrycode \
    DT \
    GGally \
    ggmap \
    gpairs \
    plotly \
    RcppTOML \
    readr \
    readtext \
    readxl \
    RgoogleMaps \
    RJSONIO \
    reshape2 \
    sas7bdat \
    shinyalert \
    shinycssloaders \
    shinydashboard \
    shinyjs \
    shinyWidgets \
    srvyr \
    styler \
    survey \
    viridis \
    XML
RUN R -e 'install.packages(c("iNZightMaps", \
                             "iNZightMR", \
                             "iNZightPlots", \
                             "iNZightRegression", \
                             "iNZightTools", \
                             "iNZightTS"), \
                           repos = c("https://r.docker.stat.auckland.ac.nz", \
                                     "https://cloud.r-project.org"), \
                           type = "source", dependencies = TRUE)'
COPY . /srv/shiny-server
RUN cp /srv/shiny-server/VARS.default /srv/shiny-server/VARS \
  && sed -i "s/^\(lite.update=\).*/\1$(date '+%d %B %Y')/g" /srv/shiny-server/VARS
RUN chown -R shiny:shiny /srv/shiny-server

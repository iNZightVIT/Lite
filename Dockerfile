FROM rocker/shiny-verse:4.2
RUN rm -rf /srv/shiny-server/index.html /srv/shiny-server/sample-apps
RUN apt-get update && apt-get install -y \
    cmake \
    libpoppler-cpp-dev
    ARG GITHUB_PAT
    ENV GITHUB_PAT ${GITHUB_PAT}
    # write GITHUB_PAT to .Renviron
    # 'iNZightVIT/iNZightTS@2.0.0', \
    RUN echo "GITHUB_PAT=${GITHUB_PAT}" >> .Renviron

RUN install2.r --error --skipinstalled \
    colorspace countrycode DT GGally ggmap plotly RcppTOML readr readtext readxl RgoogleMaps RJSONIO reshape2 sas7bdat shinyalert shinycssloaders shinydashboard shinyjs shinyWidgets srvyr styler survey viridis XML remotes vctrs pillar magrittr lifecycle crayon tibble rlang fansi cli ps rprojroot fs desc processx proxy wk e1071 units s2 Rcpp DBI classInt stringi generics cpp11 tidyselect stringr purrr dplyr sys openssl jsonlite curl sp png colorspace viridisLite RColorBrewer farver scales isoband gtable minqa MatrixModels SparseM timechange bit tzdb vroom hms blob zoo lubridate tidyr srvyr readr forcats dbplyr sass tinytex bslib xfun highr evaluate rmarkdown yaml digest rstudioapi htmltools htmlwidgets checkmate knitr matrixStats htmlTable data.table maditr mvtnorm estimability httr rex waldo pkgload callr sf jpeg plyr ggplot2 s20x quantreg hexbin expss emmeans dichromat chron covr testthat rgeos lwgeom ggmap countrycode maptools XML settings validate markdown gridtext patchwork ggtext RcppEigen nloptr lme4 labelled sandwich TH.data pbkrtest abind carData broom.helpers multcomp ggrepel car Rttf2pt1 extrafontdb extrafont FNN productplots vipor beeswarm waffle hextri gridSVG ggthemes ggridges ggmosaic ggbeeswarm shinylogs wkb

RUN Rscript setup.R

RUN rm .Renviron
COPY . /srv/shiny-server
RUN cp /srv/shiny-server/VARS.default /srv/shiny-server/VARS \
  && sed -i "s/^\(lite.update=\).*/\1$(date '+%d %B %Y')/g" /srv/shiny-server/VARS
RUN chown -R shiny:shiny /srv/shiny-server

RUN echo "options(\
  shiny.host = '0.0.0.0',\
  shiny.autoload.r = FALSE \
)" >> /usr/local/lib/R/etc/Rprofile.site

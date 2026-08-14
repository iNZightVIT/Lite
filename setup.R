# Install latest version of 'pak'
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))

# Set up repository URLs
pak::repo_add("https://r.docker.stat.auckland.ac.nz")

# Lite app packages (DESCRIPTION Depends/Imports + a few UI helpers).
# openssl, readtext, wkb, shinydashboard are Lite Imports that are not hard
# deps of the iNZight stack — they must be listed explicitly once we stop
# installing Suggests for the whole dependency tree.
pkgs <- c(
    "markdown",
    "shiny",
    "htmltools",
    "GGally",
    "RJSONIO",
    "shinyjs",
    "plotly",
    "shinyWidgets",
    "DT",
    "shinycssloaders",
    "shinyalert",
    "rjson",
    "shinylogs",
    "bit64",
    "sas7bdat",
    "shinyStorePlus",
    "reshape2",
    "sortable",
    "openssl",
    "readtext",
    "wkb",
    "shinydashboard"
)

# iNZight packages (pinned) + multivariate
pkgs <- c(
    "tmelliott/surveyspec@0.1.1",
    "iNZightVIT/iNZightTools@2.0.1",
    "iNZightVIT/iNZightTS@2.0.3",
    "iNZightVIT/iNZightTS@legacy",
    "iNZightVIT/iNZightRegression@1.3.3",
    "iNZightVIT/iNZightMR@2.3.1",
    "iNZightVIT/iNZightPlots@2.16.0",
    "iNZightMultivariate",
    pkgs
)

# Suggests from iNZight* + surveyspec only (not from shiny/plotly/ggplot2/…).
# Default pak dependencies (= NA) installs hard deps only, so optional features
# from these packages must be requested here.
# Skipped: covr, testthat (dev-only); ggmosaic, waffle (not on CRAN).
inzight_suggests <- c(
    # iNZightTools@2.0.1
    "chron",
    "dbplyr",
    "expss",
    "haven",
    "jsonlite",
    "knitr",
    "lubridate",
    "RCurl",
    "readxl",
    "RSQLite",
    "styler",
    "tsibble",
    "validate",
    "yaml",
    # iNZightRegression@1.3.3
    "broom.helpers",
    "survival",
    # iNZightPlots@2.16.0
    "forcats",
    "DBI",
    "ggbeeswarm",
    "ggplot2",
    "ggridges",
    "ggtext",
    "ggthemes",
    "gridSVG",
    "hextri",
    "kableExtra",
    "RColorBrewer",
    "tibble",
    "tidyr",
    "viridis"
    # surveyspec@0.1.1: RCurl only (listed above)
    # iNZightTS / iNZightMR / iNZightMultivariate: no runtime Suggests
)

# Ignore unused optional stacks that some Suggests still declare as hard deps
# (e.g. kableExtra -> webshot -> magick). Lite does not use GDAL/ImageMagick.
pak::pak(
    c(
        pkgs,
        inzight_suggests,
        "sf=?ignore",
        "lwgeom=?ignore",
        "rgdal=?ignore",
        "rgeos=?ignore",
        "maptools=?ignore",
        "magick=?ignore"
    )
)

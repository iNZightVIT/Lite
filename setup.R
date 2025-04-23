# Install latest version of 'pak'
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))

# Set up repository URLs
pak::repo_add("https://r.docker.stat.auckland.ac.nz")



# pkgs <- read.delim(textConnection("colorspace countrycode DT# GGally ggmap plotly RcppTOML readr readtext readxl RgoogleMaps RJSONIO reshape2 sas7bdat shinyalert shinycssloaders shinydashboard shinyjs shinyWidgets srvyr styler survey viridis XML remotes vctrs pillar magrittr lifecycle crayon tibble rlang fansi cli ps rprojroot fs desc processx proxy wk e1071 units s2 Rcpp DBI classInt stringi generics cpp11 tidyselect stringr purrr dplyr sys openssl jsonlite curl sp png colorspace viridisLite RColorBrewer farver scales isoband gtable minqa Matrix MatrixModels SparseM timechange bit tzdb vroom hms blob zoo lubridate tidyr srvyr readr forcats dbplyr sass tinytex bslib xfun highr evaluate rmarkdown yaml digest rstudioapi htmltools htmlwidgets checkmate knitr matrixStats htmlTable data.table maditr mvtnorm estimability@1.4.1 httr rex waldo pkgload callr sf jpeg plyr ggplot2 s20x quantreg hexbin expss emmeans dichromat chron covr testthat rgeos lwgeom ggmap countrycode maptools XML settings validate markdown gridtext patchwork ggtext RcppEigen nloptr lme4 labelled sandwich TH.data pbkrtest abind carData broom.helpers multcomp ggrepel car Rttf2pt1 extrafontdb extrafont FNN productplots vipor beeswarm waffle hextri gridSVG ggthemes ggridges ggmosaic ggbeeswarm shinylogs wkb"), sep = " ", header = FALSE, comment.char = "#") |> as.character()

pkgs <- c("markdown", "GGally", "RJSONIO", "shinyjs", "plotly", "shinyWidgets", "DT", "shinycssloaders", "shinyalert", "rjson", "shinylogs", "bit64", "htmltools", "shiny", "sas7bdat", "shinyStorePlus", "reshape2")

pkgs <-
    c(
        # "Matrix@1.6-5",
        "tmelliott/surveyspec@0.1.1",
        "iNZightVIT/iNZightTools@2.0.1",
        "iNZightVIT/iNZightTS@2.0.0",
        "iNZightVIT/iNZightTS@legacy",
        "iNZightVIT/iNZightRegression@1.3.3",
        "iNZightVIT/iNZightMR@2.2.7",
        "iNZightVIT/iNZightPlots@v2.14",
        "cran/maptools",
        "cran/rgeos",
        "iNZightVIT/iNZightMaps@2.3.1",
        "iNZightMultivariate",
        pkgs
    )
# pak::pak(pkgs, lib = ".lib")

pak::pak(pkgs, dependencies = TRUE)

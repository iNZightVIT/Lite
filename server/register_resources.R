# This ensures all instances have the same resource paths
library(shiny)

# Pre-register all htmlwidget resources
.pkgs <- c("shinyWidgets", "crosstalk", "htmlwidgets", "DT", "plotly", "leaflet")
for (pkg in .pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        www_dir <- system.file("www", package = pkg)
        if (www_dir != "" && dir.exists(www_dir)) {
            # Register with versioned and non-versioned paths
            version <- as.character(packageVersion(pkg))
            addResourcePath(paste0(pkg, "-", version), www_dir)
            addResourcePath(pkg, www_dir)
        }

        # Also check for htmlwidgets directory
        hw_dir <- system.file("htmlwidgets", package = pkg)
        if (hw_dir != "" && dir.exists(hw_dir)) {
            addResourcePath(paste0(pkg, "-widgets"), hw_dir)
        }
    }
}

# Specifically handle crosstalk version path
if (requireNamespace("crosstalk", quietly = TRUE)) {
    addResourcePath("crosstalk-1.2.0", system.file("www", package = "crosstalk"))
}

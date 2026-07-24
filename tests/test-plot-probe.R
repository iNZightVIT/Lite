# Automated checks for Visualize plot-probe args (names + data, not column vectors).
#
# Run from the Lite repo root:
#   Rscript tests/test-plot-probe.R
#
# Manual UI checklist (exercises every probe_plot() call site) is printed at the end.

fail <- function(...) {
  cat("FAIL:", sprintf(...), "\n")
  quit(status = 1, save = "no")
}

pass <- function(...) {
  cat("OK:  ", sprintf(...), "\n")
}

suppressPackageStartupMessages(library(iNZightPlots))

# Mirrors panels/C1_Visualize/2_visualize-panel-server.R helpers
probe_plot_args <- function(vari1, vari2 = "none", data, ...) {
  args <- list(
    x = as.name(vari1),
    data = data,
    plot = FALSE,
    ...
  )
  if (!is.null(vari2) && !identical(vari2, "none")) {
    args$y <- as.name(vari2)
  }
  args
}

probe_plot <- function(vari1, vari2 = "none", data, ...) {
  try(
    do.call(iNZightPlots:::iNZightPlot, probe_plot_args(vari1, vari2, data, ...)),
    silent = TRUE
  )
}

expect_plot <- function(label, result) {
  if (inherits(result, "try-error")) {
    fail("%s -> %s", label, as.character(result))
  }
  if (!inherits(result, "inzplotoutput")) {
    fail("%s -> expected inzplotoutput, got %s", label, paste(class(result), collapse = "/"))
  }
  pass("%s", label)
}

expect_error_message <- function(label, expr, pattern) {
  err <- tryCatch(force(expr), error = function(e) conditionMessage(e))
  if (!is.character(err) || !grepl(pattern, err, fixed = TRUE)) {
    fail("%s -> expected error matching %s, got: %s", label, pattern, err)
  }
  pass("%s (legacy vector path still errors as expected)", label)
}

# --- Regression: old vector API fails with value-as-name errors ---
load("data/Examples/census.at.school.500.rda")
cas <- census.at.school.500
load("data/Examples/gapminder.rda")

expect_error_message(
  "cellsource vector -> parent",
  do.call(iNZightPlots:::iNZightPlot, list(x = cas$cellsource, plot = FALSE)),
  "object 'parent' not found"
)
expect_error_message(
  "Country vector -> Albania",
  do.call(iNZightPlots:::iNZightPlot, list(x = gapminder$Country, plot = FALSE)),
  "object 'Albania' not found"
)

# --- Fixed API: names + data ---
expect_plot("CAS factor only", probe_plot("cellsource", "none", cas))
expect_plot("CAS numeric only", probe_plot("height", "none", cas))
expect_plot("CAS scatter", probe_plot("height", "armspan", cas))
expect_plot("CAS factor x numeric", probe_plot("gender", "height", cas))
expect_plot(
  "CAS with locate.extreme",
  probe_plot("height", "none", cas, locate.extreme = 5)
)
expect_plot("gapminder Country", probe_plot("Country", "none", gapminder))

# Args shape guards
args <- probe_plot_args("cellsource", "none", cas)
if (!is.name(args$x) || !identical(as.character(args$x), "cellsource")) {
  fail("probe_plot_args x should be name 'cellsource'")
}
if (!is.data.frame(args$data)) {
  fail("probe_plot_args should include data")
}
if (!isFALSE(args$plot)) {
  fail("probe_plot_args plot should be FALSE")
}
pass("probe_plot_args shape (x name + data + plot=FALSE)")

args2 <- probe_plot_args("height", "armspan", cas)
if (!is.name(args2$y) || !identical(as.character(args2$y), "armspan")) {
  fail("probe_plot_args y should be name 'armspan'")
}
pass("probe_plot_args includes y when vari2 set")

# Source-file wiring: every former vector probe site should call probe_plot()
src <- paste(
  readLines("panels/C1_Visualize/2_visualize-panel-server.R"),
  collapse = "\n"
)
n_probe <- length(gregexpr("\\bprobe_plot\\(", src)[[1]])
# call sites only (definition is "probe_plot <- function")
if (isTRUE(n_probe < 10)) {
  fail("expected >= 10 probe_plot( call sites in visualize server, found %s", n_probe)
}
pass("visualize server has probe_plot call sites (%s)", n_probe)

# Axis-limits panel keeps column vectors for range() only; probes use probe_plot().
if (!grepl("Column vectors for range\\(\\) only", src)) {
  fail("axis-limits panel should document range()-only column vectors")
}
pass("axis-limits keeps vectors for range() only")

if (grepl(
  "temp\\$x <- get\\.data\\.set\\(\\)\\[, input\\$vari1\\][\\s\\S]{0,200}do\\.call\\(iNZightPlots:::",
  src
)) {
  fail("still found vector assignment followed by do.call(iNZightPlots:::...)")
}
pass("no vector assignment immediately feeding iNZightPlots do.call")

cat("\nAll automated checks passed.\n\n")

cat("======================================================================\n")
cat("MANUAL UI CHECKLIST (run shiny::runApp(); watch R console for errors)\n")
cat("======================================================================\n")
cat("
Console must stay free of:
  Error in eval(as.name(z), data, env) : object '...' not found

[ ] 1. File > Dataset Examples > Examples > census.at.school.500 > Select set
        Lands on Visualize; first var often cellsource (values include 'parent').
        Covers: plot type probe (~2080), appearance/large-sample (~2556),
        select_additions (~6225).

[ ] 2. Variable 1 = cellsource, Variable 2 = none
        Plot type / Add to Plot panels render; no console error.
        Covers: advanced_options observer (~3128).

[ ] 3. Variable 1 = height, Variable 2 = none
        Dot plot / histogram options appear; open Add to Plot > Axis Limits.
        Limits fields populate; no 'temp' / parent console error.
        Covers: axis-limits probe + range() (~4989).

[ ] 4. Variable 1 = height, Variable 2 = armspan
        Scatter options: open Add to Plot.
        [ ] Jitter / Rugs / Join points sections (probes ~4721, 4788, 4858)
        [ ] Point options / colour-by path if shown (~4171)

[ ] 5. Variable 1 = gender, Variable 2 = height
        Box-plot style options; no console error (~4171 / additions).

[ ] 6. Identify / locate extremes (Add to Plot > Identify points if available):
        set extreme count, Store observations.
        Covers: locate.extreme probe (~6137).

[ ] 7. File > Dataset Examples > gapminder > Select set
        Variable 1 resets to a gapminder column (e.g. Country);
        no `object 'height' not found` / undefined columns errors.

[ ] 8. Switch back to census.at.school.500; confirm still clean on Visualize.
        Optional: open Add to Plot > Identify points after switching datasets.

Automated: Rscript tests/test-plot-probe.R
")

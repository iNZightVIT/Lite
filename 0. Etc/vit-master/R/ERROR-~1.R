# Helper function that ensures that confidence intervals are not being
# attempted with multivariate data.
confidence_check <- function(e) {
    if (!is.null(e$xData) & !is.null(e$yData)) {
        e$confirmDialog(
"VIT cannot apply confidence interval methods to 
more than one variable at a time. The statistic
of interest will be changed to the mean.",
            handler = function(h, ...) {
                svalue(e$stat) <- "mean"
                dispose(h$obj)
            })
    }
}

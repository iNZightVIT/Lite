##' Print method for iNZightTS (time series) objects.
##'
##' The \code{full} argument controls whether to print all the data from
##' which the \code{iNZightTS} object has been created. The default is set
##' to \code{FALSE} and only the \code{head()} of the data will be printed.
##'
##' @title Print an iNZightTS object
##'
##' @param x the \code{iNZightTS} object to be printed
##'
##' @param full whether to print all the underlying data
##'
##' @param ... Unused arguments. Only here for consistency with the base S3 method.
##'
##' @seealso \code{\link{print}}, \code{\link{iNZightTS}}
##'
##' @examples print(iNZightTS(UKgas))
##'
##' @export
print.iNZightTS <-
function(x, full = FALSE, ...) {
    writeLines("Current Variable:")
    print(x$currVar)

    if (x$freq > 1)
        writeLines(paste("\nTime Series:\nStart =",
                         paste(x$start, collapse = ", "),
                         "\nEnd =",
                         paste(x$end, collapse = ", "),
                         "\nFrequency =",
                         paste(x$freq,  collapse = ", ")))
    else
        writeLines("\n")
    print(x$tsObj)

    writeLines("\nData:")
    if (full)
        print(x$data)
    else {
        print(head(x$data))
        writeLines("...")
    }
}

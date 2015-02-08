##' Draws a plot of a given \code{iNZightTS} object with the trend superimposed.
##'
##' If animate is set to \code{TRUE}, a scatterplot of all points in the
##' time series will appear followed by slowly drawn lines connecting the
##' points, simulating the drawing of a time series by hand.
##'
##' @title Draw a simple time series plot
##'
##' @param obj an \code{iNZightTS} object
##'
##' @param multiplicative logical. If \code{TRUE}, a multiplicative model is used,
##' otherwise an additive model is used by default.
##'
##' @param xlab a title for the x axis
##'
##' @param ylab a title for the y axis
##'
##' @param animate animate the plotting process?
##'
##' @param e \code{NULL} by default to support animation stop
##'
##' @keywords timeseries
##'
##' @export
rawplot <-
    function(obj, multiplicative = FALSE, ylab = "", xlab = "",
             animate = FALSE, e = NULL) {

        ## the e argument to support animation stop
        if (is.null(e)) {
            e <- new.env()
            e$stopAnimation <- FALSE
        }

        if (any(grepl("^iNZightMTS$", class(data))))
            stop("Time-Series must be univariate")

        height = 5; width = 6

        ## x and y coordinates of the time series tsObj
        tsObj = obj$tsObj
        xlist = get.x(tsObj)
        x = xlist$x
        x.units = xlist$x.units
        y = tsObj@.Data
        y.units = unit(y, "native")

        ## We want a trend line, so do a decomposition
        if (frequency(tsObj) > 1) {
            decomp = decomposition(obj, ylab = "", multiplicative = multiplicative)$decompVars
            if (multiplicative)
                smooth = exp(log(decomp$components[,"trend"]))
            else
                smooth = decomp$components[,"trend"]
        } else {
            smooth = loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted
        }

        ## Height of the plotting viewport needs to be scale.factor times the height
        ## of the trend viewport in the decomposition plot
        ## browser()
        plotHeight = 2

        parent.vp = viewport(layout = grid.layout(3, 3,
                                 heights = unit(c(1, plotHeight, 1),
                                     c("null", "inches", "null")),
                                 widths = unit(c(1.2, 1, .2),
                                     c("inches", "null", "inches"))),
            name = "parent")
        head.vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2, name = "head")
        left.vp = viewport(layout.pos.row = 2, layout.pos.col = 1, name = "left")
        right.vp = viewport(layout.pos.row = 2, layout.pos.col = 3, name = "right")
        bottom.vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2, name = "bottom")

        plot.vp = viewport(name = "plot", layout.pos.row = 2, layout.pos.col = 2,
            xscale = extendrange(r = range(x)),
            yscale = extendrange(r = range(y, smooth)))
        plot.vptree = vpTree(parent.vp, vpList(head.vp, left.vp, plot.vp, right.vp,
            bottom.vp))

        ## The following creates a gTree which contains all of our grobs
        headtitle <- ifelse(ylab != "", ylab, "Time series plot")
        grobList = gList(rectGrob(vp = vpPath("parent", "plot"), name = "border"),
            linesGrob(x.units, y.units, vp = vpPath("parent", "plot"),
                      name = "line", gp = gpar(col = "black", lwd = 1)),
            linesGrob(x.units, unit(smooth, "native"), name = "smooth",
                      gp = gpar(col = "red"), vp = vpPath("parent", "plot")),
            yaxisGrob(vp = vpPath("parent", "plot"), name = "yAxis",
                      gp = gpar(cex = .8)),
            textGrob(ylab, x= 0, y = 0.5, vjust = -6,
                     rot = 90,
                     vp = vpPath("parent", "plot"), name = "yAxisLabel",
                     gp = gpar(cex = .8)),
            xaxisGrob(vp = vpPath("parent", "plot"), name = "xAxis",
                      gp = gpar(cex = .8)),
            textGrob(xlab, x= 0.5, y = 0, vjust = 5,
                     vp = vpPath("parent", "plot"), name = "xAxisLabel",
                     gp = gpar(cex = .8)),
            textGrob(paste(headtitle,"for", obj$currVar),
                     hjust = 0.5, vjust = -1.5, y = 0,
                     name = "topLabel",
                     vp = vpPath("parent", "head")))


        image = gTree(name = "image", children = grobList, childrenvp = plot.vptree)

        ##  newdevice - is this really necessary?
        ## newdevice(width = width, height = height)

        ##  Animations are useful for demonstrating theoretical concepts.
        ##  An animation is essentially a rapid display of a sequence of
        ##  static images that generates an illusion of movement, due to
        ##  a visual phenomenon called "persistence of vision".
        ##
        ##  While we can watch animation in R's graphics devices, some
        ##  devices under UNIX-based operating systems lack the capability
        ##  of double buffering, causing animations to flicker. Strangely
        ##  enough, relying on the supposedly "cross-platform" Cairo
        ##  device seems not to support double buffering under Windows -
        ##  clearly a bug. So we instead export animations from R for:
        ##
        ##  - better performance
        ##  - better portability
        ##
        ##-----------------------------------------##        
        ##  HAD TO INSTALL IMAGEMAGICK FOR LINUX.  ##
        ##-----------------------------------------##
        if (animate) {
            ##  We require the animation package.
            require(animation)
            par(mar = c(0, 0, 0, 0))
            ##  ani.options((convert = "/etc/alternatives/convert"))
            ##  We set up grid graphics objects and compute the number of
            ##  points required.
            saveGIF({
                final.line <- getGrob(image, "line")
                final.smooth <- getGrob(image, "smooth")
                image <- removeGrob(image, "line")
                image <- removeGrob(image, "smooth")
                n.points <- length(final.line$x)
                p <- pointsGrob(x = final.line$x, y = final.line$y,
                                vp = vpPath("parent", "plot"), size = unit(2.5, "mm"),
                                pch = 19, name = "points", gp = gpar(col = "darkgray"))
                ##  We draw the initial image, which is a scatter plot with no lines.
                image <- addGrob(image, p)
                ##  Save the animation as a .gif file.
                ##  Change wd to tempdir() but store the current wd.
                ## Set the plotting region.
                for (i in 1:n.points) {
                    if ((get("stopAnimation", envir = e) && i < n.points))
                        next
                    ##  Draw initial image.
                    if (i == 1) {
                        ## dev.hold()
                        pauseImage(image)
                        ## dev.flush()
                        ani.pause(.001) # .01
                    }
                    l <- linesGrob(x = final.line$x[1:i], y = final.line$y[1:i],
                                   vp = vpPath("parent", "plot"),
                                   name = "line", gp = gpar(col = 1, lwd = 1.5))
                    image <- addGrob(image, l)
                    ## dev.hold()
                    pauseImage(image)
                    ## dev.flush()
                    ani.pause(.001) 
                }
            },
                    movie.name = "timeseries.gif", img.name = "TS",
                    interval = 0.001, nmax = 999,
                    ani.height = 400, ani.width = 1000,
                    clean = TRUE, autobrowse = FALSE)            
        }
        dev.off()
    }



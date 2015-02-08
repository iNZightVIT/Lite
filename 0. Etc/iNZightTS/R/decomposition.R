decomposition <-
    function(obj, ylab = "", xlab = "", trendCol = "black", seasonCol = "#45a8ff",
             randCol = seasonCol, multiplicative=FALSE) {
        xlist <- get.x(obj$tsObj)
        x <- xlist$x
        x.units <- xlist$x.units

        if (obj$freq > 1) {
            if (multiplicative)
                tsObj <- log(obj$tsObj)
            else
                tsObj <- obj$tsObj
            decomp <- stl(tsObj, "periodic")
        }
        else {
          ## freq == 1, non seasonal fitted.
          if (multiplicative)  {
          trend.comp <-
                loess(log(obj$data[1:length(obj$tsObj), obj$currVar]) ~ x)$fitted +
                    obj$tsObj * 0

            residuals.comp <- log(obj$tsObj) - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <-
                as.ts(data.frame(seasonal = seasons.comp,
                                 trend = trend.comp,
                                 remainder = residuals.comp))
          }
          else{
            trend.comp <-
              loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted +
              obj$tsObj * 0

            residuals.comp <- obj$tsObj - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <-
              as.ts(data.frame(seasonal = seasons.comp,
                               trend = trend.comp,
                               remainder = residuals.comp))
          }
        }

        decompData <- decomp$time.series    # returns matrix

        ## x and y coordinates
        Y <- obj$tsObj

        if (multiplicative) {

          y <- log(obj$tsObj@.Data)
            y.random <- exp(y - (decompData[,"trend"] + decompData[,"seasonal"]))  # if we not backtransform here
            y.trend <- exp(decompData[,"trend"])  # the decompositionplot will all in log scale
            y.season <- exp(decompData[,"seasonal"])
          y.season2 <- exp(decompData[,"trend"] + decompData[,"seasonal"]) - exp(decompData[,"trend"])
          y.random2 <- Y - as.numeric(exp(decompData[,"trend"] + decompData[,"seasonal"]))
            decompData[, "trend"] <- y.trend
            decompData[, "seasonal"] <- y.season
            decompData[, "remainder"] <- y.random
          y.season <- y.season2
          y.random <- y.random2
        } else {
          y <- obj$tsObj@.Data
            y.trend <- decompData[,"trend"]
            y.season <- decompData[,"seasonal"]
            y.random <- decompData[,"remainder"]
        }
        y.trend.units <- unit(y.trend, "native")
        y.season.units <- unit(y.season, "native")
        y.random.units <- unit(y.random, "native")


        Y <- obj$tsObj@.Data

        ## We want each component plotted on the same scale, so we need
        ## to find the y-axis interval for the most variable component
        minmax <- apply(decompData, 2, function(x) range(x))
        ranges <- minmax[2,] - minmax[1,]
        ranges[2] <- max(y) - min(y)

        expandBy <- (max(y) - min(y)) * 0.1

        expandBy.trend <- (max(Y) - min(Y)) * 0.1


        #if (multiplicative)  #%
          y <- obj$tsObj@.Data  #%
        ## here we edit the viewport y values to provide an extra gap at
        ## the top of each panel
        trend.vp.y <- y


        trend.vp.y[which.max(y)] <- max(y) + expandBy.trend
        trend.vp.y[which.min(y)] <- min(y) - expandBy.trend


        season.vp.y <- y.season

        max.index <- ifelse(obj$freq > 1, which.max(y.season), 1)
        season.vp.y[max.index] <- max(y.season) + expandBy

        if (obj$freq == 1)
          season.vp.y[2] <- -expandBy

        random.vp.y <- y.random
        random.vp.y[which.max(y.random)] <- max(y.random) + expandBy

        ranges <- ranges + expandBy
        which.max.range <- which.max(ranges)
        y.interval <- diff(pretty(minmax[,which.max.range]))[1]

        ## Need to find the proportion of the plot each subplot will be
        ## allocated, and create the viewports
        props <- ranges/sum(ranges)



        ## The following defines the viewport layout for the plot
        ## parent.vp holds everything - with a main central viewport
        ## and 4 viewports around it that act as margins
        vp.heights <- c(.6, 1, .6)
        vertMargins <- sum(vp.heights[-2])
        parent.vp <- viewport(name = "parent",
                              layout =
                              grid.layout(3, 3,
                                          heights =
                                          unit(vp.heights,
                                               c("inches", "null", "inches")),
                                          widths =
                                          unit(c(1.1, 1, 1),
                                               c("inches", "null", "inches"))))
        head.vp <- viewport(layout.pos.row = 1, layout.pos.col = 1:2, name = "head")
        left.vp <- viewport(layout.pos.row = 2, layout.pos.col = 1, name = "left")
        right.vp <- viewport(layout.pos.row = 2, layout.pos.col = 3, name = "right")
        bottom.vp <- viewport(layout.pos.row = 3, layout.pos.col = 1:2, name = "bottom")
        plots.vp <- viewport(layout =
                             grid.layout(3, 1, heights = props[c(2, 1, 3)]),
                               name = "plots", layout.pos.row = 2, layout.pos.col = 2)
        trend.vp <- dataViewport(x, trend.vp.y, name = "trend", layout.pos.row = 1)
        season.vp <- dataViewport(x, season.vp.y, name = "season", layout.pos.row = 2)
        random.vp <- dataViewport(x, random.vp.y, name = "random", layout.pos.row = 3)

        plots.vptree <- vpTree(plots.vp, vpList(trend.vp, season.vp, random.vp))
        final.vptree <- vpTree(parent.vp, vpList(head.vp, left.vp, plots.vptree,
                                                 right.vp, bottom.vp))

        xlims <- season.vp$xscale
        dotted.xcoords <- c(xlims[1], x, xlims[2])
        dotted.point <- 0 #ifelse(multiplicative, 1, 0) #%
        dotted.ycoords <- rep(dotted.point, length(dotted.xcoords))

        ## The following creates a gTree which contains all of our grobs
        grobs = gList()

        grobs$trendBorder <-
            rectGrob(vp = vpPath("parent", "plots", "trend"),
                     name = "trendBorder", gp = gpar(lwd = 2))
        grobs$raw.ghost <-
            linesGrob(x.units, unit(y, "native"),
                      vp = vpPath("parent", "plots", "trend"),
                      name = "raw.ghost",
                      gp = gpar(col = "#bbbbbb"))
        grobs$trendLine <-
            linesGrob(x.units, y.trend.units,
                      vp = vpPath("parent", "plots", "trend"),
                      name = "trendLine",
                      gp = gpar(col = trendCol, lwd = 2))
        grobs$trendYaxis <-
            yaxisGrob(main = TRUE,
                      vp = vpPath("parent", "plots", "trend"),
                      name = "trendYaxis",
                      gp = gpar(cex = .8))
        grobs$trendYlab <-
          textGrob(ylab, x= 0, y= 0.5, rot = 90,
                   vjust = -5,
                   vp = vpPath("parent", "plots", "trend"),
                   name = "trendYlab")

        gap <- unit(2, "mm")
        space <- unit(8, "mm")
        lineWidth <- unit(6, "mm")
        xc <- unit(2, "mm")
        grobs$trendLabel <-
            textGrob("Trend", just = c("left", "bottom"), x = xc,
                     y = unit(1, "npc") - unit(1, "lines"), name = "trendLabel",
                     vp = vpPath("parent", "plots", "trend"),
                     gp = gpar(cex = .9, col = "black", fontface = "bold"))
        xc <- xc + stringWidth(grobs$trendLabel$label) + gap
        grobs$trendKey <-
            linesGrob(x = unit.c(xc, xc + lineWidth),
                      y = unit(1, "npc") - unit(0.6, "lines"),
                      vp = vpPath("parent", "plots", "trend"),
                      name = "trendKey",
                      gp = gpar(col = trendCol, lwd = 2))
        xc <- xc + lineWidth + space
        grobs$rawKeyText <-
            textGrob("Raw data", just = c("left", "bottom"),
                     x = xc,
                     y = unit(1, "npc") - unit(1, "lines"), name = "rawKeyText",
                     vp = vpPath("parent", "plots", "trend"),
                     gp = gpar(cex = .9, col = "#bbbbbb", fontface = "bold"))
        xc <- xc + stringWidth(grobs$rawKeyText$label) + gap
        grobs$rawKey <-
            linesGrob(unit.c(xc, xc + lineWidth),
                      unit(1, "npc") - unit(0.6, "lines"),
                      vp = vpPath("parent", "plots", "trend"),
                      name = "rawKey",
                      gp = gpar(col = "#bbbbbb"))

        grobs$seasonBorder <- rectGrob(vp = vpPath("parent", "plots", "season"),
                                       name = "seasonBorder", gp = gpar(lwd = 2))
        grobs$season.y0 <-
            linesGrob(unit(dotted.xcoords, "native"),
                      unit(dotted.ycoords, "native"),
                      vp = vpPath("parent", "plots", "season"),
                      name = "season.y0",
                      gp = gpar(col = "#aaaaaa", lty = "1313"))
        grobs$seasonLine <-
            linesGrob(x.units, y.season.units,
                      vp = vpPath("parent", "plots", "season"),
                      name = "seasonLine",
                      gp = gpar(col = seasonCol))
        grobs$seasonYaxis <-
            yaxisGrob(main = FALSE,
                      vp = vpPath("parent", "plots", "season"),
                      name = "seasonYaxis",
                      gp = gpar(cex = .8))
        grobs$seasonLabel <-
            textGrob("Seasonal Swing",
                     vp = vpPath("parent", "plots", "season"),
                     name = "seasonLabel",
                     gp = gpar(cex = .9, col = "black", fontface = "bold"),
                     x = unit(0.02, "npc"), y = unit(0.97, "npc"),
                     hjust = 0, vjust = 1)

        grobs$randomBorder <-
            rectGrob(vp = vpPath("parent", "plots", "random"),
                     name = "randomBorder", gp = gpar(lwd = 2))
        grobs$random.y0 <-
            linesGrob(unit(dotted.xcoords, "native"),
                      unit(dotted.ycoords, "native"),
                      vp = vpPath("parent", "plots", "random"),
                      name = "random.y0",
                      gp = gpar(col = "#aaaaaa", lty = "1313"))
        grobs$randomLine <-
            linesGrob(x.units, y.random.units,
                      vp = vpPath("parent", "plots", "random"),
                      name = "randomLine",
                      gp = gpar(col = randCol))
        grobs$randomYaxis <-
            yaxisGrob(main = TRUE,
                      vp = vpPath("parent", "plots", "random"),
                      name = "randomYaxis",
                      gp = gpar(cex = .8))
        grobs$Xaxis <-
            xaxisGrob(gp = gpar(cex = .8),
                      vp = vpPath("parent", "plots", "random"),
                      name = "Xaxis")

        grobs$XaxisLabel <-
          textGrob(xlab, x= 0.5, y= 0, vjust = 3,
                   vp = vpPath("parent", "plots", "random"),
                   name = "XaxisLabel")

        grobs$randomLabel <-
            textGrob("Residuals",
                     vp = vpPath("parent", "plots", "random"),
                     name = "randomLabel",
                     gp = gpar(cex = .9, col = "black", fontface = "bold"),
                     x = unit(0.02, "npc"), y = unit(0.97, "npc"),
                     hjust = 0, vjust = 1)



        data.name = ifelse(ylab=="", "data", ylab)
        grobs$statusText <-
            textGrob(paste0("Decomposition of ", data.name, ":", obj$currVar),
                     vp = vpPath("parent", "head"),
                     name = "statusText")

        image <- gTree(name = "image", children = grobs,
                       childrenvp = final.vptree)

        ## return a list with all the variables we need
        decompVars <- list(tree = image, ranges = ranges, props = props,
                           data.name = data.name,
                           raw = obj$tsObj@.Data,
                           components = decompData,
                           #currentName =  obj$currVar,
                           #components = decomp$time.series,
                           vertMargins = vertMargins,
                           multiplicative = multiplicative)
        obj$decompVars <- decompVars
        obj
    }


##' Decomposes a time series into trend, seasonal and residual components
##' using \code{loess}.
##'
##' If the frequency is greater than 1, the components are found using the
##' \code{\link{stl}} function with \code{s.window} set to \code{TRUE}
##' (effectively replacing smoothing by taking the mean).
##' If the frequency is 1, the trend component is found directly by using
##' \code{\link{loess}} and the residuals are the difference between trend
##' and actual values.
##' The trend, seasonal and residual components are plotted on the same
##' scale allowing for easy visual analysis.
##'
##' @title Plot a Time Series Decomposition
##'
##' @param obj an \code{iNZightTS} object
##' 
##' @param xlab a title for the x axis
##' 
##' @param ylab a title for the y axis
##' 
##' @param multiplicative logical. If \code{TRUE}, a multiplicative model is used,
##' otherwise an additive model is used by default.
##'
##' @return The original \code{iNZightTS} object with an item \code{decompVars}
##' appended, containing results from the decomposition.
##'
##' @references R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3iV73.
##'
##' @seealso \code{\link{stl}}, \code{\link{loess}}, \code{\link{iNZightTS}}
##'
##' @examples z <- iNZightTS(ldeaths)
##' y <- decompositionplot(z)
##'
##' @export
decompositionplot <-
    function(obj, ylab = "", xlab = "", multiplicative=FALSE) {
        vars <- decomposition(obj, ylab, xlab, multiplicative = multiplicative)
        newdevice(width = 6, height = 5)
        drawImage(vars$decompVars$tree)
        vars
    }

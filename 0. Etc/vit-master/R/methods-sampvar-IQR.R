canvasSampvarIQR <- setRefClass("canvasSampvarIQRClass", contains = "canvasPlotClass",
                                methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampvarLevelsIQR(.self, env, i, ...)
    },

    showLabels = function() {
        sampvarDiffLabels(.self)
    },

    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        calcIQR(samples[[i]], level.samples[[i]], .self)
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        calcIQR(xs, ys, .self)
    },

    plotDataStat = function(env, ...) {
        linesOnQuartilesSampvar(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotIQRRatioDist(.self, env)
    },

    animateSample = function(env, n.steps, n.slow, opts) {
        dropSampvarPointsIQR(.self, env, opts$drop.points, n.steps, n.slow)
    },

    animateStat = function(env, n.steps) {
        dropIQRStat(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showCIandStatsIQR(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        IQRRatio1000(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeIQRRatioCI(.self, env, ...)
    }))

load_sampvar_iqr_ratio <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarIQR$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$c1$orderLevels()
    e$difference <- TRUE
}

linesOnQuartilesSampvar <- function(canvas, e){
    canvas$plotData()
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    for (i in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image, boxplotGrob
                                        (x[levels == ylevels[i]],
                                         stat = NULL, show.m = FALSE,
                                         show.w = FALSE, gp = gpar(lwd = 2),
                                         name = paste("dataPlot.boxplot", i, sep = "."),
                                         vp = canvas$graphPath("data", i)))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(fivenum(x[levels == ylevels[i]])[c(2, 4)],
                                          "native"),
                                 y = unit(if (i == 1) c(0.25, 0.25) else c(0.05, 0.05), "npc"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 gp = gpar(lwd = 3, col = "purple"),
                                 vp = canvas$graphPath("data", i),
                                 name = paste("dataPlot", "line", i, sep = ".")))
    }
    iqrs <- calcIQR(x, levels, canvas)
    # In the case where an invalid ratio is present just show NA
    if (any(iqrs == 0))
        iqr.labeltext <- "NA"
    else
        iqr.labeltext <- paste(round(iqrs[2], canvas$dp), ":", round(iqrs[1], canvas$dp), collapse = "")
    iqr.labeltext <- c("IQR Ratio:", iqr.labeltext)

    iqr.label <- textGrob(iqr.labeltext,
                          x = unit(0.8, "npc"),
                          y = unit.c(unit(2.4, "native") + unit(1, "lines"),
                                     unit(2.4, "native")),
                          just = c("centre", "bottom"),
                          name = "dataPlot.labeltext.1",
                          vp = canvas$graphPath("animation.field"))
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.8, "npc"),
                             y = unit(2.4, "native") - unit(1, "mm"),
                             width = max(stringWidth(iqr.labeltext)) + unit(2, "mm"),
                             height = unit(2, "lines"),
                             just = c("centre", "bottom"),
                             gp = gpar(col = "black", fill = "white", alpha = 0.8),
                             name = "dataPlot.labelrect.1",
                             vp = canvas$graphPath("animation.field")))
    canvas$image <- addGrob(canvas$image, iqr.label)
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rep(0, 2), "native"),
                                                    y = unit(c(0, 0.4), "npc"),
                                                    gp = gpar(lty = "dashed", lwd = 1),
                                                    vp = canvas$graphPath("stat"),
                                                    name = "zeroline.1"))
    statline <- linesGrob(x = unit(rep(log10(iqrs[2]), 2), "native"),
                          y = unit(c(0, 1), "npc") - unit(c(1, 0), "lines"),
                          gp = gpar(lty = "dashed", lwd = 2),
                          vp = canvas$graphPath("stat"),
                          name = "statline.1")
    canvas$image <- addGrob(canvas$image, statline)

}

dropSampvarPointsIQR <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5) {
    canvas$rmGrobs(c("samplePlot.databox.text.2", "samplePlot.stat.2", "samplePlot.labelrect.1", "samplePlot.labeltext.1"))
    canvas$rmGrobs(paste("samplePlot", c("points", "line", "boxplot", "text"), rep(1:2, each = 4), sep = "."))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    levels <- as.character(canvas$levels[index])
    y <- 0.5*canvas$y[index] + 0.5*(levels == canvas$ylevels[2])
    n <- canvas$n
    iqrs <- calcIQR(x, levels, canvas)

    if (n < 100) {
        sampSelectLabel <- textGrob("Selecting sample...",
                                    x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                    vp = canvas$graphPath("animation.field"),
                                    gp = gpar(fontface = 2),
                                    name = "samplePlot.sampSelectLabel")
        canvas$image <- addGrob(canvas$image, sampSelectLabel)

        n.slow <- min(n.slow, n)
        y.start <- y + 2
        y.end <- 0.5*old.stackPoints(x, levels = levels, vp = canvas$graphPath("sample")) + 1 +
                 0.5*(levels == canvas$ylevels[2])
        y.step <- (y.end - y.start) / n.steps

        ## Animation of slow points.
        for (i in seq(from = 1, by = 1, length.out = n.slow)) {
            ## Light up point to drop
            if (drop.points) {
                temp.point <- pointsGrob(x = x[1:i], y = y.start[1:i], pch = 19,
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.data.points.")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
        }
        ## Animation of fast points.
        length.out <- max(n - n.slow, 0)
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)) {
            ## Light up point to drop
            if (drop.points) {
                temp.point <- pointsGrob(x = x[1:i], y = y.start[1:i], pch = 19,
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.data.points.")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }

        for (j in 1:n.steps) {
            ## Drop point
            if (drop.points) {
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x = x, y = y.start + j*y.step, pch = 19,
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp"))
            }
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        canvas$pauseImage(10)
        canvas$rmGrobs(c("samplePlot.temp", "samplePlot.sampSelectLabel",
                         "samplePlot.temp.data.points."))
        plotSampvarLevelsIQR(canvas, e, canvas$which.sample, show.iqr = FALSE)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)

        # Now that points have been dropped, show calculation of the IQR ratio
        # Only do this on the '1' sample animation
        if (n.slow > 0) {
            n.steps <- 10
            ylevels <- canvas$ylevels
            # Start with top arrow
            curr.arrows <- fivenum(x[levels == ylevels[2]])[c(2, 4)]
            curr.arrows <- c(curr.arrows, fivenum(x[levels == ylevels[1]])[c(2, 4)])
            maxarrow <- max(abs(c(diff(curr.arrows[1:2]), diff(curr.arrows[3:4]))))
            y.start <- c(1.525, 1.125)
            y.end <- c(1.475, 1.325)
            y.step <- (y.start - y.end)/n.steps
            x0.start <- unit(curr.arrows[c(1, 3)], "native")
            x1.start <- unit(curr.arrows[c(2, 4)], "native")
            x0.end <- unit(0.5, "npc") - 0.5*(x1.start - x0.start)
            x.step <- (x0.end - x0.start)*(1/n.steps)
            for (i in 0:n.steps){
                arrows <- segmentsGrob(x0 = x0.start + i*x.step, y0 = y.start - i*y.step,
                                       x1 = x1.start + i*x.step, y1 = y.start - i*y.step,
                                       default.units = "native",
                                       arrow = arrow(length = unit(0.1, "inches")),
                                       name = "samplePlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                                       vp = canvas$graphPath("animation.field"))
                canvas$image <- addGrob(canvas$image, arrows)
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
            }

            canvas$rmGrobs(c("samplePlot.temp.arrows", "dataPlot.ci.1"))
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = unit(maxarrow, "native") + unit(0.2, "inches"),
                                     height = unit(0.1, "native") + unit(0.4, "inches"),
                                     name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()

            currstat <- iqrs[2] / iqrs[1]
            if (any(iqrs == 0))
                currstat <- NA
            halfheight <- 0.10
            fontsize <- 36
            stattext <- textGrob(label = format(round(currstat, canvas$dp), nsmall = canvas$dp), x = unit(0.5, "npc"),
                                 y = unit(1.4, "native"), gp = gpar(col = "red", fontsize = fontsize),
                                 name = "samplePlot.temp.text")
            divchar <- pointsGrob(pch = as.hexmode("00F7"),
                                  x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                  gp = gpar(col = "red", cex = fontsize / 10),
                                  name = "samplePlot.temp.divchar",
                                  vp = canvas$graphPath("animation.field"))
            canvas$image <- addGrob(canvas$image, arrows) # Adding arrows above the black rect
            canvas$image <- addGrob(canvas$image, divchar)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(10)
            canvas$rmGrobs(c("samplePlot.temp.divchar", "samplePlot.temp.text", "samplePlot.temp.arrows"))
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = unit(halfheight/2, "npc"),
                                     height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            canvas$drawImage()
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = unit(halfheight, "npc"),
                                     height = unit(halfheight/2, "npc"), name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            canvas$drawImage()
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = grobWidth(stattext) + unit(5, "mm"),
                                     height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            stattext <- editGrob(stattext, vp = canvas$graphPath("animation.field"))
            canvas$image <- addGrob(canvas$image, stattext)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(20)
            canvas$rmGrobs(c("samplePlot.temp.text", "samplePlot.temp.rect"))
        }

        canvas$plotSample(e, canvas$which.sample)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
    }
}

plotSampvarLevelsIQR <- function(canvas, e, i, alpha = 0.25, show.iqr = TRUE) {
    canvas$rmGrobs(c("samplePlot.labeltext.1", "samplePlot.labelrect.1"))
    x <- canvas$samples[[i]]
    levels <- canvas$level.samples[[i]]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    cols <- character(canvas$n)
    cols[levels == canvas$ylevels[1]] <- getColour(1, 2, l = 35)
    cols[levels == canvas$ylevels[2]] <- getColour(2, 2, l = 35)
    # Plotting samples and labels
    for (j in 1:length(ylevels)) {
        plotPoints(canvas, x[levels == ylevels[j]], y[levels == ylevels[j]],
                   col = getColour(j, length(ylevels)),
                   vp = canvas$graphPath("sample", j),
                   name = "samplePlot")
        canvas$image <- addGrob(canvas$image, boxplotGrob
                                (x[levels == ylevels[j]],
                                 stat = NULL,
                                 show.w = FALSE, show.m = FALSE,
                                 gp = gpar(lwd = 2),
                                 name = paste("samplePlot.boxplot", j, sep = "."),
                                 vp = canvas$graphPath("sample", j)))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(canvas$quartiles[i, 1, ylevels[j]],
                                            canvas$quartiles[i, 3, ylevels[j]]),
                                          "native"),
                                 y = unit(if (j == 1) c(0.25, 0.25) else c(0.05, 0.05), "npc"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 gp = gpar(lwd = 3, col = "purple"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot", "line", j, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (ylevels[j], x = 1, y = unit(1, "mm"),
                                 just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot.text", j, sep = ".")))
    }

    # Adding data points in black
    index <- canvas$indexes[[i]]
    data.y <- 2 + 0.5*canvas$y[index] + 0.5*(canvas$levels[index] == canvas$ylevels[2])
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x = x, y = data.y, vp = canvas$graphPath("animation.field"),
                                       name = "samplePlot.temp.point",
                                       gp = gpar(col = cols), pch = 19))
    if (show.iqr) {
        iqrs <- calcIQR(x, levels, canvas)

        # Handling the case where an IQR ratio is invalid
        if (any(iqrs == 0))
            iqr.labeltext <- "NA"
        else
            iqr.labeltext <- paste(round(iqrs[2], canvas$dp), ":", round(iqrs[1], canvas$dp), collapse = "")

        iqr.labeltext <- c("IQR Ratio:", iqr.labeltext)
        iqr.label <- textGrob(iqr.labeltext,
                              x = unit(0.8, "npc"),
                              y = unit.c(unit(1.4, "native") + unit(1, "lines"),
                                         unit(1.4, "native")),
                              just = c("centre", "bottom"),
                              name = "samplePlot.labeltext.1",
                              vp = canvas$graphPath("animation.field"))
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.8, "npc"),
                                 y = unit(1.4, "native") - unit(1, "mm"),
                                 width = max(stringWidth(iqr.labeltext)) + unit(2, "mm"),
                                 height = unit(2, "lines"),
                                 just = c("centre", "bottom"),
                                 gp = gpar(col = "black", fill = "white", alpha = 0.8),
                                 name = "samplePlot.labelrect.1",
                                 vp = canvas$graphPath("animation.field")))
        canvas$image <- addGrob(canvas$image, iqr.label)
    }
}

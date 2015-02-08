canvasPermSlope <- setRefClass("canvasPermSlopeClass", contains = "canvasPlotClass",
                               methods = list(
    plotSample = function(env, i = which.sample) {
        plotSampleScatter(.self, env, i)
    },

    showLabels = function() {
        permRegressionLabels(.self)
    },

    calcStat = function(i = which.sample, ys = NULL) {
        calcCoeffs(samples[[i]], ys)
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        calcCoeffs(xs, ys)
    },

    plotDataStat = function(env, ...) {
        showExtraRect(env)
    },

    plotStatDist = function(env, ...) {
        plotSlope(.self, env)
    },

    animateSample = function(...) {
        permDataText(.self, ...)
    },

    trackSample = function() {
    },

    animateStat = function(env, n.steps, move.point = TRUE) {
        dropSlope(.self, env, n.steps, move.point)
    },

    displayResult = function(env, ...) {
        showSlopeTail(env)
    },

    handle1000 = function(env, ...) {
        slope1000(.self, env, ...)
    },

    fadePlots = function(env, ...) {
    },

    calcxScaleYs = function(env, x.scale) {
        x.scale.ys <<- laply(stat.dist, function(x) x[1] + x[2]*x.scale)
    }))

load_permutation_slope <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$perm.method <- svalue(e$perm.choices)
    e$c1$stat.method <- e$perm.method
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasPermSlope$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    ## Gives a value for the additional room given to each end of the x axis scale.
    e$extra <- 0
}

calcCoeffs <- function(x, y){
    coefficients(lm(y ~ x))
}

permRegressionLabels <- function(canvas) {
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("data"),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Randomisation test",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
                          vp = canvas$graphPath("canvas.header"))
    varlabel <- textGrob("Parameter: slope",
                         x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                         just = "left",
                         gp = gpar(fontsize = 10, fontface = "italic"),
                         name = "varLabel",
                         vp = canvas$graphPath("canvas.header"))
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = varlabel$x + stringWidth(varlabel$label) + unit(6, "mm"),
                          just = "left",
                          name = "fileLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    dataLabelBackground <- rectGrob(x = unit(0, "npc") + unit(1, "mm"),
                                    y = unit(1, "npc") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(2, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("data"))
    resamplabel <- textGrob("Re-randomised data",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(1, "npc") - unit(2, "mm"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample"),
                            gp = gpar(fontface = 2))
    resampLabelBackground <- rectGrob(x = unit(0, "npc") + unit(1, "mm"),
                                      y = unit(1, "npc") - unit(2, "mm"),
                                      width = stringWidth(resamplabel$label) + unit(4, "mm"),
                                      height = stringHeight(resamplabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "resampLabelBackground",
                                      vp = canvas$graphPath("sample"))
    statlabel <- textGrob("Re-randomisation distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(2, "lines"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    permlabels <- grobTree(methlabel, varlabel, filelabel,
                           infosep,
                           dataLabelBackground,
                           samplabel,
                           resampLabelBackground,
                           resamplabel, statlabel,
                           name = "permlabels")
    canvas$image <- addGrob(canvas$image, permlabels)
    canvas$image <- addGrob(canvas$image, segmentsGrob
                        (x0 = unit(rep(1.5, 2), "lines"),
                         x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                         y0 = unit(rep(2/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                         y1 = unit(rep(2/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                         unit(1.5, "lines")),
                         arrow = arrow(length = unit(0.075, "inches")),
                         vp = canvas$graphPath("stat.extrabox"),
                         name = "dataPlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(2/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 5/6, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.ylabel.1"))
    x <- canvas$x
    y <- canvas$levels
    slope <- coefficients(lm(y ~ x))[2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c("slope =", format(round(slope, canvas$dp),
                             nsmall = canvas$dp)),
                             x = 0.5, y = unit(5/6, "npc") + unit(c(0.5, -0.5), "lines"),
                             gp = gpar(col = c("black", "red"), fontface = 1:2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.slopetext.1"))
}

permDataText <- function(canvas, e, n.steps, mix = TRUE){
    n.steps <- 2*n.steps
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.points.1",
                   "samplePlot.line.1", "samplePlot.databox.text.2",
                   "samplePlot.labelaxes.1", "samplePlot.xlabel.1",
                   "samplePlot.ylabel.1", "samplePlot.slopetext.1"))
    x <- canvas$x
    y <- canvas$levels
    n <- canvas$n
    max <- 50
    ntext <- min(n, max)
    npcs <- (ntext:0)/ntext
    yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))[-1]
    ## Setting the number of points appearing in the data textbox (including "...").
    if (n > ntext){
        yunit <- unit.c(yunit, rep(yunit[ntext], n - ntext))
    }
    ## Setting data textbox to red.
    canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                             gp = gpar(fontface = c(1, rep(2, ntext)),
                             col = c("black", rep("red", ntext))))
    ## Putting title in sample textbox.
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = "", title = "Re-randomised", x = 0.5,
                             max  = 50, name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    canvas$pauseImage(5)
    ## Giving the order for the values to appear in the sample textbox.
    ord <- order(canvas$indexes[[canvas$which.sample]])
    ## Going into the viewport so all units can be converted to npcs.
    top.level <- downViewport(vpPath("canvas.all", "canvas.boxes"))
    ## Specifying a random order for the values to move from the data
    ## textbox to the sample textbox.
    rand <- sample(1:n)
    text <- format(round(y, canvas$dp), nsmall = canvas$dp)
    textrand <- text[rand]
    x.start <- 0.375
    x.end <- 0.75
    x.step <- (x.end - x.start)/n.steps
    yunit <- convertY(yunit, "npc", valueOnly = TRUE)
    y.start <- yunit[rand]
    y.end <- yunit[order(ord)][rand]
    y.step <- (y.end - y.start)/n.steps
    ## Code for sequential moving of values.
    if (mix){
        ## Calculating coordinates for each frame.
        x.pos.mat <- matrix(0, nrow = n, ncol = n + n.steps)
        y.pos.mat <- matrix(0, nrow = n, ncol = n + n.steps)
        x.pos.mat[, 1] <- x.start
        y.pos.mat[, 1] <- y.start
        for (i in 2:(n + n.steps)){
            rows <- 1:n <= (i - 1) & (i - 1) - 1:n < n.steps
            x.pos.mat[, i] <- x.pos.mat[, i - 1] + rows*x.step
            y.pos.mat[, i] <- y.pos.mat[, i - 1] + rows*y.step
        }
        upViewport(top.level)
        for (i in 2:(n + n.steps)){
            ## Moving values in textboxes. If a value either hasn't
            ## moved or has reached its destination then it is drawn
            ## within a datatextGrob to avoid messy overwriting of
            ## textGrobs.
            x.pos <- x.pos.mat[, i]
            y.pos <- y.pos.mat[, i]
            sub1 <- x.pos == x.start
            cols1 <- character(ntext)
            fface1 <- numeric(ntext)
            cols1[sub1] <- "red"
            cols1[!sub1] <- "black"
            fface1[sub1] <- 2
            fface1[!sub1] <- 1
            cols1 <- cols1[order(rand)]
            fface1 <- fface1[order(rand)]
            sub2 <- round(x.pos, 2) == x.end
            sub3 <- sub2[order(rand)]
            sub3 <- sub3[ord]
            db2text <- text[ord]
            db2text[!sub3] <- ""
            canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                     gp = gpar(col = c("black", cols1),
                                     fontface = c(1, fface1)))
            canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                     data = db2text, gp = gpar(col = c("black", rep("red", ntext)),
                                     fontface = c(1, rep(2, ntext))))
            if (any(!sub1 & !sub2)){
                canvas$image <- addGrob(canvas$image, textGrob
                                        (textrand[!sub1 & !sub2], x = x.pos[!sub1 & !sub2],
                                         y = y.pos[!sub1 & !sub2],
                                         gp = gpar(fontface = 2, col = "red"), just = "top",
                                         vp = vpPath("canvas.all", "canvas.boxes"),
                                         name = "samplePlot.temp.text"))
            } else {
                canvas$rmGrobs("samplePlot.temp.text")
            }
            canvas$drawImage()
        }
        ## Code for simultaneous movement of points.
    } else {
        upViewport(top.level)
        canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                 gp = gpar(col = "black", fontface = 1))
        for (i in 1:(n.steps - 1)){
            canvas$image <- addGrob(canvas$image, textGrob
                                    (textrand, x = x.start + i*x.step, y = y.start + i*y.step,
                                     gp = gpar(fontface = 2, col = "red"), just = "top",
                                     vp = vpPath("canvas.all", "canvas.boxes"),
                                     name = "samplePlot.temp.text"))
            canvas$drawImage()
        }
        canvas$rmGrobs("samplePlot.temp.text")
    }
    ## Final drawing of values in sample textbox.
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = text[ord], title = "Re-randomised",
                             x = 0.5, max = 50,
                             gp = gpar(fontface = c(1, rep(2, ntext)),
                                       col = c("black", rep("red", ntext))),
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
}

plotSampleScatter <- function(canvas, e, i) {
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    x <- canvas$samples[[i]]
    y <- canvas$levels
    text <- as.character(format(round(y, canvas$dp), nsmall = canvas$dp)[order(canvas$indexes[[i]])])
    if (diff(range(x)) == 0){
        x.scale <- range(x) + c(-1, 1)
    } else {
        x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
    }
    x0s <- rep(x.scale[1], length(canvas$sampled.stats))
    x1s <- rep(x.scale[2], length(canvas$sampled.stats))
    y0s <- canvas$x.scale.ys[canvas$sampled.stats, 1]
    y1s <- canvas$x.scale.ys[canvas$sampled.stats, 2]
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0s, x1 = x1s, y0 = y0s, y1 = y1s,
                             default.units = "native",
                             gp = gpar(col = "skyblue2"),
                             vp = canvas$graphPath("sample"),
                             name = "samplePlot.ghosts.1"))
    plotScatterPlot(canvas, x, y, "sample", "blue")
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = text, title = "Re-randomised",
                             x = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    slope <- coefficients(lm(y ~ x))[2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c("slope =", format(round(slope, canvas$dp),
                             nsmall = canvas$dp)),
                             x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                             gp = gpar(col = c("black", "red"), fontface = 1:2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.slopetext.1"))
}

plotSampleScatterAlpha <- function(canvas, e, i) {
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    x <- canvas$samples[[i]]
    y <- canvas$levels
    text <- as.character(format(round(y, canvas$dp), nsmall = canvas$dp)[order(canvas$indexes[[i]])])
    if (diff(range(x)) == 0){
        x.scale <- range(x) + c(-1, 1)
    } else {
        x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
    }
    x0s <- rep(x.scale[1], length(canvas$sampled.stats))
    x1s <- rep(x.scale[2], length(canvas$sampled.stats))
    y0s <- canvas$x.scale.ys[canvas$sampled.stats, 1]
    y1s <- canvas$x.scale.ys[canvas$sampled.stats, 2]
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0s, x1 = x1s, y0 = y0s, y1 = y1s,
                             default.units = "native",
                             gp = gpar(col = "blue", alpha = 0.1),
                             vp = canvas$graphPath("sample"),
                             name = "samplePlot.ghosts.1"))
    plotScatterPlot(canvas, x, y, "sample", "blue")
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = text, title = "Re-randomised",
                             x = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    slope <- coefficients(lm(y ~ x))[2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c("slope =", format(round(slope, canvas$dp),
                             nsmall = canvas$dp)),
                             x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                             gp = gpar(col = c("black", "red"), fontface = 1:2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.slopetext.1"))
}

dropSlope <- function(canvas, env, n.steps, move.point = TRUE){
    if (is.list(canvas$stat.dist)){
        stat <- canvas$stat.dist[[canvas$which.sample]][2]
        slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)][canvas$plotted.stats]
    } else if (is.matrix(canvas$stat.dist)){
        stat <- canvas$stat.dist[canvas$which.sample, 2]
        slopes <- canvas$stat.dist[, 2][canvas$plotted.stats]
    }
    slopes <- c(slopes, stat)
    y.bounds <- canvas$x.scale.ys[canvas$which.sample, ]
    ## Weird stuff going on with viewports, seekViewport functions provide a quick fix.
    seekViewport(canvas$graphPath("sample"))
    #top.level <- downViewport(canvas$graphPath("sample"))
    sampunit <- convertY(unit(y.bounds, "native"), "npc", valueOnly = TRUE)
    seekViewport("animation.field2")
    #upViewport(top.level)
    seekViewport(canvas$graphPath("stat"))
    #top.level <- downViewport(canvas$graphPath("stat"))
    y.end <- convertY(unit(0, "native"), "npc", valueOnly = TRUE)/3
    seekViewport("animation.field2")
    #upViewport(top.level)
    y.start <- 1/3 + sampunit/3
    y.end <- c(y.end, y.end + diff(y.start))
    y.step <- (y.end - y.start)/n.steps
    for (i in 0:n.steps){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1, y = y.start + i*y.step,
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
    }
    if (move.point){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x = 0, y = unit(stat, "native")*(1/3), pch = 19,
                                 gp = gpar(col = "red"), vp = canvas$graphPath("stat.extrabox"),
                                 name = "statPlot.point.1"))
        canvas$pauseImage(5)
        y <- stackPoints(slopes, vp = canvas$graphPath("stat.extrabox"), y.min = 0.2)
        x.start <- 0
        x.end <- y[length(y)]
        x.step <- (x.end - x.start)/round(n.steps/2)
        for (i in 1:round(n.steps/2)){
            canvas$image <- editGrob(canvas$image, gPath("statPlot.point.1"),
                                     x = unit(i*x.step, "npc"))
            canvas$drawImage()
        }
        canvas$rmGrobs("statPlot.point.1")
    }
}

plotSlope <- function(canvas, e) {
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    nsamps <- length(canvas$plotted.stats)
    if (is.list(canvas$stat.dist)){
        slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)][canvas$plotted.stats]
    } else if (is.matrix(canvas$stat.dist)){
        slopes <- canvas$stat.dist[, 2][canvas$plotted.stats]
    }
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "skyblue2"), name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1,
                             y = unit(c(0, slopes[length(slopes)]), "native"),
                             gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                             vp = canvas$graphPath("stat")))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    y <- stackPoints(slopes, vp = canvas$graphPath("stat.extrabox"), y.min = 0.2)
    plotPoints(canvas, y, unit(slopes, "native")*(1/3), vp = canvas$graphPath("stat.extrabox"), name = "statPlot")
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
}

plotSlopeAlpha <- function(canvas, e) {
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    nsamps <- length(canvas$plotted.stats)
    if (is.list(canvas$stat.dist)){
        slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)][canvas$plotted.stats]
    } else if (is.matrix(canvas$stat.dist)){
        slopes <- canvas$stat.dist[, 2][canvas$plotted.stats]
    }
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "blue", alpha = 0.2), name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1,
                             y = unit(c(0, slopes[length(slopes)]), "native"),
                             gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                             vp = canvas$graphPath("stat")))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    y <- stackPoints(slopes, vp = canvas$graphPath("stat.extrabox"), y.min = 0.2)
    plotPoints(canvas, y, unit(slopes, "native")*(1/3), vp = canvas$graphPath("stat.extrabox"), name = "statPlot")
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
}

slope1000 <- function(canvas, e) {
    slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
    x <- canvas$x
    y <- canvas$levels
    if.plotted <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000)[order(order(slopes))]
    x0 <- rep(0, 1000)
    x1 <- rep(1, 1000)
    y0 <- unit(rep(0, 1000), "native")
    y1 <- unit(slopes, "native")
    if (diff(range(x)) == 0){
        x.scale <- range(canvas$x) + c(-1, 1)
    } else {
        x.scale <- range(canvas$x) + c(-0.04, 0.04)*diff(range(x))
    }
    x0s <- rep(x.scale[1], 1000)
    x1s <- rep(x.scale[2], 1000)
    y0s <- canvas$x.scale.ys[, 1]
    y1s <- canvas$x.scale.ys[, 2]
    n <- 1
    runs <- if (if.plotted[1000])
                which(if.plotted)
            else
                c(which(if.plotted), 1000)
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    for (i in runs) {
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0s[1:i][if.plotted[1:i]], x1 = x1s[1:i][if.plotted[1:i]],
                                 y0 = y0s[1:i][if.plotted[1:i]], y1 = y1s[1:i][if.plotted[1:i]],
                                 default.units = "native",
                                 gp = gpar(col = "skyblue2"),
                                 vp = canvas$graphPath("sample"),
                                 name = "samplePlot.ghosts.1"))
        plotScatterPlot(canvas, canvas$samples[[i]], y, "sample", "blue")
        text <- as.character(format(round(y, canvas$dp),
                                    nsmall = canvas$dp)[order(canvas$indexes[[i]])])
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = text, title = "Re-randomised",
                                 x = 0.5, max = 50,
                                 name = "samplePlot.databox.text.2",
                                 vp = canvas$graphPath("databox", 2)))
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0[1:i][if.plotted[1:i]], x1 = x1[1:i][if.plotted[1:i]],
                                 y0 = y0[1:i][if.plotted[1:i]], y1 = y1[1:i][if.plotted[1:i]],
                                 gp = gpar(col = "skyblue2"),
                                 name = "statPlot.statlines.1",
                                 vp = canvas$graphPath("stat")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1,
                                 y = unit(c(0, slopes[i]), "native"),
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("stat")))
        sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        statlabel <- getGrob(canvas$image, gPath("statLabel"))
        canvas$image <- addGrob(canvas$image, statlabel)
        plotPoints(canvas, canvas$stat.ypos[1:i]*0.8 + 0.2, unit(slopes[1:i], "native")*(1/3),
                   vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
        slope <- slopes[i]
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = c("slope =", format(round(slope, canvas$dp),
                                 nsmall = canvas$dp)),
                                 x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                                 gp = gpar(col = c("black", "red"), fontface = 1:2),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "samplePlot.slopetext.1"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
        n <- n + 1
    }
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.databox.text.2", "samplePlot.points.1",
                     "samplePlot.line.1", "samplePlot.ylabel.1", "samplePlot.xlabel.1",
                     "samplePlot.labelaxes.1", "samplePlot.slopetext.1"))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.statlines.1", "samplePlot.ghosts.1", "statPlot.points.2"))
}

slope1000Alpha <- function(canvas, e) {
    slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
    x <- canvas$x
    y <- canvas$levels
    x0 <- rep(0, 1000)
    x1 <- rep(1, 1000)
    y0 <- unit(rep(0, 1000), "native")
    y1 <- unit(slopes, "native")
    if (diff(range(x)) == 0){
        x.scale <- range(canvas$x) + c(-1, 1)
    } else {
        x.scale <- range(canvas$x) + c(-0.04, 0.04)*diff(range(x))
    }
    x0s <- rep(x.scale[1], 1000)
    x1s <- rep(x.scale[2], 1000)
    y0s <- canvas$x.scale.ys[, 1]
    y1s <- canvas$x.scale.ys[, 2]
    n <- 1
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    for (i in seq(1, 1000, 50)) {
        line.filter <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000) 
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0s[1:i][line.filter[1:i]], x1 = x1s[1:i][line.filter[1:i]],
                                 y0 = y0s[1:i][line.filter[1:i]], y1 = y1s[1:i][line.filter[1:i]],
                                 default.units = "native",
                                 gp = gpar(col = "blue", alpha = 0.2),
                                 vp = canvas$graphPath("sample"),
                                 name = "samplePlot.ghosts.1"))
        plotScatterPlot(canvas, canvas$samples[[i]], y, "sample", "blue")
        text <- as.character(format(round(y, canvas$dp),
                                    nsmall = canvas$dp)[order(canvas$indexes[[i]])])
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = text, title = "Re-randomised",
                                 x = 0.5, max = 50,
                                 name = "samplePlot.databox.text.2",
                                 vp = canvas$graphPath("databox", 2)))
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0[1:i][line.filter[1:i]], x1 = x1[1:i][line.filter[1:i]],
                                 y0 = y0[1:i][line.filter[1:i]], y1 = y1[1:i][line.filter[1:i]],
                                 gp = gpar(col = "blue", alpha = 0.2),
                                 name = "statPlot.statlines.1",
                                 vp = canvas$graphPath("stat")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1,
                                 y = unit(c(0, slopes[i]), "native"),
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("stat")))
        sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        statlabel <- getGrob(canvas$image, gPath("statLabel"))
        canvas$image <- addGrob(canvas$image, statlabel)
        plotPoints(canvas, canvas$stat.ypos[1:i]*0.8 + 0.2, unit(slopes[1:i], "native")*(1/3),
                   vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
        slope <- slopes[i]
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = c("slope =", format(round(slope, canvas$dp),
                                 nsmall = canvas$dp)),
                                 x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                                 gp = gpar(col = c("black", "red"), fontface = 1:2),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "samplePlot.slopetext.1"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
        n <- n + 1
    }
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.databox.text.2", "samplePlot.points.1",
                     "samplePlot.line.1", "samplePlot.ylabel.1", "samplePlot.xlabel.1",
                     "samplePlot.labelaxes.1", "samplePlot.slopetext.1"))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.statlines.1", "samplePlot.ghosts.1", "statPlot.points.2"))
}

showExtraRect <- function(e){
    canvas <- e$c1
    canvas$image <- addGrob(canvas$image, rectGrob
                            (y = 0, height = 1/3, just = "bottom",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "extraboxRect"))
}

showSlopeTailAlpha <- function(e) {
    canvas <- e$c1
    y <- canvas$levels
    x <- canvas$x
    if (diff(range(x)) == 0){
        x.scale <- range(x) + c(-1, 1)
    } else {
        x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
    }
    coeffs <- coefficients(lm(y ~ x))
    y.bounds <- coeffs[1] + coeffs[2]*x.scale
    n.steps <- 5
    ## Replotting all permutations
    slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
    x0s <- rep(x.scale[1], 1000)
    x1s <- rep(x.scale[2], 1000)
    y0s <- canvas$x.scale.ys[, 1]
    y1s <- canvas$x.scale.ys[, 2]
    x0 <- rep(0, 1000)
    x1 <- rep(1, 1000)
    y0 <- unit(rep(0, 1000), "native")
    y1 <- unit(slopes, "native")
    line.filter <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000) 
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0s[line.filter], x1 = x1s[line.filter],
                             y0 = y0s[line.filter], y1 = y1s[line.filter],
                             default.units = "native",
                             gp = gpar(col = "blue", alpha = 0.2),
                             vp = canvas$graphPath("sample"),
                             name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0[line.filter], x1 = x1[line.filter],
                             y0 = y0[line.filter], y1 = y1[line.filter],
                             gp = gpar(col = "blue", alpha = 0.2),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    plotPoints(canvas, canvas$stat.ypos*0.8 + 0.2, unit(slopes, "native")*(1/3),
               vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
    ## Dropping data line.
    top.level <- downViewport(canvas$graphPath("data"))
    dataunit <- convertY(unit(y.bounds, "native"), "npc", valueOnly = TRUE)
    upViewport(top.level)
    top.level <- downViewport(canvas$graphPath("stat"))
    y.end <- convertY(unit(0, "native"), "npc", valueOnly = TRUE)/3
    upViewport(top.level)
    y.start <- 2/3 + dataunit/3
    y.end <- c(y.end, y.end + diff(y.start))
    y.step <- (y.end - y.start)/n.steps
    for (i in 0:n.steps){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1, y = y.start + i*y.step,
                                 gp = gpar(lwd = 2, col = "purple3"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("animation.field")))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        canvas$drawImage()
    }
    ## Showing tail
    if (coeffs[2] > 0){
        out <- slopes >= coeffs[2]
    } else {
        out <- slopes <= coeffs[2]
    }
    linecols <- rep("blue", 1000)
    linecols[!out] <- "grey60"
    linealphas <- rep(0.2, 1000)
    linealphas[!out] <- 0.05
    pointcols <- rep("grey60", 1000)
    pointcols[!out] <- "lightgrey"
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(0:1, "npc"), y = unit(coeffs[2], "native")*(1/3),
                             gp = gpar(col = "purple3", lwd = 2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "statPlot.line.2"))
    canvas$image <- editGrob(canvas$image, gPath("statPlot.statlines.1"),
                             gp = gpar(col = linecols, alpha = linealphas))
    canvas$image <- editGrob(canvas$image, gPath("statPlot.points.2"),
                             gp = gpar(col = pointcols))
    textline.1 <- paste(sum(out), "/", "1000", sep = " ")
    textline.2 <- paste("=", mean(out), sep = " ")
    box.width <- max(stringWidth(textline.1), stringWidth(textline.2)) + unit(2, "mm")
    box.height <- unit(2, "lines") + unit(2, "mm")
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = 1, y = unit(coeffs[2], "native")*(1/3),
                             just = c("right", "bottom"), height = box.height,
                             width = box.width, gp = gpar(fill = "white"),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "statPlot.box.2"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c(textline.1, textline.2),
                             x = unit(1, "npc") - 0.5*box.width,
                             y = unit.c(unit(coeffs[2], "native")*(1/3) + unit(2, "mm")
                             + unit(1, "lines"), unit(coeffs[2], "native")*(1/3) + unit(2, "mm")),
                             just = c("centre", "bottom"),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "statPlot.text.2"))
    canvas$drawImage()
    e$clearPanel("stat")
    e$clearPanel("sample")
}

showSlopeTail <- function(e) {
    canvas <- e$c1
    y <- canvas$levels
    x <- canvas$x
    if (diff(range(x)) == 0){
        x.scale <- range(x) + c(-1, 1)
    } else {
        x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
    }
    coeffs <- coefficients(lm(y ~ x))
    y.bounds <- coeffs[1] + coeffs[2]*x.scale
    n.steps <- 5
    ## Replotting all permutations
    slopes <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
    if.plotted <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000)[order(order(slopes))]
    x0s <- rep(x.scale[1], 1000)
    x1s <- rep(x.scale[2], 1000)
    y0s <- canvas$x.scale.ys[, 1]
    y1s <- canvas$x.scale.ys[, 2]
    x0 <- rep(0, 1000)
    x1 <- rep(1, 1000)
    y0 <- unit(rep(0, 1000), "native")
    y1 <- unit(slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0s[if.plotted], x1 = x1s[if.plotted],
                             y0 = y0s[if.plotted], y1 = y1s[if.plotted],
                             default.units = "native",
                             gp = gpar(col = "skyblue2"),
                             vp = canvas$graphPath("sample"),
                             name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0[if.plotted], x1 = x1[if.plotted],
                             y0 = y0[if.plotted], y1 = y1[if.plotted],
                             gp = gpar(col = "skyblue2"),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    plotPoints(canvas, canvas$stat.ypos*0.8 + 0.2, unit(slopes, "native")*(1/3),
               vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
    ## Dropping data line.
    top.level <- downViewport(canvas$graphPath("data"))
    dataunit <- convertY(unit(y.bounds, "native"), "npc", valueOnly = TRUE)
    upViewport(top.level)
    top.level <- downViewport(canvas$graphPath("stat"))
    y.end <- convertY(unit(0, "native"), "npc", valueOnly = TRUE)/3
    upViewport(top.level)
    y.start <- 2/3 + dataunit/3
    y.end <- c(y.end, y.end + diff(y.start))
    y.step <- (y.end - y.start)/n.steps
    for (i in 0:n.steps){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1, y = y.start + i*y.step,
                                 gp = gpar(lwd = 2, col = "purple3"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("animation.field")))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        canvas$drawImage()
    }
    ## Showing tail
    if (coeffs[2] > 0){
        out <- slopes >= coeffs[2]
    } else {
        out <- slopes <= coeffs[2]
    }
    linecols <- rep("skyblue2", 1000)
    linecols[!out] <- "lightcyan2"
    linecols <- linecols[if.plotted]
    pointcols <- rep("grey60", 1000)
    pointcols[!out] <- "lightgrey"
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(0:1, "npc"), y = unit(coeffs[2], "native")*(1/3),
                             gp = gpar(col = "purple3", lwd = 2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "statPlot.line.2"))
    canvas$image <- editGrob(canvas$image, gPath("statPlot.statlines.1"),
                             gp = gpar(col = linecols))
    canvas$image <- editGrob(canvas$image, gPath("statPlot.points.2"),
                             gp = gpar(col = pointcols))
    textline.1 <- paste(sum(out), "/", "1000", sep = " ")
    textline.2 <- paste("=", mean(out), sep = " ")
    box.width <- max(stringWidth(textline.1), stringWidth(textline.2)) + unit(2, "mm")
    box.height <- unit(2, "lines") + unit(2, "mm")
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = 1, y = unit(coeffs[2], "native")*(1/3),
                             just = c("right", "bottom"), height = box.height,
                             width = box.width, gp = gpar(fill = "white"),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "statPlot.box.2"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c(textline.1, textline.2),
                             x = unit(1, "npc") - 0.5*box.width,
                             y = unit.c(unit(coeffs[2], "native")*(1/3) + unit(2, "mm")
                             + unit(1, "lines"), unit(coeffs[2], "native")*(1/3) + unit(2, "mm")),
                             just = c("centre", "bottom"),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "statPlot.text.2"))
    canvas$drawImage()
    e$clearPanel("stat")
    e$clearPanel("sample")
}

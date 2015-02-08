canvasSampvarPropDiff <- setRefClass("canvasSampvarPropDiffClass", contains = "canvasPlotClass",
                                     methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        props <- calcPropDiff(samples[[i]], level.samples[[i]], canvas)
        # Sorting in ylevel order but reversing so we end up
        # with correct differences
        rev(props[canvas$ylevels])
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        props <- calcPropDiff(xs, ys, canvas)
        # Sorting in ylevel order but reversing so we end up
        # with correct differences
        rev(props[canvas$ylevels])
    },

    animateStat = function(env, n.steps) {
        dropBootPropDiffArrow(.self, env, n.steps)
    },

    plotSample = function(env, i = which.sample, alpha = 0.2, ...) {
        plotKSampvarProportions(.self, env, i, alpha = alpha)
    },

    showLabels = function() {
        sampvarDiffLabels(.self)
    },

    plotDataStat = function(env, ...) {
        dataDiffArrowProp(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(e, n.steps, n.slow, opts) {
        dropSampvarPropDiffSample(.self, e, n.steps, n.slow)
    },

    handle1000 = function(env, ...) {
        sampvarDiff1000(.self, env, ...)
    },

    displayResult = function(env, ...){
        plotSampvarTwoSampPropTheoDist(.self, env, ...)
        }))

load_sampvar_proportion_diff <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$sampvar.method <- svalue(e$sampvar.choices)
    e$c1$stat.method <- e$sampvar.method
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarPropDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- TRUE
}

plotKSampvarProportions <- function(canvas, e, i, alpha = 0.2) {
    index <- canvas$indexes[[i]]
    x <- canvas$x[index]
    x[x != canvas$loi] <- canvas$loi.alt
    data.levels <- as.character(canvas$levels)
    levels <- data.levels[index]
    ylevels <- canvas$ylevels
    n <- canvas$n
    props <- rev(calcPropDiff(x, levels, canvas)[ylevels])
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    all.props <- canvas$stat.dist[canvas$sampled.stats, 2:1, drop = FALSE]

    # Finding the max width to place the levels labels within
    seekViewport(canvas$graphPath("sample", 1))
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "native", valueOnly = TRUE)
    upViewport(0)
    for (j in 1:length(ylevels)) {
        level.props <- all.props[, j]
        yloc <- if (j == 1) 0.5 else -0.1
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(level.props, "npc"),
                                         y = unit(yloc, "npc"), height = unit(0.1, "npc"),
                                         width = unit(0, "npc"), just = c("left", "bottom"),
                                         gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                         vp = canvas$graphPath("sample", j),
                                         name = paste("samplePlot.ghosts.", j, sep = "")))
        # We add a blank white rect to obscure most of the ghosting lines
        # so that they are not visible through the propbar.
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(0, "npc"), y = unit(0, "npc"),
                                         width = unit(1, "npc"), height = unit(0.5, "npc"),
                                         just = c("left", "bottom"),
                                         gp = gpar(fill = "white", col = "white"),
                                         vp = canvas$graphPath("sample", j),
                                         name = paste("samplePlot.blank.", j, sep = "")))

        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x[levels == ylevels[j]],
                                 ylab = ylevels[j],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(j, length(ylevels)),
                                 draw.points = FALSE,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = 0, height = 0.5, vpcex = 1 / length(ylevels),
                                 name = paste("samplePlot.propbar", j, sep = "."),
                                 vp = canvas$graphPath("sample", j)))
    }

    # Now we are creating the points which we light up if possible
    dataProp1 <- getGrob(canvas$image, gPath("dataPlot.propbar.1"))
    dataProp2 <- getGrob(canvas$image, gPath("dataPlot.propbar.2"))
    show.points <- dataProp1$points.present & dataProp2$points.present
    pointCols <- ifelse(levels == ylevels[1],
                        ifelse(x == canvas$loi,
                               getColour(1, 2, l = 85),
                               getColour(1, 2, l = 45)),
                        ifelse(x == canvas$loi,
                               getColour(2, 2, l = 85),
                               getColour(2, 2, l = 45)))
    if (show.points) {
        # Need to dive into each VP and grab the size of the units relative
        # animation.field VP. Because we're dealing with two data VPs, each
        # will be scaled by a half.
        depth <- downViewport(canvas$graphPath("data", 1))
        prop1.dpx <- convertX(dataProp1$x, "native", valueOnly = TRUE)
        prop1.dpy <- convertY(dataProp1$y, "native", valueOnly = TRUE) / length(ylevels)
        upViewport(depth)
        depth <- downViewport(canvas$graphPath("data", 2))
        prop2.dpx <- convertX(dataProp2$x, "native", valueOnly = TRUE)
        prop2.dpy <- convertY(dataProp2$y, "native", valueOnly = TRUE) / length(ylevels)
        upViewport(depth)

        # Need to translate the coordinates so that they fit in the
        # animation.field viewport
        prop1.dpy <- prop1.dpy + 2
        prop2.dpy <- prop2.dpy + 2.5
    }

    if (show.points) {
        # Calculating index vectors so that the i'th
        # value corresponds to the correct location within
        # each of the data propbars.
        first.level.ind <- 1
        second.level.ind <- 1
        data.index <- integer(length(data.levels))
        for (i in 1:length(data.levels)) {
            if (data.levels[i] == ylevels[1]) {
                data.index[i] <- first.level.ind
                first.level.ind <- first.level.ind + 1
            } else {
                data.index[i] <- second.level.ind
                second.level.ind <- second.level.ind + 1
            }
        }
    }

    # Animation of slow points.
    pxs <- numeric(n)
    pys <- numeric(n)
    if (show.points) {
        # Light up point to drop
        for (i in 1:n) {
            pxs[i] <- if (levels[i] == ylevels[1])
                          prop1.dpx[data.index[index[i]]]
                      else
                          prop2.dpx[data.index[index[i]]]
            pys[i] <- if (levels[i] == ylevels[1])
                          prop1.dpy[data.index[index[i]]]
                      else
                          prop2.dpy[data.index[index[i]]]
        }
    }

    # Plot the points that have been sampled.
    if (show.points) {
        temp.point2 <- pointsGrob(pxs,
                                  pys,
                                  pch = 19, gp = gpar(col = pointCols),
                                  vp = canvas$graphPath("animation.field"),
                                  name = "samplePlot.datapoints")
        canvas$image <- addGrob(canvas$image, temp.point2)
    }

    # If we're not doing ANOVA, show the difference
    if (length(ylevels) == 2) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(props, "native"),
                                 y = unit(0.75, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("sample", 1),
                                 name = "samplePlot.line.1"))
    }
}

dropSampvarPropDiffSample <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50) {
    rmvec <- 1:length(canvas$ylevels)
    canvas$rmGrobs(c(paste("samplePlot.blank", rmvec, sep = "."),
                     paste("samplePlot.propbar", rmvec, sep = "."),
                     "samplePlot.line.1", "samplePlot.datapoints"))

    n <- canvas$n
    index <- canvas$indexes[[canvas$which.sample]]
    sample <- canvas$x
    levels <- as.character(canvas$levels)
    resample <- sample[index]
    resample.levels <- levels[index]
    ylevels <- canvas$ylevels
    dataProp1 <- getGrob(canvas$image, gPath("dataPlot.propbar.1"))
    dataProp2 <- getGrob(canvas$image, gPath("dataPlot.propbar.2"))
    animate.points <- dataProp1$points.present & dataProp2$points.present
    pointCols <- ifelse(resample.levels == ylevels[1],
                        ifelse(resample == canvas$loi,
                               getColour(1, 2, l = 85),
                               getColour(1, 2, l = 45)),
                        ifelse(resample == canvas$loi,
                               getColour(2, 2, l = 85),
                               getColour(2, 2, l = 45)))

    # In the case that we're tracking, get rid of the sample plots
    if (drop.points & animate.points)
        canvas$rmGrobs(c("samplePlot.propbar.1", "samplePlot.propbar.2",
                         "samplePlot.ghosts.1", "samplePlot.ghosts.2",
                         "samplePlot.blank.1", "samplePlot.blank.2",
                         "samplePlot.line.1"))

    if (n < 100 & length(canvas$x) < 100) {
        if (animate.points) {
            # Need to dive into each VP and grab the size of the units relative
            # animation.field VP. Because we're dealing with two data VPs, each
            # will be scaled by a half.
            depth <- downViewport(canvas$graphPath("data", 1))
            prop1.dpx <- convertX(dataProp1$x, "native", valueOnly = TRUE)
            prop1.dpy <- convertY(dataProp1$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)
            depth <- downViewport(canvas$graphPath("data", 2))
            prop2.dpx <- convertX(dataProp2$x, "native", valueOnly = TRUE)
            prop2.dpy <- convertY(dataProp2$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)

            # Creating sample propbars and grabbing the point locations
            # from each of the propbars.
            sampleProp1 <- kpropbarGrob(resample[resample.levels == ylevels[1]],
                                        ylab = ylevels[1],
                                        draw.points = animate.points,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        y = 0.1, height = 0.5,
                                        name = paste("samplePlot.propbar", 1, sep="."),
                                        vp = canvas$graphPath("sample", 1))
            sampleProp2 <- kpropbarGrob(resample[resample.levels == ylevels[2]],
                                        ylab = ylevels[2],
                                        draw.points = animate.points,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        y = 0, height = 0.5,
                                        name = paste("samplePlot.propbar", 2, sep="."),
                                        vp = canvas$graphPath("sample", 2))
            depth <- downViewport(canvas$graphPath("sample", 1))
            prop1.spx <- convertX(sampleProp1$x, "native", valueOnly = TRUE)
            prop1.spy <- convertY(sampleProp1$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)
            depth <- downViewport(canvas$graphPath("sample", 2))
            prop2.spx <- convertX(sampleProp2$x, "native", valueOnly = TRUE)
            prop2.spy <- convertY(sampleProp2$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)

            # Need to translate the coordinates so that they fit in the
            # animation.field viewport
            prop1.dpy <- prop1.dpy + 2
            prop2.dpy <- prop2.dpy + 2.5
            prop1.spy <- prop1.spy + 1
            prop2.spy <- prop2.spy + 1.5
        }

        first.group <- levels == ylevels[1]
        max.width.x <- max(convertX(stringWidth(sample),
                                    "cm", valueOnly = TRUE))
        max.width.y <- max(convertX(stringWidth(ylevels),
                                    "cm", valueOnly = TRUE))
        max.width.x <- unit(max.width.x, "cm")
        max.width.y <- unit(max.width.y, "cm")

        if (drop.points & animate.points) {
            # Drawing labels to more easily identify which level each point belongs to.
            depth <- downViewport(canvas$graphPath("sample", 1))
            ylab.height <- convertHeight(max(stringHeight(ylevels)), "native", valueOnly = TRUE)
            upViewport(depth)
            for (i in 1:length(ylevels)) {
                canvas$image <- addGrob(canvas$image, textGrob
                                        (ylevels[i],
                                         x = unit(1, "npc"),
                                         y = unit(0.7, "native"),
                                         just = c("right", "bottom"),
                                         gp = gpar(col = getColour(i, length(ylevels))),
                                         name = paste("samplePlot.yLab", i, sep = "."),
                                         vp = canvas$graphPath("sample", i)))
            }
        }

        if (animate.points) {
            # Calculating index vectors so that the i'th
            # value corresponds to the correct location within
            # each of the data propbars.
            first.level.ind <- 1
            second.level.ind <- 1
            data.index <- integer(length(levels))
            for (i in 1:length(levels)) {
                if (levels[i] == ylevels[1]) {
                    data.index[i] <- first.level.ind
                    first.level.ind <- first.level.ind + 1
                } else {
                    data.index[i] <- second.level.ind
                    second.level.ind <- second.level.ind + 1
                }
            }

            first.level.ind <- 1
            second.level.ind <- 1
            sample.index <- integer(length(levels))
            for (i in 1:length(resample)) {
                if (resample.levels[i] == ylevels[1]) {
                    sample.index[i] <- first.level.ind
                    first.level.ind <- first.level.ind + 1
                } else {
                    sample.index[i] <- second.level.ind
                    second.level.ind <- second.level.ind + 1
                }
            }

            # Initialising vectors for points so that we can draw them more easily later
            points.xs <- numeric(n)
            points.ys <- numeric(n)
            xs.step <- numeric(n)
            ys.step <- numeric(n)
            points.cols <- character(n)
            # Because the position of the point is dependent on both the ylevel and the level
            # we need to work out the appropriate stepping from the data to the sample.
            for (i in 1:length(resample)) {
                if (resample.levels[i] == ylevels[1]) {
                    if (levels[index[i]] == ylevels[1]) {
                        xs.step[i] <- (prop1.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                        ys.step[i] <- (prop1.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                    } else {
                        xs.step[i] <- (prop2.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                        ys.step[i] <- (prop2.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                    }
                    points.xs[i] <- prop1.spx[sample.index[i]]
                    points.ys[i] <- prop1.spy[sample.index[i]]
                    points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
                } else {
                    if (levels[index[i]] == ylevels[1]) {
                        xs.step[i] <- (prop1.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                        ys.step[i] <- (prop1.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                    } else {
                        xs.step[i] <- (prop2.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                        ys.step[i] <- (prop2.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                    }
                    points.xs[i] <- prop2.spx[sample.index[i]]
                    points.ys[i] <- prop2.spy[sample.index[i]]
                    points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
                }
            }
        }



        n.slow <- min(n.slow, n)
        if (drop.points & animate.points) {
            for (j in 1:length(ylevels)) {
                canvas$image <- addGrob(canvas$image, rectGrob
                                        (x = unit(0, "native"), y = unit(if (j == 1) 0.1 else 0, "native"),
                                         width = unit(1, "native"), height = unit(0.5, "native"),
                                         just = c("left", "bottom"),
                                         gp = gpar(col = getColour(j, length(ylevels)),
                                                   fill = "transparent", lwd = 3),
                                         name = paste("samplePlot.boundRect", j, sep = "."),
                                         vp = canvas$graphPath("sample", j)))
                canvas$image <- addGrob(canvas$image, rectGrob
                                        (x = unit(0, "native"), y = unit(0.1, "native"),
                                         width = unit(1, "native"), height = unit(0.6, "native"),
                                         just = c("left", "bottom"),
                                         gp = gpar(col = getColour(j, length(ylevels)),
                                                   fill = "transparent", lwd = 3),
                                         name = paste("samplePlot.databoundRect", j, sep = "."),
                                         vp = canvas$graphPath("data", j)))
            }
        }
        # Ensure labels are drawn on top of new shell rects
        canvas$showLabels()

        # Animation of slow points.
        pxs <- numeric(n)
        pys <- numeric(n)
        for (i in seq(from = 1, by = 1, length.out = n.slow)) {
            # Light up point to drop
            if (drop.points & animate.points) {
                pxs[i] <- if (resample.levels[i] == ylevels[1])
                              prop1.dpx[data.index[index[i]]]
                          else
                              prop2.dpx[data.index[index[i]]]
                pys[i] <- if (resample.levels[i] == ylevels[1])
                              prop1.dpy[data.index[index[i]]]
                          else
                              prop2.dpy[data.index[index[i]]]
                temp.point <- pointsGrob(x = pxs[1:i],
                                         y = pys[1:i],
                                         pch = 19, gp = gpar(col = pointCols[1:i]),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
        }

        # Animation of fast points.
        length.out <- max(0, n - n.slow)
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)) {
            # Light up point to drop
            if (drop.points & animate.points) {
                pxs[i] <- if (resample.levels[i] == ylevels[1])
                              prop1.dpx[data.index[index[i]]]
                          else
                              prop2.dpx[data.index[index[i]]]
                pys[i] <- if (resample.levels[i] == ylevels[1])
                              prop1.dpy[data.index[index[i]]]
                          else
                              prop2.dpy[data.index[index[i]]]
                temp.point <- pointsGrob(pxs[1:i],
                                         pys[1:i],
                                         pch = 19, gp = gpar(col = pointCols[1:i]),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }

        # Plot the points that have been sampled.
        if (drop.points & animate.points) {
            for (j in 1:n.steps) {
                temp.point2 <- pointsGrob(pxs - j*xs.step,
                                          pys - j*ys.step,
                                          pch = 19, gp = gpar(col = pointCols),
                                          vp = canvas$graphPath("animation.field"),
                                          name = "samplePlot.temp.point2")
                canvas$image <- addGrob(canvas$image, temp.point2)
                canvas$drawImage()
            }
        }

        canvas$pauseImage(10)
    }

    # Removing animation grobs before plotting the sample
    # Because we're plotting within the animation.field VP, there is no vpNumber()
    # associated with our samplePlot.points, thus, we omit the numeric index
    canvas$rmGrobs(c("samplePlot.temp.point", "samplePlot.temp.point2", "samplePlot.points.",
                     paste("samplePlot.yLab", 1:length(ylevels), sep="."),
                     paste("samplePlot.boundRect", 1:length(ylevels), sep="."),
                     paste("samplePlot.databoundRect", 1:length(ylevels), sep=".")))

    # Now that animation grobs have been removed, draw the sample
    canvas$plotSample(e, canvas$which.sample)
    canvas$showLabels()
    canvas$pauseImage(5)
}

plotSampvarTwoSampPropTheoDist <- function(canvas, e) {
    ## Replotting statistic distribution
    x <- apply(canvas$stat.dist, 1, diff)
    y <- canvas$stat.ypos
    plotPoints(canvas, x, y, canvas$graphPath("stat"),
               "statPlot", black = FALSE, alpha = 0.7)
    stat.mean <- mean(x)
    stat.sd <- sd(x)

    pop1 <- canvas$x[canvas$levels == canvas$ylevels[1]]
    pop2 <- canvas$x[canvas$levels == canvas$ylevels[2]]
    n1 <- length(pop1)
    n2 <- length(pop2)
    N <- n1 + n2
    p1 <- mean(pop1 == canvas$loi)
    p2 <- mean(pop2 == canvas$loi)
    mean <- p1 - p2
    var1 <- p1*(1 - p1)/(canvas$n*n1/N)
    var2 <- p2*(1 - p2)/(canvas$n*n2/N)
    sd <- sqrt(var1 + var2)
    ## Getting statistic panel x-scale.
    top.level <- downViewport(canvas$graphPath("stat"))
    stat.scale <- current.viewport()$xscale
    upViewport(top.level)
    ## Calculating normal density under the CLT.
    xs <- seq(stat.scale[1], stat.scale[2], length.out = 300)
    ys <- dnorm(xs, mean, sd)
    ## We need a sense of "density scale" for the y-axis. Fitting a
    ## kernel density estimator can provide this. We calculate the
    ## KDE, find the maximum point, map this to meet up with the top
    ## of the topmost point in the statistic panel, and scale the
    ## normal density curve accordingly. This ensures that the normal
    ## density curve has about the same area below it as the area
    ## taken up by the points; the normal density will have the same
    ## area as the KDE, which, in turn, will have a similar area to
    ## the points.
    dens <- density(x, from = stat.scale[1], to = stat.scale[2])
    ys <- ys/max(dens$y)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(xs, "native"), y = (y.max*ys) + ys*unit(0.5, "char"),
                             gp = gpar(lwd = 2, col = "red"),
                             name = "statPlot.theodist.1",
                             vp = canvas$graphPath("stat")))
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1",
                     "samplePlot.datapoints.points.1", "samplePlot.databox.text.2",
                     "samplePlot.stat.1", "samplePlot.hist.1"))
}

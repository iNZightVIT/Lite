canvasPermvarMeanDiff <- setRefClass("canvasPermvarMeanDiffClass", contains = "canvasPlotClass",
                                     methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "difference") {
            if (all(ylevels == sort(unique(ys)))) {
                calcDiff(samples[[i]], ys, fun = mean)
            } else {
                rev(calcDiff(samples[[i]], ys, fun = mean))
            }
        } else if (stat.method == "t-pooled") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcT(samples[[i]], ys, fun = mean)
            } else {
                calcT(samples[[i]], ys, fun = mean)
            }
        } else if (stat.method == "t-Welch") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcTWelch(samples[[i]], ys, fun = mean)
            } else {
                calcTWelch(samples[[i]], ys, fun = mean)
            }
        }
    },

    calcAllStats = function(xs, ys = NULL, fun = mean) {
        if (stat.method == "difference") {
            if (all(ylevels == sort(unique(ys)))) {
                calcDiff(xs, ys, fun = mean)
            } else {
                rev(calcDiff(xs, ys, fun = mean))
            }
        } else if (stat.method == "t-pooled") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcT(xs, ys, fun = mean)
            } else {
                calcT(xs, ys, fun = mean)
            }
        } else if (stat.method == "t-Welch") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcTWelch(xs, ys, fun = mean)
            } else {
                calcTWelch(xs, ys, fun = mean)
            }
        }
    },

    animateStat = function(env, n.steps) {
        if (stat.method == "difference") {
            dropPermArrow(.self, env, n.steps)
        } else if (stat.method %in% c("t-pooled", "t-Welch")) {
            animateTStat(.self, env, n.steps)
        }
    },

    displayResult = function(env, ...) {
        if (stat.method == "difference") {
            showTailPerm2Sample(.self, env, fun = mean, ...)
        } else if (stat.method %in% c("t-pooled", "t-Welch")) {
            showTailTStat(.self, env, ...)
        }
    },

    ## plotKSample comes from methods-permutation-mean-ksample.R
    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, fun = mean)
    },

    showLabels = function() {
        permvarLabels(.self)
    },

    plotDataStat = function(env, ...) {

    },

    plotStatDist = function(env, ...) {
        plotDiffDist(.self, env)
    },

    animateSample = function(...) {
        permvarTwoSample(.self, ...)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permvar_mean_diff <- function(e) {
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
    tmp.canvas <- canvasPermvarMeanDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

permvarTwoSample <- function(canvas, e, n.steps, mix = TRUE){
    canvas$rmGrobs("samplePlot.databox.text.2")
    e$clearPanel("sample")
    ## Drop samples down to middle plot.
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    y <- old.stackPoints(canvas$x, vp = canvas$graphPath("data"), y.min = 0.3, y.max = 1) + 2
    y.start <- y
    y.end <- ((y - 2.3) * (2 / 7)) + 1.4
    y.end <- old.stackPoints(canvas$x, y.min = 1.4, y.max = 1.5,
                             vp = canvas$graphPath("animation.field"))
    if (! mix)
        n.steps <- 3
    y.step <- (y.start - y.end)/n.steps
    ## Dropping samples
    for (i in 1:n.steps) {
        plotPoints(canvas, x, y.start - i*y.step,
                   canvas$graphPath("animation.field"),
                   "samplePlot.temp", col = "black")
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(10)
    canvas$rmGrobs("samplePlot.temp.points.")
    ## Separating samples
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.start <- old.stackPoints(x, vp = canvas$graphPath("sample", 1),
                               y.min = 0.8, y.max = 1)
    y.start <- y.start[canvas$indexes[[canvas$which.sample]]]
    y.start.1 <- y.start[levels == ylevels[1]]
    y.start.2 <- y.start[levels == ylevels[2]]
    text <-  as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[sort(ylevels)]
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text, title = "Random groups",
                             cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    plotPoints(canvas, x.sample[levels == ylevels[1]],
               y.start.1, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.join1", col = getColour(1, 2))
    plotPoints(canvas, x.sample[levels == ylevels[2]],
               y.start.2, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.join2", col = getColour(2, 2))
    y.sample <- old.stackPoints(x.sample, levels, vp = canvas$graphPath("sample"))
    y.end.1 <- y.sample[levels == ylevels[1]]
    y.end.2 <- y.sample[levels == ylevels[2]] + 1
    y.step.1 <- (y.start.1 - y.end.1)/n.steps
    y.step.2 <- (y.start.2 - y.end.2)/n.steps
    plotPoints(canvas, x.sample[levels == ylevels[1]],
               y.start.1,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin1",
               col = getColour(1, 2))
    plotPoints(canvas, x.sample[levels == ylevels[2]],
               y.start.2,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin2",
               col = getColour(2, 2))
    canvas$pauseImage(5)
    for (i in 1:n.steps) {
        plotPoints(canvas, x.sample[levels == ylevels[1]],
                   y.start.1 - i*y.step.1,
                   vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin1",
                   col = getColour(1, 2))
        plotPoints(canvas, x.sample[levels == ylevels[2]],
                   y.start.2 - i*y.step.2,
                   vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin2",
                   col = getColour(2, 2))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs(paste(c("samplePlot.tempjoin1", "samplePlot.tempjoin2", "samplePlot.join1", "samplePlot.join2"),
                           "points.1", sep = "."))
}

permvarLabels <- function(canvas) {
    samplabel <- textGrob("Data",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(3, "native") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("animation.field"),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Randomisation variation",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
                          vp = canvas$graphPath("canvas.header"))
    if (is.categorical(canvas$x)) {
        vlabels <- c("Variable: ", canvas$x.name, " (",
                     canvas$loi, " | ",
                     canvas$loi.alt, ")")
        vlabelXs <- unit(0, "npc")
        for (i in 1:(length(vlabels) - 1))
            vlabelXs <- unit.c(vlabelXs, vlabelXs[i] + stringWidth(vlabels[i]))
        varlabel <- textGrob(vlabels,
                             x = vlabelXs + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             name = "varLabel",
                             gp = gpar(col = c(rep("black", 3), "blue", "black", "red", "black"),
                                       fontsize = 10, fontface = "italic"),
                             vp = canvas$graphPath("canvas.header"))
    } else {
        varlabel <- textGrob(paste("Variable:", canvas$x.name),
                             x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             gp = gpar(fontsize = 10, fontface = "italic"),
                             name = "varLabel",
                             vp = canvas$graphPath("canvas.header"))
    }
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    if (! is.null(canvas$stat.method)) {
        quantmethlabel <- textGrob(paste("Statistic:", canvas$stat.method),
                                   x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                                   just = "left",
                                   name = "quantmethLabel",
                                   gp = gpar(fontsize = 10, fontface = "italic"),
                                   vp = canvas$graphPath("canvas.header"))
        filelabel <- textGrob(paste("File:", canvas$data.file),
                              x = quantmethlabel$x + stringWidth(quantmethlabel$label) + unit(6, "mm"),
                              just = "left",
                              name = "fileLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    } else {
        filelabel <- textGrob(paste("File:", canvas$data.file),
                              x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                              just = "left",
                              name = "fileLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    }
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    dataLabelBackground <- rectGrob(x = unit(0, "npc"),
                                    y = unit(3, "native") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(2, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("animation.field"))
    rerandomlabel <- textGrob("Randomly grouped data",
                              x = unit(0, "npc") + unit(1, "mm"),
                              y = unit(1.9, "native"),
                              just = c("left", "top"),
                              name = "sampleLabel",
                              vp = canvas$graphPath("animation.field"),
                              gp = gpar(fontface = 2))
    rerandomLabelBackground <- rectGrob(x = unit(0, "npc"),
                                        y = unit(1.9, "native") + unit(0.5, "lines") - unit(1, "mm"),
                                        width = stringWidth(rerandomlabel$label) + unit(4, "mm"),
                                        height = stringHeight(rerandomlabel$label) + unit(2, "mm"),
                                        just = c("left", "top"),
                                        gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                        name = "rerandomLabelBackground",
                                        vp = canvas$graphPath("animation.field"))
    statlabel <- textGrob("Randomisation distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          name = "statLabel",
                          gp = gpar(fontface = 2))
    if (! is.null(canvas$stat.method)) {
        permvarlabels <- grobTree(methlabel, varlabel, quantitylabel, quantmethlabel, filelabel,
                                  infosep,
                                  dataLabelBackground,
                                  samplabel,
                                  rerandomLabelBackground,
                                  rerandomlabel,
                                  statlabel,
                                  name = "permlabels")
    } else {
        permvarlabels <- grobTree(methlabel, varlabel, quantitylabel, filelabel,
                                  infosep,
                                  dataLabelBackground,
                                  samplabel,
                                  rerandomLabelBackground,
                                  rerandomlabel,
                                  statlabel,
                                  name = "permlabels")
    }
    canvas$image <- addGrob(canvas$image, permvarlabels)
}

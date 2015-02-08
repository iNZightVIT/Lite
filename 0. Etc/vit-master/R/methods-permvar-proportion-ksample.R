canvasPermvarPropKSample <- setRefClass("canvasPermvarPropKSampleClass", contains = "canvasPlotClass",
                                        methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "average deviation") {
            calcAveDevProp(samples[[i]], ys, canvas)
        } else if (stat.method == "chi-square statistic") {
            calcChiSq(samples[[i]], ys, canvas)
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (stat.method == "average deviation") {
           calcAveDevProp(xs, ys, canvas)
        } else if (stat.method == "chi-square statistic") {
            calcChiSq(xs, ys, canvas)
        }
    },

    displayResult = function(env, ...) {
        if (stat.method == "average deviation") {
            showTailKSample(.self, env, fstat = FALSE, ...)
        } else if (stat.method == "chi-square statistic") {
            showTailKSample(.self, env, fstat = TRUE, ...)
        }
    },

    plotSample = function(env, i = which.sample) {
        plotPermKProportions(.self, env, i)
    },

    showLabels = function() {
        permvarLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addPermvarPropLinesKSamp(canvas = .self, e = env)
    },

    plotStatDist = function(env, ...) {
        plotPermStat(.self, env)
    },

    animateSample = function(...) {
        animatePermvarKSampleProp(.self, ...)
    },

    animateStat = function(env, n.steps) {
        animatePropStat(.self, env, n.steps, env$perm.method)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permvar_proportion_ksample <- function(e) {
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
    tmp.canvas <- canvasPermvarPropKSample$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

animatePermvarKSampleProp <- function(canvas, e, n.steps, mix = TRUE) {
    e$clearPanel("sample")
    ## Drop samples down to middle plot
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    depth <- downViewport("animation.field")
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "npc", valueOnly = TRUE)
    upViewport(depth)
    ngroups <- length(ylevels)
    y.start <- 2
    y.end <- 1 + (1 - 0.6 / ngroups) / 2
    y.step <- (y.start - y.end) / n.steps
    height.start <- 0.45
    height.end <- 0.6 / ngroups
    height.step <- (height.start - height.end) / n.steps
    line.height.start <- 0.1
    line.height.end <- 0.1 / ngroups
    line.step <- (line.height.start - line.height.end) / n.steps
    # Removing any existing grobs in the sample area
    canvas$rmGrobs(c("samplePlot.tempTotalPropbar",
                     paste("samplePlot.tempPropbar", 1:ngroups, sep = "."),
                     paste("samplePlot.propbar", 1:ngroups, sep = ".")))

    if (mix) {
        # Dropping samples
        for (i in 0:n.steps) {
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                                    (x,
                                     ylab = "",
                                     ylab.pos = 1 - max.str.width,
                                     statLineHeight = line.height.start - i*line.step,
                                     height = height.start - i*height.step,
                                     draw.points = FALSE,
                                     lois = c(canvas$loi, canvas$loi.alt),
                                     y = 2 - (i * y.step),
                                     vpcex = 10*(line.height.start - i*line.step),
                                     name = paste("samplePlot.tempTotalPropbar"),
                                     vp = canvas$graphPath("animation.field")))
            canvas$showLabels()
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
    }
    canvas$rmGrobs("samplePlot.tempTotalPropbar")

    # Drawing the total propbar which we will later break apart.
    x.sample <- canvas$samples[[canvas$which.sample]]
    if (ngroups > 2) {
        y.end <- seq(1, 1 + (ngroups - 1) / ngroups, length.out = ngroups)
        y.end <- y.end + (0.1 / ngroups)
        y.start <- 1 + (1 - 0.6 / ngroups) / 2
        y.step <- (y.start - y.end) / n.steps
    } else {
        y.start <- 1 + (1 - 0.6 / ngroups) / 2
        y.end <- c(1, 1.5)
        y.step <- (y.start - y.end) / n.steps
    }

    # Drawing the datatext grob, it should get overwritten anyway when
    # we call plotSample()
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[order(ylevels)]
    text <- as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text, title = "Random groups", cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2))) 

    canvas$image <- addGrob(canvas$image, kpropbarGrob
                    (x,
                     ylab = "",
                     ylab.pos = 1 - max.str.width,
                     draw.points = FALSE,
                     lois = c(canvas$loi, canvas$loi.alt),
                     y = y.start,
                     statLineHeight = 0.1 / ngroups,
                     height = 0.6 / ngroups, vpcex = 1 / ngroups,
                     name = "samplePlot.tempTotalPropbar",
                     vp = canvas$graphPath("animation.field")))

    # Splitting apart randomised samples
    for (i in 0:n.steps) {
        for (j in 1:ngroups) {
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                            (x.sample[levels == ylevels[j]],
                             ylab = ylevels[j],
                             ylab.pos = 1 - max.str.width,
                             ylab.col = getColour(j, ngroups),
                             draw.points = FALSE,
                             lois = c(canvas$loi, canvas$loi.alt),
                             y = y.start - i*y.step[j],
                             statLineHeight = 0.1 / ngroups,
                             height = 0.6 / ngroups, vpcex = 1 / ngroups,
                             name = paste("samplePlot.tempPropbar", j, sep="."),
                             gp = gpar(alpha = i / n.steps),
                             vp = canvas$graphPath("animation.field")))
        }
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                        (x,
                         ylab = "",
                         draw.points = FALSE,
                         lois = c(canvas$loi, canvas$loi.alt),
                         y = y.start,
                         statLineHeight = 0.1 / ngroups,
                         height = 0.6 / ngroups, vpcex = 1 / ngroups,
                         name = "samplePlot.tempTotalPropbar",
                         gp = gpar(alpha = 1 - (i / n.steps)),
                         vp = canvas$graphPath("animation.field")))
        canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs(c("samplePlot.tempTotalPropbar", paste("samplePlot.tempPropbar", 1:ngroups, sep = ".")))
}

addPermvarPropLinesKSamp <- function(canvas, e) {
    xs <- canvas$x
    totalprop <- length(xs[xs == canvas$loi]) / length(xs)
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(totalprop, 2), "native"),
                             y = unit(c(1, 3), "native"),
                             gp = gpar(lty = "dashed"),
                             name = "statLine",
                             vp = canvas$graphPath("animation.field")))
}

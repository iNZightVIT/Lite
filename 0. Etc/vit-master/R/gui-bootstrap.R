bootstrapGUIHandler <- function(e){
    e$method <- "bootstrap"

    # Rewriting the menus to link only to other modules
    svalue(e$g.menu) <- getModuleMenu(e)

    e$data.boxes <- TRUE
    e$replace <- TRUE
    e$stat.scale <- TRUE
    tbl <- glayout(container = e$upper)
    tbl[1, 1] <- glabel("Quantity: ", container = tbl)
    tbl[1, 2] <- (e$stat <- gcombobox(c("mean", "median"),
                                      editable = TRUE, container = tbl))
    tbl[2, 1] <- (e$loi.label <- glabel("Level of interest: ", container = tbl))
    tbl[2, 2] <- (e$loi.choices <- gcombobox("", editable = TRUE, container = tbl,
                                             handler = function(h, ...)
                                         {
                                             enabled(e$obj) <- FALSE
                                             if (!e$loaded){
                                                 e$resetCanvas()
                                                 loadStatDetails(e)
                                                 e$c1$drawImage()
                                                 e$loaded <- FALSE
                                             }
                                             enabled(e$obj) <- TRUE
                                         }))
    tbl[3, 2] <- additionalOptions <- gexpandgroup("Additional options", horizontal = FALSE,
                                                   container = tbl)
    e$resamp.within <- gcheckbox("Resample within groups", container = tbl)
    add(additionalOptions, e$resamp.within)
    e$paired.samples <- gcheckbox("Paired samples", container = tbl,
                                  handler = function(h, ...){
                                      if (enabled(e$paired.samples)) {
                                          enabled(e$obj) <- FALSE
                                          if (svalue(e$paired.samples)){
                                              e$stat[] <- c("mean", "median", "lower quartile",
                                                            "upper quartile")
                                              svalue(e$stat) <- "mean"
                                              e$pairedSamples <- TRUE
                                          } else {
                                              e$stat[] <- "slope"
                                              svalue(e$stat) <- "slope"
                                              e$pairedSamples <- FALSE
                                          }
                                          if (!e$loaded) {
                                              e$buildCanvas(svalue(e$paired.samples))
                                              e$c1$buildBoxes(svalue(e$paired.samples))
                                              e$c1$drawImage()
                                          }
                                          enabled(e$obj) <- TRUE
                                      } else {
                                          e$pairedSamples <- FALSE
                                      }
                                  })
    add(additionalOptions, e$paired.samples)
    enabled(e$paired.samples) <- FALSE
    e$pairedSamples <- FALSE
    if (is.categorical(e$c1$x) & ! is.null(e$c1$levels))
        e$loi.choices[] <- sort(unique(e$c1$x))
    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h, ...) {
                # Resetting the GUI elements
                enabled(e$obj) <- FALSE
                enabled(e$show.ci) <- FALSE
                enabled(e$show.summary) <- FALSE
                enabled(e$fade.plots) <- FALSE
                if (! enabled(e$paired.samples)) {
                    e$pairedSamples <- FALSE
                    svalue(e$paired.samples) <- FALSE
                }
                e$buildCanvas(enabled(e$paired.samples) & svalue(e$paired.samples))
                e$c1$data.file <- tag(e$obj, "data.file") # Grabbing data filename for labels
                e$c1$buildBoxes(enabled(e$paired.samples) & svalue(e$paired.samples))
                e$c1$drawImage()
                loadStatDetails(e)
                if (e$loaded){
                    e$c1$makeSamples(e$replace, sample.both = e$difference,
                                     resamp.within = enabled(e$resamp.within) & svalue(e$resamp.within),
                                     perm.paired.samples = FALSE)
                    if (svalue(e$stat) %in% c("median", "upper quartile", "lower quartile",
                                              "IQR ratio")){
                        e$c1$makeQuartiles()
                        if (!is.null(e$c1$levels))
                            e$c1$orderLevels()
                    }
                    e$c1$makeStatistics()
                    if (e$difference & ! is.categorical(e$xData)) {
                        y.scale <- NULL
                        stat.y.scale <- NULL
                        extra.box <- FALSE
                        if (svalue(e$stat) == "IQR ratio") {
                            # Level 2 is, on average, larger than level 1
                            # therefore create stats relative to that.
                            # Use a log10 scale to plot stats.
                            valid.xs <- apply(e$c1$stat.dist, 1, function (x) all(x != 0))
                            stats <- log10(e$c1$stat.dist[valid.xs, 2] / e$c1$stat.dist[valid.xs, 1])
                            stat.scale <- c(min(stats), max(stats))

                            # We want nice labels for the scale of our
                            # stat viewport.  As a result, we need to
                            # rescale the viewport to ensure that our
                            # nice labels can be created, whilst also
                            # ensuring that the distribution spans as
                            # much of the stat viewport as possible.
                            unlogged.range <- 10^stat.scale
                            log2.range <- log(unlogged.range, base = 2)
                            twos.ranges <- c(floor(log2.range[1]), ceiling(log2.range[2]))
                            logged.scale <- twos.ranges[1]:twos.ranges[2]
                            nice.scale <- 2^logged.scale
                            stat.scale <- range(log10(nice.scale))
                        } else if (svalue(e$stat) == "slope"){
                            ## For scatterplots:
                            ## Calculating x.scale.
                            if (diff(range(e$c1$x)) == 0){
                                x.scale <- range(e$c1$x) + c(-1, 1)
                            } else {
                                x.scale <- range(e$c1$x) + c(-0.04, 0.04)*diff(range(e$c1$x))
                            }
                            ## y.scale as defined by data.
                            if (diff(range(e$c1$levels)) == 0){
                                y.scale <- range(e$c1$levels) + c(-1, 1)
                            } else {
                                y.scale <- range(e$c1$levels) +
                                    c(-0.04, 0.04)*diff(range(e$c1$levels))
                            }
                            e$c1$calcxScaleYs(e, x.scale)
                            coeffs <- coefficients(lm(e$c1$levels ~ e$c1$x))
                            current.stats <- coeffs[1] + coeffs[2]*x.scale
                            all.ys <- rbind(e$c1$x.scale.ys, current.stats)
                            x.scale.y.diffs <- apply(all.ys, 1, diff)
                            y.diff.1 <- diff(range(c(y.scale, all.ys)))
                            y.diff.2 <- diff(c(min(c(0, x.scale.y.diffs)),
                                             max(c(0, x.scale.y.diffs))))
                            y.diff <- max(c(y.diff.1, y.diff.2))
                            if (y.diff.2 < y.diff.1){
                                y.scale <- range(c(y.scale, all.ys))
                                extra.on.stat <- diff(c(y.diff.2, y.diff))/2
                                stat.y.scale <- (c(min(c(0, x.scale.y.diffs)),
                                             max(c(0, x.scale.y.diffs))) +
                                                 c(-1, 1)*extra.on.stat)/diff(x.scale)
                            } else if (y.diff.2 >= y.diff.1){
                                extra.on.data <- diff(c(y.diff.1, y.diff))/2
                                y.scale <- range(c(y.scale, all.ys)) + c(-1, 1)*extra.on.data
                                stat.y.scale <- c(min(c(0, x.scale.y.diffs)),
                                             max(c(0, x.scale.y.diffs)))/diff(x.scale)
                            }
                            stats <- e$c1$stat.dist[, 2]
                            stat.scale <- c(0, 1)
                            extra.box <- TRUE
                        } else {
                            stats <- apply(e$c1$stat.dist, 1, diff)
                            meanstat <- mean(stats)
                            stat.scale <- meanstat + c(-0.5, 0.5)*diff(range(e$xData))
                        }
                        if (svalue(e$stat) != "slope"){
                            x.scale <- NULL
                        }
                        if (any(stats < stat.scale[1] | stats > stat.scale[2]) &
                            svalue(e$stat) != "IQR ratio" & svalue(e$stat) != "slope") {
                            stat.scale <- range(stats)
                            scale.diff <- diff(stat.scale) - diff(range(e$xData))
                            if (scale.diff > 0){
                                x.scale <- range(e$xData) + c(-0.5, 0.5)*scale.diff
                            } else {
                                stat.scale <- stat.scale + c(-0.5, 0.5)*abs(scale.diff)
                            }
                        }
                        buildViewports(e$c1, e$xData, e$yData, boxes =  e$data.boxes,
                                       x.scale = x.scale, y.scale = y.scale,
                                       stat.scale = stat.scale, stat.y.scale = stat.y.scale,
                                       extra.box = extra.box)
                    } else if (e$difference) {
                        stats <- apply(e$c1$stat.dist, 1, diff)
                        meanstat <- mean(stats)
                        stat.scale <- c(-0.5, 0.5)
                        x.scale <- NULL
                        if (any(stats < stat.scale[1] | stats > stat.scale[2])){
                            stat.scale <- range(stats)
                            scale.diff <- (diff(stat.scale) - 1) / 2
                            if (scale.diff > 0) {
                                x.scale <- c(0-scale.diff, 1+scale.diff)#* scale.diff
                            } else {
                                stat.scale <- stat.scale + c(-0.5, 0.5)*abs(scale.diff)
                            }
                        }
                        buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                       x.scale = x.scale, stat.scale = stat.scale)
                    } else {
                        buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                       x.scale = NULL, stat.scale = NULL)
                    }
                    e$c1$buildImage(!is.null(e$c1$levels) & !is.categorical(e$c1$levels))
                    if (is.categorical(e$xData)) {
                        e$c1$image <- editGrob(e$c1$image, gPath("dataAxis"), at = seq(0, 1, by = 0.2))
                        e$c1$image <- editGrob(e$c1$image, gPath("sampleAxis"), at = seq(0, 1, by = 0.2))
                    }else  if (e$c1$stat.in.use == "slope"){
                        e$c1$image <- editGrob(e$c1$image, gPath("statAxis"),
                                               at = c(0, 1))
                    }
                    if (svalue(e$stat) == "IQR ratio") {
                        nice.labels <- sapply(nice.scale, function(x) {
                            if (x < 1) {
                                paste("1 :", round(1 / x, 2))
                            } else {
                                paste(x, ": 1")
                            }
                        })
                        e$c1$image <- editGrob(e$c1$image, gPath("statAxis"),
                                               at = log10(nice.scale), label = nice.labels)
                    }
                    e$c1$buildBoxes(enabled(e$paired.samples) & svalue(e$paired.samples))
                    pushViewport(e$c1$viewports)
                    e$c1$plotData()
                    e$c1$showLabels()
                    e$c1$plotDataStat(e)
                    e$c1$drawImage()
                    e$c1$getStatYPos()
                    enabled(e$lower) <- TRUE
                }
                enabled(e$obj) <- TRUE
            })
    e$vit.resamp <- glabel("Re-sampling", container = e$lower)
    e$boot.frame.cont <- ggroup(container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$boot.frame.cont)
    e$redraw.radio <- gradio(c(1, 5, 20, 1000), horizontal=FALSE,
                             handler = function(h, ...){
                                 if (svalue(e$redraw.radio) == 1){
                                     enabled(e$animate.points) <- TRUE
                                     } else {
                                         svalue(e$animate.points) <- FALSE
                                         enabled(e$animate.points) <- FALSE
                                         }
                                 })
    add(vit.bootbox, e$redraw.radio)
    add(e$boot.frame.cont, vit.bootbox)
    e$animate.points <- gcheckbox("Animate points and track sample", container = e$lower)
    buttons1 <- ggroup(container = e$lower)
    e$clear.stat <- FALSE
    e$points <- FALSE
    e$get.sample <- gbutton("Go", container = e$lower, expand = TRUE,
                            handler = function(h, ...){
                                if (svalue(e$get.sample) == "Go") {
                                    e$c1$animationPaused <- FALSE
                                    # Resetting the widgets to whatever settings are currently in use
                                    svalue(e$stat) <- e$c1$stat.in.use
                                    svalue(e$resamp.within) <- e$resampWithin
                                    svalue(e$paired.samples) <- e$pairedSamples
                                    if (enabled(e$paired.samples)){
                                        if (svalue(e$paired.samples)){
                                            e$stat[] <- c("mean", "median", "lower quartile",
                                                          "upper quartile")
                                            svalue(e$stat) <- "mean"
                                        } else {
                                            e$stat[] <- "slope"
                                            svalue(e$stat) <- "slope"
                                        }
                                    }
                                    if (is.categorical(e$c1$x)){
                                        svalue(e$loi.choices) <- e$c1$loi
                                    }
                                    enabled(e$stop.button) <- TRUE
                                    enabled(e$upper) <- FALSE
                                    enabled(e$vit.resamp) <- FALSE
                                    enabled(e$boot.frame.cont) <- FALSE
                                    enabled(e$animate.points) <- FALSE
                                    enabled(e$stat.label) <- FALSE
                                    enabled(e$diff.box.cont) <- FALSE
                                    enabled(e$get.dist) <- FALSE
                                    enabled(e$lowest) <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    n <- svalue(e$redraw.radio)
                                    if (n == 1) {
                                        e$c1$reset()
                                        e$c1$resetPlots(e)
                                        e$c1$drawImage()
                                    }
                                    if (n == 1000) {
                                        e$clearPanel(panel = "stat")
                                        e$clearPanel(panel = "sample")
                                        e$c1$handle1000(e, points = FALSE)
                                        if (! e$c1$stopAnimation) {
                                            enabled(e$lowest) <- !e$difference
                                            enabled(e$show.ci) <- !e$difference
                                            enabled(e$show.summary) <- !e$difference & svalue(e$stat) != "IQR ratio"
                                            enabled(e$fade.plots) <- e$difference
                                            e$clear.stat <- TRUE
                                            e$clear.sample <- TRUE
                                            e$summary.shown <- FALSE
                                            e$points <- FALSE
                                        }
                                    } else {
                                        if (e$clear.stat){
                                            e$clearPanel("stat")
                                            e$clearPanel("sample")
                                            e$clear.stat <- FALSE
                                        }
                                        for (i in 1:n){
                                            # When we have stopped, don't bother animating further
                                            if (! e$c1$stopAnimation) {
                                                if (n == 1){
                                                    e$c1$animateSample(e, drop.points = svalue(e$animate.points),
                                                                       n.steps = 10, n.slow = 10)
                                                    if (svalue(e$animate.points) & ! e$c1$stopAnimation)
                                                        e$c1$trackSample(e)
                                                } else if (n == 5) {
                                                    e$slowIteration <- FALSE
                                                    e$c1$animateSample(e, drop.points = FALSE,
                                                                       n.steps = 10, n.slow = 0)
                                                } else if (n == 20 & e$c1$stat.in.use == "slope"){
                                                    e$c1$animateSample(e, drop.points = FALSE,
                                                                       n.steps = 3, n.slow = 0,
                                                                       slope.only = TRUE)
                                                }
                                                # If we have stopped during our animation don't do anything more
                                                if (! e$c1$stopAnimation) {
                                                    e$c1$plotSample(e)
                                                    if (i == n & i > 1)
                                                        e$c1$rmGrobs("samplePlot.boxplot.1") # Removing boxplot at end of 5/20 animations
                                                    e$c1$showLabels()
                                                    e$c1$drawImage()
                                                    e$c1$advanceWhichSample()
                                                }
                                            }
                                        }
                                        enabled(e$lowest) <- FALSE
                                    }

                                    # If we have stopped at some point, ensure we clean up
                                    # to avoid an inconsistent state
                                    if (e$c1$stopAnimation) {
                                        e$c1$reset()
                                        e$c1$resetPlots(e)
                                        e$c1$stopAnimation <- FALSE
                                        e$c1$drawImage()
                                    }

                                    enabled(e$stop.button) <- FALSE
                                    enabled(e$upper) <- TRUE
                                    enabled(e$vit.resamp) <- TRUE
                                    enabled(e$boot.frame.cont) <- TRUE
                                    enabled(e$animate.points) <- svalue(e$redraw.radio) == 1
                                    enabled(e$stat.label) <- TRUE
                                    enabled(e$diff.box.cont) <- TRUE
                                    enabled(e$get.dist) <- TRUE
                                    svalue(e$get.sample) <- "Go"
                                    return()
                                }

                                if (svalue(e$get.sample) == "Pause") {
                                    e$c1$animationPaused <- TRUE
                                    svalue(e$get.sample) <- "Play"
                                    return()
                                }

                                if (svalue(e$get.sample) == "Play") {
                                    e$c1$animationPaused <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    return()
                                }
                            })

    addSpace(e$lower, 20, horizontal=FALSE)

    e$stat.label <- glabel("Include bootstrap distribution", container = e$lower)
    e$diff.box.cont <- ggroup(container = e$lower)
    vit.diffbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$diff.box.cont)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)
    add(e$diff.box.cont, vit.diffbox)

    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    e$get.dist <- gbutton(text = "Go", expand = TRUE,
                          container = buttons2, handler = function(h, ...) {
                              if (svalue(e$get.dist) == "Go") {
                                  e$c1$animationPaused <- FALSE
                                  # Resetting the widgets to whatever settings are currently in use
                                  svalue(e$stat) <- e$c1$stat.in.use
                                  if (is.categorical(e$c1$x))
                                      svalue(e$loi.choices) <- e$c1$loi

                                  enabled(e$stop.button) <- TRUE
                                  enabled(e$upper) <- FALSE
                                  enabled(e$vit.resamp) <- FALSE
                                  enabled(e$boot.frame.cont) <- FALSE
                                  enabled(e$animate.points) <- FALSE
                                  enabled(e$stat.label) <- FALSE
                                  enabled(e$diff.box.cont) <- FALSE
                                  enabled(e$get.sample) <- FALSE
                                  enabled(e$lowest) <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  if (e$clear.stat){
                                      e$clearPanel("stat")
                                      e$clearPanel("sample")
                                      e$clear.stat <- FALSE
                                  }
                                  if (svalue(e$bootstrap.radio) == 1000){
                                      e$clearPanel("stat")
                                      e$clearPanel("sample")
                                      e$c1$handle1000(e, points = TRUE)
                                      if (! e$c1$stopAnimation) {
                                          enabled(e$lowest) <- TRUE
                                          enabled(e$show.ci) <- TRUE
                                          enabled(e$fade.plots) <- FALSE
                                          enabled(e$show.summary) <- svalue(e$stat) != "IQR ratio"
                                          e$clear.stat <- TRUE
                                          e$summary.shown <- FALSE
                                          e$points <- TRUE
                                      }
                                  } else {
                                      n <- svalue(e$bootstrap.radio)
                                      for (i in 1:n){
                                          if (! e$c1$stopAnimation) {
                                              if (n == 1){
                                                  e$c1$animateSample(e, drop.points = TRUE,
                                                                       n.steps = 10, n.slow = 0)
                                              }
                                              if (! e$c1$stopAnimation) {
                                                  e$c1$plotSample(e)
                                                  e$c1$showLabels()
                                                  if (n != 20){
                                                      e$c1$animateStat(e, 15)
                                                  } else if (e$c1$stat.in.use == "slope"){
                                                      e$c1$animateStat(e, 3, move.point = FALSE)
                                                  }
                                                  e$c1$plotStatDist(e)
                                                  e$c1$advanceWhichSample()
                                                  e$c1$drawImage()
                                              }
                                          }
                                      }
                                      enabled(e$lowest) <- FALSE
                                  }

                                  # If we have stopped at some point, ensure we clean up
                                  # to avoid an inconsistent state
                                  if (e$c1$stopAnimation) {
                                      e$c1$reset()
                                      e$c1$resetPlots(e)
                                      e$c1$stopAnimation <- FALSE
                                      e$c1$drawImage()
                                  }

                                  enabled(e$stop.button) <- FALSE
                                  enabled(e$upper) <- TRUE
                                  enabled(e$vit.resamp) <- TRUE
                                  enabled(e$boot.frame.cont) <- TRUE
                                  enabled(e$animate.points) <- svalue(e$redraw.radio) == 1
                                  enabled(e$stat.label) <- TRUE
                                  enabled(e$diff.box.cont) <- TRUE
                                  enabled(e$get.sample) <- TRUE
                                  svalue(e$get.dist) <- "Go"
                                  return()
                              }

                              if (svalue(e$get.dist) == "Pause") {
                                  e$c1$animationPaused <- TRUE
                                  svalue(e$get.dist) <- "Play"
                                  return()
                              }

                              if (svalue(e$get.dist) == "Play") {
                                  e$c1$animationPaused <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  return()
                              }
                          })

    addSpace(e$lower, 10, horizontal = FALSE)
    e$lowest <- ggroup(horizontal = FALSE, container = e$lower, expand = TRUE)
    ci.layout <- ggroup(horizontal = TRUE, container = e$lowest, expand = TRUE)
    e$show.ci <- gbutton(text = "Show CI", expand = TRUE,
                         container = ci.layout, handler = function(h, ...){
                             if (svalue(e$show.ci) == "Show CI") {
                                 e$c1$animationPaused <- FALSE
                                 # Resetting the widgets to whatever settings are currently in use
                                 svalue(e$stat) <- e$c1$stat.in.use
                                 svalue(e$resamp.within) <- e$resampWithin
                                 svalue(e$paired.samples) <- e$pairedSamples
                                 if (enabled(e$paired.samples)){
                                     if (svalue(e$paired.samples)){
                                         e$stat[] <- c("mean", "median", "lower quartile",
                                                       "upper quartile")
                                         svalue(e$stat) <- "mean"
                                     } else {
                                         e$stat[] <- "slope"
                                         svalue(e$stat) <- "slope"
                                     }
                                 }

                                 if (is.categorical(e$c1$x))
                                     svalue(e$loi.choices) <- e$c1$loi

                                 enabled(e$stop.button) <- TRUE
                                 enabled(e$upper) <- FALSE
                                 enabled(e$vit.resamp) <- FALSE
                                 enabled(e$boot.frame.cont) <- FALSE
                                 enabled(e$animate.points) <- FALSE
                                 enabled(e$stat.label) <- FALSE
                                 enabled(e$diff.box.cont) <- FALSE
                                 enabled(e$get.sample) <- FALSE
                                 enabled(e$get.dist) <- FALSE
                                 enabled(e$fade.plots) <- FALSE
                                 enabled(e$show.summary) <- FALSE
                                 svalue(e$show.ci) <- "Pause"
                                 e$c1$displayResult(e, ci = TRUE, points = e$points)
                                 enabled(e$show.ci) <- FALSE
                                 enabled(e$fade.plots) <- TRUE
                                 e$fade <- TRUE

                                 # If we have stopped at some point, ensure we clean up
                                 # to avoid an inconsistent state
                                 if (e$c1$stopAnimation) {
                                     enabled(e$fade.plots) <- FALSE
                                     enabled(e$show.summary) <- FALSE
                                     e$c1$reset()
                                     e$c1$resetPlots(e)
                                     e$c1$stopAnimation <- FALSE
                                     e$c1$drawImage()
                                 } else {
                                     if (svalue(e$stat) == "IQR ratio")
                                         enabled(e$show.summary) <- FALSE
                                     else
                                         enabled(e$show.summary) <- ! e$summary.shown
                                 }

                                 enabled(e$stop.button) <- FALSE
                                 enabled(e$upper) <- TRUE
                                 enabled(e$vit.resamp) <- TRUE
                                 enabled(e$boot.frame.cont) <- TRUE
                                 enabled(e$animate.points) <- TRUE
                                 enabled(e$stat.label) <- TRUE
                                 enabled(e$diff.box.cont) <- TRUE
                                 enabled(e$get.sample) <- TRUE
                                 enabled(e$get.dist) <- TRUE
                                 enabled(e$show.ci) <- FALSE
                                 svalue(e$show.ci) <- "Show CI"
                                 return()
                             }

                             if (svalue(e$show.ci) == "Pause") {
                                 e$c1$animationPaused <- TRUE
                                 svalue(e$show.ci) <- "Play"
                                 return()
                             }

                             if (svalue(e$show.ci) == "Play") {
                                 e$c1$animationPaused <- FALSE
                                 svalue(e$show.ci) <- "Pause"
                                 return()
                             }
                         })

    e$fade.plots <- gbutton(text = "Fade on/off", expand = TRUE, container = ci.layout,
                            handler = function(h, ...){
                                e$c1$animationPaused <- FALSE
                                # Resetting the widgets to whatever settings are currently in use
                                svalue(e$stat) <- e$c1$stat.in.use
                                svalue(e$resamp.within) <- e$resampWithin
                                svalue(e$paired.samples) <- e$pairedSamples
                                if (enabled(e$paired.samples)){
                                    if (svalue(e$paired.samples)){
                                        e$stat[] <- c("mean", "median", "lower quartile",
                                                      "upper quartile")
                                        svalue(e$stat) <- "mean"
                                    } else {
                                        e$stat[] <- "slope"
                                        svalue(e$stat) <- "slope"
                                    }
                                }
                                if (is.categorical(e$c1$x))
                                    svalue(e$loi.choices) <- e$c1$loi

                                enabled(e$obj) <- FALSE
                                if (e$fade) e$c1$fadePlots(e) else e$c1$drawImage()
                                enabled(e$show.summary) <- FALSE
                                e$fade <- !e$fade
                                enabled(e$show.summary) <- e$fade & !e$summary.shown & svalue(e$stat) != "IQR ratio"
                                enabled(e$obj) <- TRUE
                            })

    e$show.summary <- gbutton(text = "Bootstrap distribution summaries", expand = TRUE,
                              container = e$lowest, handler = function(h, ...){
                                  e$c1$animationPaused <- FALSE
                                  # Resetting the widgets to whatever settings are currently in use
                                  svalue(e$stat) <- e$c1$stat.in.use
                                  if (is.categorical(e$c1$x))
                                      svalue(e$loi.choices) <- e$c1$loi

                                  e$c1$displayResult(e, ci = FALSE, points = e$points)
                                  enabled(e$show.summary) <- FALSE
                                  e$summary.shown <- TRUE
                              })

    enabled(e$lowest) <- FALSE
}


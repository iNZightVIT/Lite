permGUIHandler <- function(e){
    e$method <- "permutation"

    # Rewriting the menus to link only to other modules
    svalue(e$g.menu) <- getModuleMenu(e)

    e$data.boxes <- TRUE
    e$replace <- FALSE
    e$stat.scale <- FALSE
    tbl <- glayout(container = e$upper)
    tbl[1, 1] <- glabel("Quantity: ", container = tbl)
    tbl[1, 2] <- (e$stat <- gcombobox(c("mean", "median"), editable = TRUE, container = tbl,
                  handler = function(h, ...){
                      if (!is.null(e$c1$levels) & !is.categorical(e$c1$x)){
                          if (length(unique(e$c1$levels)) > 2){
                              if (svalue(e$stat) == "mean"){
                                  svalue(e$perm.choices) <- "average deviation"
                                  e$perm.choices[] <- c("average deviation", "F-statistic")
                              } else if (svalue(e$stat) == "median"){
                                  svalue(e$perm.choices) <- "average deviation"
                                  e$perm.choices[] <- c("average deviation", "pseudo F-statistic")
                              }
                          } else if (length(unique(e$c1$levels)) == 2){
                              svalue(e$perm.choices) <- "difference"
                              if (svalue(e$stat) == "mean"){
                                  e$perm.choices[] <- c("difference", "t-pooled", "t-Welch")
                                  enabled(e$perm.choices) <- TRUE
                              } else if (svalue(e$stat) == "median") {
                                  enabled(e$perm.choices) <- FALSE
                              }
                          }
                      }
                  }))
    tbl[2, 1] <- (e$perm.label <- glabel("Statistic: ", container = tbl))
    tbl[2, 2] <- (e$perm.choices <- gcombobox("", editable = TRUE, container = tbl))
    tbl[3, 1] <- (e$loi.label <- glabel("Level of interest: ", container = tbl))
    tbl[3, 2] <- (e$loi.choices <- gcombobox("", editable = TRUE, container = tbl,
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
    tbl[4, 2] <- (e$paired.samples <- gcheckbox("Paired samples", container = tbl,
                                                handler = function(h, ...) {
                                                    if (enabled(e$paired.samples)) {
                                                        enabled(e$obj) <- FALSE
                                                        if (svalue(e$paired.samples)){
                                                            e$perm.choices[] <- "difference"
                                                            svalue(e$perm.choices) <- "difference"
                                                            e$stat[] <- c("mean", "median")
                                                            svalue(e$stat) <- "mean"
                                                            e$pairedSamples <- TRUE
                                                        } else {
                                                            e$perm.choices[] <- ""
                                                            svalue(e$perm.choices) <- ""
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
                                                }))
    enabled(e$paired.samples) <- FALSE
    e$pairedSamples <- FALSE
    if (is.categorical(e$c1$x) & ! is.null(e$c1$levels)) {
        e$loi.choices[] <- sort(unique(e$c1$x))
        e$perm.choices[] <- "average deviation"
    }
    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h, ...) {
                enabled(e$obj) <- FALSE
                if (! enabled(e$paired.samples)) {
                    e$pairedSamples <- FALSE
                    svalue(e$paired.samples) <- FALSE
                }
                e$buildCanvas(enabled(e$paired.samples) & svalue(e$paired.samples))
                e$c1$data.file <- tag(e$obj, "data.file") # Grabbing data filename for labels
                e$c1$buildBoxes(enabled(e$paired.samples) & svalue(e$paired.samples))
                e$c1$drawImage()
                loadStatDetails(e)
                if (e$loaded) {
                    e$c1$makeSamples(e$replace,
                                     perm.paired.samples = enabled(e$paired.samples) &
                                                           svalue(e$paired.samples))
                    e$c1$makeStatistics()
                    if (!is.categorical(e$c1$x)) {
                        if (!is.categorical(e$c1$levels)){
                            if (enabled(e$paired.samples) & svalue(e$paired.samples)){
                                maxx <- max(abs(e$c1$x))
                                buildViewports(e$c1, e$c1$x, boxes = e$data.boxes,
                                               x.scale = c(-maxx, maxx))
                            } else {
                                ## Viewport code for scatterplots.
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
                                ## Calculating y-coordinates of lines at x.scale.
                                e$c1$calcxScaleYs(e, x.scale)
                                coeffs <- coefficients(lm(e$c1$levels ~ e$c1$x))
                                current.stats <- coeffs[1] + coeffs[2]*x.scale
                                all.ys <- rbind(e$c1$x.scale.ys, current.stats)
                                x.scale.y.diffs <- apply(all.ys, 1, diff)
                                y.diff.1 <- diff(range(c(y.scale, all.ys)))
                                y.diff.2 <- diff(range(x.scale.y.diffs))
                                y.diff <- max(c(y.diff.1, y.diff.2))
                                if (y.diff.2 < y.diff.1){
                                    y.scale <- range(c(y.scale, all.ys))
                                    extra.on.stat <- diff(c(y.diff.2, y.diff))/2
                                    stat.y.scale <- (range(x.scale.y.diffs) +
                                        c(-1, 1)*extra.on.stat)
                                } else if (y.diff.2 >= y.diff.1){
                                    extra.on.data <- diff(c(y.diff.1, y.diff))/2
                                    y.scale <- range(c(y.scale, all.ys)) + c(-1, 1)*extra.on.data
                                    stat.y.scale <- range(x.scale.y.diffs)
                                }
                                buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                               boxes = e$data.boxes,
                                               x.scale = x.scale, y.scale = y.scale,
                                               stat.scale = c(0, 1),
                                               stat.y.scale = stat.y.scale/diff(x.scale),
                                               extra.box = TRUE)
                            }
                        } else if (length(e$c1$ylevels) > 2) {
                            if (svalue(e$perm.choices) == "F-statistic" |
                                svalue(e$perm.choices) == "pseudo F-statistic"){
                                maxstat <- max(c(c(e$c1$stat.dist, recursive = TRUE),
                                                 e$c1$calcAllStats(e$c1$x, e$c1$levels)))
                                buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                               boxes = e$data.boxes,
                                               x.scale = NULL, stat.scale = c(0, maxstat))
                            } else if (svalue(e$perm.choices) == "average deviation"){
                                buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                               boxes = e$data.boxes, x.scale = NULL,
                                               stat.scale = c(0, diff(range(e$xData))))
                            }
                        } else  if (length(e$c1$ylevels) == 2){
                            if (svalue(e$perm.choices) == "difference"){
                                maxstat <- max(abs(sapply(e$c1$stat.dist, diff)))
                                if (maxstat > (max(e$c1$x) - mean(range(e$c1$x)))) {
                                    e$extra <- maxstat - (max(e$c1$x) - mean(range(e$c1$x)))
                                    x.scale <- c(min(e$c1$x) - e$extra, max(e$c1$x) + e$extra)
                                    buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                                   boxes = e$data.boxes, x.scale = x.scale,
                                                   stat.scale = x.scale - mean(x.scale))
                                }
                            } else {
                                statrange <- range(c(c(e$c1$stat.dist, recursive = TRUE),
                                                     e$c1$calcAllStats(e$c1$x, e$c1$levels)))

                                buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                               boxes = e$data.boxes, x.scale = NULL,
                                               stat.scale = statrange)
                            }
                        }
                    } else {
                        if (length(e$c1$ylevels) > 2) {
                            if (svalue(e$perm.choices) == "chi-square statistic"){
                                maxstat <- max(c(c(e$c1$stat.dist, recursive = TRUE),
                                                 e$c1$calcAllStats(e$c1$x, e$c1$levels)))
                                buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                               boxes = e$data.boxes,
                                               x.scale = NULL, stat.scale = c(0, maxstat))
                            } else if (svalue(e$perm.choices) == "average deviation"){
                                buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                               boxes = e$data.boxes,
                                               x.scale = NULL, stat.scale = NULL)
                            }
                        } else {
                            stats <- sapply(e$c1$stat.dist, diff)
                            meanstat <- mean(stats)
                            stat.scale <- c(-0.5, 0.5)
                            x.scale <- NULL
                            if (any(stats < stat.scale[1] | stats > stat.scale[2])){
                                stat.scale <- range(stats)
                                scale.diff <- (diff(stat.scale) - 1) / 2
                                if (scale.diff > 0) {
                                    x.scale <- c(0-scale.diff, 1+scale.diff)
                                } else {
                                    stat.scale <- stat.scale + c(-0.5, 0.5)*abs(scale.diff)
                                }
                            }
                            buildViewports(canvas = e$c1, x = e$c1$x, y = e$c1$levels,
                                           boxes = e$data.boxes,
                                           x.scale = x.scale, stat.scale = stat.scale)
                        }
                    }

                    e$c1$buildImage(!is.null(e$c1$levels) & !is.categorical(e$c1$levels))
                    if (is.categorical(e$xData)) {
                        e$c1$image <- editGrob(e$c1$image, gPath("dataAxis"),
                                               at = seq(0, 1, by = 0.2))
                        e$c1$image <- editGrob(e$c1$image, gPath("sampleAxis"),
                                               at = seq(0, 1, by = 0.2))
                    } else if (e$c1$stat.in.use == "slope"){
                        e$c1$image <- editGrob(e$c1$image, gPath("statAxis"),
                                               at = c(0, 1))
                    }
                    e$c1$buildBoxes(enabled(e$paired.samples) & svalue(e$paired.samples))
                    pushViewport(e$c1$viewports)
                    e$c1$plotData()
                    e$c1$showLabels()
                    e$c1$plotDataStat(e)
                    e$c1$drawImage()
                    e$c1$getStatYPos()
                    enabled(e$show.tail) <- FALSE
                    enabled(e$lower) <- TRUE
                }
                enabled(e$obj) <- TRUE
            })
    e$vit.resamp <- glabel("Permuting data", container = e$lower)
    e$boot.frame.cont <- ggroup(container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$boot.frame.cont)
    e$redraw.radio <- gradio(c(1, 5, 20), horizontal = FALSE)
    add(vit.bootbox, e$redraw.radio)
    add(e$boot.frame.cont, vit.bootbox)
    buttons1 <- ggroup(container = e$lower)

    ## Handler to go in here.
    e$get.sample <- gbutton("Go", container = buttons1, expand = TRUE,
                            handler = function(h, ...){
                                if (svalue(e$get.sample) == "Go") {
                                    # Resetting the widgets to whatever settings are currently in use
                                    svalue(e$stat) <- e$c1$stat.in.use
                                    svalue(e$perm.choices) <- e$perm.method
                                    svalue(e$paired.samples) <- e$pairedSamples
                                    if (enabled(e$paired.samples)){
                                        if (svalue(e$paired.samples)){
                                            e$perm.choices[] <- "difference"
                                            svalue(e$perm.choices) <- "difference"
                                            e$stat[] <- c("mean", "median")
                                            svalue(e$stat) <- "mean"
                                        } else {
                                            e$perm.choices[] <- ""
                                            svalue(e$perm.choices) <- ""
                                            e$stat[] <- c("slope")
                                            svalue(e$stat) <- "slope"
                                        }
                                    }
                                    if (is.categorical(e$c1$x))
                                        svalue(e$loi.choices) <- e$c1$loi

                                    enabled(e$stop.button) <- TRUE
                                    enabled(e$upper) <- FALSE
                                    enabled(e$vit.resamp) <- FALSE
                                    enabled(e$boot.frame.cont) <- FALSE
                                    enabled(e$stat.label) <- FALSE
                                    enabled(e$diff.box.cont) <- FALSE
                                    enabled(e$get.dist) <- FALSE
                                    enabled(e$show.tail) <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    enabled(e$show.tail) <- FALSE
                                    if (e$clear.stat){
                                        e$clearPanel(panel = "stat")
                                        e$clear.stat <- FALSE
                                    }
                                    n <- svalue(e$redraw.radio)
                                    for (i in 1:n){
                                        if (! e$c1$stopAnimation) {
                                          if (n != 20){
                                              if (n == 1){
                                                  e$c1$animateSample(e, n.steps = if (is.categorical(e$c1$x)) 3 else 10, mix = TRUE)
                                              } else {
                                                  e$c1$animateSample(e, n.steps = if (is.categorical(e$c1$x)) 3 else 10, mix = FALSE)
                                              }
                                          }
                                          if (! e$c1$stopAnimation) {
                                              e$c1$plotSample(e)
                                              e$c1$showLabels()
                                              if (n != 5) e$c1$drawImage() else e$c1$pauseImage(10)
                                              e$c1$advanceWhichSample()
                                          }
                                        }
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

    addSpace(e$lower, 20, horizontal = FALSE)

    e$stat.label <- glabel("Include statistic distribution", container = e$lower)
    e$diff.box.cont <- ggroup(container = e$lower)
    vit.diffbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$diff.box.cont)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)
    add(e$diff.box.cont, vit.diffbox)
    e$clear.stat <- FALSE
    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    e$get.dist <- gbutton(text = "Go", expand = TRUE, container = buttons2,
                          handler = function(h, ...){
                              if (svalue(e$get.dist) == "Go") {
                                  e$c1$animationPaused <- FALSE
                                  # Resetting the widgets to whatever settings are currently in use
                                  svalue(e$stat) <- e$c1$stat.in.use
                                  svalue(e$perm.choices) <- e$perm.method
                                  svalue(e$paired.samples) <- e$pairedSamples
                                  if (enabled(e$paired.samples)){
                                      if (svalue(e$paired.samples)){
                                          e$perm.choices[] <- "difference"
                                          svalue(e$perm.choices) <- "difference"
                                          e$stat[] <- c("mean", "median")
                                          svalue(e$stat) <- "mean"
                                      } else {
                                          e$perm.choices[] <- ""
                                          svalue(e$perm.choices) <- ""
                                          e$stat[] <- c("slope")
                                          svalue(e$stat) <- "slope"
                                      }
                                  }
                                  if (is.categorical(e$c1$x))
                                      svalue(e$loi.choices) <- e$c1$loi

                                  enabled(e$stop.button) <- TRUE
                                  enabled(e$upper) <- FALSE
                                  enabled(e$vit.resamp) <- FALSE
                                  enabled(e$boot.frame.cont) <- FALSE
                                  enabled(e$stat.label) <- FALSE
                                  enabled(e$diff.box.cont) <- FALSE
                                  enabled(e$get.sample) <- FALSE
                                  enabled(e$show.tail) <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  n <- svalue(e$bootstrap.radio)
                                  if (n == 1000){
                                      e$clearPanel("stat")
                                      e$clearPanel("sample")
                                      e$c1$handle1000(e)
                                      if (! e$c1$stopAnimation) {
                                          e$clear.stat <- TRUE
                                          enabled(e$show.tail) <- TRUE
                                      }
                                  } else {
                                      if (e$clear.stat){
                                         e$clearPanel("stat")
                                         e$clearPanel("sample")
                                         e$clear.stat <- FALSE
                                     }
                                      enabled(e$show.tail) <- FALSE
                                      for (i in 1:n){
                                          if (! e$c1$stopAnimation) {
                                              if (n == 1)
                                                  e$c1$animateSample(e, n.steps = if (is.categorical(e$c1$x)) 3 else 10, mix = FALSE)
                                              if (! e$c1$stopAnimation) {
                                                  e$c1$plotSample(e)
                                                  e$c1$showLabels()
                                                  if (n != 20){
                                                      e$c1$animateStat(e, if (is.categorical(e$c1$x)) 3 else 10)
                                                  } else if (e$c1$stat.in.use == "slope"){
                                                      e$c1$animateStat(e, 3, move.point = FALSE)
                                                  }
                                                  if (! e$c1$stopAnimation) {
                                                      e$c1$plotStatDist(e)
                                                      e$c1$advanceWhichSample()
                                                      e$c1$drawImage()
                                                  }
                                              }
                                          }
                                      }
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

    e$show.tail <- gbutton(text = "Show tail proportion", expand = TRUE,
                           container = buttons2,
                           handler = function(h, ...){
                               if (svalue(e$show.tail) == "Show tail proportion") {
                                   e$c1$animationPaused <- FALSE
                                   # Resetting the widgets to whatever settings are currently in use
                                   svalue(e$stat) <- e$c1$stat.in.use
                                   svalue(e$perm.choices) <- e$perm.method
                                   svalue(e$paired.samples) <- e$pairedSamples
                                   if (enabled(e$paired.samples)){
                                       if (svalue(e$paired.samples)){
                                           e$perm.choices[] <- "difference"
                                           svalue(e$perm.choices) <- "difference"
                                           e$stat[] <- c("mean", "median")
                                           svalue(e$stat) <- "mean"
                                       } else {
                                           e$perm.choices[] <- ""
                                           svalue(e$perm.choices) <- ""
                                           e$stat[] <- c("slope")
                                           svalue(e$stat) <- "slope"
                                       }
                                   }
                                   if (is.categorical(e$c1$x))
                                       svalue(e$loi.choices) <- e$c1$loi

                                   enabled(e$stop.button) <- TRUE
                                   enabled(e$upper) <- FALSE
                                   enabled(e$vit.resamp) <- FALSE
                                   enabled(e$boot.frame.cont) <- FALSE
                                   enabled(e$stat.label) <- FALSE
                                   enabled(e$diff.box.cont) <- FALSE
                                   enabled(e$get.sample) <- FALSE
                                   enabled(e$get.dist) <- FALSE
                                   svalue(e$show.tail) <- "Pause"
                                   e$c1$displayResult(e)

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
                                   enabled(e$stat.label) <- TRUE
                                   enabled(e$diff.box.cont) <- TRUE
                                   enabled(e$get.sample) <- TRUE
                                   enabled(e$get.dist) <- TRUE
                                   enabled(e$show.tail) <- FALSE
                                   svalue(e$show.tail) <- "Show tail proportion"
                                   return()
                               }

                               if (svalue(e$show.tail) == "Pause") {
                                   e$c1$animationPaused <- TRUE
                                   svalue(e$show.tail) <- "Play"
                                   return()
                               }

                               if (svalue(e$show.tail) == "Play") {
                                   e$c1$animationPaused <- FALSE
                                   svalue(e$show.tail) <- "Pause"
                                   return()
                               }
                           })
}

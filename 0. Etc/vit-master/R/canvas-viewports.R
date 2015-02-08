## Makes a viewport scheme to facilitate animation and plotting a statistic underneath the data

makeVitGraphViewports <- function(x.scale, y.scale, nlevels.y, stat.scale, stat.y.scale, extra.box,
                                  numeric.ys, yaxis.space) {
    if (extra.box){
        pwid <- 1.5
        cwid <- 0.5
    } else {
        pwid <- 2
        cwid <- 0
    }
    if (is.null(yaxis.space)){
        yaxis.space <- unit(0.075, "null")
    }
    widths <- unit.c(yaxis.space, unit(2, "null"))
    canvas.all.layout <- grid.layout(ncol = 2, widths = widths)
    canvas.all <- plotViewport(c(3, 1, 0, 1), layout = canvas.all.layout, name = "canvas.all")
    canvas.frame.layout <- grid.layout(nrow = 2, ncol = 2,
                                       heights = unit(1, c("line", "null")),
                                       widths = unit(c(pwid, cwid), "null"))
    canvas.frame <- viewport(layout = canvas.frame.layout,
                                 layout.pos.col = 2, name = "canvas.frame")
    canvas.header <- viewport(layout.pos.col = 1, layout.pos.row = 1,
                              layout = canvas.frame.layout, name = "canvas.header")
    animation.layout <- grid.layout(nrow = 3)
    animation.field <- dataViewport(xscale = x.scale, yscale = c(0, 3),
                                    layout.pos.row = 2, layout.pos.col = 1,
                                    layout = animation.layout, name = "animation.field")
    if (numeric.ys){
        animation.field2 <- dataViewport(xscale = c(0, 1), yscale = stat.y.scale,
                                         layout.pos.col = 2,
                                         layout.pos.row = 2, layout = animation.layout,
                                         name = "animation.field2")
    }
    data <- splitDataPane(x.scale = x.scale, y.scale = y.scale, n = nlevels.y, layout.pos.row = 1,
                          name = "data.data")
    sample <- splitDataPane(x.scale = x.scale, y.scale = y.scale, n = nlevels.y,
                            layout.pos.row = 2, name = "sample.data")
    stat <- dataViewport(xscale = stat.scale, yscale = stat.y.scale, layout.pos.row = 3,
                         name = "stat.data.1")
    if (numeric.ys){
        frame <- vpTree(canvas.frame, vpList(canvas.header, animation.field2,
                                             vpTree(animation.field,
                                                    vpList(data, sample, stat))))
    } else {
        frame <- vpTree(canvas.frame, vpList(canvas.header,
                                             vpTree(animation.field,
                                                    vpList(data, sample, stat))))
    }
    vpTree(canvas.all, vpList(frame))
}

makeVitGraphViewportsBoxes <- function(x.scale, y.scale, nlevels.y, stat.scale, stat.y.scale,
                                       extra.box, ngroups, numeric.ys, yaxis.space) {
    data.nlevels.y <- if (! is.null(ngroups)) 1 else nlevels.y

    if (extra.box){
        pwid <- 1.5
        cwid <- 0.5
    } else {
        pwid <- 2
        cwid <- 0
    }
    if (is.null(yaxis.space)){
        yaxis.space <- unit(0.075, "null")
    }
    widths <- unit.c(unit(1, "null"), yaxis.space, unit(2, "null"))
    canvas.all.layout <- grid.layout(ncol = 3, widths = widths)
    canvas.all <- viewport(layout = canvas.all.layout, name = "canvas.all")
    canvas.boxes.layout <- grid.layout(ncol = 2)
    canvas.boxes <- viewport(layout = canvas.boxes.layout, name = "canvas.boxes",
                             layout.pos.col = 1)
    databox.1 <- viewport(layout.pos.col = 1, name = "databox.1")
    databox.2 <- viewport(layout.pos.col = 2, name = "databox.2")
    canvas.plots <- viewport(layout.pos.col = 3, name = "canvas.plots")
    canvas.frame.layout <- grid.layout(nrow = 2, ncol = 2, heights = unit(1, c("line", "null", "line")),
                                       widths = unit(c(pwid, cwid), "null"))
    canvas.frame <- plotViewport(c(3, 1, 0, 1), layout = canvas.frame.layout,
                                 name = "canvas.frame")
    canvas.header <- viewport(layout.pos.col = 1:2, layout.pos.row = 1,
                              layout = canvas.frame.layout, name = "canvas.header")
    animation.layout <- grid.layout(nrow = 3)
    animation.field <- dataViewport(xscale = x.scale, yscale = c(0, 3), layout.pos.col = 1,
                                    layout.pos.row = 2, layout = animation.layout,
                                    name = "animation.field")
    data <- splitDataPane(x.scale = x.scale, y.scale = y.scale, n = data.nlevels.y, layout.pos.row = 1,
                          name = "data.data")
    sample <- splitDataPane(x.scale = x.scale, y.scale = y.scale, n = nlevels.y,
                            layout.pos.row = 2, name = "sample.data")
    stat <- dataViewport(xscale = stat.scale, yscale = stat.y.scale, layout.pos.row = 3,
                         name = "stat.data.1")
    extra.field <- dataViewport(xscale = x.scale, yscale = c(0, 3), layout.pos.col = 2,
                                    layout.pos.row = 2, layout = animation.layout,
                                    name = "extra.field")
    if (numeric.ys){
        animation.field2 <- dataViewport(xscale = c(0, 1), yscale = stat.y.scale, layout.pos.col = 2,
                                         layout.pos.row = 2, layout = animation.layout,
                                         name = "animation.field2")
    }
    stat.extrabox <- dataViewport(xscale = x.scale, yscale = y.scale,
                                  layout.pos.row = 3, name = "stat.extrabox")
    ## stat.extrabox <- dataViewport(x = 0, height = unit(1/3, "npc"), xscale = c(0, 1),
    ##                               just = "bottom", yscale = stat.y.scale,
    ##                               layout.pos.row = 2, layout.pos.col = 2, name = "stat.extrabox")
    plots.tree <- vpTree(animation.field, vpList(data, sample, stat))
    #extra.tree <- vpTree(extra.field, vpList(stat.extrabox))
    boxes <- vpTree(canvas.boxes, vpList(databox.1, databox.2))
    main.plot <- if (numeric.ys)
                     vpList(vpTree(animation.field, vpList(data, sample, stat)), animation.field2)
                 else
                     vpList(vpTree(animation.field, vpList(data, sample, stat)))
    plots <- vpTree(canvas.plots, vpList(vpTree(canvas.frame, vpList(canvas.header, main.plot))))
    vpTree(canvas.all, vpList(boxes, plots))
}

splitDataPane <- function(x.scale, y.scale, n, layout.pos.row, name) {
    frame.layout <- grid.layout(nrow = n)
    frame <- viewport(layout.pos.row = layout.pos.row, layout = frame.layout,
                      name = paste(name, "placer", sep = "."))

    data.vps <- list()
    for(i in 1:n) {
        data.vps[[i]] <- dataViewport(xscale = x.scale, yscale = y.scale,
                                      layout.pos.row = n - i + 1, name = paste(name, i, sep = "."))
    }

    vpTree(frame, do.call("vpList", data.vps))
}


## returns the vpPath to the graph viewport for the specified field.

graphPathNoBoxes <- function(plot.name = "sample", number = "1") {
    if(!(plot.name %in% c("data", "sample", "stat", "animation.field", "stat.extrabox", "canvas.header")))
        stop("plot.name must be \'data', \'sample', or \'stat'.")
    if (is.numeric(number)) number <- as.character(number)
    if ( !(number %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")))
        stop("number must be \'1', \'2', \'3', \'4', \'5', \'6', \'7', \'8', or \'9'.")
    if (plot.name == "animation.field"){
        vpPath("canvas.all", "canvas.frame", "animation.field")
    } else if (plot.name == "stat.extrabox"){
        vpPath("canvas.all", "canvas.frame", "animation.field2")
    } else if (plot.name == "canvas.header") {
        vpPath("canvas.all", "canvas.frame", "canvas.header")
    }else {
        if (plot.name == "stat") {
            vpPath("canvas.all", "canvas.frame", "animation.field",
                   paste(plot.name, "data", number, sep = "."))
        } else {
            vpPath("canvas.all", "canvas.frame", "animation.field",
                   paste(plot.name, "data.placer", sep = "."),
                   paste(plot.name, "data", number, sep = "."))
        }
    }
}

graphPathBoxes <- function(plot.name = "sample", number = "1") {
    if(!(plot.name %in% c("data", "sample", "stat", "databox", "animation.field", "canvas.header",
                          "stat.extrabox")))
        stop("plot.name must be \'data', \'sample', or \'stat'.")
    if (is.numeric(number)) number <- as.character(number)
    if ( !(number %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")))
        stop("number must be \'1', \'2', \'3', \'4', \'5', \'6', \'7', \'8', or \'9'.")
    if (plot.name == "animation.field"){
        vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field")
    } else if (plot.name == "stat.extrabox"){
        vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field2")
    } else if (plot.name == "canvas.header") {
        vpPath("canvas.all", "canvas.plots", "canvas.frame", "canvas.header")
    } else {
        if (plot.name == "databox")
            vpPath("canvas.all", "canvas.boxes", paste(plot.name, number, sep = ".")) else{
                if (plot.name == "stat") {
                    vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field",
                           paste(plot.name, "data", number, sep = "."))
                } else {
                    vpPath("canvas.all", "canvas.plots", "canvas.frame", "animation.field",
                           paste(plot.name, "data.placer", sep = "."),
                           paste(plot.name, "data", number, sep = "."))
                }
            }
    }
}

## x.scale and stat.scale provide the scale of the x axis for the data
## and sample plots / stat plots respectively. If x.scale is NULL then
## the data and sample plots will have an x-axis scale that takes the
## range of the data. If stat.scale is TRUE then the stat plot takes
## the same x-axis scale as the data and sample plots. If false, it is
## centered on 0 but has the same scale. If it is a vector of length 2
## then this explicitly provides the x-axis scale for the stat
## plot. Note that the range of this scale should usually be the same as the
## range of the x-axis scale for the other two plots.

buildViewports <- function(canvas, x, y = NULL, boxes = FALSE, x.scale = NULL, y.scale = NULL, stat.scale = NULL, stat.y.scale = NULL, extra.box = FALSE) {
    yaxis.space <- unit(0, "cm")
    ## Setting x.scale and stat.scale.
    if (is.null(x.scale)) {
        if (! is.null(y) & !is.categorical(y)){
            if (diff(range(x)) == 0){
                x.scale <- range(x) + c(-1, 1)
            } else {
                x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
            }
        } else if (is.categorical(x)) {
            x.scale <- c(0, 1)
            if (is.null(stat.scale) & is.categorical(y))
                stat.scale <- if (length(canvas$ylevels) > 2) c(0, 1) else c(-0.5, 0.5)
        } else {
            x.scale <- range(x)
        }
    }
    if (is.null(stat.scale)) {
        if (is.categorical(x) & !is.null(y)) {
            stat.scale <- if (length(canvas$ylevels) > 2) c(0, 1) else c(-0.5, 0.5)
        } else {
            stat.scale <- x.scale
        }
    }
    ## Setting n.y and y.scale.
    if (is.null(stat.y.scale)){
        stat.y.scale <- c(0, 1)
    }
    if (is.null(y)) {
        n.y <- 1
        y.scale <- c(0,1)
    } else if (is.categorical(y)) {
        n.y <- length(unique(y))
        y.scale <- c(0,1)
    } else {
        n.y <- 1
        if (is.null(y.scale)){
            if (diff(range(y)) == 0){
                y.scale <- range(y) + c(-1, 1)
            } else {
                y.scale <- range(y) + c(-0.04, 0.04)*diff(range(y))
            }
        }
        par(usr = c(x.scale, y.scale))
        y.labels <- axTicks(2)
        par(usr = c(stat.scale, stat.y.scale))
        stat.y.labels <- axTicks(2)
        yaxis.space <- max(stringWidth(c(y.labels, stat.y.labels)))
    }
    if (boxes){
        canvas$viewports <- makeVitGraphViewportsBoxes(x.scale, y.scale,
                                                       n.y, stat.scale, stat.y.scale,
                                                       extra.box, canvas$ngroups,
                                                       numeric.ys = ! is.null(canvas$levels) && is.numeric(canvas$levels), yaxis.space)
    } else {
        canvas$viewports <- makeVitGraphViewports(x.scale, y.scale,
                                                  n.y, stat.scale, stat.y.scale, extra.box,
                                                  numeric.ys = !is.null(canvas$levels) && is.numeric(canvas$levels), yaxis.space)
    }
}


##' returns the number at the end of the viewport name
vpNumber <- function(vp) {
    text <- as.character(vp$name)
    m <- nchar(text)
    last <- substr(text, m, m)
    if (last %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
        return(last)
    else
        return("")
}


##' appends a vpPath to include the number n on the bottommost viewport. If the
##' bottom most viewport ends in a number, it replaces that number with n.
appendPath <- function(vp, n) {
    text <- as.character(vp$name)
    m <- nchar(text)
    if (substr(text, m - 1, m - 1) == ".") substr(text, m, m) <- as.character(n)
    else text <- paste(text, n, sep = ".")

    structure(list(path = vp$path, name = text, n = vp$n),
              class = c("vpPath", "path"))
}

##' helper function for programming use
showVPs <- function() {
    vps <- as.character(current.vpTree())
    vps <- gsub("viewport\\[", "", vps)
    vps <- gsub("\\]", "", vps)
    vps <- gsub("\\(", "", vps)
    vps <- gsub("\\)", "", vps)
    vps <- gsub(" ", "", vps)
    vps <- gsub("->", ",", vps)

    vlist <- strsplit(vps, ",")

    for (name in vlist[[1]][-1]) {
        seekViewport(name)
        grid.rect(gp = gpar(col = "black", alpha = 0.5))
    }
}


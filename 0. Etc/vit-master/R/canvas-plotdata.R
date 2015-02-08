# The canvasPlot class is mostly a wrapper around the base refclass
# canvas.
#
# The key difference is that the canvasPlot class knows exactly what
# it needs to be able to draw a plot upon creation. This is necessary
# because due to namespacing semantics with Reference Classes, we need
# to be sure that the methods are not variable, and thus each of our
# methods must be fixed at the point where we build a package.
#
# We leave the utility methods to the base refclass.

canvasPlot <- setRefClass("canvasPlotClass", fields = c("data.boxes", "plot.load.function"), contains = "canvasClass",
                          methods = list(
    initialize = function(x = NULL, y = NULL, levels = NULL, x.name = NULL, y.name = NULL, paired.data = NULL,
                          samples = NULL, level.samples = NULL, ylevels = NULL, loi = NULL, loi.alt = NULL, loi.data = NULL,
                          stat.in.use = NULL, stat.method = NULL, ngroups = NULL, max.hist.height = NULL, viewports = NULL,
                          image = NULL, n = 0, progressType = "text", data.file = NULL, which.sample = 0, if.permswap = NULL,
                          indexes = NULL, animationPaused = FALSE, stopAnimation = FALSE, stat.dist = NULL, quartiles = NULL,
                          stat.ypos = NULL, x.scale.ys = NULL, sampled.stats = NULL, plotted.stats = NULL,
                          data.boxes = TRUE, plot.load.function = NULL, ...) {
        x <<- x
        y <<- y
        x.name <<- x.name
        y.name <<- y.name
        levels <<- levels 
        samples <<- samples
        level.samples <<- level.samples
        ylevels <<- ylevels
        image <<- image
        indexes <<- indexes
        loi <<- loi
        loi.alt <<- loi.alt
        loi.data <<- loi.data
        stat.in.use <<- stat.in.use
        stat.method <<- stat.method
        ngroups <<- ngroups
        hist.cutoff <<- 1000 # Sample size where we start drawing hists
        max.hist.height <<- max.hist.height
        viewports <<- viewports
        n <<- length(x)
        paired.data <<- paired.data
        progressType <<- progressType
        data.file <<- data.file
        which.sample <<- which.sample
        if.permswap <<- if.permswap
        animationPaused <<- animationPaused
        stopAnimation <<- stopAnimation
        dp <<- 2
        stat.dist <<- stat.dist
        quartiles <<- quartiles
        stat.ypos <<- stat.ypos
        x.scale.ys <<- x.scale.ys
        sampled.stats <<- sampled.stats
        plotted.stats <<- plotted.stats
        data.boxes <<- data.boxes
        plot.load.function <<- plot.load.function
        invisible(.self)
    },
    # The graphPath method gives us a convenient way of grabbing a
    # vpPath. It means we only need to specify the "region" of the
    # plot that we're interested in and optionally the group number.
    #
    # plot.name: The region of the plot we're interested in, e.g.
    #            "data", "sample, "stat", etc.
    #
    # number: If we have more than one group in a region, this
    #         specifies a subregion of "plot.name".
    #         The higher the number, the higher up the subregion is
    #         within the "plot.name" region.
    graphPath = function(plot.name = "sample", number = "1") {
        # If we have data boxes to the left of our plots, as is the
        # case with all methods besides ci and sampvar, use a
        # different graphPath function to give us a vpPath.
        if (data.boxes)
            graphPathBoxes(plot.name, number)
        else
            graphPathNoBoxes(plot.name, number)
    },
    # The plotData method is typically called only a few times. It is
    # the method which determines what appears in the top panel of our
    # plots. We need to call a different function for each different
    # kind of data that VIT handles. As this method is required to be
    # fixed, we need to work out which plotting function should be
    # called up-front.
    plotData = function() {
        if (is.null(levels)) {
            if (is.categorical(x)) {
                # Single categorical
                plotPropbar(.self)
            } else {
                # Single numeric
                plotPointsAndBoxplot(.self)
            }
        } else if (is.categorical(levels)) {
            # Even if we have specified that levels exist, the permvar
            # methods require that we keep the data in its original
            # form. This means that if "ngroups" has a valid value (as
            # set within the permvar methods), then we need to show
            # the same data plot as in the single variable case. If
            # ngroups has not been set, plot data in groups.
            if (! is.null(ngroups)) {
                if (is.categorical(x)) {
                    # Single categorical
                    plotPropbar(.self)
                } else {
                    # Single numeric
                    plotPointsAndBoxplot(.self)
                } 
            } else {
                if (is.categorical(x)) {
                    # Two categorical variables
                    plotKProportions(.self)
                } else {
                    # Single numeric and single categorical
                    plotPointGroupsNoBoxplot(.self)
                }
            }
        } else {
            # Because we provide the option of having paired sample
            # data, we cannot assume that the remaining cases are
            # scatter plots even with two numeric variables. As a
            # result, we need to have specified a plotting function to
            # use. The strings we are comparing against are the names
            # of the functions that we used to use when we were able
            # to dynamically override methods.
            #
            # If the plot function is 1d, then we are dealing with
            # paired samples, otherwise plot a scatterplot as per
            # usual.
            if (plot.load.function == "load_numeric_1d")
                plotPointsAndBoxplot(.self)
            if (plot.load.function == "load_numeric_2d")
                plotScatterPlot(.self, x, levels, "data", "purple3")
        }
    },
    buildImage = function(add.yaxes = FALSE) {
        # Builds an initial image for a canvas object. The initial
        # image is just the background, with nothing added.
        dataAxis <- xaxisGrob(name = "dataAxis", label = !add.yaxes,
                              vp = graphPath("data"))
        sampleAxis <- xaxisGrob(name = "sampleAxis", vp = graphPath("sample"))
        statAxis <- xaxisGrob(name = "statAxis", vp = graphPath("stat"))
        image <<- gTree(name = "image", childrenvp = viewports,
                        children = gList(dataAxis, sampleAxis, statAxis))
        # In the case that we're having to deal with two numeric
        # variables (that are not paired samples) we need to set up y
        # axes and bounding rects for our VPs.
        if (add.yaxes) {
            dataYAxis <- yaxisGrob(name = "dataYAxis", vp = graphPath("data"))
            sampleYAxis <- yaxisGrob(name = "sampleYAxis", vp = graphPath("sample"))
            statYAxis <- yaxisGrob(name = "statYAxis", vp = graphPath("stat"))
            dataRect <- rectGrob(name = "dataRect", vp = graphPath("data"))
            sampleRect <- rectGrob(name = "sampleRect", vp = graphPath("sample"))
            statRect <- rectGrob(name = "statRect", vp = graphPath("stat"))
            image <<- addGrob(image, dataYAxis)
            image <<- addGrob(image, sampleYAxis)
            image <<- addGrob(image, statYAxis)
            image <<- addGrob(image, dataRect)
            image <<- addGrob(image, sampleRect)
            image <<- addGrob(image, statRect)
        }
    }))

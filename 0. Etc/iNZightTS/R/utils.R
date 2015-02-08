###  File : utils.R
###  Modified by : Chris Park <cpar137@aucklanduni.ac.nz>
###  Description : Modified the "newdevice" function.
###  Last modified : December 16, 2014.

get.x <-
    function(tsObj) {
        ##  figure out the limits and step size along the x axis
        f = frequency(tsObj)
        s = start(tsObj)
        if (f == 1) {
            start.x = s[1]
            step.x = 1
            end.x = start.x + length(tsObj) - 1
        }
        else {
            step.x = 1/f
            start.x = s[1] + (s[2] - 1) * step.x
            end.x = start.x + step.x * (length(tsObj) - 1)
        }

        x = seq(start.x, end.x, by = step.x)
        x.units = unit(x, "native")
        list(x = x, x.units = x.units)
    }

get.x2 <-
    function(tsObj) {
        ##  figure out the limits and step size along the x axis
        f = frequency(tsObj)
        s = start(tsObj)
        if (f == 1) {
            start.x = s[1]
            step.x = 1
            end.x = start.x + length(tsObj) - 1
        }
        else {
            step.x = 1/f
            start.x = s[1] + (s[2] - 1) * step.x
            end.x = start.x + step.x * (length(tsObj) - 1)
        }

        x = seq(start.x, end.x, by = step.x)
        x = order(x)
        x = x/max(x)
        x.units = unit(x, "native")
        list(x = x, x.units = x.units)
    }

get.line.coords <-
    function(vars.decomp, vpName, lineGrobName) {
        decomp = vars.decomp$decompVars
        seekViewport(vpName)
        line = getGrob(decomp$tree, lineGrobName)
        line.y = convertUnit(line$y, attr(line$y[1], "unit"), valueOnly = TRUE)
        line.vp.yrange = current.viewport()$yscale
        line.y.npc = (line.y - line.vp.yrange[1]) / diff(line.vp.yrange)
        line.y.parent = switch(vpName,
            season = decomp$props["remainder"] +
            line.y.npc * decomp$props["seasonal"],
            random = line.y.npc * decomp$props["remainder"],
            trend = line.y.npc * decomp$props["trend"] +
            decomp$props["seasonal"] +
            decomp$props["remainder"])
        line.x = convertUnit(line$x, "native", valueOnly = TRUE)
        line.vp.xrange = current.viewport()$xscale
        line.x.npc = (line.x - line.vp.xrange[1]) / diff(line.vp.xrange)
        x.parent = line.x.npc

        list(line.y = line.y, line.vp.yrange = line.vp.yrange,
             line.y.npc = line.y.npc, line.y.parent = line.y.parent,
             line.x = line.x, line.vp.xrange = line.vp.xrange,
             line.x.npc = line.x.npc, x.parent = x.parent,
             line.col = line$gp$col)
    }



add.line.plots.vp <-
    function(vars.decomp, vpName, lineCol = "red3",
             name = paste(vpName, "copy", sep = ".")) {
        z = get.line.coords(vars.decomp, vpName, paste(vpName, "Line", sep = ""))
        lineCopy = linesGrob(unit(z$x.parent, "npc"),
            unit(z$line.y.parent, "npc"),
            name = name,
            vp = vpPath("parent", "plots"),
            gp = gpar(col = lineCol))
        updated.tree = addGrob(vars.decomp$decompVars$tree, lineCopy)
        vars.decomp$decompVars$tree = updated.tree
        vars.decomp
    }


###  If OS is windows, simply use the default graphics device.
###  If OS is non-windows, we perform a series of checks.
###  First, we check if "Acinonyx" or "cairoDevice" loads properly.
###  If they BOTH load correctly, we choose "Acinonyx".
###  If a package is installed but can't be loaded properly, a helpful
###  warning message is issued. Finally, if BOTH the packages cannot
###  be loaded properly, we use the X11(type = "cairo") device, as it
###  supports dev.hold() and dev.flush() which we will be using for
###  producing animations, as part of the puaseImage() function.

newdevice <-
    function(width, height, ...) {
            ##  Main function starts here.
            if (.Platform$OS.type == "windows") {
                dev.new(width = width, height = height, ...)
            } else {
                ##  See if 'Acinonyx' is installed properly.
                instAc <- "Acinonyx" %in% rownames(installed.packages())
                loadAc <- suppressWarnings(require(Acinonyx, quietly = TRUE,
                                                  warn.conflicts = TRUE))
                ##  See if 'cairoDevice' is installed properly.
                instCa <- "cairoDevice" %in% rownames(installed.packages())
                loadCa <- suppressWarnings(require(cairoDevice, quietly = TRUE,
                                                  warn.conflicts = TRUE))
                if (loadAc || (loadAc && loadCa))  {
                    width.in <- round(width * 90)
                    height.in <- round(height * 90)
                    Acinonyx::idev(width = width.in, height = height.in)
                }
                if (instAc && !loadAc) {
                    warning("The 'Acinonyx' package seems to have been
                             installed incorrectly. We suggest you re-install
                             the 'Acinonyx' package. If the issue persists,
                             please consult the package help page on CRAN.")
                }
                if (loadCa && !loadAc) {
                    cairoDevice::Cairo(width = width, height = height, ...)
                }
                if (instCa && !loadCa) {
                    warning("The 'cairoDevice' package seems to have been
                             installed incorrectly. We suggest you re-install
                             the 'cairoDevice' package. If the issue persists,
                             please consult the package help page on CRAN.")
                }
                if (!loadCa && !loadAc || !instCa && !instAc) {
                    X11(width = width, height = height, type = "cairo", ...)
                    warning("We recommend you download either the 'cairoDevice'
                            (for Linux/Windows users) or the 'Acinonyx' package
                            (for Mac OS X users) for better animations.")
                }
            }
        }


## hold.frame <-
##     function(frame = 1L) {
##         if (dev.interactive()) {
##             if (exists("dev.hold"))
##                 dev.hold(frame)
##         }
##     }

## flush.frame <-
##     function(frame = 1L, interval = .01, sleep = FALSE) {
##         if (dev.interactive()) {
##             if (exists("dev.flush")) {
##                 dev.flush(frame)
##             }
##             if (sleep) {
##                 Sys.sleep(interval)
##             }
##         }
##     }

drawImage <-
    function(image) {
        ##  if ("Acinonyx" %in% rownames(installed.packages()))
        ##  if "Acinonyx" is loaded, then use plot.new(.)
        ##  Unsure as to why this is necessary, but left as is
        ##  for now.
        ##  if ("package:Acinonyx" %in% search())
        ## plot.new()
        ##  Draws current image in device.
        dev.hold()
        grid.newpage()
        ##  On some devices (notably on Mac) we end up being unable to
        ##  see anything besides a single frame due to buffering.
        ##  dev.hold() and dev.flush() will force the device to hold
        ##  and flush currently buffered frames.
        grid.draw(image)
        dev.flush()
    }


###  The dev.hold() and dev.flush() functions hold and flush frames.
###  Moreover, they are part of the grDevices package, which is
###  included in R by default. For non-windows graphics devices
###  that may or may not come with "double buffering", holding
###  and flushing an image for every iteration allows us to overcome
###  the problem of "flickering".

pauseImage <-
    function(image, pause = 0.001) {
            drawImage(image)
            Sys.sleep(pause)
    }



rmGrobs <-
    function(image, grobs) {
        for (i in grobs) {
            if (i %in% childNames(image)) {
                image <- removeGrob(image, gPath(i))
            }
        }
        image
    }


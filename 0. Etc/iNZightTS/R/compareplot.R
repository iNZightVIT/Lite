
compareplot <-
  function(x,...) {
    if (!any(grepl("^iNZightMTS$", class(x))))
      stop("x is not an iNZightMTS object")
    if (x$freq > 1) {
      compareplot.2p(x, ...)
    } else {
      compareplot.1(x, ...)
    }
  }


compareplot.1 <-
  function(vars, ylab = "", multiplicative = FALSE) {
    ##########################################
    # Plot comparison of multiple time series which have frequency > 1
    # in one window.
    #
    # Args:
    #  vars: iNZightTS object with the data and frequency information included
    #  cols: Columns of the data from vars which will be used to plot the ts
    ##########################################
    
    varNums = 1:length(vars$currVar)
    
    #trendCol = "blue"
    trendCol = "black"
    trendSeasonCol = "#0e8c07"
    rawCol = "black"
    seasonCol = "red"
    groupCol =  hcl(h = seq(30, 300, by = 270/(length(vars$currVar)-1)), c = 50, l = 70)
    groupCol.text = hcl(h = seq(30, 300, by = 270/(length(vars$currVar)-1)), c = 50, l = 40)
    
    ### put all the necessary "vars" variables into a list
    listVars = vector("list")
    varNames = character(0)
    for (i in vars$currVar) {
      # add the time and the data for the ts
      vardata = cbind(vars$data[, 1, drop = FALSE], vars$data[, i, drop = FALSE])
      curr.vars = vars
      curr.vars$data = vardata
      curr.vars$tsObj = ts(vars$data[, i], vars$start, vars$end, vars$freq)
      curr.vars$currVar = i
      curr.vars = decomposition(curr.vars, ylab="", multiplicative = multiplicative)
      name = gsub("[[:space:]]+", "_", curr.vars$currVar)
      listVars[[name]] = curr.vars
    }
    
    whether.multi <- curr.vars$decompVars$multiplicative #%
    
    n = length(varNums)
    x.vals = get.x2(listVars[[1]]$tsObj)  ## we always use the first one here, some doubt
    
    freq = listVars[[1]]$freq
    subset = 1
    
    ### form the viewports
    trends.vps = seasons.vps = vpList()
    # What we do below is want to use range function to find the range of all the data
    raw.y.vals = matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    trend.y.vals = matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    deTrend <- matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    season.y.vals <- matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    joint.y.vals <- matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    
    for (i in 1:n) {
      varNames[i] = listVars[[i]]$currVar
      raw.y.vals[,i] = listVars[[i]]$decompVars$raw
      trend.y.vals[,i] = listVars[[i]]$decompVars$components[,"trend"]@.Data
      if (whether.multi)
        deTrend[,i] = raw.y.vals[,i] / trend.y.vals[,i]
      else
        deTrend[,i] = raw.y.vals[,i] - trend.y.vals[,i]
      
      season.y.vals[,i] = listVars[[i]]$decompVars$components[,"seasonal"]@.Data
      joint.y.vals[,i] = trend.y.vals[,i] + season.y.vals[,i]
    } #
    # } I think we can stop the iteration here and combine the result into data frame
    
    #trendVpName = paste("trendStack", i, sep = "")
    trendVpName = "trendStack"
    
    
    
    trends.vps[[trendVpName]] =
      dataViewport(c(x.vals$x,1.2), range(raw.y.vals, joint.y.vals), #% check range from raw y value and trend+season y value
                   name = trendVpName, layout.pos.row = 2,
                   extension = 0.04,
                   #clip = "on",
                   #xscale = c(min(x.vals$x)-(2*freq/(diff(range(x.vals$x)))),
                   #            max(x.vals$x)+(2*freq/(diff(range(x.vals$x))))),
                   
      )
    #extension = 0.1) # 2i --> 2
    #trendGapVpName = paste("trendGap", i, sep = "")
    trendGapVpName =   "trendGap"
    trends.vps[[trendGapVpName]] = viewport(layout.pos.row = 1,  # 2i-1 --> 1
                                            name = trendGapVpName)
    
    plots.heights = unit(rep(c(0.25, 1), length = 2*n),
                         rep(c("inches", "null"), length = 2*n))
    
    ### The following defines the viewport layout
    ### parent.vp holds everything - a main central viewport,
    ### and 4 viewports around it that act as margins.
    ### If freq > 1, we have a seasonal panel on the right hand side
    ### giving us 5 columns, otherwise we have just 3 columns.
    n.cols = 3
    widths = unit(c(1.2, 1, 0.4), c("inches", "null", "inches"))
    
    parent.vp =
      viewport(name = "parent",
               layout = grid.layout(3, n.cols,
                                    heights = unit(c(.3, 1, .8),
                                                   c("inches", "null", "inches")),
                                    widths = widths))
    
    left.vp = viewport(layout.pos.row = 1:3, layout.pos.col = 1, name = "left")
    right.vp = viewport(layout.pos.row = 1:3, layout.pos.col = n.cols, name = "right")
    
    trends.head.vp = viewport(layout.pos.row = 1, layout.pos.col = 2,
                              name = "trends.head")
    trendsPanel.vp =
      viewport(layout = grid.layout(2, 1, heights = plots.heights),
               name = "trends.panel", layout.pos.row = 2, layout.pos.col = 2)
    trends.bottom.vp = viewport(layout.pos.row = 3, layout.pos.col = 2,
                                name = "trends.bottom")
    trends.vptree = vpTree(trendsPanel.vp, trends.vps)
    
    final.vptree = vpTree(parent.vp,
                          vpList(trends.head.vp, left.vp, trends.vptree,
                                 right.vp, trends.bottom.vp))
    
    
    
    
    ### Form the gTree
    list.grobs = gList()
    dims = c(7.5, 6.5)
    newdevice(width = dims[1], height = dims[2])
    
    ### Trends part
    #vpName = paste("trendStack", i, sep = "")
    #gapName = paste("trendGap", i, sep = "")
    vpName = "trendStack"
    gapName = "trendGap"
    vpObj = trends.vps[[vpName]]
    
    opar = par(usr = c(vpObj$xscale, vpObj$yscale)
               
    )
    
    
    ## pretty label customed
    time.data = vars$star[1]:vars$end[1]
    time.data = pretty(time.data)[-1]
    numeric.time= seq(vars$star, vars$end[1],
                      len = nrow(vars$data))
    label.id = which(numeric.time %in% time.data)
    x.at = c(x.vals$x,1.2)[c(label.id, FALSE)]
    x.label = numeric.time[label.id]
    ## pretty label customed
    


    
    par(opar)
    
    for (i in 1:n) {
      
      trend.y.vals = listVars[[i]]$decompVars$components[,"trend"]@.Data
      season.y.vals = listVars[[i]]$decompVars$components[,"seasonal"]@.Data
      
      if (whether.multi)
        joint.y.vals = trend.y.vals * season.y.vals
      else
        joint.y.vals = trend.y.vals + season.y.vals   # * if multiplicative
      
      
      raw.y.vals = listVars[[i]]$decompVars$raw
      ordered.vals = numeric(freq)
      ordered.vals[subset] = season.y.vals[1:freq]
      
      rectName = paste("trend", "border", i, sep = ".")
      trendName = paste("trend", "series", i, sep = ".")
      trendLabelName = paste("trend", "label", i, sep = ".")
      rawName = paste("raw", "series", i, sep = ".")
      trendSeasonName = paste("trendSeason", "series", i, sep = ".")
      yAxisName = paste("trend", "yAxis", i, sep = ".")
      gridlinesName = paste("trend", "gridlines", i, sep = ".")
      labelName = paste("trend", "label", i, sep = ".")
      symbolName = paste("label", "symbol", i, sep = ".")
      labelName2 = paste("season", "label", i, sep = ".")
      SymbolName2 = paste("season", "symbol", i, sep = ".")
      y1labelName = paste("y1", "label", 1, sep = ".")
      panelName = "trends.panel"
      
      list.grobs[[rectName]] =
        rectGrob(vp = vpPath("parent", panelName, vpName),
                 name = rectName, gp = gpar(fill=NA))
      
      
      list.grobs[[gridlinesName]] =
        segmentsGrob(x0 = unit(x.at, "native"),
                     y0 = unit(vpObj$yscale[1], "native"),
                     x1 = unit(x.at, "native"),
                     y1 = unit(vpObj$yscale[2], "native"),
                     gp = gpar(col = "#cccccc", lty = "dashed"),
                     vp = vpPath("parent", panelName, vpName),
                     name = gridlinesName)
      
      
      #list.grobs[[trendSeasonName]] =
      #  linesGrob(x = x.vals$x.units,
      #            y = unit(joint.y.vals, "native"),
      #            vp = vpPath("parent", panelName, vpName),
      #            gp = gpar(col = trendSeasonCol),
      #            name = trendSeasonName)
      
      list.grobs[[rawName]] =
        linesGrob(x = x.vals$x.units,
                  y = unit(raw.y.vals, "native"),
                  vp = vpPath("parent", panelName, vpName),
                  gp = gpar(col = groupCol[i], lwd = 2), name = rawName)
      
      list.grobs[[trendName]] =
        linesGrob(x = x.vals$x.units,
                  y = unit(trend.y.vals, "native"),
                  vp = vpPath("parent", panelName, vpName),
                  gp = gpar(col = groupCol[i], lty = 3, lwd = 3), name = trendName)
      
      list.grobs[[trendLabelName]] =
        textGrob(varNames[i], x = x.vals$x.units[length(x.vals$x.units)],
                 y = unit(trend.y.vals, "native")[length(x.vals$x.units)],
                 hjust = -0.2, 
                 vjust = 2, 
                 #just = c("left", "bottom"),
                 vp = vpPath("parent", panelName, vpName),
                 gp = gpar(col = groupCol.text[i], cex = 0.6), name = trendLabelName)
      
      list.grobs[[symbolName]] =
        pointsGrob(x = x.vals$x.units[length(x.vals$x.units)],
                   y = unit(trend.y.vals, "native")[length(x.vals$x.units)],
                   pch = i, size = unit(0.5,"char"),
                   vp = vpPath("parent", panelName, vpName),
                   gp = gpar(col = groupCol[i], cex = 2, lwd= 3), name = symbolName)
      
      list.grobs[[yAxisName]] =
        yaxisGrob(vp = vpPath("parent", panelName, vpName),
                  gp = gpar(cex = 0.8), name = yAxisName)
      
      
      
      if (i == 1)  {
        if(nchar(ylab)>8)
          list.grobs[[y1labelName]] =
          textGrob(ylab, x = 0, rot= 90, vjust = -6, 
                   vp = vpPath("parent", panelName, vpName),
                   just = c("right","center"),
                   gp = gpar(cex = 0.8),
                   name = y1labelName)
        else
          list.grobs[[y1labelName]] =
          textGrob(ylab, x = 0, rot= 90, vjust = -6, 
                   vp = vpPath("parent", panelName, vpName),
                   gp = gpar(cex = 0.8),
                   name = y1labelName)
      }
      
      list.grobs$xAxis1 =
        xaxisGrob(vp = vpPath("parent", "trends.panel",
                              "trendStack"),
                  at = x.at,
                  label = x.label,
                  gp = gpar(cex = 0.8), name = "xAxis1")
      
      list.grobs$xAxisLabel1 =
        textGrob("Time", vp = vpPath("parent", "trends.bottom"),
                 name = "xAxisLabel1", gp = gpar(cex = 0.8),
                 y = unit(3, "mm"), vjust = 0)
      
      
      #list.grobs[[labelName]] =
      #  textGrob(varNames[i], x = unit(30*(i-1), "mm"), y = unit(1, "mm"),
      #           just = c("left", "bottom"),
      #           vp = vpPath("parent", panelName, gapName),
      #           gp = gpar(cex = 0.8, fontface = "bold.italic", col = groupCol[i]),
      #           name = labelName)
      
      #list.grobs[[SymbolName2]] =
      #  pointsGrob(x= 1.2, y = 0.5*(n-i)/n + 0.4,
      #             pch = i, size = unit(0.5,"char"),
      #             vp = vpPath("parent", panelName, vpName),
      #             gp = gpar(col = groupCol[i]),
      #             name = SymbolName2)
      
      #list.grobs[[labelName2]] =
      #  textGrob(varNames[i], x= 1.2, y = 0.5*(n-i)/n + 0.4, 
      #           vp = vpPath("parent", panelName, vpName),
      #           just = c("left","center"),
      #           gp = gpar(cex = 0.8,col = groupCol.text[i]),
      #           name = labelName2)
      
    }
    
    
    
    ### Add a legend above the left panel
    xc = unit(2, "mm")
    gap = unit(2, "mm")
    space = unit(8, "mm")
    lineWidth = unit(6, "mm")
    
    list.grobs$trendKey =
      linesGrob(x = unit.c(xc, xc + lineWidth*1.5),
                y = unit(1, "npc") - unit(0.6, "lines"),
                vp = vpPath("parent", "trends.head"),
                name = "trendKey",
                gp = gpar(col = trendCol, lwd = 2, lty = 3))
    
    xc = xc + lineWidth + gap
    list.grobs$trendKeyText =
      textGrob("Trend", x = xc, y = unit(1, "npc") - unit(1, "lines"),
               just = c("left", "bottom"),
               vp = vpPath("parent", "trends.head"),
               name = "trendKeyText", gp = gpar(cex = .8))
    
    list.grobs$titleText = 
      textGrob(paste0("Comparing series ", 
                      ifelse(ylab=="", "", ylab),
                      ifelse(whether.multi, " --- Multiplicative", "")), 
               x = 0.3, y = unit(1, "npc") - unit(1, "lines"),
               just = c("left", "bottom"),
               vp = vpPath("parent", "trends.head"),
               name = "titleText", gp = gpar(cex = 1))
    
    #xc = xc + stringWidth(list.grobs$trendKeyText$label) + space
    #list.grobs$rawKey =
    #  linesGrob(x = unit.c(xc, xc + lineWidth),
    #            y = unit(1, "npc") - unit(0.6, "lines"),
    #            vp = vpPath("parent", "trends.head"),
    #            name = "rawKey",
    #            gp = gpar(col = rawCol, lwd = 2))
    
    #xc = xc + lineWidth + gap
    #list.grobs$rawKeyText =
    #  textGrob("Raw data", x = convertUnit(xc, "mm"),
    #           y = unit(1, "npc") - unit(1, "lines"),
    #           just = c("left", "bottom"),
    #           vp = vpPath("parent", "trends.head"),
    #           name = "rawKeyText", gp = gpar(cex = .8))
    
    #xc = xc + stringWidth(list.grobs$rawKeyText$label) + space
    #list.grobs$trendSeasonKey =
    #  linesGrob(x = unit.c(xc, xc + lineWidth),
    #            y = unit(1, "npc") - unit(0.6, "lines"),
    #            vp = vpPath("parent", "trends.head"),
    #            name = "trendSeasonKey",
    #            gp = gpar(col = trendSeasonCol, lwd = 2))
    
    #xc = xc + lineWidth + gap
    #trendandseason <- ifelse(multiplicative, "Trend * seasonal", "Trend + seasonal")
    #list.grobs$trendSeasonKeyText =
    #  textGrob(trendandseason, x = convertUnit(xc, "mm"),
    #           y = unit(1, "npc") - unit(1, "lines"),
    #           just = c("left", "bottom"),
    #           vp = vpPath("parent", "trends.head"),
    #           name = "trendSeasonKeyText", gp = gpar(cex = .8))
    
    image = gTree(name = "image", children = list.grobs,
                  childrenvp = final.vptree)
    
    drawImage(image)
  }



compareplot.2p <-
  function(vars, ylab = "", multiplicative = FALSE) {
    ##########################################
    # Plot comparison of multiple time series which have frequency > 1
    # in one window.
    #
    # Args:
    #  vars: iNZightTS object with the data and frequency information included
    #  cols: Columns of the data from vars which will be used to plot the ts
    ##########################################
    
    varNums = 1:length(vars$currVar)
    
    #trendCol = "blue"
    trendCol = "black"
    trendSeasonCol = "#0e8c07"
    rawCol = "black"
    seasonCol = "red"
    groupCol =  hcl(h = seq(30, 300, by = 270/(length(vars$currVar)-1)), c = 50, l = 70)
    groupCol.text = hcl(h = seq(30, 300, by = 270/(length(vars$currVar)-1)), c = 50, l = 40)
    
    ### put all the necessary "vars" variables into a list
    listVars = vector("list")
    varNames = character(0)
    for (i in vars$currVar) {
      # add the time and the data for the ts
      vardata = cbind(vars$data[, 1, drop = FALSE], vars$data[, i, drop = FALSE])
      curr.vars = vars
      curr.vars$data = vardata
      curr.vars$tsObj = ts(vars$data[, i], vars$start, vars$end, vars$freq)
      curr.vars$currVar = i
      curr.vars = decomposition(curr.vars, ylab = "", multiplicative = multiplicative)
      
      name = gsub("[[:space:]]+", "_", curr.vars$currVar)
      listVars[[name]] = curr.vars
    }
    
    whether.multi <- curr.vars$decompVars$multiplicative #%
    
    n = length(varNums)
    x.vals = get.x2(listVars[[1]]$tsObj)  ## we always use the first one here, some doubt
    

    
    freq = listVars[[1]]$freq
    startSeason = listVars[[1]]$start[2]
    subset = 1:freq
    if (startSeason > 1) {
      subset = c(startSeason:freq, 1:(startSeason-1))
    } else {
      subset = 1:freq
    }
    
  
    
    ### form the viewports
    trends.vps = seasons.vps = vpList()
    # What we do below is want to use range function to find the range of all the data
    raw.y.vals = matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    trend.y.vals = matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    deTrend <- matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    season.y.vals <- matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    joint.y.vals <- matrix(NA, nrow= length(listVars[[1]]$decompVars$raw), ncol= n)
    
    for (i in 1:n) {
      varNames[i] = listVars[[i]]$currVar
      raw.y.vals[,i] = listVars[[i]]$decompVars$raw
      trend.y.vals[,i] = listVars[[i]]$decompVars$components[,"trend"]@.Data
      if (whether.multi)
        deTrend[,i] = raw.y.vals[,i] / trend.y.vals[,i]
      else
        deTrend[,i] = raw.y.vals[,i] - trend.y.vals[,i]
      
      season.y.vals[,i] = listVars[[i]]$decompVars$components[,"seasonal"]@.Data
      joint.y.vals[,i] = trend.y.vals[,i] + season.y.vals[,i]
    } #
    # } I think we can stop the iteration here and combine the result into data frame
   
      #trendVpName = paste("trendStack", i, sep = "")
    trendVpName = "trendStack"
    
    

    trends.vps[[trendVpName]] =
        dataViewport(c(x.vals$x,1.4), range(raw.y.vals, joint.y.vals), #% check range from raw y value and trend+season y value
                     name = trendVpName, layout.pos.row = 2,
                     extension = 0.04,
                     #clip = "on",
                     #xscale = c(min(x.vals$x)-(2*freq/(diff(range(x.vals$x)))),
                    #            max(x.vals$x)+(2*freq/(diff(range(x.vals$x))))),
                  
                    )
                     #extension = 0.1) # 2i --> 2
      #trendGapVpName = paste("trendGap", i, sep = "")
    trendGapVpName =   "trendGap"
    trends.vps[[trendGapVpName]] = viewport(layout.pos.row = 1,  # 2i-1 --> 1
                                              name = trendGapVpName)
      
      #seasonVpName = paste("seasonStack", i, sep = "")
    seasonVpName = "seasonStack"
    seasons.vps[[seasonVpName]] =
        dataViewport(1:freq, range(season.y.vals[1:freq], deTrend), #% check range from season factor value and de-trend value
                     name = seasonVpName, layout.pos.row = 2) # 2i -->2
    seasonGapVpName =   "seasonGap"
    #seasonGapVpName = paste("seasonGap", i, sep = "")
      seasons.vps[[seasonGapVpName]] = viewport(layout.pos.row = 1, # 2i- 1 --> 1
                                                name = seasonGapVpName)
    
    
    plots.heights = unit(rep(c(0.25, 1), length = 2),
                         rep(c("inches", "null"), length = 2))
    
    ### The following defines the viewport layout
    ### parent.vp holds everything - a main central viewport,
    ### and 4 viewports around it that act as margins.
    ### If freq > 1, we have a seasonal panel on the right hand side
    ### giving us 5 columns, otherwise we have just 3 columns.
    n.cols = 5
    widths = unit(c(1.2, 1.4, 0.8, 0.5, 0.3),
                  c("inches", "null", "inches", "null", "inches"))
    parent.vp =
      viewport(name = "parent",
               layout = grid.layout(4, n.cols,
                                    heights = unit(c(.3, max(8,n)/2, 1, .8),
                                                   c("inches", "inches", "null", "inches")),
                                    widths = widths))
    
    left.vp = viewport(layout.pos.row = 1:3, layout.pos.col = 1, name = "left")
    middle.vp = viewport(layout.pos.row = 1:3, layout.pos.col = 3, name = "middle")
    right.vp = viewport(layout.pos.row = 1:3, layout.pos.col = n.cols, name = "right")
    
    trends.head.vp = viewport(layout.pos.row = 1, layout.pos.col = 2,
                              name = "trends.head")
    trendsPanel.vp =
      #viewport(layout = grid.layout(2*n, 1, heights = plots.heights), #%
      viewport(layout = grid.layout(2:3, 1, heights = plots.heights),
               name = "trends.panel", layout.pos.row = 2:3, layout.pos.col = 2)
    trends.bottom.vp = viewport(layout.pos.row = 4, layout.pos.col = 2,
                                name = "trends.bottom")
    trends.vptree = vpTree(trendsPanel.vp, trends.vps)
    
    
    seasons.head.vp = viewport(layout.pos.row = 1:2, layout.pos.col = 4,
                               name = "seasons.head")
    seasonsPanel.vp =
      #viewport(layout = grid.layout(2*n, 1, heights = plots.heights), #%
      viewport(layout = grid.layout(2, 1, heights = plots.heights), 
               name = "seasons.panel", layout.pos.row = 3, layout.pos.col = 4)
    seasons.bottom.vp = viewport(layout.pos.row = 4, layout.pos.col = 4,
                                 name = "seasons.bottom")
    seasons.vptree = vpTree(seasonsPanel.vp, seasons.vps)
    
    final.vptree = vpTree(parent.vp,
                          vpList(trends.head.vp, seasons.head.vp, left.vp,
                                 trends.vptree, middle.vp, seasons.vptree,
                                 right.vp, trends.bottom.vp,
                                 seasons.bottom.vp))
    
    
    
    
    ### Form the gTree
    list.grobs = gList()
    dims = c(9, 7)
    newdevice(width = dims[1], height = dims[2])

      ### Trends part
      #vpName = paste("trendStack", i, sep = "")
      #gapName = paste("trendGap", i, sep = "")
      vpName = "trendStack"
      gapName = "trendGap"
      vpObj = trends.vps[[vpName]]
   
      opar = par(usr = c(vpObj$xscale, vpObj$yscale)

      )
    
    
    x.at = axTicks(1)
    
    ## pretty label customed
    time.data = vars$star[1]:vars$end[1]
    numeric.time= seq(vars$star[1] + vars$star[2]/freq, vars$end[1] + vars$end[2]/freq,
                      len = nrow(vars$data))
    time.data = pretty(time.data)[-1]
    label.id = which(numeric.time %in% time.data)
    x.at = c(x.vals$x,1.2)[c(label.id,FALSE)]
    x.label = numeric.time[label.id]
    ## pretty label customed
    
      par(opar)
    
    for (i in 1:n) {
      
      trend.y.vals = listVars[[i]]$decompVars$components[,"trend"]@.Data
      season.y.vals = listVars[[i]]$decompVars$components[,"seasonal"]@.Data
      
      if (whether.multi)
        joint.y.vals = trend.y.vals * season.y.vals
      else
        joint.y.vals = trend.y.vals + season.y.vals   # * if multiplicative
      
      
      raw.y.vals = listVars[[i]]$decompVars$raw
      ordered.vals = numeric(freq)
      ordered.vals[subset] = season.y.vals[1:freq]
      
      rectName = paste("trend", "border", i, sep = ".")
      trendName = paste("trend", "series", i, sep = ".")
      trendLabelName = paste("trend", "label", i, sep = ".")
      rawName = paste("raw", "series", i, sep = ".")
      trendSeasonName = paste("trendSeason", "series", i, sep = ".")
      yAxisName = paste("trend", "yAxis", i, sep = ".")
      gridlinesName = paste("trend", "gridlines", i, sep = ".")
      labelName = paste("trend", "label", i, sep = ".")
      symbolName = paste("label", "symbol", i, sep = ".")
      y1labelName = paste("y1", "label", 1, sep = ".")
      panelName = "trends.panel"
      
      list.grobs[[rectName]] =
        rectGrob(vp = vpPath("parent", panelName, vpName),
                 name = rectName, gp = gpar(fill=NA))
      
      
      list.grobs[[gridlinesName]] =
        segmentsGrob(x0 = unit(x.at, "native"),
                     y0 = unit(vpObj$yscale[1], "native"),
                     x1 = unit(x.at, "native"),
                     y1 = unit(vpObj$yscale[2], "native"),
                     gp = gpar(col = "#cccccc", lty = "dashed"),
                     vp = vpPath("parent", panelName, vpName),
                     name = gridlinesName)
      
      
      #list.grobs[[trendSeasonName]] =
      #  linesGrob(x = x.vals$x.units,
      #            y = unit(joint.y.vals, "native"),
      #            vp = vpPath("parent", panelName, vpName),
      #            gp = gpar(col = trendSeasonCol),
      #            name = trendSeasonName)
      
      list.grobs[[rawName]] =
        linesGrob(x = x.vals$x.units,
                  y = unit(raw.y.vals, "native"),
                  vp = vpPath("parent", panelName, vpName),
                  gp = gpar(col = groupCol[i], lwd = 2), name = rawName)
      
      list.grobs[[trendName]] =
        linesGrob(x = x.vals$x.units,
                  y = unit(trend.y.vals, "native"),
                  vp = vpPath("parent", panelName, vpName),
                  gp = gpar(col = groupCol[i], lty = 3, lwd = 3), name = trendName)
      
      list.grobs[[trendLabelName]] =
        textGrob(varNames[i], x = x.vals$x.units[length(x.vals$x.units)],
                 y = unit(trend.y.vals, "native")[length(x.vals$x.units)],
                 hjust = -0.2, 
                 vjust = 2, 
                 #just = c("left", "bottom"),
                 vp = vpPath("parent", panelName, vpName),
                 gp = gpar(col = groupCol.text[i], cex = 0.6), name = trendLabelName)
      
      list.grobs[[symbolName]] =
        pointsGrob(x = x.vals$x.units[length(x.vals$x.units)],
                 y = unit(trend.y.vals, "native")[length(x.vals$x.units)],
                 pch = i, size = unit(0.5,"char"),
                 vp = vpPath("parent", panelName, vpName),
                 gp = gpar(col = groupCol[i], cex = 2, lwd= 3), name = symbolName)
      
      list.grobs[[yAxisName]] =
        yaxisGrob(vp = vpPath("parent", panelName, vpName),
                  gp = gpar(cex = 0.8), name = yAxisName)
      

      
      list.grobs$xAxis1 =
        xaxisGrob(vp = vpPath("parent", "trends.panel",
                              "trendStack"),
                  at = x.at,
                  label = x.label,
                  gp = gpar(cex = 0.8), name = "xAxis1")
      
      list.grobs$xAxisLabel1 =
        textGrob("Time", vp = vpPath("parent", "trends.bottom"),
                 name = "xAxisLabel1", gp = gpar(cex = 0.8),
                 y = unit(3, "mm"), vjust = 0)
      
      if (i == 1)  {
        if(nchar(ylab)>8)
          list.grobs[[y1labelName]] =
          textGrob(ylab, x = 0, rot= 90, vjust = -5, 
                   vp = vpPath("parent", panelName, vpName),
                   just = c("right","center"),
                   gp = gpar(cex = 0.8),
                   name = y1labelName)
        else
          list.grobs[[y1labelName]] =
          textGrob(ylab, x = 0, rot= 90, vjust = -5, 
                   vp = vpPath("parent", panelName, vpName),
                   gp = gpar(cex = 0.8),
                   name = y1labelName)
      }
      
      #list.grobs[[labelName]] =
      #  textGrob(varNames[i], x = unit(30*(i-1), "mm"), y = unit(1, "mm"),
      #           just = c("left", "bottom"),
      #           vp = vpPath("parent", panelName, gapName),
      #           gp = gpar(cex = 0.8, fontface = "bold.italic", col = groupCol[i]),
      #           name = labelName)
    }
      
      #################
      #################
      
      ### Seasonal part
      

      vpName = "seasonStack"
      gapName = "seasonGap"
      vpObj = seasons.vps[[vpName]]
      x.at2 = unique(round(pretty(1:freq)))
      x.at2 = x.at2[x.at2 > vpObj$xscale[1] & x.at2 < vpObj$xscale[2]]
    
    for (i in 1:n) {  
      rectName = paste("season", "border", i, sep = ".")
      lineName = paste("season", "series", i, sep = ".")
      y0Name = paste("season", "0_line", i, sep = ".")
      pointsName = paste("season", "points", i, sep = ".")
      yAxisName = paste("season", "yAxis", i, sep = ".")
      gridlinesName = paste("season", "gridlines", i, sep = ".")
      labelName = paste("season", "label", i, sep = ".")
      SymbolName = paste("season", "symbol", i, sep = ".")
      panelName = "seasons.panel"
      
      trend.y.vals = listVars[[i]]$decompVars$components[,"trend"]@.Data
      season.y.vals = listVars[[i]]$decompVars$components[,"seasonal"]@.Data
      
      if (whether.multi)
        joint.y.vals = trend.y.vals * season.y.vals
      else
        joint.y.vals = trend.y.vals + season.y.vals   # * if multiplicative
      
      
      raw.y.vals = listVars[[i]]$decompVars$raw
      ordered.vals = numeric(freq)
      ordered.vals[subset] = season.y.vals[1:freq]
      
      list.grobs[[gridlinesName]] =
        segmentsGrob(x0 = unit(x.at2, "native"),
                     y0 = unit(vpObj$yscale[1], "native"),
                     x1 = unit(x.at2, "native"),
                     y1 = unit(vpObj$yscale[2], "native"),
                     gp = gpar(col = "#cccccc", lty = "dashed"),
                     vp = vpPath("parent", panelName, vpName),
                     name = gridlinesName)
      
      ### Have all observed seasonal effects appear in behind
      ### Only valid if frequency is greater than 1
    
      s = listVars[[i]]$start[2]
      n.obs = length(listVars[[i]]$tsObj)
      r = n.obs - (freq + 1 - s)
      numSeries = 1 + r %/% freq + (r %% freq > 0)
      ind1 = 1
      ind2 = length(s:freq)
      xcoords = rep(1:freq, length = n.obs + s - 1)[s:(n.obs + s - 1)]
      if (whether.multi)
        deTrend = raw.y.vals / trend.y.vals
      else
        deTrend = raw.y.vals - trend.y.vals
      
      
      
      #deTrend = raw.y.vals - trend.y.vals
      #for (j in 1:numSeries) {
      #  ind = ind1:ind2
      #  greyLineName = paste("actualSeason", i, j, sep = ".")
      #  list.grobs[[greyLineName]] =
      #    linesGrob(x = unit(xcoords[ind], "native"),
      #              y = unit(deTrend[ind], "native"),
      #              vp = vpPath("parent", panelName, vpName),
      #              name = greyLineName,
      #              gp = gpar(col = "#B8B8B8"))
      #  ind1 = ind2 + 1
      #  ind2 = min(ind1 + freq - 1, n.obs)
        
      #}
      
      list.grobs[[rectName]] =
        rectGrob(vp = vpPath("parent", panelName, vpName),
                 name = rectName, gp = gpar(fill=NA))
      
      list.grobs[[y0Name]] =
        linesGrob(x = unit(vpObj$xscale, "native"),
                  y = unit(rep(ifelse(whether.multi,1,0), 2), "native"),
                  gp = gpar(lty = "dashed"),
                  vp = vpPath("parent", panelName, vpName),
                  name = y0Name)
      
      
      
      list.grobs[[lineName]] =
        linesGrob(x = unit(1:freq, "native"),
                  y = unit(ordered.vals, "native"),
                  vp = vpPath("parent", panelName, vpName),
                  gp = gpar(col = groupCol[i], lwd = 2),
                  name = lineName)
      
      list.grobs[[pointsName]] =
        pointsGrob(x = unit(1:freq, "native"),
                   y = unit(ordered.vals, "native"),
                   vp = vpPath("parent", panelName, vpName),
                   gp = gpar(col = groupCol[i], cex = 0.7, lwd = 2),
                   name = pointsName)
      
      list.grobs[[yAxisName]] =
        yaxisGrob(vp = vpPath("parent", panelName, vpName),
                  gp = gpar(cex = 0.8), name = yAxisName)
      
      #list.grobs[[labelName]] =
      #  textGrob(varNames[i], x = unit(30*(i-1), "mm"), y = unit(1, "mm"),
      #           just = c("left", "bottom"),
      #           vp = vpPath("parent", panelName, gapName),
      #           gp = gpar(cex = 0.8, fontface = "bold.italic", col = groupCol[i]),
      #           name = labelName)
      
      
      effects <- ifelse(multiplicative, "Multiplicative Seasonal effects", "Additive Seasonal effects")
      list.grobs$seasonsLabel =
        textGrob(effects, y = 0, vp = vpPath("parent", "seasons.head"),
                 just = "bottom",
                 name = "seasonsLabel")
     
      list.grobs[[SymbolName]] =
      pointsGrob(x= 0, y = 0.5*(n-i)/(2*n) + 0.5,
        pch = i, size = unit(0.5,"char"),
        vp = vpPath("parent", "seasons.head"),
        gp = gpar(col = groupCol[i]),
        name = SymbolName)
      
      list.grobs[[labelName]] =
        textGrob(varNames[i], x= 0.1, y = 0.5*(n-i)/(2*n) + 0.5, 
                 vp = vpPath("parent", "seasons.head"),
                 just = c("left","center"),
                 gp = gpar(cex = 0.8,col = groupCol.text[i]),
                 name = labelName)
      

      if (freq == 12) {
        labs = substring(month.abb, 1, 1)
        xlab = "Month"
      }
      else if (freq == 4) {
        labs = paste(month.abb[c(1, 4, 7, 10)],
                     month.abb[c(3, 6, 9, 12)],
                     sep = " - ")
        xlab = "Quarter"
      }
      else if (freq == 7) {
        labs = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
        xlab = "Day"
      }
      else {
        labs = 1:freq
        xlab = "Season"
      }
      
      list.grobs$xAxis2 =
        xaxisGrob(vp = vpPath("parent", "seasons.panel",
                              "seasonStack"),
                  gp = gpar(cex = 0.8), name = "xAxis2",
                  at = 1:freq, label = labs)
      list.grobs$xAxisLabel2 =
        textGrob(xlab, vp = vpPath("parent", "seasons.bottom"),
                 name = "xAxisLabel2", gp = gpar(cex = 0.8),
                 y = unit(3, "mm"), vjust = 0)
    }
    

    
    
    
    
    
    
    ### Add a legend above the left panel
    xc = unit(2, "mm")
    gap = unit(2, "mm")
    space = unit(8, "mm")
    lineWidth = unit(6, "mm")
    
    list.grobs$trendKey =
      linesGrob(x = unit.c(xc, xc + lineWidth*1.5),
                y = unit(1, "npc") - unit(0.6, "lines"),
                vp = vpPath("parent", "trends.head"),
                name = "trendKey",
                gp = gpar(col = trendCol, lwd = 2, lty = 3))
    
    xc = xc + lineWidth + gap
    list.grobs$trendKeyText =
      textGrob("Trend", x = xc, y = unit(1, "npc") - unit(1, "lines"),
               just = c("left", "bottom"),
               vp = vpPath("parent", "trends.head"),
               name = "trendKeyText", gp = gpar(cex = .8))
    
    list.grobs$titleText = 
      textGrob(paste0("Comparing series ", 
                      ifelse(ylab=="", "", ylab),
                      ifelse(whether.multi, " --- Multiplicative", "")), 
               x = 0.3, y = unit(1, "npc") - unit(1, "lines"),
               just = c("left", "bottom"),
               vp = vpPath("parent", "trends.head"),
               name = "titleText", gp = gpar(cex = 1))
    
    #xc = xc + stringWidth(list.grobs$trendKeyText$label) + space
    #list.grobs$rawKey =
    #  linesGrob(x = unit.c(xc, xc + lineWidth),
    #            y = unit(1, "npc") - unit(0.6, "lines"),
    #            vp = vpPath("parent", "trends.head"),
    #            name = "rawKey",
    #            gp = gpar(col = rawCol, lwd = 2))
    
    #xc = xc + lineWidth + gap
    #list.grobs$rawKeyText =
    #  textGrob("Raw data", x = convertUnit(xc, "mm"),
    #           y = unit(1, "npc") - unit(1, "lines"),
    #           just = c("left", "bottom"),
    #           vp = vpPath("parent", "trends.head"),
    #           name = "rawKeyText", gp = gpar(cex = .8))
    
    #xc = xc + stringWidth(list.grobs$rawKeyText$label) + space
    #list.grobs$trendSeasonKey =
    #  linesGrob(x = unit.c(xc, xc + lineWidth),
    #            y = unit(1, "npc") - unit(0.6, "lines"),
    #            vp = vpPath("parent", "trends.head"),
    #            name = "trendSeasonKey",
    #            gp = gpar(col = trendSeasonCol, lwd = 2))
    
    #xc = xc + lineWidth + gap
    #trendandseason <- ifelse(multiplicative, "Trend * seasonal", "Trend + seasonal")
    #list.grobs$trendSeasonKeyText =
    #  textGrob(trendandseason, x = convertUnit(xc, "mm"),
    #           y = unit(1, "npc") - unit(1, "lines"),
    #           just = c("left", "bottom"),
    #           vp = vpPath("parent", "trends.head"),
    #           name = "trendSeasonKeyText", gp = gpar(cex = .8))
    
    image = gTree(name = "image", children = list.grobs,
                  childrenvp = final.vptree)
    
    drawImage(image)
  }



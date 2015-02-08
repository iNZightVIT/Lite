recompose <-
  function(vars.decomp, animate = TRUE,e = NULL) {
    
    # the e argument to support animation stop 
    if (is.null(e)) {
      e <- new.env()
      e$stopAnimation <- FALSE
    }
    
    vars.decomp = add.line.plots.vp(vars.decomp, "trend", lineCol = "blue")
    vars.decomp = add.line.plots.vp(vars.decomp, "season")
    
    
    tree = vars.decomp$decompVars$tree
    tree = removeGrob(tree, "rawKey")
    tree = removeGrob(tree, "trendKey")
    tree = removeGrob(tree, "rawKeyText")
    
    
    if (animate)
      drawImage(tree)
    topLabel = getGrob(tree, "statusText")
    trendLabel = getGrob(tree, "trendLabel")
    
    
    trend.attr = get.line.coords(vars.decomp, "trend", "trendLine")
    topLabel = editGrob(topLabel, label = "Recomposing...")
    trendLabel = editGrob(trendLabel, label = "Adding seasonal swing to trend...")
    tree = addGrob(tree, topLabel)
    tree = addGrob(tree, trendLabel)
    vars.decomp$decompVars$tree = tree
    vars.decomp = shiftLineUp(vars.decomp, "season", trend.attr$line.y.parent,
                              n.steps = if (animate) 1 else 30, animate = animate, e=e)
    vars.decomp = add.seasonal(trend.attr, vars.decomp, animate = animate,e=e)
    

    
    if (! get("stopAnimation", envir = e) && animate)
      drawImage(vars.decomp$decompVars$tree)
    
    
    trendSeason = getGrob(vars.decomp$decompVars$tree, "trendSeason")
    trendSeason.y.parent = trendSeason$y

    trendSeason.y.parent = convertUnit(trendSeason.y.parent,
                                       attr(trendSeason.y.parent[1], "unit"),
                                       valueOnly = TRUE)
    
    
    trendLabel = editGrob(trendLabel, label = "Adding residuals to trend+seasonal swing...")
    vars.decomp = add.line.plots.vp(vars.decomp, "random")
    tree = vars.decomp$decompVars$tree
    tree = addGrob(tree, topLabel)
    tree = addGrob(tree, trendLabel)
    vars.decomp$decompVars$tree = tree
  
    vars.decomp = shiftLineUp(vars.decomp, "random", 
                              #trend.attr$line.y.parent,
                              trendSeason.y.parent,
                              n.steps = if (animate) 1 else 30, animate = animate, e=e)
    vars.decomp = add.random(trend.attr, vars.decomp, animate = animate,e=e)
    
    data.name <- vars.decomp$decompVars$data.name
    topLabel = editGrob(topLabel, label = paste0("Recomposed ",data.name, ":", vars.decomp$currVar))
    tree = vars.decomp$decompVars$tree
    tree = rmGrobs(tree, c("random.copy", "random.y0.copy"))
    tree = addGrob(tree, topLabel)
    tree = removeGrob(tree, "trendLabel")
    
    
    
    
    ### we need to rename the line grobs!
    
    
    real.trend = getGrob(tree, "trend.copy")
    trend.season = getGrob(tree, "trendLine")
    recomposed = getGrob(tree, "trendSeason")
    real.trend = editGrob(real.trend, name = "trendLine")
    trend.season = editGrob(trend.season, name = "trendSeason")
    recomposed = editGrob(recomposed, name = "recomposed")
    tree = removeGrob(tree, "trend.copy")
    tree = addGrob(tree, real.trend)
    tree = addGrob(tree, trend.season)
    tree = addGrob(tree, recomposed)
    
    
    
    
    
    
    ### Need to add a "legend" now that animation had finished
    
    
    rawKey = linesGrob(x = unit(c(2, 8), "mm"),
                       y = unit(1, "npc") - unit(0.6, "lines"),
                       vp = vpPath("parent", "plots", "trend"),
                       name = "rawKey",
                       gp = gpar(col = recomposed$gp$col, lwd = 2))
    rawKeyText = textGrob(x = unit(10, "mm"),
                          y = unit(1, "npc") - unit(1, "lines"),
                          label = "Raw data", just = c("left", "bottom"),
                          vp = vpPath("parent", "plots", "trend"),
                          name = "rawKeyText",
                          gp = gpar(cex = .9, fontface = "bold"))
    stringw = stringWidth(rawKeyText$label)
    trendKey = linesGrob(x = unit(c(19, 25), "mm") + stringw,
                         y = unit(1, "npc") - unit(0.6, "lines"),
                         vp = vpPath("parent", "plots", "trend"),
                         name = "trendKey",
                         gp = gpar(col = real.trend$gp$col, lwd = 2))
    trendKeyText = textGrob(x = unit(27, "mm") + stringw,
                            y = unit(1, "npc") - unit(1, "lines"),
                            label = "Trend", just = c("left", "bottom"),
                            vp = vpPath("parent", "plots", "trend"),
                            name = "trendKeyText",
                            gp = gpar(cex = .9, fontface = "bold"))
    
    
    stringw = stringw + stringWidth(trendKeyText$label)
    trendSeasonKey = linesGrob(x = unit(c(36, 42), "mm") + stringw,
                               y = unit(1, "npc") - unit(0.6, "lines"),
                               vp = vpPath("parent", "plots", "trend"),
                               name = "trendSeasonKey",
                               gp = gpar(col = trend.season$gp$col))
    #trendseasonTXT <- ifelse(vars.decomp$decompVars$multiplicative, "Trend * Seasonal Swing", "Trend + Seasonal Swing")
    trendseasonTXT = "Trend + Seasonal Swing"
    trendSeasonKeyText = textGrob(x = unit(44, "mm") + stringw,
                                  y = unit(1, "npc") - unit(1, "lines"),
                                  label = trendseasonTXT,
                                  just = c("left", "bottom"),
                                  vp = vpPath("parent", "plots", "trend"),
                                  name = "trendSeasonKeyText",
                                  gp = gpar(cex = .9, fontface = "bold"))
    
    
    tree = addGrob(tree, rawKey)
    tree = addGrob(tree, rawKeyText)
    tree = addGrob(tree, trendKey)
    tree = addGrob(tree, trendKeyText)
    tree = addGrob(tree, trendSeasonKey)
    tree = addGrob(tree, trendSeasonKeyText)
    
    
    drawImage(tree)
    vars.decomp$decompVars$tree = tree
    vars.decomp
  }






shiftLineUp <-
  function(vars.decomp, vpName, destination.y.parent, n.steps = 15, animate = TRUE, e=tsenv) {
    lineGrobName = paste(vpName, "Line", sep = "")
    line.attr = get.line.coords(vars.decomp, vpName, lineGrobName)
    
    
    y0.attr = get.line.coords(vars.decomp, vpName, paste(vpName, ".y0", sep = ""))
    y0.copy = getGrob(vars.decomp$decompVars$tree, paste(vpName, ".y0", sep = ""))
    y0.new = linesGrob(unit(y0.attr$x.parent[-c(1, length(y0.attr$x.parent))], "npc"),
                       unit(y0.attr$line.y.parent[-(1:2)], "npc"),
                       name = paste(vpName, ".y0", ".copy", sep = ""),
                       vp = vpPath("parent", "plots"))
    y0.new = editGrob(y0.new, gp = y0.copy$gp)
    
    
    updated.tree = addGrob(vars.decomp$decompVars$tree, y0.new)
    
    
    start = y0.attr$line.y.parent
    end = destination.y.parent
    step.size = (end[1] - start[1]) / n.steps
    
    
    for (i in 1:n.steps) {
      if (get("stopAnimation", envir = e) && i < n.steps)
        next
      line.copy = getGrob(updated.tree, paste(vpName, ".copy", sep = ""))
      line.copy2 = editGrob(line.copy, y = unit(line.attr$line.y.parent + i*step.size, "npc"))
      y0.new = editGrob(y0.new, y = unit(start + i*step.size, "npc"))
      updated.tree = addGrob(updated.tree, line.copy2)
      updated.tree = addGrob(updated.tree, y0.new)
      if (! get("stopAnimation", envir = e) && animate)
        drawImage(updated.tree)
    }
    
    
    vars.decomp$decompVars$tree = updated.tree
    vars.decomp
  }








add.seasonal <-
  function(trend.attr, vars.decomp, animate = TRUE, e= tsenv) {
    decomp = vars.decomp$decompVars
    season.copy = getGrob(decomp$tree, "season.copy")
    y0.copy = getGrob(decomp$tree, "season.y0.copy")
    trendLine = getGrob(decomp$tree, "trendLine")
    season.attr = get.line.coords(vars.decomp, "season", "seasonLine")
    #if (decomp$multiplicative)
    #  new.y = exp(log(trend.attr$line.y) + log(season.attr$line.y))
    #else
      new.y = trend.attr$line.y + season.attr$line.y

    
    new.y.npc = (new.y - trend.attr$line.vp.yrange[1]) / diff(trend.attr$line.vp.yrange)
    new.y.parent = decomp$props["trend"] * new.y.npc +
      decomp$props["seasonal"] + decomp$props["remainder"]
    
    
    n.points = length(decomp$raw)
    trendLine = editGrob(trendLine, gp = list(col = "#0e8c07", lwd = 1))
    trend.copy = getGrob(decomp$tree, "trend.copy")
    trend.copy = editGrob(trend.copy, gp = list(lwd = 2))
    updated.tree = addGrob(decomp$tree, trend.copy)
    
    
    start.seq <- if (animate) 1 else n.points
    for (i in start.seq:n.points) {
      if (get("stopAnimation", envir = e) && i < n.points)
        next
      
      trendLine = editGrob(trendLine,
                           x = unit(trend.attr$x.parent[1:i], "npc"),
                           y = unit(new.y.parent[1:i], "npc"),
                           vp = vpPath("parent", "plots"))
      updated.tree = addGrob(updated.tree, trendLine)
      
      
      if (i < n.points) {
        season.newY = season.copy$y[-1]
        y0.newY = y0.copy$y[-1]
        if (i > 1) {
          deltaY = diff(trend.attr$line.y.parent[(i-1):i])
          season.newY = season.newY + unit(deltaY, "npc")
          y0.newY = y0.newY + unit(deltaY, "npc")
          season.copy = editGrob(season.copy, y = season.newY,
                                 x = season.copy$x[-1])
          y0.copy = editGrob(y0.copy, y = y0.newY, x = y0.copy$x[-1])
          updated.tree = addGrob(updated.tree, season.copy)
          updated.tree = addGrob(updated.tree, y0.copy)
        }
      } else if (animate) {
        updated.tree = removeGrob(updated.tree, "season.copy")
        updated.tree = removeGrob(updated.tree, "season.y0.copy")
      }
      
      
      #drawImage(updated.tree)
      if (animate)
        pauseImage(updated.tree, 2)
    }
    
    
    ### copy the trend+seasonal line
    trendSeason = getGrob(updated.tree, "trendLine")
    trendSeason$name = "trendSeason"
    updated.tree = addGrob(updated.tree, trendSeason)
    
    
    updated.tree <- rmGrobs(updated.tree, c("season.copy", "season.y0.copy"))
    drawImage(rmGrobs(updated.tree, c("random.copy", "random.y0.copy")))
    
    
    vars.decomp$decompVars$tree = updated.tree
    vars.decomp
  }








add.random <-
  function(trend.attr, vars.decomp, animate = TRUE, e=tsenv) {
    decomp = vars.decomp$decompVars
    random.copy = getGrob(decomp$tree, "random.copy")
    y0.copy = getGrob(decomp$tree, "random.y0.copy")
    trendSeason = getGrob(decomp$tree, "trendSeason")
    random.attr = get.line.coords(vars.decomp, "random", "randomLine")

    
    new.y = decomp$raw
    new.y.npc = (new.y - trend.attr$line.vp.yrange[1]) / diff(trend.attr$line.vp.yrange)
    new.y.parent = decomp$props["trend"] * new.y.npc +
      decomp$props["seasonal"] + decomp$props["remainder"]
    
    
    updated.tree = decomp$tree
    trendSeason = editGrob(trendSeason, gp = list(col = "black", lwd = 2))
    
    
    
    
    ### need to 'centre' random line using the trendSeason line points as the origin
    rand.npc.scale = random.attr$line.vp.yrange / diff(random.attr$line.vp.yrange)    
    adjustment =  (random.attr$line.y.npc + rand.npc.scale[1]) * decomp$props["remainder"]
    
    
    ### In order to make the shiftlineup and add.random pattern consistent, 
    ### we divide 30 here to fix the proportion problem. 
    ### 30  is kind of guess, the more deeper change here have to think about the
    ### range, proportion method for drawing the line, the line here is drawing by
    ### unit(...,"npc") way not the actual coordination.
    #if (decomp$multiplicative){
      
    #  adjustment = adjustment / 30
    #}
    
    n.points = length(decomp$raw)
    start.seq <- if (animate) 1 else n.points + 1
    for (i in start.seq:(n.points+1)) {
      if (get("stopAnimation", envir = e) && i <= n.points)
        next
      if (i > 1) {
        trendSeason2 = editGrob(trendSeason,
                                x = unit(trend.attr$x.parent[1:(i-1)], "npc"),
                                y = unit(new.y.parent[1:(i-1)], "npc"),
                                vp = vpPath("parent", "plots"))
        updated.tree = addGrob(updated.tree, trendSeason2)
        if (animate)
          drawImage(updated.tree)
      }
      
      
      if (i > 1 & i < n.points) {
        deltaY = trendSeason$y[i] - random.copy$y[2] + unit(adjustment[i], "npc")
        newY = convertUnit(random.copy$y[-1] + deltaY, "npc")
        y0.newY = convertUnit(y0.copy$y[-1] + deltaY, "npc")
        
        
        random.copy = editGrob(random.copy, y = newY, x = random.copy$x[-1])
        updated.tree = addGrob(updated.tree, random.copy)
        y0.copy = editGrob(y0.copy, y = y0.newY, x = y0.copy$x[-1])
        updated.tree = addGrob(updated.tree, y0.copy)
      }
      if (i == n.points) {
        updated.tree = removeGrob(updated.tree, "random.copy")
        updated.tree = removeGrob(updated.tree, "random.y0.copy")
      }
      
      
      if (! get("stopAnimation", envir = e) && i > 1 & animate) {
        pauseImage(updated.tree, 10)
      }
    }
    
    
    updated.tree <- rmGrobs(updated.tree, c("random.copy", "random.y0.copy"))
    drawImage(updated.tree)
    
    
    vars.decomp$decompVars$tree = updated.tree
    vars.decomp
  }

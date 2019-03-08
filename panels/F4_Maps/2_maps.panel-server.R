###---------------------------------------------------###
###  Server Functions for the "Maps" Module  ###
###---------------------------------------------------###
###
###  Date Created  : Feb 22, 2017.
###  Last Modified : May 20, 2018.
###  
###
###  * Note: This is to be sourced within "server.R" *



## initialize gui
output$newmaps.panel = renderUI({
  get.data.set()
  newmaps.panel.ui(get.data.set())
})

## parameters for plotting maps
args = reactiveValues(
  x = NULL,
  varnames = list(colby = NULL,
                  sizeby = NULL,
                  g1 = NULL,
                  g2 = NULL),
  colby = NULL,
  sizeby = NULL,
  opacity = NULL,
  reverse.opacity = NULL,
  type = NULL,
  col.pt = "mediumvioletred",
  cex.pt = NULL,
  alpha = NULL,
  join = NULL,
  col.line = NULL,
  g1 = NULL,
  g1.level = NULL,
  g2 = NULL,
  g2.level = NULL,
  variable = NULL,
  col = NULL,
  col.fun = NULL,
  na.fill = NULL,
  name = NULL,
  main = NULL,
  cex = NULL,
  resizemethod = NULL,
  pch = NULL,
  symbolby = NULL,
  lwd.pt = NULL,
  reverse.palette = NULL,
  col.method = NULL
)


colourPalettes =
  list(cat = c(
    #      if (.rcb)
    list("contrast (max 8)" =
           function(n)
             if (n > 8) inzpar()$col.default$cat(n)
         else RColorBrewer::brewer.pal(n, "Set2")[1:n],
         "bright (max 9)" =
           function(n)
             if (n > 9) inzpar()$col.default$cat(n)
         else RColorBrewer::brewer.pal(n, "Set1")[1:n],
         "light (max 12)" =
           function(n)
             if (n > 12) inzpar()$col.default$cat(n)
         else RColorBrewer::brewer.pal(n, "Set3")[1:n]),
    #      if (.viridis)
    list(viridis = viridis::viridis,
         magma = viridis::magma,
         plasma = viridis::plasma,
         inferno = viridis::inferno),
    list("Colourblind Friendly" = inzpar()$col.default$cat,
         'rainbow (hcl)' = function(n) hcl((1:n) / n * 360, c = 80, l = 50))
  ),
  cont = c(
    #      if (.viridis)
    list(viridis = viridis::viridis,
         magma = viridis::magma,
         plasma = viridis::plasma,
         inferno = viridis::inferno),
    list(
      'rainbow (hcl)' = function(n) hcl((1:n) / n * 320 + 60, c = 100, l = 50),
      blue =
        function(n) sequential_hcl(n, h = 260, c. = c(80, 10), l = c(30, 95), power = 0.7),
      green =
        function(n) sequential_hcl(n, h = 135, c. = c(50, 10), l = c(40, 95), power = 0.4),
      red =
        function(n) sequential_hcl(n, h = 10, c. = c(80, 10), l = c(30, 95), power = 0.7),
      "green-yellow" =
        function(n) terrain_hcl(n, h = c(130, 30), c. = c(65, 0), l = c(45, 90),
                                power = c(0.5, 1.5)),
      "red-blue" =
        function(n) terrain_hcl(n, h = c(0, -100), c. = c(80, 40), l = c(40, 75),
                                power = c(1, 1)),
      terrain = terrain_hcl,
      heat = heat_hcl,
      "blue/white/pink" =
        function(n) diverge_hcl(n, h = c(180, 330), c = 59, l = c(75, 95), power = 1.5),
      "blue/white/red" =
        function(n) diverge_hcl(n, h = c(260, 0), c = 100, l = c(50, 90), power = 1))
  ),
  emphasize = function(n, k, cat = TRUE, ncat = 5,
                       fn = if (cat) inzpar()$col.default$cat else inzpar()$col.default$cont) {
    cols <- fn(n)
    if (!cat) {
      ks <- floor(seq(1, n, length = ncat + 1))
      k <- ks[k]:ks[k+1]
    }
    #cols[k] <- iNZightPlots:::shade(cols[k], -0.4)
    cols[-k] <- iNZightPlots:::shade(cols[-k], 0.7)
    cols
  })


plot.args = reactive({
  plot.args = modifyList(list(), reactiveValuesToList(args), keep.null = FALSE)
})




## arguments for a new version of map module (Regions)
args2 = reactiveValues(
  
  updateplot = 0,
  updateplot1 = 0,
  
  combinedData = NULL,
  mapData = NULL,
  
  mapName = "",
  mapType = NULL,
  mapVars = NULL,
  mapSizeVar = NULL,
  mapSequenceVar = NULL,
  
  match.list = NULL,
#  has.multipleobs = FALSE,
  
  plotTitle = "",
  plotAxes = FALSE,
  plotXLab = "Longitude",
  plotYLab = "Latitude",
  plotDatumLines = FALSE,
  plotProjection = NULL,
  plotTheme = FALSE,
  plotPalette = "Default",
  plotConstantAlpha = 1.0,
  plotConstantSize = 2.0,
  plotCurrentSeqVal = NULL,
  timer = NULL,
  
  multipleObsOption = NULL,
  plotSparklinesType = "Absolute",
  plotScaleLimits = NULL, 
  plotLabelVar = NULL,
  
  plotLabelScale = 4,
  plotAxisScale = 11, 

  mapRegionsPlot = NULL,
  mapExcludedRegions = TRUE,
  
  proj.df = iNZightMaps::iNZightMapProjections()
)


plot.args2 = reactive({
  plot.args2 = modifyList(list(), reactiveValuesToList(args2), keep.null = FALSE)
})



## maps plot
#output$maps_plot = renderPlot({
#  if(input$map_type == 1) {
#    condition1 = !is.null(input$select_latitude) &&
#      input$select_latitude %in% colnames(get.data.set()) &&
#      !is.null(input$select_longitude) &&
#      input$select_longitude %in% colnames(get.data.set())
#    if(condition1) {
#      temp = plot.args()
#      tryCatch({do.call(plot, temp)}, 
#               error = function(e) {
#                 print(e)
#               }, finally = {})
#    }
#  }
#  else if(input$map_type == 2) {
#    condition2 = !is.null(input$maplocation) &&
#      input$maplocation != "Select Map Location" && 
#      !is.null(input$locationvariable) &&
#      input$locationvariable %in% colnames(get.data.set()) &&
#      !is.null(input$plottingvariable) &&
#      input$plottingvariable %in% colnames(get.data.set())
#    if(condition2) {
#      temp = plot.args()
#      tryCatch({do.call(plot, temp)}, 
#               error = function(e) {
#                 print(e)
#               }, finally = {})
#    }
#  }
#})


## get only numeric type variables
numericVars = function() {
  temp = get.data.set()
  colnames(temp)[sapply(temp, is.numeric)]
}

## get only character type variables
characterVars = function() {
  temp = get.data.set()
  colnames(temp)[!sapply(temp, is.numeric)]
}



## set up latitude_panel
output$latitude_panel = renderUI({
  get.data.set()
  isolate({
    sel = ""
    if("latitude" %in% colnames(get.data.set()))
      sel = "latitude"
    else if("Latitude" %in% colnames(get.data.set()))
      sel = "Latitude"
    get.vars = parseQueryString(session$clientData$url_search)
    if(!is.null(get.vars$url)) {
      temp = session$clientData$url_search
      get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
    }
    if(length(get.vars) > 0 &&
       (any(names(get.vars) %in% "url") ||
        any(names(get.vars) %in% "example")) &&
       (any(names(get.vars) %in% "latitude") &&
        !get.vars$latitude %in% "")){
      sel = get.vars$latitude
    }
    if(length(get.vars) > 0 &&
       (any(names(get.vars) %in% "url") ||
        any(names(get.vars) %in% "example")) &&
       (any(names(get.vars) %in% "Latitude") &&
        !get.vars$Latitude %in% "")) {
      sel = get.vars$Latitude
    }
    selectInput(inputId = "select_latitude",
                label = "Latitude:",
                choices = c("Select Latitude Information", colnames(get.data.set())),
                selected = sel,
                selectize = FALSE)
  })
})

## set up longitude_panel
output$longitude_panel = renderUI({
  get.data.set()
  isolate({
    sel = ""
    if("longitude" %in% colnames(get.data.set()))
      sel = "longitude"
    else if("Longitude" %in% colnames(get.data.set()))
      sel = "Longitude"
    get.vars = parseQueryString(session$clientData$url_search)
    if(!is.null(get.vars$url)) {
      temp = session$clientData$url_search
      get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
    }
    if(length(get.vars)>0 &&
       (any(names(get.vars) %in% "url")||
        any(names(get.vars) %in% "example"))&&
       (any(names(get.vars) %in% "longitude")&&
        !get.vars$longitude %in% "")){
      sel=get.vars$longitude
    }
    if(length(get.vars)>0 &&
       (any(names(get.vars) %in% "url")||
        any(names(get.vars) %in% "example"))&&
       (any(names(get.vars) %in% "Longitude")&&
        !get.vars$Longitude %in% "")){
      sel=get.vars$Longitude
    }
    selectInput(inputId = "select_longitude",
                label = "Longitude:",
                choices = c("Select Longitude Information", colnames(get.data.set())),
                selected = sel,
                selectize = FALSE)
  })
})

## update mapsplot.obj when input$map_type == 1
observe({
  input$select_latitude
  input$select_longitude
  isolate({
    if(input$map_type == 1 &&
       !is.null(input$select_latitude) &&
       input$select_latitude %in% colnames(get.data.set()) &&
       !is.null(input$select_longitude) &&
       input$select_longitude %in% colnames(get.data.set())) {
      mapsplot.obj = iNZightMaps::iNZightMap(lat = eval(parse(text = paste("~", input$select_latitude))),
                                             lon = eval(parse(text = paste("~", input$select_longitude))),
                                             data = get.data.set(),
                                             name = get.data.name())
      args$x = mapsplot.obj
    }
  })
})




## update args$colby, args$sizeby, args$opacifyby
observe({
  input$colourby
  isolate({
    if(!is.null(input$colourby) &&
       input$colourby %in% colnames(get.data.set())) {
      temp.data = get.data.set()
      index = which(colnames(temp.data) == input$colourby)
      args$colby = temp.data[, index]
      args$varnames$colby = input$colourby
    }
    else if(!is.null(input$colourby) &&
            input$colourby == "") {
      args$colby = NULL
      args$varnames$colby = NULL
    }
  })
})

observe({
  input$sizeby
  isolate({
    if(!is.null(input$sizeby) &&
       input$sizeby %in% colnames(get.data.set())) {
      temp.data = get.data.set()
      index = which(colnames(temp.data) == input$sizeby)
      args$sizeby = temp.data[, index]
      args$varnames$sizeby = input$sizeby
    }
    else if(!is.null(input$sizeby) &&
            input$sizeby == "") {
      args$sizeby = NULL
      args$varnames$sizeby = NULL
    }
  })
})

observe({
  input$opacifyby
  isolate({
    if(!is.null(input$opacifyby) &&
       input$opacifyby %in% colnames(get.data.set())) {
      temp.data = get.data.set()
      index = which(colnames(temp.data) == input$opacifyby)
      args$opacity = input$opacifyby
    }
    else if(!is.null(input$opacifyby) &&
            input$opacifyby == "") {
      args$opacity = NULL
    }
  })
})







## set up plot_maptype_panel and plot_colour_panel
output$plot_maptype_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "plot_maptype",
                label = "Map type:",
                choices = c("terrain", "terrain-background", "toner-lite", "toner"),
                selectize = FALSE)
  })
})



## set up plot_colourcheck_panel
output$plot_colourcheck_panel = renderUI({
  get.data.set()
  isolate({
    checkboxInput(inputId = "map1_colourcheck",
                  label = "Colour",
                  value = input$map1_colourcheck)
    
  })
})




output$plot_colour_panel = renderUI({
  get.data.set()
  input$map1_colourcheck
  ret = NULL
  
  isolate({
    if(!is.null(input$map1_colourcheck) && input$map1_colourcheck)
      ret = list(fixedRow(column(3, h5("Point colour:")),
                          column(6, selectInput(inputId = "plot_colour",
                                                label = NULL,
                                                choices = c("mediumvioletred", "grey50", "black", "darkblue", "darkgreen",
                                                            "darkmagenta", "darkslateblue", "hotpink4",
                                                            "lightsalmon2", "palegreen3", "steelblue3"),
                                                selectize = FALSE))),
                 
                 fixedRow(column(3, h5("Colour by:")),
                          column(6, selectInput(inputId = "colourby",
                                                label = NULL,
                                                choices = c("", colnames(get.data.set())),
                                                selectize = FALSE))))
    
  })
  
  ret 
})


output$plot_colpalette_panel = renderUI({
  get.data.set()
  input$map1_colourcheck
  input$colourby
  ret0 = NULL
  ret1 = NULL
#  ret = NULL
  
  isolate({
    if(!is.null(input$map1_colourcheck) && input$map1_colourcheck) {
      if(!is.null(input$colourby) && input$colourby %in% numericVars()) {
        ret0 = fixedRow(column(3, h5("Palette:")),
                        column(6, selectInput(inputId = "select.colour.palette", 
                                              label=NULL,
                                              choices = c("inferno", names(colourPalettes$cont)[-4]),
                                              selectize = FALSE)))
      }
      else if(!is.null(input$colourby) && input$colourby %in% characterVars()) {
        ret0 = fixedRow(column(3, h5("Palette:")),
                        column(6, selectInput(inputId = "select.colour.palette", 
                                              label=NULL,
                                              choices = names(colourPalettes$cat),
                                              selectize = FALSE)))
      }
      
      if(!is.null(input$colourby) && input$colourby != "")
        ret1 = list(fixedRow(column(5, checkboxInput(inputId = "reversepalette",
                                                     label = "Reverse palette",
                                                     value = input$reversepalette)),
                             column(5, checkboxInput(inputId = "usepercentiles",
                                                     label = "Use Percentiles",
                                                     value = input$usepercentiles))))
    }
    
    
    ret = list(ret0,
               ret1)
  })
  
  ret
})


observe({
  input$select.colour.palette
  isolate({
    if(!is.null(input$select.colour.palette)) {
      if(input$select.colour.palette %in% names(colourPalettes$cat))
        args$col.fun = colourPalettes$cat[[input$select.colour.palette]]
      else if(input$select.colour.palette %in% names(colourPalettes$cont))
        args$col.fun = colourPalettes$cont[[input$select.colour.palette]]
    }
      
  })
})


observe({
  input$reversepalette
  isolate({
    if(length(input$reversepalette) > 0) 
      args$reverse.palette = input$reversepalette
    else
      args$reverse.palette = NULL
  })
})


observe({
  input$usepercentiles
  isolate({
    if(length(input$usepercentiles) > 0) 
      args$col.method = ifelse(input$usepercentiles, "rank", "linear")
    else
      args$col.method = NULL
  })
})






## set up plot_plottitle_panel
output$plot_plottitle_panel = renderUI({
  get.data.set()
  isolate({
    list(textInput("type1_plottitle",
                   label = "Plot title:",
                   value = input$type1_plottitle),
         
         actionButton(inputId = "type1_plottitle_confirm",
                      label = "Confirm Title",
                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    
  })
})

## update args$main when observe input$type1_plottitle_confirm
observe({
  input$type1_plottitle_confirm
  isolate({
    if(length(input$type1_plottitle) > 0 && input$type1_plottitle != "") 
      args$main = input$type1_plottitle
    else
      args$main = NULL
  })
})


## set up plot_overallsize_panel
output$plot_overallsize_panel = renderUI({
  get.data.set()
  isolate({
    sliderInput("type1_overallsize_slider", 
                label = "Overall size scale:", 
                min = 0.5, 
                max = 2, 
                value = 1, step = 0.05, ticks = FALSE)
  })
})

## update args$cex
observe({
  input$type1_overallsize_slider
  isolate({
    if(!is.null(input$type1_overallsize_slider)) 
      args$cex = input$type1_overallsize_slider
  })
})


## update args$type and args$col.pt
observe({
  input$plot_maptype
  isolate({
    if(!is.null(input$plot_maptype))
      args$type = input$plot_maptype
  })
})

observe({
  input$plot_colour
  isolate({
    if(!is.null(input$plot_colour))
      args$col.pt = input$plot_colour
  })
})



## set up plot_size_panel and plot_sizecheck_panel

output$plot_sizecheck_panel = renderUI({
  get.data.set()
  
  isolate({
    checkboxInput(inputId = "map1_sizecheck",
                  label = "Size",
                  value = input$map1_sizecheck)
  })
})


output$plot_size_panel = renderUI({
  get.data.set()
  input$map1_sizecheck
  ret = NULL
  
  isolate({
    if(!is.null(input$map1_sizecheck) && input$map1_sizecheck)
      ret = list(fixedRow(column(3, h5("Overall:")),
                          column(6, sliderInput(inputId = "pointsize", 
                                                label = NULL, 
                                                min = 0.05, max = 3.5, 
                                                value = 1.1, step = 0.05, ticks = FALSE))),
                 fixedRow(column(3, h5("Size by:")),
                          column(6, selectInput(inputId = "sizeby",
                                                label = NULL,
                                                choices = c("", numericVars()),
                                                selectize = FALSE))),
                 conditionalPanel("input.sizeby != ''",
                                  fixedRow(column(3, h5("Resize method:")),
                                           column(6, selectInput(inputId = "resizemethod",
                                                                 label = NULL,
                                                                 choices = c("proportional", "emphasize"),
                                                                 selectize = FALSE)))))
  })
  ret
})


## update args$resize.method
observe({
  input$resizemethod
  isolate({
    if(!is.null(input$resizemethod))
      args$resize.method = input$resizemethod
  })
})


## update args$cex.pt, args$alpha, args$join
observe({
  input$pointsize
  isolate({
    if(!is.null(input$pointsize))
      args$cex.pt = input$pointsize
  })
})



## set up plot_trans_panel and plot_transcheck_panel

output$plot_transcheck_panel = renderUI({
  get.data.set()
  
  isolate({
    checkboxInput(inputId = "map1_transcheck",
                  label = "Transparency",
                  value = input$map1_transcheck)
  })
})

output$plot_trans_panel = renderUI({
  get.data.set()
  input$map1_transcheck
  ret = NULL
  
  isolate({
    if(!is.null(input$map1_transcheck) && input$map1_transcheck)
      ret = list(fixedRow(column(3, h5("Overall:")),
                          column(6, sliderInput(inputId = "transparency", 
                                                label = NULL, 
                                                min = 0, max = 100, 
                                                value = 0, step = 1, ticks = FALSE))),
                 
                 fixedRow(column(3, h5("Opacify by:")),
                          column(6, selectInput(inputId = "opacifyby",
                                                label = NULL,
                                                choices = c("", numericVars()),
                                                selectize = FALSE))),
                 checkboxInput(inputId = "reverseop",
                               label = "Reverse Opacification",
                               value = input$reverseop))
    
  })
  
  ret
})



## update args$reverse.opacity
observe({
  input$reverseop
  isolate({
    if(!is.null(input$reverseop))
      args$reverse.opacity = input$reverseop
  })
})




#observe({
#  input$transparency
#  isolate({
#    if(!is.null(input$transparency))
#      args$alpha = 1 - input$transparency/100
#  })
#})


## set up plot_pointsymbol_panel and plot_symbolcheck_panel

output$plot_symbolcheck_panel = renderUI({
  get.data.set()
  
  isolate({
    checkboxInput(inputId = "map1_symbolcheck",
                  label = "Point Symbol",
                  value = input$map1_symbolcheck)
  })
})


output$plot_pointsymbol_panel = renderUI({
  get.data.set()
  input$map1_symbolcheck
  ret = NULL
  
  isolate({
    if(!is.null(input$map1_symbolcheck) && input$map1_symbolcheck)
      ret = list(fixedRow(column(3, h5("Symbol:")),
                          column(6, selectInput(inputId = "symbol",
                                                label = NULL,
                                                choices = c("circle", "square", "diamond", "triangle", "inverted triangle"),
                                                selectize = FALSE))),
                 
                 checkboxInput(inputId = "filledin",
                               label = "Filled in symbols",
                               value = input$filledin),
                 
                 fixedRow(column(3, h5("Symbol by:")),
                          column(6, selectInput(inputId = "symbolby",
                                                label = NULL,
                                                choices = c("", characterVars()),
                                                selectize = FALSE))),
                 
                 fixedRow(column(3, h5("Symbol line width:")),
                          column(6, selectInput(inputId = "symbollwd",
                                                label = NULL,
                                                choices = c(1:4),
                                                selectize = FALSE,
                                                selected = 2))))
    
  })
  
  ret 
})


observe({
  input$symbol
  isolate({
    if(!is.null(input$symbol))
      args$pch = switch(input$symbol,
                        "circle"            = 21,
                        "square"            = 22,
                        "diamond"           = 23,
                        "triangle"          = 24,
                        "inverted triangle" = 25)
  })
})


observe({
  input$filledin
  input$transparency
  isolate({
    if(!is.null(input$filledin) && !is.null(input$transparency)) {
      if(input$filledin && input$transparency == 0)
        args$alpha = 0.999
      else
        args$alpha = 1 - input$transparency/100
    }
  })
})


observe({
  input$symbolby
  isolate({
    if(!is.null(input$symbolby)) {
      if(input$symbolby == "")
        args$symbolby = NULL
      else {
        temp.data = get.data.set()
        args$symbolby = temp.data[[input$symbolby]]
        args$varnames$symbolby = input$symbolby
      }
        
    }
  })
})


observe({
  input$symbollwd
  isolate({
    if(!is.null(input$symbollwd))
      args$lwd.pt = input$symbollwd
  })
})




observe({
  input$connectpoints
  isolate({
    if(!is.null(input$connectpoints))
      args$join = input$connectpoints
  })
})


## set up linescolour_panel
output$linescolour_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "linescolour",
                label = "Lines colour:",
                choices = c("blue", "black", "red", "green4", "yellow",
                            "pink", "grey", "orange"),
                selectize = FALSE)
  })
})


## update args$col.line
observe({
  input$connectpoints
  input$linescolour
  isolate({
    if(!is.null(input$linescolour) &&
       input$connectpoints)
      args$col.line = input$linescolour
  })
})


## set up mapssubset1_panel and mapssubset2_panel
## update mapssubset1_panel and mapssubset2_panel
## set up mapssubset1_slider_panel and mapssubset2_slider_panel
## update sliders for both subset1 and subset2
## update args$g1, args$g2, args$g1.level and args$g2.level

output$mapssubset1_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "mapssubset1",
                label = "Select subset variable 1:",
                choices = c("None", colnames(get.data.set())),
                selectize = FALSE)
  })
})


observe({
  input$mapssubset2
  isolate({
    data.temp = get.data.set()
    ch = colnames(data.temp)
    if(!is.null(input$mapssubset2) &&
       input$mapssubset2 == "None") {
      updateSelectInput(session, "mapssubset1", choices = c("None", ch), selected = input$mapssubset1)
      args$g2 = NULL
      args$g2.level = NULL
      args$varnames$g2 = NULL
    }
    else if(!is.null(input$mapssubset2) &&
            input$mapssubset2 %in% colnames(get.data.set())) {
      ch  = ch[-which(ch %in% input$mapssubset2)]
      updateSelectInput(session, "mapssubset1", choices = c("None", ch), selected = input$mapssubset1)
      index = which(input$mapssubset2 == colnames(data.temp))
      args$varnames$g2 = colnames(data.temp)[index]
      args$g2.level = "_ALL"
      if(input$map_type == 1) {
        g2 = convert.to.factor(data.temp[, index])
        args$g2 = g2
      }
      else if(input$map_type == 2) {
        if(!is.null(input$maplocation) &&
           !is.null(input$locationvariable) &&
           input$locationvariable %in% colnames(data.temp)) {
          temp = plot.args()
          data.temp2 = temp$x$data
          if(!is.numeric(data.temp2[, index]) & !is.ordered(data.temp2[, index]))
            g2 = factor(data.temp2[, index], ordered = TRUE)
          else
            g2 = convert.to.factor(data.temp2[, index])
          args$g2 = g2
        }
      }
    }
  })
})


output$mapssubset1_slider_panel = renderUI({
  get.data.set()
  if(!is.null(input$mapssubset1) &&
     input$mapssubset1 %in% colnames(get.data.set())) {
    isolate({
      data.temp = get.data.set()
      index = which(input$mapssubset1 == colnames(data.temp))
      if(input$map_type == 1)
        temp = convert.to.factor(data.temp[, index])
      else if(input$map_type == 2) {
#        mapsplot.obj = iNZightShapeMap(data = data.temp,
#                                       location = input$maplocation,
#                                       data.region = input$locationvariable)
#        data.temp2 = mapsplot.obj$data
#        temp = convert.to.factor(data.temp2[, index])
        data.temp2 = plot.args()$x$data
        if(!is.numeric(data.temp2[, index]) & !is.ordered(data.temp2[, index]))
          temp = factor(data.temp2[, index], ordered = TRUE)
        else
          temp = convert.to.factor(data.temp2[, index])
      }
      n.levels = length(levels(temp))
      sliderInput(inputId = "mapssubset1_slider",
                  label = paste("Subset '", input$mapssubset1, "':", "_MULTI"),
                  min = 0, max = n.levels, value = 0, step = 1,
                  animate = TRUE, ticks = F)
    })
  }
})



observe({
  input$mapssubset1_slider
  isolate({
    if(!is.null(input$mapssubset1) &&
       input$mapssubset1 %in% colnames(get.data.set())) {
      data.temp = get.data.set()
      index = which(input$mapssubset1 == colnames(data.temp))
#      temp = convert.to.factor(data.temp[, index])
      if(input$map_type == 1)
        temp = convert.to.factor(data.temp[, index])
      else if(input$map_type == 2) {
        #        mapsplot.obj = iNZightShapeMap(data = data.temp,
        #                                       location = input$maplocation,
        #                                       data.region = input$locationvariable)
        #        data.temp2 = mapsplot.obj$data
        #        temp = convert.to.factor(data.temp2[, index])
        data.temp2 = plot.args()$x$data
        if(!is.numeric(data.temp2[, index]) & !is.ordered(data.temp2[, index]))
          temp = factor(data.temp2[, index], ordered = TRUE)
        else
          temp = convert.to.factor(data.temp2[, index])
      }
      level = input$mapssubset1_slider
      args$varnames$g1 = colnames(data.temp)[index]
      if(input$mapssubset1_slider == 0) {
        updateSliderInput(session, "mapssubset1_slider",
                          label = paste("Subset '", input$mapssubset1, "': ", "_MULTI"))
        args$g1.level = "_MULTI"
      }
      else {
        updateSliderInput(session, "mapssubset1_slider",
                          label = paste("Subset '", input$mapssubset1, "': ", levels(temp)[level]))
        args$g1.level = levels(temp)[level]
      }
      if(input$map_type == 1) {
        g1 = convert.to.factor(data.temp[, index])
        args$g1 = g1
      }
      else if(input$map_type == 2) {
        if(!is.null(input$maplocation) &&
           !is.null(input$locationvariable) &&
           input$locationvariable %in% colnames(data.temp)) {
#          mapsplot.obj = iNZightShapeMap(data = data.temp,
#                                         location = input$maplocation,
#                                         data.region = input$locationvariable)
#          data.temp2 = mapsplot.obj$data
#          g1 = convert.to.factor(data.temp2[, index])
#          args$g1 = g1
        }
      }
    }
  })
})




output$mapssubset2_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "mapssubset2",
                label = "Select subset variable 2:",
                choices = c("None", colnames(get.data.set())),
                selectize = FALSE)
  })
})


observe({
  input$mapssubset1
  isolate({
    data.temp = get.data.set()
    ch = colnames(data.temp)
    if(!is.null(input$mapssubset1) &&
       input$mapssubset1 %in% colnames(get.data.set())) {
      ch  = ch[-which(ch %in% input$mapssubset1)]
      updateSelectInput(session, "mapssubset2", choices = c("None", ch), selected = input$mapssubset2)
      index = which(input$mapssubset1 == colnames(data.temp))
      args$varnames$g1 = colnames(data.temp)[index]
      args$g1.level = "_MULTI"
      if(input$map_type == 1) {
        g1 = convert.to.factor(data.temp[, index])
        args$g1 = g1
      }
      else if(input$map_type == 2) {
        if(!is.null(input$maplocation) &&
           !is.null(input$locationvariable) &&
           input$locationvariable %in% colnames(data.temp)) {
#          mapsplot.obj = iNZightMaps::iNZightShapeMap(data = data.temp,
#                                                      location = input$maplocation,
#                                                      data.region = input$locationvariable)
#          data.temp2 = mapsplot.obj$data
          temp = plot.args()
          data.temp2 = temp$x$data
          if(!is.numeric(data.temp2[, index]) & !is.ordered(data.temp2[, index]))
            g1 = factor(data.temp2[, index], ordered = TRUE)
          else
            g1 = convert.to.factor(data.temp2[, index])
          args$g1 = g1
        }
      }
    }
    else if(!is.null(input$mapssubset1) &&
            input$mapssubset1 == "None") {
      updateSelectInput(session, "mapssubset2", choices = c("None", ch), selected = input$mapssubset2)
      args$g1 = NULL
      args$g1.level = NULL
      args$varnames$g1 = NULL
    } 
  })
})


output$mapssubset2_slider_panel = renderUI({
  get.data.set()
  if(!is.null(input$mapssubset2) &&
     input$mapssubset2 %in% colnames(get.data.set())) {
    isolate({
      data.temp = get.data.set()
      index = which(input$mapssubset2 == colnames(data.temp))
#      temp = convert.to.factor(data.temp[, index])
      if(input$map_type == 1)
        temp = convert.to.factor(data.temp[, index])
      else if(input$map_type == 2) {
        data.temp2 = plot.args()$x$data
        if(!is.numeric(data.temp2[, index]) & !is.ordered(data.temp2[, index]))
          temp = factor(data.temp2[, index], ordered = TRUE)
        else
          temp = convert.to.factor(data.temp2[, index])
      }
      n.levels = length(levels(temp))
      sliderInput(inputId = "mapssubset2_slider",
                  label = paste("Subset '", input$mapssubset2, "': ", "_ALL"),
                  min = 0, max = n.levels + 1, value = 0, step = 1,
                  animate = TRUE, ticks = F)
    })
  }
})


observe({
  input$mapssubset2_slider
  isolate({
    if(!is.null(input$mapssubset2) &&
       input$mapssubset2 %in% colnames(get.data.set())) {
      data.temp = get.data.set()
      index = which(input$mapssubset2 == colnames(data.temp))
#      temp = convert.to.factor(data.temp[, index])
      if(input$map_type == 1)
        temp = convert.to.factor(data.temp[, index])
      else if(input$map_type == 2) {
        data.temp2 = plot.args()$x$data
        if(!is.numeric(data.temp2[, index]) & !is.ordered(data.temp2[, index]))
          temp = factor(data.temp2[, index], ordered = TRUE)
        else
          temp = convert.to.factor(data.temp2[, index])
      }
      n.levels = length(levels(temp))
      level = input$mapssubset2_slider
      args$varnames$g2 = colnames(data.temp)[index]
      if(input$mapssubset2_slider == 0) {
        updateSliderInput(session, "mapssubset2_slider",
                          label = paste("Subset '", input$mapssubset2, "': ", "_ALL"))
        args$g2.level = "_ALL"
      }
      else if(input$mapssubset2_slider == (n.levels + 1)) {
        updateSliderInput(session, "mapssubset2_slider",
                          label = paste("Subset '", input$mapssubset2, "': ", "_MULTI"))
        args$g2.level = "_MULTI"
      }
      else {
        updateSliderInput(session, "mapssubset2_slider",
                          label = paste("Subset '", input$mapssubset2, "': ", levels(temp)[level]))
        args$g2.level = levels(temp)[level]
      }
      if(input$map_type == 1) {
        g2 = convert.to.factor(data.temp[, index])
        args$g2 = g2
      }
      else if(input$map_type == 2) {
        if(!is.null(input$maplocation) &&
           !is.null(input$locationvariable) &&
           input$locationvariable %in% colnames(data.temp)) {
#          mapsplot.obj = iNZightShapeMap(data = data.temp,
#                         location = input$maplocation,
#                         data.region = input$locationvariable)
#          data.temp2 = mapsplot.obj$data
#          g2 = convert.to.factor(data.temp2[, index])
#          args$g2 = g2
        }
      }
    }
  })
})



## set up selectmap_panel, inbuiltmap_panel

output$selectmap_panel = renderUI({
  get.data.set()
  isolate({
    radioButtons(
      inputId = "selectshapefile",
      label = NULL,
      choices =
        c("Use Inbuilt Map" = 1,
          "Import Shapefile" = 2),
      selected = 1,
      inline = TRUE
    )
  })
})


output$inbuiltmap_panel = renderUI({
  get.data.set()
  isolate({
    radioButtons(inputId = "selectinbuiltmap",
                 label = NULL,
                 choices =
                   c("Continents" = 1,
                     "Countries" = 2,
                     "World" = 3),
                 selected = 1,
                 inline = TRUE)
  })
})


output$continentsoptions_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "continentsmap", 
                label = NULL, 
                choices = c( 
                            "Africa", "Asia", "Europe", "North America", "Oceania", "South America"), 
                multiple = FALSE,
                selectize = FALSE,
                size = 4)
  })
})


output$countriesoptions_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "countriesmap", 
                label = NULL, 
                choices = c( 
                            "New Zealand General Electoral Districts (2017)",
                            "New Zealand DHBs (2012)",
                            "New Zealand Regional Councils (2017)",
                            "New Zealand Territorial Authorities (2017)",
                            "US States",
                            "US States (Contiguous)"), 
                multiple = FALSE,
                selectize = FALSE,
                size = 4)
  })
})


output$worldoptions_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "worldmap", 
                label = NULL, 
                choices = c(
                            "World Map (Natural Earth) (incl. Antarctica)", 
                            "World Map (Thematic Mapping)"), 
                multiple = FALSE,
                selectize = FALSE,
                size = 4)
  })
})


## set up select variables panel

output$datavariable_panel = renderUI({
  get.data.set()
  isolate({
    fixedRow(column(5, h5("Data Variable:")),
             column(7, selectInput(inputId = "datavariable", 
                                   label = NULL, 
                                   choices = colnames(vis.data()),
                                   selectize = F)))
  })
})


output$mapvairable_panel = renderUI({
  get.data.set()
  isolate({
    fixedRow(column(5, h5("Map Variable:")),
             column(7, selectInput(inputId = "mapvariable", 
                                   label = NULL, 
                                   choices = "",
                                   selectize = F)))
  })
})


## update datavariable_panel and mapvairable_panel 
## after shapefiles selected and "import map" clicked
observe({
  get.data.set()
  input$importmap
#  input$datavariable
#  input$mapvariable
#  input$sequencevariable
  isolate({
    
    temp.data = get.data.set()
    
    if(!is.null(input$selectinbuiltmap) && input$selectinbuiltmap %in% 1:3) {
      
      if(input$selectinbuiltmap == 1) {
        dirpath = "shapefiles/continents/"
        filename = switch(input$continentsmap,
                          "Africa" = "africa.rds", 
                          "Asia" = "asia.rds", 
                          "Europe" = "europe.rds", 
                          "North America" = "northamerica.rds", 
                          "Oceania" = "oceania.rds", 
                          "South America" = "southamerica.rds")
        
      }
      else if(input$selectinbuiltmap == 2) {
        dirpath = "shapefiles/countries/"
        filename = switch(input$countriesmap,
                          "New Zealand General Electoral Districts (2017)" = "nzl/constituency-2018.rds",
                          "New Zealand DHBs (2012)" = "nzl/dhb-2012.rds",
                          "New Zealand Regional Councils (2017)" = "nzl/regional-council-2018.rds",
                          "New Zealand Territorial Authorities (2017)" = "nzl/territorial-authority-2018.rds",
                          "US States" = "usa/states-2016.rds",
                          "US States (Contiguous)" = "usa/states-contig-2016.rds")
      }
      else if(input$selectinbuiltmap == 3) {
        dirpath = "shapefiles/world/"
        filename = switch(input$worldmap,
                          "World Map (Natural Earth) (incl. Antarctica)" = "natural-earth-4.0.rds", 
                          "World Map (Thematic Mapping)" = "thematic-mapping-0.3.rds")
      }
      
      ## obtain the filepath and read the map file
      filepath = paste(dirpath, filename, sep = "")
      mapData = iNZightMaps::retrieveMap(filepath)
      args2$mapData = mapData
      map.vars = as.data.frame(mapData)[, !(colnames(mapData) %in% "geometry"), drop = FALSE]
      ## get the choices for mapvairable_panel
      mapvars.update = colnames(map.vars[, !(apply(map.vars, 2, anyDuplicated, incomparables = c(NA, ""))), drop = FALSE])
      
      ## Find the pair of variables with the highest number of matches
      best.vars = iNZightMaps::findBestMatch(temp.data, map.vars)
      best.data.var = best.vars[1]
      best.map.var =  best.vars[2]
      
      if(length(input$mapvariable) > 0 && length(input$datavariable) > 0 &&
         input$mapvariable == best.map.var && input$datavariable == best.data.var) {
        temp = plot.args2()
        args2$updateplot = temp$updateplot + 1
      }
      
      ## update mapvairable_panel after map file selected
      updateSelectInput(session, "mapvariable", choices = mapvars.update, selected = best.map.var)
      ## update datavariable_panel
      updateSelectInput(session, "datavariable", selected = best.data.var)

    }
    
  })
})


## observe shapefiles loaded by users
observeEvent(input$loadshapefiles, { 
  
  temp.data = get.data.set()
  
  isolate({
    
    filepath = input$loadshapefiles$datapath
    mapData = iNZightMaps::retrieveMap(filepath)
    args2$mapData = mapData
    map.vars = as.data.frame(mapData)[, !(colnames(mapData) %in% "geometry"), drop = FALSE]
    ## get the choices for mapvairable_panel
    mapvars.update = colnames(map.vars[, !(apply(map.vars, 2, anyDuplicated, incomparables = c(NA, ""))), drop = FALSE])
    
    ## Find the pair of variables with the highest number of matches
    best.vars = iNZightMaps::findBestMatch(temp.data, map.vars)
    best.data.var = best.vars[1]
    best.map.var =  best.vars[2]
    
    ## update mapvairable_panel after map file selected
    updateSelectInput(session, "mapvariable", choices = mapvars.update, selected = best.map.var)
    ## update datavariable_panel
    updateSelectInput(session, "datavariable", selected = best.data.var)
    
  })

})



## everything reset to default when a new dataset is loaded
observe({
  get.data.set()
  
  isolate({
    
    args2$combinedData = NULL
#    mapData = NULL,
    
#    mapName = "",
#    mapType = NULL,
#    mapVars = NULL,
#    mapSizeVar = NULL,
#    mapSequenceVar = NULL,
    
    args2$match.list = NULL
    #  has.multipleobs = FALSE,
    
#    plotTitle = "",
#    plotAxes = FALSE,
#    plotXLab = "",
#    plotYLab = "",
#    plotDatumLines = FALSE,
#    plotProjection = NULL,
#    plotTheme = FALSE,
#    plotPalette = "Default",
#    plotConstantAlpha = 1.0,
#    plotConstantSize = 1.0,
#    plotCurrentSeqVal = NULL,
#    timer = NULL,
    
#    multipleObsOption = NULL,
#    plotSparklinesType = "Absolute",
#    plotScaleLimits = NULL, 
#    plotLabelVar = NULL,
    
#    plotLabelScale = 4,
#    plotAxisScale = 11, 
    
#    proj.df = iNZightMaps::iNZightMapProjections()
  })
})






## update "combinedData" information after loading data
observe({
  get.data.set()
  input$datavariable
  input$mapvariable
  input$sequencevariable
#  input$unmatched
#  args2$mapData
  
  args2$updateplot
  
  isolate({
    temp.data = get.data.set()
    temp = plot.args2()
    mapData = temp$mapData
    match.list = temp$match.list
    sequence.var = NULL
    if(!is.null(mapData) && !is.null(match.list) &&
       !is.null(input$datavariable) && input$datavariable %in% colnames(temp.data) &&
       !is.null(input$mapvariable) && input$mapvariable != "") {
      
      if(match.list$multiple.obs) {
        has.multipleobs = TRUE
        if(!is.null(input$sequencevariable) && input$sequencevariable %in% colnames(temp.data))
          sequence.var = input$sequencevariable
      }
      
      else {
        has.multipleobs = FALSE
      }
    
      args2$combinedData = suppressWarnings(iNZightMaps::iNZightMapPlot(data = temp.data,
                                                                        map = mapData,
                                                                        type = "region",
                                                                        by.data = input$datavariable,
                                                                        by.map = input$mapvariable,
                                                                        simplification.level = 0.01,
                                                                        multiple.obs = has.multipleobs,
                                                                        sequence.var = sequence.var))
      
      args2$updateplot1 = temp$updateplot1 + 1
        
      
    }
    
  })
  
})


output$sequencevariable_panel = renderUI({
  get.data.set()
  input$datavariable
  input$mapvariable
  input$loadshapefiles
  ret = NULL
  
  isolate({
    temp.data = get.data.set()
    temp = plot.args2()
    match.list = temp$match.list
    if(!is.null(match.list)) {
      if(match.list$multiple.obs) {
        
        textpanel = h5(strong("Multiple observations for each region were found!"))
        timevar = grepl("(year|date)", colnames(temp.data), ignore.case = TRUE)
        
        if(any(timevar)) {
          
          ret = list(textpanel,
                     fixedRow(column(5, h5("Sequence Variable:")),
                              column(7, selectInput(inputId = "sequencevariable", 
                                                    label = NULL, 
                                                    choices = colnames(vis.data()),
                                                    selected = colnames(vis.data())[timevar][1],
                                                    selectize = F))))
        }
        else {
          ret = list(textpanel,
                     fixedRow(column(5, h5("Sequence Variable:")),
                              column(7, selectInput(inputId = "sequencevariable", 
                                                    label = NULL, 
                                                    choices = colnames(vis.data()),
                                                    selectize = F))))
        }
      }
      else
        textpanel = ""
    }
  })
  ret
})


## setup unmateched data panel

output$unmatched_panel = renderUI({
  get.data.set()
#  input$importmap
#  input$datavariable
#  input$mapvariable
  
#  input$loadshapefiles
  args2$match.list
  
  ret = NULL
  isolate({
    temp = plot.args2()
  
    if(!is.null(temp$match.list)) {
      match.list = temp$match.list
      table.nonmatched = match.list$data.vect[!(match.list$data.matched)]
      ret = list(h4("Unmatched Data"),
                 h5("Observations in the dataset with no corresponding region in the map file"),
                 selectInput(inputId = "unmatched", 
                             label = NULL, 
                             choices = table.nonmatched, 
                             multiple = FALSE,
                             selectize = FALSE,
                             size = 3))
      }
        
    ret
  })
})


output$unmatchedcounts_panel = renderText({
  get.data.set()
#  input$importmap
#  input$datavariable
#  input$mapvariable
  
#  input$loadshapefiles
  
  args2$match.list
  
  ret = NULL
  isolate({
    temp = plot.args2()
    if(!is.null(temp$match.list)) {
      match.list = temp$match.list
      matchedcount = sum(match.list$data.matched)
      unmatchedcount = sum(!match.list$data.matched)
      ret = paste("Matched Count:", matchedcount, "\n", "Unmatched Count:", unmatchedcount, sep = "")
      
    }
    
    ret
  })
})



## set up advancedmapoptions_panel
output$advancedmapoptions_panel = renderUI({
  get.data.set()
  
#  proj.df = iNZightMaps::iNZightMapProjections() 
  ret = NULL
  isolate({
    advancedmapoptions.title = checkboxInput(inputId = "advancedmapoptions_title",
                                             label = strong("Advanced Map Options"),
                                             value = input$advancedmapoptions_title)
    
    advancedmapoptions.contents = conditionalPanel(condition = "input.advancedmapoptions_title",
                                                   fixedRow(column(3, h5("Projection:")),
                                                            column(6, selectInput(inputId = "advancedmapoptions_projection",
                                                                                  label = NULL,
                                                                                  choices = plot.args2()$proj.df$Name,
                                                                                  selected = input$advancedmapoptions_projection,
                                                                                  selectize = F))),
                                                   uiOutput("checkregion_panel"))
    ret = list(advancedmapoptions.title, 
               advancedmapoptions.contents)
    
  })
  
  ret
})


## set up checkregion_panel
output$checkregion_panel = renderUI({
  get.data.set()
  input$datavariable
  input$mapvariable
  input$sequencevariable
  input$importmap
  input$loadshapefiles
  
  ret = NULL
  
  isolate({
    temp = plot.args2()
    if(!is.null(temp$combinedData)) {
      options = iNZightMaps::iNZightMapRegions(temp$combinedData)
      ret = selectInput(inputId = "check_regions", 
                        label = NULL, 
                        choices = options, 
                        selected = options,
                        multiple = TRUE,
                        selectize = FALSE,
                        size = 4)
    }
      
  })
  
  ret
})



## observe checkregion_panel
observe({
  input$check_regions
  isolate({
    temp = plot.args2()
    options = iNZightMaps::iNZightMapRegions(temp$combinedData)
    if(!is.null(input$check_regions) && length(input$check_regions) > 0 &&
       length(input$check_regions) != length(options)) {
      args2$mapRegionsPlot = input$check_regions
      args2$mapExcludedRegions = TRUE
    }
    else
      args2$mapRegionsPlot = NULL
  })
})


## observe advancedmapoptions_projection
observe({
  input$advancedmapoptions_projection
  isolate({
    projection.index = match(input$advancedmapoptions_projection, plot.args2()$proj.df$Name)
    
    args2$plotProjection = plot.args2()$proj.df[projection.index, "PROJ4"]
  })
})



## set up advancedplotoptions_panel
output$advancedplotoptions_panel = renderUI({
  get.data.set()
  input$datavariable
  input$mapvariable
  input$sequencevariable

    
  ret = NULL
  isolate({
    temp = plot.args2()
    if(!is.null(temp$combinedData)) {
      advancedplotoptions.title = checkboxInput(inputId = "advancedplotoptions_title",
                                                label = strong("Advanced Plot Options"),
                                                value = input$advancedplotoptions_title)
      
      advancedplotoptions.contents = conditionalPanel(condition = "input.advancedplotoptions_title",
                                                      list(fixedRow(column(3, h5("Plot Title:")),
                                                                    column(6, textInput("advancedplotoptions_plottitle",
                                                                                        label = NULL,
                                                                                        value = input$advancedplotoptions_plottitle))),
                                                           
                                                           fixedRow(column(3, h5("Map Palette:")),
                                                                    column(6, selectInput(inputId = "advancedplotoptions_mappalette",
                                                                                          label = NULL,
                                                                                          choices = c("Default", "Viridis", 
                                                                                                      "Magma", "Plasma", 
                                                                                                      "Inferno", "BrBG", 
                                                                                                      "PiYG", "PRGn", 
                                                                                                      "Accent", "Dark2", 
                                                                                                      "Paired", "Pastel1", 
                                                                                                      "Set1", "Blues", 
                                                                                                      "BuGn", "BuPu", "GnBu"),
                                                                                          selected = input$advancedplotoptions_mappalette,
                                                                                          selectize = F))),
                                                           fixedRow(column(3, NULL),
                                                                    column(2, checkboxInput(inputId = "advancedplotoptions_dark",
                                                                                            label = "Dark",
                                                                                            value = input$advancedplotoptions_dark)),
                                                                    column(2, checkboxInput(inputId = "advancedplotoptions_gridlines",
                                                                                            label = "Grid Lines",
                                                                                            value = input$advancedplotoptions_gridlines)),
                                                                    column(2, checkboxInput(inputId = "advancedplotoptions_axislabels",
                                                                                            label = "Axis Labels",
                                                                                            value = input$advancedplotoptions_axislabels))),
                                                           
                                                           fixedRow(column(3, NULL),
                                                                    column(6, conditionalPanel(condition = "input.advancedplotoptions_axislabels & input.advancedplotoptions_title",
                                                                                               fixedRow(column(6, h5("x-axis Label:")),
                                                                                                        column(6, textInput("advancedplotoptions_xaxislabel",
                                                                                                                            label = NULL,
                                                                                                                            value = temp$plotXLab))),
                                                                                               fixedRow(column(6, h5("y-axis Label:")),
                                                                                                        column(6, textInput("advancedplotoptions_yaxislabel",
                                                                                                                            label = NULL,
                                                                                                                            value = temp$plotYLab)))))),
                                                           
                                                           fixedRow(column(3, h5("Map Scales:")),
                                                                    column(6, selectInput(inputId = "advancedplotoptions_mapscales",
                                                                                          label = NULL,
                                                                                          choices = c("Independent scales",
                                                                                                      "Same for all plots",
                                                                                                      "Scales fixed at 0-1",
                                                                                                      "Scales fixed at 0-100",
                                                                                                      "Scales fixed at custom range"),
                                                                                          selected = input$advancedplotoptions_mapscales,
                                                                                          selectize = F))),
                                                           conditionalPanel(condition = "input.advancedplotoptions_mapscales == 'Scales fixed at custom range'",
                                                                            fixedRow(column(3, textInput("scales_min",
                                                                                                         label = NULL,
                                                                                                         value = "")),
                                                                                     column(3, textInput("scales_max",
                                                                                                         label = NULL,
                                                                                                         value = "")),
                                                                                     column(3, actionButton(inputId = "scales_confirm",
                                                                                                            label = "Confirm",
                                                                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
                                                           fixedRow(column(3, h5("Plot title font size:")),
                                                                    column(6, sliderInput("plottitlefontsize_slider", 
                                                                                          label = NULL, 
                                                                                          min = 7, 
                                                                                          max = 17, 
                                                                                          value = 11, step = 1, ticks = FALSE))),
                                                           fixedRow(column(3, checkboxInput(inputId = "regionlabels_click",
                                                                                            label = "Region Labels:",
                                                                                            value = input$advancedplotoptions_dark)),
                                                                    column(6, conditionalPanel(condition = "input.regionlabels_click & input.advancedplotoptions_title",
                                                                                               selectInput(inputId = "advancedplotoptions_regionlabels",
                                                                                                           label = NULL,
                                                                                                           choices = c("Current Variable", iNZightMaps::iNZightMapVars(temp$combinedData, map.vars = TRUE)),
                                                                                                           selected = input$advancedplotoptions_regionlabels,
                                                                                                           selectize = F),
                                                                                               fixedRow(column(5, h5("Label font size:")),
                                                                                                        column(7, sliderInput("regionlabels_slider", 
                                                                                                                              label = NULL, 
                                                                                                                              min = 1, 
                                                                                                                              max = 10, 
                                                                                                                              value = 4, step = 0.5, ticks = FALSE))))))))
      
      ret = list(advancedplotoptions.title,
                 advancedplotoptions.contents)  
      
    }
    
    
  })
  
  ret
})


## observe scales_confirm
observe({
  input$scales_confirm
  isolate({
    if(!is.null(input$scales_min) && length(input$scales_min) > 0 &&
       !is.null(input$scales_max) && length(input$scales_max) >0) {
      args2$plotScaleLimits = as.numeric(c(input$scales_min, input$scales_max))
    } 
  })
  
})


## observe plottitlefontsize_slider
## observe regionlabels_slider
observe({
  input$plottitlefontsize_slider
  isolate({
    args2$plotAxisScale  = input$plottitlefontsize_slider
  })
})


## observe regionlabels_slider
observe({
  input$regionlabels_slider
  isolate({
    args2$plotLabelScale = input$regionlabels_slider
  })
})


## observe regionlabels_click and advancedplotoptions_regionlabels
observe({
  input$regionlabels_click
  input$advancedplotoptions_regionlabels
  
  isolate({
    if(!is.null(input$regionlabels_click) && input$regionlabels_click) {
      if(!is.null(input$advancedplotoptions_regionlabels) && length(input$advancedplotoptions_regionlabels) > 0) {
        if(input$advancedplotoptions_regionlabels == "Current Variable")
          args2$plotLabelVar = "use_colour_var"
        else
          args2$plotLabelVar = input$advancedplotoptions_regionlabels
      }
    }
    else 
      args2$plotLabelVar = NULL
  })
})


## observe advancedplotoptions_mapscales
observe({
  input$advancedplotoptions_mapscales
  isolate({
    if(!is.null(input$advancedplotoptions_mapscales) && length(input$advancedplotoptions_mapscales) > 0) {
      if(!is.null(input$vartodisplay) && length(input$vartodisplay) > 0)
        args2$plotScaleLimits = switch(input$advancedplotoptions_mapscales,
                                       "Independent scales" = NULL,
                                       "Same for all plots" = iNZightMaps::getMinMax(plot.args2()$combinedData, input$vartodisplay),
                                       "Scales fixed at 0-1" = c(0, 1),
                                       "Scales fixed at 0-100" = c(0, 100)
                                       )  
         
    }
    
  })
})



## observe advancedplotoptions_mappalette
observe({
  input$advancedplotoptions_mappalette
  isolate({
    args2$plotPalette = input$advancedplotoptions_mappalette
  })
})


## observe advancedplotoptions_dark
observe({
  input$advancedplotoptions_dark
  isolate({
    args2$plotTheme = input$advancedplotoptions_dark
  })
})

## observe advancedplotoptions_gridlines
observe({
  input$advancedplotoptions_gridlines
  isolate({
    args2$plotDatumLines = input$advancedplotoptions_gridlines
  })
})



## observe advancedplotoptions_axislabels
observe({
  input$advancedplotoptions_axislabels
  isolate({
    args2$plotAxes = input$advancedplotoptions_axislabels
  })
})


## observe advancedplotoptions_xaxislabel and advancedplotoptions_yaxislabel
observe({
  input$advancedplotoptions_xaxislabel
  isolate({
    if(length(input$advancedplotoptions_xaxislabel) > 0)
      args2$plotXLab = input$advancedplotoptions_xaxislabel
  })
})

observe({
  input$advancedplotoptions_yaxislabel
  isolate({
    if(length(input$advancedplotoptions_yaxislabel) > 0)
      args2$plotYLab = input$advancedplotoptions_yaxislabel
  })
})


## observe advancedplotoptions_transparency
#observe({
#  input$advancedplotoptions_transparency
#  isolate({
#    if(length(input$advancedplotoptions_transparency) > 0)
#      args2$plotConstantAlpha = 1-input$advancedplotoptions_transparency
#  })
#})


## observe advancedplotoptions_size
#observe({
#  input$advancedplotoptions_size
#  isolate({
#    if(length(input$advancedplotoptions_size) > 0)
#      args2$plotConstantSize = input$advancedplotoptions_size
#  })
#})


## set up variabletodisplay_panel
output$variabletodisplay_panel = renderUI({
  get.data.set()
  input$importmap
  input$datavariable
  input$mapvariable
  
  ret = NULL
  isolate({
    temp = plot.args2()
    if(!is.null(temp$combinedData)) {
      var.vect = iNZightMaps::iNZightMapVars(temp$combinedData)
      ret = list(h4("Select Variable/s to Display"),
                 h5("(Use Ctrl+Click to select multiple variables)"),
                 selectInput(inputId = "vartodisplay", 
                             label = NULL, 
                             choices = var.vect, 
                             multiple = TRUE,
                             selected = input$vartodisplay,
                             selectize = FALSE,
                             size = 4))
    }
  
    ret
  })
})


## set up multipleobsoption_panel

output$multipleobsoption_panel = renderUI({
  get.data.set()
#  plot.args2()
  input$datavariable
  input$mapvariable
  
  ret = NULL
  isolate({
    temp.data = get.data.set()
    temp = plot.args2()
    
    if(!is.null(temp$match.list) && temp$match.list$multiple.obs) {
#      unique.singlevals = unique(as.data.frame(temp$combinedData[["region.data"]])[, temp$combinedData$sequence.var])
#      unique.singlevals = unique.singlevals[!is.na(unique.singlevals)]
      
      ret = list(h5(strong("Dataset has multiple observations for regions:")),
                 radioButtons(inputId = "multipleobsoption",
                              label = NULL,
                              choices =
                                c("Single Value" = 1,
                                  "All Values" = 2,
                                  "Aggregate" = 3),
                              selected = input$multipleobsoption,
                              inline = TRUE),
                 
                 ## a slider for the sequence variables
                 
                 conditionalPanel(condition = "input.multipleobsoption == 1",
                                  fixedRow(column(3, textOutput("printseqvar")),
                                           column(6, uiOutput("seqvar_slider_panel")))),
                 
                 conditionalPanel(condition = "input.multipleobsoption == 2",
                                  
                                  fixedRow(column(3, h5("")),
                                           column(6, radioButtons(inputId = "Sparklinesoption",
                                                                  label = NULL,
                                                                  choices = c("Sparklines" = 1),
                                                                  selected = 1,
                                                                  inline = TRUE))),
                                  
                                  fixedRow(column(3, h5("Line Chart Type:")),
                                           column(6, selectInput(inputId = "linecharttypeoption", 
                                                                 label = NULL, 
                                                                 choices = c("Absolute", "Relative", "Percent Change"),
                                                                 selected = input$linecharttypeoption,
                                                                 selectize = F)))),
                 
                 conditionalPanel(condition = "input.multipleobsoption == 3",
                                  selectInput(inputId = "aggregateoption", 
                                              label = NULL, 
                                              choices = c("Mean", "Median"),
                                              selected = input$aggregateoption,
                                              selectize = F)))
    }


  })
  
  ret
})


## print out sequence variable and show slider
output$printseqvar = renderText({
  get.data.set()
  input$sequencevariable
  input$seqvar_slider
  isolate({
    temp = plot.args2()
    unique.singlevals = unique(as.data.frame(temp$combinedData[["region.data"]])[, temp$combinedData$sequence.var])
    unique.singlevals = unique.singlevals[!is.na(unique.singlevals)]
    if(!is.null(input$sequencevariable) && length(input$sequencevariable) > 0)
      paste("Value of", input$sequencevariable, ":", unique.singlevals[input$seqvar_slider])
  })
  
})

output$seqvar_slider_panel = renderUI({
  get.data.set()
  input$sequencevariable
  ret = NULL
  isolate({
    temp = plot.args2()
    
    if(!is.null(temp$match.list) && temp$match.list$multiple.obs) {
      unique.singlevals = unique(as.data.frame(temp$combinedData[["region.data"]])[, temp$combinedData$sequence.var])
      unique.singlevals = unique.singlevals[!is.na(unique.singlevals)]
      n.unique.singlevals = length(unique.singlevals)
      if(!is.null(input$sequencevariable) && length(input$sequencevariable) > 0) 
        ret = sliderInput("seqvar_slider", 
                          label = NULL, 
                          min = 1, 
                          max = n.unique.singlevals, 
                          value = n.unique.singlevals, step = 1, ticks = FALSE)
    }
    
  })
  
  ret
})



## observe multipleobsoption and seqvar_slider
observe({
  input$multipleobsoption
  input$seqvar_slider
  
  isolate({
    temp = plot.args2()
    if(!is.null(temp$match.list) && temp$match.list$multiple.obs)
      if(length(input$multipleobsoption) > 0 && input$multipleobsoption == 1) {
        
        unique.singlevals = unique(as.data.frame(temp$combinedData[["region.data"]])[, temp$combinedData$sequence.var])
        unique.singlevals = unique.singlevals[!is.na(unique.singlevals)]
        
        args2$multipleObsOption = "singleval"
        #      args2$combinedData$type = ifelse(input$plotas_options == 1, "region", "points")
        if(!is.null(input$seqvar_slider) && length(input$seqvar_slider) > 0) {
          args2$plotCurrentSeqVal = unique.singlevals[input$seqvar_slider]
          args2$combinedData = iNZightMaps::iNZightMapAggregation(temp$combinedData,
                                                                  "singlevalue",
                                                                  single.value = unique.singlevals[input$seqvar_slider])
        }
        
        
        #      print(temp$combinedData$sequence.var)
        #      print(unique.singlevals[input$seqvar_slider])
        #      print(plot.args2()$plotCurrentSeqVal)
        
        ## update the plot title 
        if(!is.null(input$vartodisplay) && length(input$vartodisplay) == 1) {
          args2$plotTitle = paste(input$vartodisplay, " (", unique.singlevals[input$seqvar_slider], ")")
          updateTextInput(session, "advancedplotoptions_plottitle", 
                          value = paste(input$vartodisplay, " (", unique.singlevals[input$seqvar_slider], ")"))
        }
        
      }
    
    
    
  })
})


## observe multipleobsoption and linecharttypeoption
observe({
  input$multipleobsoption
  input$linecharttypeoption
  
  isolate({
    if(length(input$multipleobsoption) > 0 && input$multipleobsoption == 2) {
      args2$multipleObsOption = "allvalues"
      args2$combinedData$type = "sparklines"
      args2$plotCurrentSeqVal = NULL
      args2$plotSparklinesType = input$linecharttypeoption
      
      if(!is.null(input$vartodisplay) && length(input$vartodisplay) > 0) {
        vars.to.keep = sapply(as.data.frame(plot.args2()$combinedData$region.data)[, input$vartodisplay, drop = FALSE], is.numeric)
        if(sum(vars.to.keep) > 0)
          updateSelectInput(session, "vartodisplay", selected = input$vartodisplay[vars.to.keep])
        else
          updateSelectInput(session, "vartodisplay", selected = NULL)
      }
      
    }
    
  })
})



## observe multipleobsoption and aggregateoption
observe({
  input$multipleobsoption
  input$aggregateoption
  
  isolate({
#    temp = plot.args2()
    if(length(input$multipleobsoption) > 0 && input$multipleobsoption == 3) {
      args2$multipleObsOption = "aggregate"
      args2$plotCurrentSeqVal = input$aggregateoption
      args2$combinedData = iNZightMaps::iNZightMapAggregation(args2$combinedData,
                                                              tolower(input$aggregateoption))
    }
  })
})





## set up plotas_panel
output$plotas_panel = renderUI({
  get.data.set()
#  plot.args2()
  input$multipleobsoption
  input$datavariable
  input$mapvariable
  input$sequencevariable
  
  ret = NULL
  isolate({
    
    temp = plot.args2()
    
    if(!is.null(temp$combinedData))
      ## conditional when "Single Value" or "Aggregate" is selected
      if((!is.null(input$multipleobsoption) && length(input$multipleobsoption) > 0 && input$multipleobsoption != 2) ||
         (!is.null(temp$match.list) && !temp$match.list$multiple.obs)) {
        ret = list(fixedRow(column(3, h5("Plot as:")),
                            column(6, radioButtons(inputId = "plotas_options",
                                                   label = NULL,
                                                   choices = c("Regions" = 1, "Centroids" = 2),
                                                   selected = input$plotas_options,
                                                   inline = TRUE))),
                   
                   conditionalPanel(condition = "input.plotas_options == 2",
                                        fixedRow(column(3, h5("Size by:")),
                                                 column(6, selectInput(inputId = "plotas_sizeby",
                                                                       label = NULL,
                                                                       choices = c(" ", sort(iNZightMaps::iNZightMapVars(temp$combinedData, TRUE)[temp$combinedData$var.types %in% c("numeric", "integer")])),
                                                                       selected = input$plotas_sizeby,
                                                                       selectize = F)))))
        

      }
    
    
      
  })
  
  ret
})


## observe plotas_options
#observe({
#  input$plotas_options
#  isolate({
#    if(length(input$plotas_options) > 0 && input$plotas_options == 2)
#      args2$mapType = "point"
#    else
#      args2$mapType = "region"
#  })
#})

## observe plotas_sizeby
observe({
  input$plotas_sizeby
  isolate({
    if(length(input$plotas_sizeby) > 0) {
      if(input$plotas_sizeby == " ")
        args2$mapSizeVar = NULL
      else
        args2$mapSizeVar = input$plotas_sizeby
    }
    
  })
})



## set up sizeandtransparency_panel
output$sizeandtransparency_panel = renderUI({
  get.data.set()
#  plot.args2()
  input$multipleobsoption
  input$plotas_options
  ret = NULL
  
  isolate({
    
    temp = plot.args2()
    ## conditional when "allvalues" is selected 
    
    if((length(input$multipleobsoption) > 0 && input$multipleobsoption == 2)) {
      ret = list(fixedRow(column(3, h5("Transparency:")),
                          column(6, sliderInput("advancedplotoptions_transparency", 
                                                label = NULL, 
                                                min = 0, 
                                                max = 1, 
                                                value = input$advancedplotoptions_transparency, step = 0.1, ticks = FALSE))),
                 fixedRow(column(3, h5("Size:")),
                          column(6, sliderInput("advancedplotoptions_size", 
                                                label = NULL, 
                                                min = 1, 
                                                max = 10, 
                                                value = temp$plotConstantSize, step = 1, ticks = FALSE))))
    }
    else if(length(input$plotas_options) > 0 && input$plotas_options == 2 && 
            (length(input$multipleobsoption) > 0 && (input$multipleobsoption == 1 | input$multipleobsoption == 3) ||
             !is.null(temp$match.list) && !temp$match.list$multiple.obs)) {
      ret = fixedRow(column(3, h5("Size:")),
                     column(6, sliderInput("advancedplotoptions_size", 
                                           label = NULL, 
                                           min = 1, 
                                           max = 10, 
                                           value = temp$plotConstantSize, step = 1, ticks = FALSE)))
    }

    
  })
  
  ret
})






## set up maplocation_panel and locationvariable_panel

output$maplocation_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "maplocation",
                label = "Map Location:",
                choices = c("Select Map Location", "world"),
                selected = "world",
                selectize = FALSE)
  })
})

output$locationvariable_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "locationvariable",
                label = "Location Variable:",
                choices = c("Select Location Variable", characterVars()),
                selectize = FALSE)
  })
})


## update mapsplot.obj when input$map_type == 2
observe({
  input$maplocation
  input$locationvariable
  isolate({
    temp.data = get.data.set()
    if(input$map_type == 2 &&
       !is.null(input$maplocation) &&
       !is.null(input$locationvariable) &&
       input$locationvariable %in% colnames(temp.data)) {
      mapsplot.obj = iNZightShapeMap(data = temp.data,
                                     location = input$maplocation,
                                     data.region = input$locationvariable)
      args$x = mapsplot.obj
    }
  })
})


## set up plottingvariable_panel
## update args$variable
output$plottingvariable_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "plottingvariable",
                label = NULL,
                choices = c("Select Variable", numericVars()),
                selectize = FALSE)
  })
})

observe({
  input$plottingvariable
  isolate({
    if(!is.null(input$plottingvariable) &&
       input$plottingvariable %in% colnames(get.data.set()))
      args$variable = eval(parse(text = paste("~", input$plottingvariable)))
  })
})


## set up plot_region_colour_panel, missingvaluecolour_panel and plotlabels_panel
## update args$col, args$na.fill and args$name
output$plot_region_colour_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "plot_region_colour",
                label = "Colour:",
                choices = c("red", "darkblue", "darkgreen", "darkmagenta",
                            "darkslateblue", "hotpink4", "lightsalmon2",
                            "palegreen3", "steelblue3",
                            "heat", "terrain"),
                selectize = FALSE)
  })
})


output$missingvaluecolour_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "missingvaluecolour",
                label = "Missing value colour:",
                choices = c("grey50", "lightslategrey", "white", "black", "red"),
                selectize = FALSE)
  })
})


output$plotlabels_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "plotlabels",
                label = "Plot Labels:",
                choices = c(c("None", paste(input$locationvariable, "name"), "Value", "Both")),
                selectize = FALSE)
  })
})


observe({
  input$plot_region_colour
  isolate({
    if(!is.null(input$plot_region_colour) && input$plot_region_colour %in% c("heat", "terrain")) {
      args$col.fun = input$plot_region_colour
    }
    else if(!is.null(input$plot_region_colour) && !(input$plot_region_colour %in% c("heat", "terrian")))
      args$col.fun <- NULL
      args$col = input$plot_region_colour
  })
})


observe({
  input$missingvaluecolour
  isolate({
    if(!is.null(input$missingvaluecolour))
      args$na.fill = input$missingvaluecolour
  })
})


observe({
  input$plotlabels
  isolate({
    if(!is.null(input$plotlabels)) {
      if(input$plotlabels == "None") 
        args$name = ""
      else if(input$plotlabels == "Value") 
        args$name = "v"
      else if(input$plotlabels == "Both") 
        args$name = "b"
      else args$name = "r"
    }
  })
})


## save maps
output$savemaps = downloadHandler(
  filename = function() {
    paste("Maps", input$savemapstype, sep = ".")
  },
  content = function(file) {
    
    if(input$savemapstype == "jpg")
      jpeg(file)
    else if(input$savemapstype == "png")
      png(file)
    else if(input$savemapstype == "pdf")
      pdf(file, useDingbats = FALSE)
    

    if(input$map_type == 1) {
      condition1 = !is.null(input$select_latitude) &&
        input$select_latitude %in% colnames(get.data.set()) &&
        !is.null(input$select_longitude) &&
        input$select_longitude %in% colnames(get.data.set())
      if(condition1) {
        temp = plot.args()
        tryCatch({do.call(plot, temp)}, 
                 error = function(e) {
                   print(e)
                 }, finally = {})
      }
    }
    else if(input$map_type == 2) {
      condition2 = !is.null(input$maplocation) &&
        input$maplocation != "" && 
        !is.null(input$locationvariable) &&
        input$locationvariable %in% colnames(get.data.set()) &&
        !is.null(input$plottingvariable) &&
        input$plottingvariable %in% colnames(get.data.set())
      if(condition2) {
        temp = plot.args()
        tryCatch({do.call(plot, temp)}, 
                 error = function(e) {
                   print(e)
                 }, finally = {})
      }
    }
    
    dev.off()
  }) 


## observe advancedplotoptions_plottitle
observe({
  input$advancedplotoptions_plottitle
  isolate({
    args2$plotTitle = input$advancedplotoptions_plottitle
  })
})

## update plot title
observe({
  input$vartodisplay
  input$multipleobsoption
  input$seqvar_slider
  
  isolate({
    temp = plot.args2()
    if(!is.null(input$vartodisplay) && length(input$vartodisplay) > 0) {
      
      if(length(input$vartodisplay) > 1) {
        args2$plotTitle = ""
        updateTextInput(session, "advancedplotoptions_plottitle", value = "")
      }
      else {
        if(!is.null(temp$match.list) && temp$match.list$multiple.obs) {
          if (length(input$multipleobsoption) > 0 && input$multipleobsoption == 1) {
            #                  temp = plot.args2()
            args2$plotTitle = paste(input$vartodisplay, " (", temp$plotCurrentSeqVal, ")")
            updateTextInput(session, "advancedplotoptions_plottitle", 
                            value = paste(input$vartodisplay, " (", temp$plotCurrentSeqVal, ")"))
          } 
          else if (length(input$multipleobsoption) > 0 && input$multipleobsoption == 3) {
            args2$plotTitle = value = paste(input$vartodisplay, " (", input$aggregateoption, ")")
            updateTextInput(session, "advancedplotoptions_plottitle", 
                            value = paste(input$vartodisplay, " (", input$aggregateoption, ")"))
          } 
          else {
            args2$plotTitle = input$vartodisplay
            updateTextInput(session, "advancedplotoptions_plottitle", value = input$vartodisplay)
          }
        }
        else {
          args2$plotTitle = input$vartodisplay
          updateTextInput(session, "advancedplotoptions_plottitle", value = input$vartodisplay)
        }
      }
    }
  })
})



## plot when map information obtained
output$maps_plot = renderPlot({
  get.data.set()
  
  ## coordinate
#  input$type1_plottitle_confirm
  
  
  
  args2$plotTitle
  plot.args()
  
  args2$updateplot1
  
  input$datavariable
  input$mapvariable
  
  input$vartodisplay
  
  input$advancedmapoptions_projection
  input$advancedplotoptions_mappalette
  input$advancedplotoptions_dark
  input$advancedplotoptions_gridlines
  
  input$advancedplotoptions_transparency
  input$advancedplotoptions_size
  
  input$plotas_options
  input$plotas_sizeby
  
  input$multipleobsoption
  input$aggregateoption
  input$linecharttypeoption
  input$advancedplotoptions_regionlabels
  input$regionlabels_click
  input$regionlabels_slider
  input$plottitlefontsize_slider
  input$seqvar_slider
  
  input$advancedplotoptions_mapscales
  input$scales_confirm
  
  input$advancedplotoptions_axislabels
  
  input$check_regions
  
  #  input$loadshapefiles
  
  isolate({
    if(input$map_type == 1) {
      condition1 = !is.null(input$select_latitude) &&
        input$select_latitude %in% colnames(get.data.set()) &&
        !is.null(input$select_longitude) &&
        input$select_longitude %in% colnames(get.data.set())
      if(condition1) {
        temp = plot.args()
        tryCatch({do.call(plot, temp)}, 
                 error = function(e) {
                   print(e)
                 }, finally = {})
      }
    }
    else if(input$map_type == 2) {
      temp.data = get.data.set()
      temp = plot.args2()
      mapData = temp$mapData
      
      if(!is.null(mapData) &&
         !is.null(input$datavariable) && input$datavariable %in% colnames(temp.data) &&
         !is.null(input$mapvariable)) {
        
        ## update "match.list" information after loading data        
        match.list = iNZightMaps::matchVariables(temp.data[, input$datavariable],
                                                 as.data.frame(mapData)[, input$mapvariable])
        
        args2$match.list = match.list
          
        
        if(!is.null(temp$combinedData) &&
           !is.null(input$vartodisplay) && 
           all(input$vartodisplay %in% colnames(temp.data))) {
          
          #          temp$combinedData$type = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2, 
          #                                          "point", "region")
          args2$combinedData$type = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2, 
                                           "point", "region")
          
          args2$mapType = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2,
                                 "point", "region")
          
          if(length(input$multipleobsoption) > 0 && input$multipleobsoption == 2)
            args2$combinedData$type = "sparklines"
          
          ## multiple varibles to display ?
          if(length(input$vartodisplay) > 1)
            multiple.variables = TRUE
          else
            multiple.variables = FALSE
          
          if(is.null(temp$multipleObsOption)) {
            if(multiple.variables)
              aggregate.logical = TRUE
            else
              aggregate.logical = FALSE
          }
          else {
            if(multiple.variables && temp$multipleObsOption != "allvalues")
              aggregate.logical = TRUE
            else
              aggregate.logical = FALSE
          }
          
          ## update plotConstantSize (size for points)
          if(!is.null(input$advancedplotoptions_size) && length(input$advancedplotoptions_size) > 0)
            args2$plotConstantSize = input$advancedplotoptions_size
          
          
          ## update plotConstantAlpha
          if(!is.null(input$advancedplotoptions_transparency) && length(input$advancedplotoptions_transparency) > 0)
            args2$plotConstantAlpha = 1-input$advancedplotoptions_transparency
          
          ## update advancedplotoptions_plottitle
#          if(!is.null(input$advancedplotoptions_plottitle))
#            args2$plotTitle = input$advancedplotoptions_plottitle
          
          
          
          temp = plot.args2()
          grid::grid.draw(plot(temp$combinedData, 
                               main = temp$plotTitle,
                               axis.labels = temp$plotAxes, xlab = temp$plotXLab, ylab = temp$plotYLab,
                               datum.lines = temp$plotDatumLines, 
                               projection = temp$plotProjection,
                               multiple.vars = multiple.variables, 
                               colour.var = input$vartodisplay,
                               size.var = temp$mapSizeVar, 
                               aggregate = aggregate.logical,
                               darkTheme = temp$plotTheme, 
                               alpha.const = temp$plotConstantAlpha, 
                               size.const = temp$plotConstantSize,
                               current.seq = temp$plotCurrentSeqVal, 
                               palette = temp$plotPalette,
                               sparkline.type = temp$plotSparklinesType,
                               scale.limits = temp$plotScaleLimits,
                               label.var = temp$plotLabelVar,
                               scale.label = temp$plotLabelScale,
                               scale.axis = temp$plotAxisScale,
                               regions.to.plot = temp$mapRegionsPlot, 
                               keep.other.regions = temp$mapExcludedRegions))
          
        }
        
        else {
          matchplot.colours = c("#d95f02", "#1b9e77", "#7570b3")
          plot(sf::st_geometry(mapData$geometry), col = matchplot.colours[match.list$map.matched + 1])
          legend("topleft", legend = c("Data present for region",
                                       "Data missing for region"),
                 fill = matchplot.colours[2:1])
        }
        
      }
      
    }
  })
})



## to display the interactive maps
output$interactive.maps = renderUI({
  get.data.set()
  
  args2$plotTitle
  plot.args()
  
  args2$updateplot1
  
  input$datavariable
  input$mapvariable
  
  input$vartodisplay
  
  input$advancedmapoptions_projection
  input$advancedplotoptions_mappalette
  input$advancedplotoptions_dark
  input$advancedplotoptions_gridlines
  
  input$advancedplotoptions_transparency
  input$advancedplotoptions_size
  
  input$plotas_options
  input$plotas_sizeby
  
  input$multipleobsoption
  input$aggregateoption
  input$linecharttypeoption
  input$advancedplotoptions_regionlabels
  input$regionlabels_click
  input$regionlabels_slider
  input$plottitlefontsize_slider
  input$seqvar_slider
  
  input$advancedplotoptions_mapscales
  input$scales_confirm
  
  input$advancedplotoptions_axislabels
  
  input$check_regions
  isolate({
    
    if(input$map_type == 2) {
      
      temp.data = get.data.set()
      temp = plot.args2()
      mapData = temp$mapData
      
      if(!is.null(mapData) &&
         !is.null(input$datavariable) && input$datavariable %in% colnames(temp.data) &&
         !is.null(input$mapvariable)) {
        
        ## update "match.list" information after loading data        
        match.list = iNZightMaps::matchVariables(temp.data[, input$datavariable],
                                                 as.data.frame(mapData)[, input$mapvariable])
        
        args2$match.list = match.list
        
        if(!is.null(temp$combinedData) &&
           !is.null(input$vartodisplay) && 
           all(input$vartodisplay %in% colnames(temp.data))) {
          
          args2$combinedData$type = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2, 
                                           "point", "region")
          
          args2$mapType = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2,
                                 "point", "region")
          
          if(length(input$multipleobsoption) > 0 && input$multipleobsoption == 2)
            args2$combinedData$type = "sparklines"
          
          ## multiple varibles to display ?
          if(length(input$vartodisplay) > 1)
            h4("iNZight doesn't handle interactive maps for multiple variables ... yet! 
               Please select only one variable")
#            multiple.variables = TRUE
          else {
            multiple.variables = FALSE
            
            if(is.null(temp$multipleObsOption)) {
              if(multiple.variables)
                aggregate.logical = TRUE
              else
                aggregate.logical = FALSE
            }
            else {
              if(multiple.variables && temp$multipleObsOption != "allvalues")
                aggregate.logical = TRUE
              else
                aggregate.logical = FALSE
            }
            
            ## update plotConstantSize (size for points)
            if(!is.null(input$advancedplotoptions_size) && length(input$advancedplotoptions_size) > 0)
              args2$plotConstantSize = input$advancedplotoptions_size
            
            
            ## update plotConstantAlpha
            if(!is.null(input$advancedplotoptions_transparency) && length(input$advancedplotoptions_transparency) > 0)
              args2$plotConstantAlpha = 1-input$advancedplotoptions_transparency
            
            ## update advancedplotoptions_plottitle
#            if(!is.null(input$advancedplotoptions_plottitle))
#              args2$plotTitle = input$advancedplotoptions_plottitle
            
            temp = plot.args2()
            x = plot(temp$combinedData, 
                     main = temp$plotTitle,
                     axis.labels = temp$plotAxes, xlab = temp$plotXLab, ylab = temp$plotYLab,
                     datum.lines = temp$plotDatumLines, 
                     projection = temp$plotProjection,
                     multiple.vars = multiple.variables, 
                     colour.var = input$vartodisplay,
                     size.var = temp$mapSizeVar, 
                     aggregate = aggregate.logical,
                     darkTheme = temp$plotTheme, 
                     alpha.const = temp$plotConstantAlpha, 
                     size.const = temp$plotConstantSize,
                     current.seq = temp$plotCurrentSeqVal, 
                     palette = temp$plotPalette,
                     sparkline.type = temp$plotSparklinesType,
                     scale.limits = temp$plotScaleLimits,
                     label.var = temp$plotLabelVar,
                     scale.label = temp$plotLabelScale,
                     scale.axis = temp$plotAxisScale,
                     regions.to.plot = temp$mapRegionsPlot, 
                     keep.other.regions = temp$mapExcludedRegions)            
            
            pdf(NULL)
            addr = iNZightPlots::exportHTML(x = x, 
                                            mapObj = temp$combinedData,
                                            file = tempfile(fileext = ".html"))
            
            
            
            addr = unclass(addr)
            #          temp.dir = substr(unclass(local.dir), 1, nchar(unclass(local.dir)) - 11)
            #          addResourcePath("path", temp.dir)
            temp.dir = substring(addr, 1, regexpr("file", addr)-1)
            addResourcePath("path", temp.dir)
            filename = substring(addr, regexpr("file", addr))
            tags$div(tags$a(href = paste("path/", filename, sep = ""), 
                            "Open in a new window", 
                            target="_blank"),
                     tags$iframe(
                       seamless = "seamless",
                       src = paste("path/", filename, sep = ""),
                       height = 600, width = 1200
                     ))
            #tags$iframe(
            #  seamless = "seamless",
            #  #            src = "path/index.html",
            #  src = paste("path/", filename, sep = ""),
            #  height = 600, width = 2000
            #)
          }
        }
      }
    }
    
    else if(input$map_type == 1) {
      condition1 = !is.null(input$select_latitude) &&
        input$select_latitude %in% colnames(get.data.set()) &&
        !is.null(input$select_longitude) &&
        input$select_longitude %in% colnames(get.data.set())
      if(condition1) {
        
        local.dir = iNZightPlots:::exportHTML.function(createmap.html,
                                                       width = 10, height = 6)
        
        local.dir = unclass(local.dir)
        temp.dir = substr(unclass(local.dir), 1, nchar(unclass(local.dir)) - 11)
        addResourcePath("path", temp.dir)
        tags$div(tags$a(href = "path/index.html", 
                        "Open in a new window", 
                        target="_blank"),
                 tags$iframe(
                   seamless = "seamless",
                   src = "path/index.html",
                   height = 600, width = 1200
                 ))
        #tags$iframe(
        #  seamless = "seamless",
        #  src = "path/index.html",
        #  height = 600, width = 1200
        #)
#        temp1 = plot.args()
        
#        tryCatch({do.call(plot, temp1)}, 
#                 error = function(e) {
#                   print(e)
#                 }, finally = {})
        
#        pdf(NULL)
#        addr = iNZightPlots::exportHTML(x = x, 
#                                        file = tempfile(fileext = ".html"))
        
        
        
#        addr = unclass(addr)
        
#        temp.dir = substring(addr, 1, regexpr("file", addr)-1)
#        addResourcePath("path", temp.dir)
#        filename = substring(addr, regexpr("file", addr))
#        tags$iframe(
#          seamless = "seamless",
          #            src = "path/index.html",
#          src = paste("path/", filename, sep = ""),
#          height = 600, width = 2000
#        )
      }
    }
  })
})



## the download button for the interactive plot
output$interactive.plot.download = renderUI({
  get.data.set()
  input$map_type
  isolate({
    if(input$map_type == 1)
      ret = fixedRow(column(width = 9,
                            NULL),
                     
                     column(width = 3, 
                            downloadButton(outputId = "save_interactive_mapplot1", 
                                           label = "Download Plot"))
      )
    else if(input$map_type == 2)
      ret = fixedRow(column(width = 9,
                            NULL),
                     
                     column(width = 3, 
                            downloadButton(outputId = "save_interactive_mapplot2", 
                                           label = "Download Plot"))
      )
    ret
  })
})


## reaction to the "save_interactive_mapplot" button
output$save_interactive_mapplot1 = downloadHandler(
  filename = substring(tempfile(pattern = "file", tmpdir = "", fileext = ".html"), 2),
  content = function(file) {
    
      condition1 = !is.null(input$select_latitude) &&
        input$select_latitude %in% colnames(get.data.set()) &&
        !is.null(input$select_longitude) &&
        input$select_longitude %in% colnames(get.data.set())
      if(condition1) {
        
        local.dir = iNZightPlots:::exportHTML.function(createmap.html,
                                                       width = 10, height = 6)
        
        src = normalizePath(local.dir)
        owd = setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "index.html")
        file.copy("index.html", file)

      }
  })  


output$save_interactive_mapplot2 = downloadHandler(
  filename = substring(tempfile(pattern = "file", tmpdir = "", fileext = ".html"), 2),
  content = function(file) {

      temp.data = get.data.set()
      temp = plot.args2()
      mapData = temp$mapData
      
      if(!is.null(mapData) &&
         !is.null(input$datavariable) && input$datavariable %in% colnames(temp.data) &&
         !is.null(input$mapvariable)) {
        
        ## update "match.list" information after loading data        
        match.list = iNZightMaps::matchVariables(temp.data[, input$datavariable],
                                                 as.data.frame(mapData)[, input$mapvariable])
        
        args2$match.list = match.list
        
        if(!is.null(temp$combinedData) &&
           !is.null(input$vartodisplay) && 
           all(input$vartodisplay %in% colnames(temp.data))) {
          
          args2$combinedData$type = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2, 
                                           "point", "region")
          
          args2$mapType = ifelse(length(input$plotas_options) > 0 && input$plotas_options == 2,
                                 "point", "region")
          
          if(length(input$multipleobsoption) > 0 && input$multipleobsoption == 2)
            args2$combinedData$type = "sparklines"
          
          ## multiple varibles to display ?
          if(length(input$vartodisplay) > 1)
            h4("iNZight doesn't handle interactive maps for multiple variables ... yet! 
               Please select only one variable")
          #            multiple.variables = TRUE
          else {
            multiple.variables = FALSE
            
            if(is.null(temp$multipleObsOption)) {
              if(multiple.variables)
                aggregate.logical = TRUE
              else
                aggregate.logical = FALSE
            }
            else {
              if(multiple.variables && temp$multipleObsOption != "allvalues")
                aggregate.logical = TRUE
              else
                aggregate.logical = FALSE
            }
            
            ## update plotConstantSize (size for points)
            if(!is.null(input$advancedplotoptions_size) && length(input$advancedplotoptions_size) > 0)
              args2$plotConstantSize = input$advancedplotoptions_size
            
            
            ## update plotConstantAlpha
            if(!is.null(input$advancedplotoptions_transparency) && length(input$advancedplotoptions_transparency) > 0)
              args2$plotConstantAlpha = 1-input$advancedplotoptions_transparency
            
            ## update advancedplotoptions_plottitle
            #            if(!is.null(input$advancedplotoptions_plottitle))
            #              args2$plotTitle = input$advancedplotoptions_plottitle
            
            temp = plot.args2()
            x = plot(temp$combinedData, 
                     main = temp$plotTitle,
                     axis.labels = temp$plotAxes, xlab = temp$plotXLab, ylab = temp$plotYLab,
                     datum.lines = temp$plotDatumLines, 
                     projection = temp$plotProjection,
                     multiple.vars = multiple.variables, 
                     colour.var = input$vartodisplay,
                     size.var = temp$mapSizeVar, 
                     aggregate = aggregate.logical,
                     darkTheme = temp$plotTheme, 
                     alpha.const = temp$plotConstantAlpha, 
                     size.const = temp$plotConstantSize,
                     current.seq = temp$plotCurrentSeqVal, 
                     palette = temp$plotPalette,
                     sparkline.type = temp$plotSparklinesType,
                     scale.limits = temp$plotScaleLimits,
                     label.var = temp$plotLabelVar,
                     scale.label = temp$plotLabelScale,
                     scale.axis = temp$plotAxisScale,
                     regions.to.plot = temp$mapRegionsPlot, 
                     keep.other.regions = temp$mapExcludedRegions)            
            
            pdf(NULL)
#            addr = iNZightPlots::exportHTML(x = x, 
#                                            mapObj = temp$combinedData,
#                                            file = tempfile(fileext = ".html"))
            
            
#            src = normalizePath(addr)
#            owd = setwd(tempdir())
#            on.exit(setwd(owd))
#            file.copy(src, "index.html")
#            file.copy("index.html", file)
            temp.name = tempfile(fileext = ".html")
            addr = iNZightPlots::exportHTML(x = x, 
                                            mapObj = temp$combinedData,
                                            file = temp.name)
            
            
            src = normalizePath(addr)
            owd = setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, temp.name)
            file.copy(temp.name, file)
          }
        }
      }
  })  




# function for creating html files (maps with coordinate)

createmap.html = function() {
  condition1 = !is.null(input$select_latitude) &&
    input$select_latitude %in% colnames(get.data.set()) &&
    !is.null(input$select_longitude) &&
    input$select_longitude %in% colnames(get.data.set())
  if(condition1) {
    temp = plot.args()
    tryCatch({do.call(plot, temp)}, 
             error = function(e) {
               print(e)
             }, finally = {})
  }
} 













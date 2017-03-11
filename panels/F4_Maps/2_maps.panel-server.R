###---------------------------------------------------###
###  Server Functions for the "Maps" Module  ###
###---------------------------------------------------###
###
###  Date Created  : Feb 22, 2017.
###  Last Modified : Mar 09, 2017.
###  
###
###  * Note: This is to be sourced within "server.R" *

## initialize gui
output$maps.panel = renderUI({
  get.data.set()
  maps.panel.ui(get.data.set())
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
  type = NULL,
  col.pt = NULL,
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
  na.fill = NULL,
  name = NULL
)


plot.args = reactive({
  plot.args = modifyList(list(), reactiveValuesToList(args), keep.null = FALSE)
})


## maps plot
output$maps_plot = renderPlot({
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
})


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

## set up colourby_panel, sizeby_panel and opacifyby_panel
output$colourby_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "colourby",
                label = "Colour by:",
                choices = c("", colnames(get.data.set())),
                selectize = FALSE)
  })
})

output$sizeby_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "sizeby",
                label = "Size by:",
                choices = c("", numericVars()),
                selectize = FALSE)
  })
})

output$opacifyby_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "opacifyby",
                label = "Opacify by:",
                choices = c("", numericVars()),
                selectize = FALSE)
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
                choices = c("roadmap", "satellite", "terrain", "hybrid"),
                selectize = FALSE)
  })
})

output$plot_colour_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "plot_colour",
                label = "Colour:",
                choices = c("grey50", "black", "darkblue", "darkgreen", "darkmagenta",
                            "darkslateblue", "hotpink4", "lightsalmon2", "palegreen3",
                            "steelblue3"),
                selectize = FALSE)
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


## update args$cex.pt, args$alpha, args$join
observe({
  input$pointsize
  isolate({
    if(!is.null(input$pointsize))
      args$cex.pt = input$pointsize
  })
})

observe({
  input$transparency
  isolate({
    if(!is.null(input$transparency))
      args$alpha = 1 - input$transparency/100
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
                choices = c("red", "black", "blue", "green4", "yellow",
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
          mapsplot.obj = iNZightShapeMap(data = data.temp,
                                         location = input$maplocation,
                                         data.region = input$locationvariable)
          data.temp2 = mapsplot.obj$data
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
      temp = convert.to.factor(data.temp[, index])
      n.levels = length(levels(temp))
      sliderInput(inputId = "mapssubset1_slider",
                  label = paste("Subset '", input$mapssubset1, "':", "_MULTI"),
                  min = 0, max = n.levels, value = 0, step = 1,
                  animate = TRUE, ticks = F)
    })
  }
})



#observe({
#  input$mapssubset1_slider
#  isolate({
#    if(!is.null(input$mapssubset1) &&
#       input$mapssubset1 %in% colnames(get.data.set())) {
#      data.temp = get.data.set()
#      index = which(input$mapssubset1 == colnames(data.temp))
#      temp = convert.to.factor(data.temp[, index])
#      level = input$mapssubset1_slider
#      args$varnames$g1 = colnames(data.temp)[index]
#      if(input$mapssubset1_slider == 0) {
#        updateSliderInput(session, "mapssubset1_slider",
#                          label = paste("Subset '", input$mapssubset1, "': ", "_MULTI"))
#        args$g1.level = "_MULTI"
#      }
#      else {
#        updateSliderInput(session, "mapssubset1_slider",
#                          label = paste("Subset '", input$mapssubset1, "': ", levels(temp)[level]))
#        args$g1.level = levels(temp)[level]
#      }
#      if(input$map_type == 1) {
#        g1 = convert.to.factor(data.temp[, index])
#        args$g1 = g1
#      }
#      else if(input$map_type == 2) {
#        if(!is.null(input$maplocation) &&
#           !is.null(input$locationvariable) &&
#           input$locationvariable %in% colnames(data.temp)) {
#          mapsplot.obj = iNZightShapeMap(data = data.temp,
#                                         location = input$maplocation,
#                                         data.region = input$locationvariable)
#          data.temp2 = mapsplot.obj$data
#          g1 = convert.to.factor(data.temp2[, index])
#          args$g1 = g1
#        }
#      }
#    }
#  })
#})



observe({
  input$mapssubset1_slider
  isolate({
    if(!is.null(input$mapssubset1) &&
       input$mapssubset1 %in% colnames(get.data.set())) {
      data.temp = get.data.set()
      index = which(input$mapssubset1 == colnames(data.temp))
      temp = convert.to.factor(data.temp[, index])
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
          mapsplot.obj = iNZightShapeMap(data = data.temp,
                                         location = input$maplocation,
                                         data.region = input$locationvariable)
          data.temp2 = mapsplot.obj$data
          g1 = convert.to.factor(data.temp2[, index])
          args$g1 = g1
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
          mapsplot.obj = iNZightShapeMap(data = data.temp,
                         location = input$maplocation,
                         data.region = input$locationvariable)
          data.temp2 = mapsplot.obj$data
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
      temp = convert.to.factor(data.temp[, index])
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
      temp = convert.to.factor(data.temp[, index])
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
          mapsplot.obj = iNZightShapeMap(data = data.temp,
                         location = input$maplocation,
                         data.region = input$locationvariable)
          data.temp2 = mapsplot.obj$data
          g2 = convert.to.factor(data.temp2[, index])
          args$g2 = g2
        }
      }
    }
  })
})


## set up maplocation_panel and locationvariable_panel

output$maplocation_panel = renderUI({
  get.data.set()
  isolate({
    selectInput(inputId = "maplocation",
                label = "Map Location:",
                choices = c("Select Map Location", "world"),
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
    if(!is.null(input$plot_region_colour))
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











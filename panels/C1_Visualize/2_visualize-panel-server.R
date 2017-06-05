###-----------------------------------------------###
###  Server Functions for the "Visualize" Module  ###
###-----------------------------------------------###
###
###  Date Created   :   February 1, 2015
###  Last Modified  :   May 24, 2017.
###
###  Please consult the comments before editing any code.
###
###  * Note: This file is to be sourced locally within "server.R" * 

###  And on the first day of February, God said "Let there be data":
vis.data <- reactive({
#   values$data.set
  plot.par$data = get.data.set()
#   vis.data <- 
  get.data.set()
})




###  Then on the second day, he siad let there be parameters for
###  iNZightPlot():

plot.par.stored = reactiveValues(
  locate.id=NULL
  )

plot.par <- reactiveValues(
  x = NULL,
  y = NULL,
  varnames = list(x = NULL, y = NULL,
      xlab = NULL, ylab = NULL,
      g1 = NULL, g2 = NULL,
      colby=NULL,sizeby=NULL, symbolby = NULL),
  g1 = NULL,
  g2 = NULL,
  g1.level = 0,
  g2.level = 0,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim =  NULL,
  ylim = NULL,
  inzpars = inzpar(),
  colby=NULL,
  sizeby=NULL,
  symbolby = NULL,
  data=NULL,
  locate=NULL,
  locate.id=NULL,
  locate.col=NULL,
  locate.extreme=NULL,
  zoombar=NULL,
  design=NULL
)

identified.points = reactiveValues(values=list())

get.identified.points = reactive({
  sort(unique(unlist(identified.points$values)))
})

plot.ret.para = reactiveValues(
  parameters = NULL,
  default.num.bins=NULL
  )

get.plottype = reactive({
  attr(plot.ret.para$parameters,"plottype")
})

get.nbins = reactive({
#   print(attr(plot.ret.para$parameters,"nbins"))
  attr(plot.ret.para$parameters,"nbins")
})

get.default.num.bins = reactive({
  plot.ret.para$default.num.bins
})


##  These are the list of parameters in inzPlotDefaults()
graphical.par = reactiveValues(
  alpha = 1,
  bg = "grey93", #background colour
  ##  Box
  box.col = "black",
  box.fill = "white", # fill colour for the boxplot
  ##  Bar
  bar.lwd = 1,
  bar.col = "black", # colour for borders of bars in bar plot
  bar.fill = colors()[81], # colour for inside of bars in bar plot
  ##  Line
  lwd = 1,
  lty = 1,
  lwd.pt = 2,
  col.line = "blue",
  ##  Point
  cex.pt = 0.5,
  cex.dotpt = 0.5,
  pch = 21, # fill colour of points
  col.pt = "gray50",
  fill.pt = "transparent",
  ##  Colours
  LOE = FALSE,
  col.LOE = "black",
  col.trend =  list(linear = "",
                    quadratic = "",
                    cubic =  ""),
  col.smooth = "",
  col.fun = NULL,
  col.method = "linear",
  ##  Jitter, rugs, and trend.
  jitter = "",
  rugs = "",
  trend = NULL,
  ##  Others
  cex = 1,
  quant.smooth = NULL,
  inference.type = NULL,
  inference.par = NULL,
#   largesample = NULL,
  join = FALSE,
  lines.by = FALSE,
  trend.by = FALSE,
  trend.parallel = T,
  lty.trend = list(linear = 1,
                   quadratic = 1,
                   cubic = 1),
  smooth = 0,
  szsym = 1,
  tpsym = 1,
  plottype="default",
  hist.bins=NULL,
  scatter.grid.bins=50,
  hex.bins=20,
  hex.style = "size",
  bs.inference=F,
  reverse.palette = FALSE,
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
)

##  Data handling
determine.class = function(input) {
  if (is.null(input)) {
    return(NULL)
  }
  if (class(input) == "integer") {
    input.class = "numeric"
  } else if (class(input) == "character") {
    input.class = "factor"
  } else {
    input.class = class(input)
  }
  input.class
}

##  Input Handling
#' Tests whether the input of a variable is valid.
#'
#' Returns NULL if the input is not valid or a list of two elements.
#' The elements are:
#'
#' input.out : return value of vis.data()[,input]
#' factor.levels : the number of levels or NULL if not a factor.
#'
#' @param input A shiny input variable as input$...
#' @param subs Whether (TRUE) the input variable is converted to
#' factor or (FALSE) not.
#' 
#' @return NULL if "input" is NULL or a list with two elements. 
#' If "input" is not a column name in the data the both elements 
#' of the return list are NULL, otherwise the column specified 
#' by input is returned.
#' 
#' @author Chris Park
handle.input = function(input, subs = FALSE) {
    if (is.null(input)) {
        return()
    }
    input.out = NULL
    factor.levels = NULL
    if (input != "none" && input %in% names(vis.data())) {
      if (subs) {
        if(class(vis.data()[, input])%in%"factor"||
             class(vis.data()[, input])%in%"character"){
          input.out = as.factor(vis.data()[, input])          
          factor.levels = nlevels(input.out)
        }else{
          tryCatch({
            input.out = convert.to.factor(vis.data()[, input])          
            factor.levels = nlevels(input.out)
          },error=function(e){
            print(e)
          })
        }
      } else {
        input.out = vis.data()[, input]
        factor.levels = NULL
      }
    } else {
        input.out = NULL
        factor.levels = NULL
    }
    list(input.out = input.out, factor.levels = factor.levels)
}

output$visualize.panel <- renderUI({
  get.data.set()
  isolate({
    visualize.panel.ui(get.data.set())
  })
})

x.class = reactive({
    determine.class(plot.par$x)
})

y.class = reactive({
    determine.class(plot.par$y)
})

determine.g = reactive({
    xy.class = c(x.class(), y.class())
    ##  0: x, y == NULL
    if (is.null(x.class()) && is.null(y.class())) {
        return(0)
    }
    ##  1:  x == "numeric" or y == "numeric"
    if (identical(xy.class, "numeric")) {
        return(1)
    }
    ##  2: x == "factor" or y == "factor"
    if (identical(xy.class, "factor")) {
        return(2)
    }
    ##  3: x == "factor" and y == "factor"
    if (identical(xy.class, rep("factor", 2))) {
        return(3)
    }
    ##  4: x == "factor" and y == "numeric"
    if (identical(xy.class, c("factor", "numeric"))) {
        return(4)
    }
    ##  5: x == "numeric" and y == "factor"
    if (identical(xy.class, c("numeric", "factor"))) {
        return(5)
    }
    ##  6: x == "numeric" and y == "numeric"
    if (identical(xy.class, rep("numeric", 2))) {
        return(6)
    }
    ##  7: Special structure (e.g. "ts" object)
    id = !(xy.class %in% c("numeric", "factor"))
    if (id) {
        return(c("x of incorrect class", "y of incorrect class")[id])
    }
})

##  Then on the third, he declared the need for parameters for the "visualize" module:
vis.par = reactive({
    vis.par = reactiveValuesToList(plot.par)
    if (!is.null(vis.par$x) && plot.par$varnames$x != "") {
        if(any(na.omit(vis.par$x) == "")){
          vis.par$x[which(vis.par$x == "")] = NA
        }
        if (determine.g() == 6) {
            temp = list(x = NULL, y = NULL,
                         varnames = list(x = "", y = ""))
            temp$x = vis.par$x
            temp$y = vis.par$y
            temp$varnames$x = vis.par$varnames$x
            temp$varnames$y = vis.par$varnames$y
            vis.par = modifyList(vis.par, temp, keep.null = TRUE)
        }
        vis.par = modifyList(reactiveValuesToList(graphical.par), vis.par, keep.null = TRUE)
    } else {
        NULL
    }
})

##  We write some UI outputs for variable selection and subsetting:
##
##  Variable 1
##
##  Select variable 1.
output$vari1_panel = renderUI({
  get.data.set()
  input$change_var_selection
  isolate({    
    sel = input$vari1    
    get.vars = parseQueryString(session$clientData$url_search)
    if(!is.null(get.vars$url)) {
      temp = session$clientData$url_search
      get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
    }
    if(length(get.vars)>0&&
         (any(names(get.vars)%in%"url")||
            any(names(get.vars)%in%"example"))&&
         (any(names(get.vars)%in%"x")&&
            !get.vars$x%in%"")){
      sel=get.vars$x
    }   
    if(!is.null(input$change_var_selection)){
      if(!input$change_var_selection){
        if(is.null(input$vari1) || input$vari1 == "none") {
          selectInput(inputId = "vari1",
                      label = NULL,
                      choices = c("none", colnames(vis.data())),
                      selected = sel,
                      # selectize = T,
                      selectize=F)
        }
        else {
          selectInput(inputId = "vari1",
                      label = NULL,
                      choices = c(colnames(vis.data())),
                      selected = sel,
                      # selectize = T,
                      selectize=F)
        }
        
      }else{
        selectInput(inputId = "vari1",
                    label = NULL,
                    choices = c(colnames(vis.data())),
                    selected = sel,
                    selectize=F,
                    size=2)
      }
    }
  })
})


observe({  
  input$vari1
  isolate({
    if((is.null(input$vari1) || input$vari1 == "none") && (!is.null(input$change_var_selection) && !input$change_var_selection)) {
      updateSelectInput(session, "vari1", choices = colnames(vis.data()), selected = colnames(vis.data())[1])
    }    
  })  
})



##  Update plot.par$x.
observe({
  if(!is.null(input$vari1)){
    isolate({
      vari1.par = handle.input(input$vari1)$input.out
      plot.par$x = vari1.par
      plot.par$varnames$x = input$vari1
      if(!is.null(vis.data())){
        ch  = colnames(vis.data())
        if(!is.null(input$vari1)&&input$vari1%in%ch){
          ch  = ch[-which(colnames(vis.data())%in%input$vari1)]
        }
        ch = c("none",ch)
        sel = input$vari2
        if(!is.null(sel)&&!sel%in%ch){
          sel = ch[1]
        }
        updateSelectInput(session,"vari2",choices=ch,selected=sel)
        ch  = colnames(vis.data())
        if(!is.null(input$vari1)&&input$vari1%in%ch){
          ch  = ch[-which(ch%in%input$vari1)]
        }
        if(!is.null(input$vari2)&&input$vari2%in%ch){
          ch  = ch[-which(ch%in%input$vari2)]
        }
        ch = c("none",ch)
        sel = input$subs1
        if(!is.null(sel)&&!sel%in%ch){
          sel = ch[1]
        }
        updateSelectInput(session,"subs1",choices=ch,selected=sel)
        ch  = colnames(vis.data())
        if(!is.null(input$vari1)){
          ch  = ch[-which(ch%in%input$vari1)]
        }
        if(!is.null(input$vari2)&&input$vari2%in%ch){
          ch  = ch[-which(ch%in%input$vari2)]
        }
        ch  = c("none",ch)
        sel = input$subs2
        if(!is.null(sel)&&!sel%in%ch){
          sel = ch[1]
        }
        updateSelectInput(session,"subs2",choices=ch,selected=sel)
      }
    })
  }
})


#  Subset variable 1.
output$subs1_panel = renderUI({
  get.data.set()
  isolate({
    ch  = colnames(vis.data())
    if(!is.null(input$vari1)&&input$vari1%in%ch){
      ch  = ch[-which(ch%in%input$vari1)]
    }
    if(!is.null(input$vari2)&&input$vari2%in%ch){
      ch  = ch[-which(ch%in%input$vari2)]
    }
    if(!is.null(input$subs2)&&input$subs2%in%ch){
      ch  = ch[-which(ch%in%input$subs2)]
    }
    sel = input$subs1
    selectInput(inputId = "subs1",
                label = NULL,
                choices = c("none", ch),
                selected = sel,
                selectize=F)
  })
})

##  Update plot.par$g1.
observe({
  input$subs1
  isolate({
    subs1.par = handle.input(input$subs1, subs = TRUE)$input.out
    plot.par$g1 = subs1.par
    varnames.g1 = input$subs1
    if(!is.null(varnames.g1)&&
         varnames.g1%in%"none"){
      varnames.g1 = NULL
    }
    plot.par$varnames$g1 = varnames.g1
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if(is.null(choices1)){
      choices1 = 1
    }
    if(!is.null(input$subs1)&&
         !input$subs1%in%""&&
         !input$subs1%in%"none"){
      updateSliderInput(session,"sub1_level",
                        label = paste0("Subset '", input$subs1, "':"),
                        min = 0, max = choices1, value = 0,step=1)
    }
  })
})

##  Update plot.par$g1.
observe({
  input$subs1
  isolate({
    subs1.par = handle.input(input$subs1, subs = TRUE)$input.out
    plot.par$g1 = subs1.par
    varnames.g1 = input$subs1
    if(!is.null(varnames.g1)&&
         varnames.g1%in%"none"){
      varnames.g1 = NULL
    }
    plot.par$varnames$g1 = varnames.g1
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if(is.null(choices1)){
      choices1 = 1
    }
#     print(choices1)
#     print("--mini--")
    updateSliderInput(session,"sub1_level_mini",
                      label = paste0("Subset '", input$subs1, "':"),
                      min = 0, max = choices1, value = 0,step=1)
  })
})

#  Subset level (Slider) for variable 1.
output$subs1_conditional = renderUI({
  get.data.set()
  input$speed1
  isolate({
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if (is.null(choices1)){
      choices1 = 1
    }
    v=0
    if(!is.null(input$sub1_level_mini)){
      v = input$sub1_level_mini
    }
    sliderInput(inputId = "sub1_level",
                label = paste0("Subset '", input$subs1, "':"),
                min = 0, max = choices1, value = v, step = 1,
                #animate = TRUE,
                animate = animationOptions(interval = ifelse(length(input$speed1) == 0, 600, 1000*input$speed1),
                                           playButton = icon('play', "fa-2x"),
                                           pauseButton = icon('pause', "fa-2x")),
                ticks=F)
  })
})


output$speed_value1 <- renderUI({
  fixedRow((column(5, checkboxInput("select_speed1",
                                    label = "Time delay between plots (seconds):",
                                    value = input$select_speed1))),
            column(3, conditionalPanel("input.select_speed1",
                                       numericInput("speed1", 
                                                    "", 
                                                    value = 0.6, 
                                                    min = 0.1, 
                                                    max = 3.0, 
                                                    step = 0.1))))
#  numericInput("speed1", 
#               "Time delay between plots (seconds):", 
#               value = 0.6, min = 0.1, max = 3.0, step = 0.1)
#  fixedRow(column(8, HTML("Time delay between plots (seconds):")),
#           column(4, numericInput("speed1", "", value = 0.6)))
})




#  Subset level (Slider) for variable 1 (mini plot).
output$subs1_conditional_mini = renderUI({
  get.data.set()
  isolate({
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if (is.null(choices1)){
      choices1 = 1
    }
    v = 0
    if(!is.null(input$sub1_level)){
      v = input$sub1_level
    }
    sliderInput(inputId = "sub1_level_mini",
                label = paste0("Subset '", input$subs1, "':"),
                min = 0, max = choices1, value = v, step = 1,
                animate = TRUE, 
                ticks=F)
  })
})


#  Update plot$g1.level
observe({
  g1_level = input$sub1_level
  isolate({
    if (is.null(g1_level) || g1_level == 0) {
        g1_level = NULL
    }
    plot.par$g1.level = g1_level
    if(is.null(g1_level)){
      g1_level = 0
    }
    updateSliderInput(session,"sub1_level_mini",
                      value=g1_level)
  })
})

observe({
  g1_level = input$sub1_level_mini
  isolate({
    if (is.null(g1_level) || g1_level == 0) {
      g1_level = NULL
    }
    plot.par$g1.level = g1_level
    if(is.null(g1_level)){
      g1_level = 0
    }
    updateSliderInput(session,"sub1_level",
                      value=g1_level)
  })
})

##  Variable 2  ##
##
##  Select variable 2.
output$vari2_panel = renderUI({
  get.data.set()
  input$change_var_selection

  isolate({
    
    sel = input$vari2
    get.vars = parseQueryString(session$clientData$url_search)
    if(!is.null(get.vars$url)) {
      temp = session$clientData$url_search
      get.vars$url = sub(".*?url=(.*?)&.*", "\\1", temp)
    }
    if(length(get.vars)>0&&
         (any(names(get.vars)%in%"url")||
            any(names(get.vars)%in%"example"))&&
         (any(names(get.vars)%in%"y")&&
            !get.vars$y%in%"")){
      sel=get.vars$y
    }
    if(!is.null(input$change_var_selection)){
      ch = colnames(vis.data())
      if(!is.null(input$vari1)&&
           input$vari1%in%colnames(vis.data())){
        ch = ch[-which(ch%in%input$vari1)]
      }
      if(!input$change_var_selection){
        selectInput(inputId = "vari2",
                    label = NULL,
                    choices = c("none",ch),
                    selected = sel,
                    selectize=F)
      }else{
        selectInput(inputId = "vari2",
                    label = NULL,
                    choices = c("none",ch),
                    selected = sel,
                    selectize=F,
                    size=2)
      }
    }
  })
})

##  Update plot.par$y
observe({
    vari2.par = handle.input(input$vari2)$input.out
    isolate({
      if(!is.null(vis.data())){
        plot.par$y = vari2.par
        varnames.y = input$vari2
        if(!is.null(varnames.y)&&
             varnames.y%in%"none"){
          varnames.y = NULL
        }
        plot.par$varnames$y = varnames.y
        ch  = colnames(vis.data())
        if(!is.null(input$vari1)&&input$vari1%in%ch){
          ch  = ch[-which(ch%in%input$vari1)]
        }
        if(!is.null(input$vari2)&&input$vari2%in%ch){
          ch  = ch[-which(ch%in%input$vari2)]
        }
        ch = c("none",ch)
        sel = input$subs1
        if(!is.null(sel)&&!sel%in%ch){
          sel = ch[1]
        }
        updateSelectInput(session,"subs1",choices=ch,selected=sel)
        ch  = colnames(vis.data())
        if(!is.null(input$vari1)&&input$vari1%in%ch){
          ch  = ch[-which(ch%in%input$vari1)]
        }
        if(!is.null(input$vari2)&&input$vari2%in%ch){
          ch  = ch[-which(ch%in%input$vari2)]
        }
        ch = c("none",ch)
        sel = input$subs2
        if(!is.null(sel)&&!sel%in%ch){
          sel = ch[1]
        }
        updateSelectInput(session,"subs2",choices=ch,selected=sel)
     }
    })
})

#  Subset variable 2.
output$subs2_panel = renderUI({
  get.data.set()
  isolate({
    ch = colnames(vis.data())
    if(!is.null(input$vari1)&&input$vari1%in%ch){
      ch  = ch[-which(ch%in%input$vari1)]
    }
    if(!is.null(input$vari2)&&input$vari2%in%ch){
      ch  = ch[-which(ch%in%input$vari2)]
    }
    # ..added by Wilson
    if(!is.null(input$subs1)&&input$subs1%in%ch){
      ch  = ch[-which(ch%in%input$subs1)]
    }
    
    sel = input$subs2
    selectInput(inputId = "subs2",
                label = NULL,
                choices = c("none", ch),
                selected = sel,
                selectize=F)
  })
})


##  Update plot.par$g2.
observe({
  subs2.par = handle.input(input$subs2, subs = TRUE)$input.out
  isolate({
    plot.par$g2 = subs2.par
    varnames.g2 = input$subs2
    if(!is.null(varnames.g2)&&
         varnames.g2%in%"none"){
      varnames.g2 = NULL
    }
    plot.par$varnames$g2 = varnames.g2
    ch = colnames(vis.data())
    if(!is.null(input$vari1)&&input$vari1%in%ch){
      ch  = ch[-which(ch%in%input$vari1)]
    }
    if(!is.null(input$vari2)&&input$vari2%in%ch){
      ch  = ch[-which(ch%in%input$vari2)]
    }
    updateSelectInput(session,"subs1",choices=c("none",ch),selected=input$subs1)
  })
})

##  Subset level (Slider) for variable 2.
#output$subs2_conditional = renderUI({
#  get.data.set()
#  choices2 = handle.input(input$subs2, subs = TRUE)$factor.levels
#  if (is.null(choices2))
#    choices2 = 2
#  else
#    choices2 = choices2 + 1
#  sliderInput(inputId = "sub2_level",
#              label = paste0("Subset '", input$subs2, "':"),
#              min = 0, max = choices2, value = 0, step = 1,
#              animate = TRUE,ticks=F)
#})

################ modified by Wilson #################

output$subs2_conditional = renderUI({
  get.data.set()
  choices2 = handle.input(input$subs2, subs = TRUE)$factor.levels
  if (is.null(choices2))
    choices2 = 2
  else
    choices2 = choices2 + 1
  sliderInput(inputId = "sub2_level",
              label = paste0("Subset '", input$subs2, "':"),
              min = 0, max = choices2, value = 0, step = 1,
              #animate = TRUE,
              animate = animationOptions(interval = ifelse(length(input$speed2) == 0, 600, 1000*input$speed2),
                                         playButton = icon('play', "fa-2x"),
                                         pauseButton = icon('pause', "fa-2x")),
              ticks=F)
})



output$speed_value2 <- renderUI({
  fixedRow((column(5, checkboxInput("select_speed2",
                                    label = "Time delay between plots (seconds):",
                                    value = input$select_speed2))),
           column(3, conditionalPanel("input.select_speed2",
                                      numericInput("speed2", 
                                                   "", 
                                                   value = 0.6, 
                                                   min = 0.1, 
                                                   max = 3.0, 
                                                   step = 0.1))))
})


##  Subset level (Slider) for variable 2.
output$subs2_conditional_mini = renderUI({
  get.data.set()
  choices2 = handle.input(input$subs2, subs = TRUE)$factor.levels
  if (is.null(choices2))
    choices2 = 2
  else
    choices2 = choices2 + 1
  sliderInput(inputId = "sub2_level_mini",
              label = paste0("Subset '", input$subs2, "':"),
              min = 0, max = choices2, value = 0, step = 1,
              animate = TRUE,ticks=F)
})


# ##  Update plot.par$g2.level
observe({
    g2_level = input$sub2_level
    g2 = handle.input(input$subs2, subs = TRUE)$input.out

    if (is.null(g2_level) || g2_level == 0) {
        g2_level = NULL
        g2 = NULL
    }
    
    
    g2.level.check = handle.input(input$subs2, subs = TRUE)$factor.levels + 1
    if (!is.null(g2_level) && 
          length(g2.level.check) == 1 && 
          g2_level == g2.level.check) {
        g2_level = "_MULTI"
    }
    plot.par$g2.level = g2_level
    plot.par$g2 = g2

})



################## modified by Wilson ###################


# ##  Update plot.par$g2.level
#observe({
#    input$subs2
#    g2_level = input$sub2_level
#    if (!is.null(g2_level) && g2_level == 0) {
#        g2_level = "_MULTI"
#    }
#    
#    plot.par$g2.level = g2_level
#})

##########################################################


observe({
  g2_level = input$sub2_level_mini
  g2 = handle.input(input$subs2, subs = TRUE)$input.out
  if (is.null(g2_level) || g2_level == 0) {
    g2_level = NULL
    g2 = NULL
  }
  g2.level.check = handle.input(input$subs2, subs = TRUE)$factor.levels + 1
  if (!is.null(g2_level) && 
        length(g2.level.check) == 1 && 
        g2_level == g2.level.check) {
    g2_level = "_MULTI"
  }
  plot.par$g2.level = g2_level
  plot.par$g2 = g2
})

################## modified by Wilson ###################

#observe({
#  input$subs2
#  g2_level = input$sub2_level_mini
#  if (!is.null(g2_level) && g2_level == 0) {
#    g2_level = "_MULTI"
#  }
#  plot.par$g2.level = g2_level
#})

##########################################################


output$visualize.plot = renderPlot({
  isolate({
    # some of the graphical parameters need 
    # to be reminded what there default 
    # values are
    if(is.null(graphical.par$cex.dotpt)){
      graphical.par$cex.dotpt  = 0.5
    }
    if(is.null(graphical.par$alpha)){
      graphical.par$alpha  = 1
    }
    if(is.null(graphical.par$scatter.grid.bins)){
      graphical.par$scatter.grid.bins  = 50
    }
  })
  # plot it
  if (!is.null(vis.par())) {
    dafr = get.data.set()
    if(is.numeric(plot.par$x)&
         is.numeric(plot.par$y)){
      temp = vis.par()
      temp$trend.parallel = TRUE
      temp.x = temp$x
      temp$x=temp$y
      temp$y=temp.x
      temp.varnames.x = temp$varnames$x
      temp$varnames$x = temp$varnames$y
      temp$varnames$y = temp.varnames.x
      if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
        
        tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,temp)
                  }, warning = function(w) {
                    print(w)
                  }, error = function(e) {
                    print(e)
                  }, finally = {})
      }else{
        plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
      }
    }else{
      if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
        
        tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,vis.par())
        }, warning = function(w) {
          print(w)
        }, error = function(e) {
          print(e)
        }, finally = {})
      }else{
        plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
      }
    }
#     print(plot.ret.para$parameters)
#     print('###########################################################################')
  }
})

output$mini.plot = renderPlot({
  isolate({
    # some of the graphical parameters need 
    # to be reminded what their default 
    # values are
    if(is.null(graphical.par$cex.dotpt)){
      graphical.par$cex.dotpt  = 0.5
    }
    if(is.null(graphical.par$alpha)){
      graphical.par$alpha  = 1
    }
    if(is.null(graphical.par$scatter.grid.bins)){
      graphical.par$scatter.grid.bins  = 50
    }
  })
  # plot it
  if (!is.null(vis.par())) {
    dafr = get.data.set()
    if(is.numeric(plot.par$x)&
         is.numeric(plot.par$y)){
      temp = vis.par()
      temp$trend.parallel = TRUE
      temp.x = temp$x
      temp$x=temp$y
      temp$y=temp.x
      temp.varnames.x = temp$varnames$x
      temp$varnames$x = temp$varnames$y
      temp$varnames$y = temp.varnames.x
      if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
        tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,temp)
        }, warning = function(w) {
          print(w)
        }, error = function(e) {
          print(e)
        }, finally = {})
      }else{
        plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
      }
    }else{
      if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
        tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,vis.par())
        }, warning = function(w) {
          print(w)
        }, error = function(e) {
          print(e)
        }, finally = {})
      }else{
#         print(plot.par$locate.id)
        plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
      }
    }
#     print(plot.ret.para$parameters)
#     print('###########################################################################')
  }
})



output$visualize.summary = renderPrint({
  if (is.null(plot.par$x)) {
    return(cat("Please select a variable"))
  }
  values.list = modifyList(reactiveValuesToList(plot.par),
                           reactiveValuesToList(graphical.par), keep.null = TRUE)
  if(is.numeric(plot.par$x)&
       is.numeric(plot.par$y)){
    values.list.x = values.list$x
    values.list$x=values.list$y
    values.list$y=values.list.x
    values.list.varnames.x = values.list$varnames$x
    values.list$varnames$x = values.list$varnames$y
    values.list$varnames$y = values.list.varnames.x
  }
  if(!is.null(values.list$design)){
    values.list$data = NULL
  }


  tmp.list <- values.list
  tmp.list$plottype = "hist"
  
  
  if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
      tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
    tryCatch({
      cat(do.call(iNZightPlots:::getPlotSummary, tmp.list), sep = "\n")
    }, warning = function(w) {
     print(w)
   }, error = function(e) {
    print(e)
  }, finally = {})
 }else{
   suppressWarnings(try(cat(do.call(iNZightPlots:::getPlotSummary, tmp.list), sep = "\n")))
     }
})




output$visualize.inference = renderPrint({
  if(input$plot_selector%in%"Inference"){
    input$type.inference.select
    input$vari1
    input$vari2
    input$subs1
    isolate({
      if (is.null(plot.par$x)) {
        return(cat("Please select a variable"))
      }
      values.list = modifyList(reactiveValuesToList(plot.par),
                               reactiveValuesToList(graphical.par), keep.null = TRUE)
      bs.inf= T
      if(input$type.inference.select%in%"normal"){
        bs.inf = F
      }
      values.list <- modifyList(
        values.list,
        list(bs.inference = bs.inf,
             summary.type = "inference",
             inference.type = "conf",
             inference.par = NULL),
        
        keep.null = TRUE
      )
      if(is.numeric(plot.par$x)&
           is.numeric(plot.par$y)){
        values.list.x = values.list$x
        values.list$x=values.list$y
        values.list$y=values.list.x
        values.list.varnames.x = values.list$varnames$x
        values.list$varnames$x = values.list$varnames$y
        values.list$varnames$y = values.list.varnames.x
      }
      dafr = get.data.set()
      
      pdf(NULL)
      
      tryCatch({
        cat(do.call(iNZightPlots:::getPlotSummary, values.list), sep = "\n")
      }, warning = function(w) {
        print(w)
      }, error = function(e) {
        print(e)
      }, finally = {})
      
#      if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
#           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
#        tryCatch({
#          cat(do.call(iNZightPlots:::getPlotSummary, values.list), sep = "\n")
#        }, warning = function(w) {
#          print(w)
#        }, error = function(e) {
#          print(e)
#        }, finally = {})
#      }else{
#        suppressWarnings(try(cat(do.call(iNZightPlots:::getPlotSummary, values.list), sep = "\n")))
#      }
    })
  }
})



##  Reset variable selection and graphical parameters.
observe({
  input$reset.graphics
  input$go.to.new
  input$go.to.old
  if ((!is.null(input$reset.graphics)&&input$reset.graphics > 0) ||
      (!is.null(input$go.to.new)&&input$go.to.new > 0) ||
      (!is.null(input$go.to.old)&&input$go.to.old > 0)) {
    isolate({
#      updateRadioButtons(session, "customize_plot", selected = 1)
      graphical.par$alpha = 1
      
      updateSliderInput(session,"adjust.transparency",
                        value=0)
      graphical.par$bg = "grey93" #background colour
      updateSelectInput(session,"select.bg1",selected="grey93")
      ##  Box
      graphical.par$box.col = "black"
      graphical.par$box.fill = "white" # fill colour for the boxplot
      ##  Bar
      graphical.par$bar.fill = colors()[81] # colour for inside of bars in bar plot
      updateSelectInput(session,"select.barcolor",selected=colors()[81])
      ##  Line
      updateSliderInput(session,"line.width.multiplier",value=1)
      graphical.par$lty = 1
      graphical.par$lwd.pt = 2
      graphical.par$col.line = "blue"
      graphical.par$join = FALSE
      updateCheckboxInput(session,"check.join",value=F)
      updateSelectInput(session,"color.join",selected="blue")
      ##  Point
      graphical.par$cex.pt = 0.5
      updateSliderInput(session,"adjust.size.points.scatter",value=0.5)
      graphical.par$cex.dotpt = 0.5
      updateSliderInput(session,"adjust.size.points.dot",value=0.5)
#      graphical.par$pch = 21
      
      updateSliderInput(session,"adjust.size.scale",value = 1)
      graphical.par$cex = 1
      
      updateSelectInput(session,"point_symbol", selected = "circle")
      graphical.par$pch = 21
      
      updateSliderInput(session,"symbol_linewidth",value = 2)
      graphical.par$lwd.pt = 2
      
      
      
      updateCheckboxInput(session,"color.interior", value = F)
#      graphical.par$col.pt = "gray50"
      graphical.par$fill.pt = "transparent"
      
      updateCheckboxInput(session,"colour.use.ranks", value = F)
      graphical.par$col.method = "linear"
      
      updateCheckboxInput(session,"colour.palette.reverse", value = F)
      graphical.par$reverse.palette = FALSE
      
      updateCheckboxInput(session,"point_size_title", value = F)
      updateCheckboxInput(session,"point_colour_title", value = F)
      updateCheckboxInput(session,"point_symbol_title", value = F)
      
      updateSelectInput(session,"select.dotcolor",selected="gray50")
      #graphical.par$fill.pt = "transparent"
      ##  Colours
      graphical.par$col.LOE = "black"
      graphical.par$LOE = FALSE
      updateCheckboxInput(session,"check.xyline",value=F)
      updateSelectInput(session,"color.xyline",selected="black")
      graphical.par$col.trend =
        list(linear = "",
             quadratic = "",
             cubic =  "")
      updateCheckboxInput(session,"check_linear",value=F)
      updateCheckboxInput(session,"check_quadratic",value=F)
      updateCheckboxInput(session,"check_cubic",value=F)
      updateSelectInput(session,"type.linear",selected="solid")
      updateSelectInput(session,"type.quadratic",selected="solid")
      updateSelectInput(session,"type.cubic",selected="solid")
      updateSelectInput(session,"color.linear",selected="blue")
      updateSelectInput(session,"color.quadratic",selected="red")
      updateSelectInput(session,"color.cubic",selected="green4")
      graphical.par$col.smooth = ""
      updateSelectInput(session,"color.smoother",selected="magenta")
      graphical.par$quant.smooth = NULL
      updateCheckboxInput(session,"check_smoother",value=F)
      updateCheckboxInput(session,"check.quantiles",value=F)
      updateSliderInput(session,"smoother.smooth",value=0.7)
      ##  Jitter, rugs, and trend
      graphical.par$jitter = ""
      updateCheckboxInput(session,"check.jitter.x",value=F)
      updateCheckboxInput(session,"check.jitter.y",value=F)
      graphical.par$rugs = ""
      updateCheckboxInput(session,"check.rugs.x",value=F)
      updateCheckboxInput(session,"check.rugs.y",value=F)
      graphical.par$trend = NULL
      ##  Others
      graphical.par$cex = 1
      graphical.par$inference.type = NULL
      graphical.par$inference.par = NULL
#      graphical.par$largesample = NULL
      graphical.par$lines.by = FALSE
      graphical.par$trend.by = FALSE
      updateCheckboxInput(session,"each_level",value=F)
      graphical.par$trend.parallel = T
      updateCheckboxInput(session,"each_level_seperate",value=T)
      graphical.par$smooth = 0
      graphical.par$szsym = 1
      graphical.par$tpsym = 1
      graphical.par$plottype="default"
      updateSelectInput(session,"select.plot.type",selected="default")
      graphical.par$hist.bins=get.default.num.bins()
#      updateSliderInput(session,"adjust.num.bins",value=get.default.num.bins())
      graphical.par$scatter.grid.bins=50
      updateSliderInput(session,"adjust.grid.size",value=50)
      graphical.par$hex.bins=20
      updateSliderInput(session,"adjust.hex.bins",value=20)
      graphical.par$bs.inference=F
      graphical.par$varnames = list(x = NULL, y = NULL,
                                    xlab = NULL, ylab = NULL,
                                    g1 = NULL, g2 = NULL,
                                    colby=NULL,sizeby=NULL, symbolby = NULL)
      # time delay between plots
      updateCheckboxInput(session, "select_speed1", value = F)
      updateCheckboxInput(session, "select_speed2", value = F)
      updateNumericInput(session, "speed1", value = 0.6)
      updateNumericInput(session, "speed2", value = 0.6)
      
      plot.par$main=NULL
      updateTextInput(session,"main_title_text",value="")
      plot.par$xlab=NULL
      updateTextInput(session,"x_axis_text",value="")
      plot.par$ylab=NULL
      updateTextInput(session,"y_axis_text",value="")
      plot.par$colby=NULL
      updateSelectInput(session,"color_by_select",selected=" ")
      plot.par$sizeby=NULL
      updateSelectInput(session,"resize.by.select",selected=" ")
      plot.par$symbolby = NULL
      updateSelectInput(session,"point_symbol_by",selected=" ")
      plot.par$locate=NULL
      plot.par$locate.id=NULL
      plot.par$locate.col=NULL
      plot.par$locate.extreme=NULL
      plot.par$zoombar=NULL
      plot.par$design=NULL
      design.parameters$id = formula("~1")
      design.parameters$strata = NULL
      design.parameters$fpc = NULL
      design.parameters$nest = F
      design.parameters$weights = NULL
      design.parameters$data.name = NULL   
    })
  }
})

# This refreshes the infernce parameters.
output$add_inference = renderUI({
  get.data.set()
  input$vari1
  input$vari2
  ret = NULL
  isolate({
    dafr = get.data.set()
    add_inference.check = checkboxInput("add.inference",
                                        label="Add inference",
                                        value=input$add.inference)
    mean_median.radio = radioButtons("inference_parameter1",
                                     label=h5(strong("Parameter")),
                                     choices=c("Mean","Median"),
                                     selected=input$inference_parameter1,
                                     inline=T)
    normal_bootstrap.radio = radioButtons("inference_type1",
                                          label=h5(strong("Type of inference")),
                                          choices=c("Normal","Bootstrap"),
                                          selected=input$inference_type1,
                                          inline=T)
    confidence.interval.check = checkboxInput("confidence_interval1",
                                              label="Confidence interval",
                                              value=input$confidence_interval1)
    comparison.interval.check = checkboxInput("comparison_interval1",
                                              label="Comparison interval",
                                              value=input$comparison_interval1)
    year12_bootstrap.radio = radioButtons("inference_type2",
                                          label=h5(strong("Type of inference")),
                                          choices=c("Year 12","Bootstrap"),
                                          selected=input$inference_type2,
                                          inline=T)
    intervals = NULL
    graphical.par$inference.par = NULL
    graphical.par$bs.inference = F
    if((!is.null(input$vari1)&&
          !is.null(input$vari2))&&
         (input$vari1%in%colnames(get.data.set())&&
            (input$vari2%in%colnames(get.data.set())||
               input$vari2%in%'none'))){
      if((!is.null(input$confidence_interval1)&&
            input$confidence_interval1)||
           (!is.null(input$comparison_interval1)&&
              input$comparison_interval1)){
        if(!is.null(input$confidence_interval1)&&
             input$confidence_interval1){
          intervals = c(intervals,"conf")
        }
        if(!is.null(input$comparison_interval1)&&
             input$comparison_interval1){
          intervals = c(intervals,"comp")
        }
        if(!is.null(input$inference_parameter1)&&
             input$inference_parameter1%in%"Mean"){
          graphical.par$inference.par = "mean"
        }else if(!is.null(input$inference_parameter1)&&
                   input$inference_parameter1%in%"Median"){
          graphical.par$inference.par = "median"
        }
        if((!is.null(input$inference_type1)&&
              input$inference_type1%in%"Bootstrap")||
             (!is.null(input$inference_type2)&&
                input$inference_type2%in%"Bootstrap")){
          graphical.par$bs.inference = T
        }else{
          graphical.par$bs.inference = F
        }
      }
      graphical.par$inference.type = intervals
      # vari1 = numeric; vari2 = numeric
      if(!input$vari2%in%"none"&&
           (class(dafr[,input$vari1])%in%"numeric"|
              class(dafr[,input$vari1])%in%"integer")&&
           (class(dafr[,input$vari2])%in%"numeric"|
              class(dafr[,input$vari2])%in%"integer")){
        ret = list(conditionalPanel("input.check_linear||
                                    input.check_quadratic||
                                    input.check_cubic||
                                    input.check_smoother",
                                    add_inference.check))
                                    
      # vari1 = numeric; vari2 = factor or 
      # vari1 = factor; vari2 = numeric
      }else if(!input$vari2%in%"none"&&
                 (((class(dafr[,input$vari1])%in%"numeric"|
                     class(dafr[,input$vari1])%in%"integer")&&
                    (class(dafr[,input$vari2])%in%"factor"|
                       class(dafr[,input$vari2])%in%"character"))||
                 ((class(dafr[,input$vari1])%in%"factor"|
                     class(dafr[,input$vari1])%in%"character")&&
                    (class(dafr[,input$vari2])%in%"numeric"|
                       class(dafr[,input$vari2])%in%"integer")))){
        ret = list(mean_median.radio,
                   conditionalPanel("input.inference_parameter1=='Mean'",
                                    normal_bootstrap.radio),
                   conditionalPanel("input.inference_parameter1=='Median'",
                                    year12_bootstrap.radio),
                   conditionalPanel("input.inference_parameter1=='Mean'||
                                                     (input.inference_parameter1=='Median'&&
                                                     input.inference_type2=='Bootstrap')",
                                    h5(strong("Type of interval")),
                                    confidence.interval.check,
                                    comparison.interval.check)
        )
      # vari1 = factor; vari2 = factor or vari1 = factor; vari2 = none
      }else if((!input$vari2%in%"none"&&
                  ((class(dafr[,input$vari1])%in%"factor"|
                      class(dafr[,input$vari1])%in%"character")&&
                     (class(dafr[,input$vari2])%in%"factor"|
                        class(dafr[,input$vari2])%in%"character")))||
                 (input$vari2%in%"none"&&
                    (class(dafr[,input$vari1])%in%"factor"|
                       class(dafr[,input$vari1])%in%"character"))){
        ret = list(h5(strong("Parameter")),
                   helpText("Proportions"),
                   normal_bootstrap.radio,
                   h5(strong("Type of interval")),
                   confidence.interval.check,
                   conditionalPanel("input.inference_type1=='Normal'",
                                    comparison.interval.check))
                                    
      # var1 = numeric; vari2 = none
      }else if((input$vari2%in%"none"&&
                  (class(dafr[,input$vari1])%in%"numeric"|
                     class(dafr[,input$vari1])%in%"integer"))){
        ret = list(mean_median.radio,
                   conditionalPanel("input.inference_parameter1=='Mean'",
                                    normal_bootstrap.radio),
                   conditionalPanel("input.inference_parameter1=='Median'",
                                    year12_bootstrap.radio),
                   conditionalPanel("input.inference_parameter1=='Mean'||
                                                   (input.inference_parameter1=='Median'&&
                                                   input.inference_type2=='Bootstrap')",
                                    h5(strong("Type of interval")),
                                    confidence.interval.check))
      }
    }
  })
  ret
})

# inference handles
observe({
  input$confidence_interval1
  input$comparison_interval1
  input$inference_type1
  input$inference_type2
  input$inference_parameter1
  input$vari1
  input$vari2
  input$add.inference
  isolate({
    graphical.par$inference.par = NULL
    intervals = NULL
    graphical.par$bs.inference = F
    # vari1 = numeric; vari2 = none
    if((!is.null(input$vari1)&&
         !is.null(input$vari2)&&
          input$vari1%in%colnames(get.data.set())&&
          input$vari2%in%"none")&&
         (is.numeric(get.data.set()[,input$vari1])|
            is.integer(get.data.set()[,input$vari1]))){
      if(!is.null(input$inference_parameter1)&&
           input$inference_parameter1%in%"Mean"&&
           (!is.null(input$confidence_interval1)&&
               input$confidence_interval1)){
        graphical.par$inference.par = "mean"
        if(!is.null(input$confidence_interval1)&&
             input$confidence_interval1){
          intervals = c(intervals,"conf")
        }
        if(length(intervals)>0){
          if(input$inference_type1%in%"Normal"){
            graphical.par$bs.inference = F
          }else if(input$inference_type1%in%"Bootstrap"){
            graphical.par$bs.inference = T
          }
        }
      }else if((!is.null(input$inference_parameter1)&&
                 input$inference_parameter1%in%"Median")){
        graphical.par$inference.par = "median"
        intervals = c(intervals,"conf")
        graphical.par$bs.inference = F
        if(input$inference_type2%in%"Bootstrap"&&
             (!is.null(input$confidence_interval1)&&
                input$confidence_interval1)){
          graphical.par$bs.inference = T
        }else if(input$inference_type2%in%"Bootstrap"){
          graphical.par$bs.inference = T
          intervals = NULL
        }
      }
    # vari1 = factor; vari2 = none or vari1 = factor; vari2 = factor
    }else if(!is.null(input$vari1)&&
                input$vari1%in%colnames(get.data.set())&&
                (input$vari2%in%"none"&&
                   (is.character(get.data.set()[,input$vari1])|
                   is.factor(get.data.set()[,input$vari1])))||
               ((!input$vari2%in%"none"&&
                  input$vari2%in%colnames(get.data.set()))&&                                              
                  ((is.factor(get.data.set()[,input$vari1])|
                      is.character(get.data.set()[,input$vari1]))&&
                  (is.factor(get.data.set()[,input$vari2])|
                     is.character(get.data.set()[,input$vari2]))))){
      graphical.par$inference.par = "proportion"
      if(!is.null(input$inference_type1)&&
           input$inference_type1%in%"Normal"){
        graphical.par$bs.inference = F
        if(!is.null(input$confidence_interval1)&&
             input$confidence_interval1){
          intervals = c(intervals,"conf")
        }
        if(!is.null(input$comparison_interval1)&&
             input$comparison_interval1){
          intervals = c(intervals,"comp")
        }
      }else if(!is.null(input$inference_type1)&&
                 input$inference_type1%in%"Bootstrap"){
        graphical.par$bs.inference = T
        if(!is.null(input$confidence_interval1)&&
             input$confidence_interval1){
          intervals = c(intervals,"conf")
        }
      }
    # vari1 = numeric; vari2 = numeric
    }else if((!is.null(input$vari1)&&
                !is.null(input$vari2)&&
                input$vari1%in%colnames(get.data.set())&&
                input$vari2%in%colnames(get.data.set()))&&
                (is.numeric(get.data.set()[,input$vari1])&&
                   is.numeric(get.data.set()[,input$vari2]))){
#      graphical.par$bs.inference = input$add.inference
      if(is.null(input$add.inference))
        graphical.par$bs.inference = F
      else
        graphical.par$bs.inference = input$add.inference
      
    # vari1 = numeric; vari2 = factor or 
    # vari1 = factor; vari2 = numeric
    }else if((!is.null(input$vari1)&&
                !is.null(input$vari2)&&
                input$vari1%in%colnames(get.data.set())&&
                input$vari1%in%colnames(get.data.set()))&&
               (((is.factor(get.data.set()[,input$vari1])|
                   is.character(get.data.set()[,input$vari1]))&&
                   (is.numeric(get.data.set()[,input$vari2])|
                      is.integer(get.data.set()[,input$vari2])))||
               ((is.numeric(get.data.set()[,input$vari1])|
                   is.integer(get.data.set()[,input$vari1]))&&
                  (is.factor(get.data.set()[,input$vari2])|
                     is.character(get.data.set()[,input$vari2]))))){
      if(!is.null(input$inference_parameter1)&&
           input$inference_parameter1%in%"Mean"&&
           ((!is.null(input$confidence_interval1)&&
                input$confidence_interval1)|
              (!is.null(input$comparison_interval1)&&
                 input$comparison_interval1))){
        graphical.par$inference.par = "mean"
        if(!is.null(input$inference_type1)&&
             input$inference_type1%in%"Normal"){
          graphical.par$bs.inference = F
          if(!is.null(input$confidence_interval1)&&
               input$confidence_interval1){
            intervals = c(intervals,"conf")
          }
          if(!is.null(input$comparison_interval1)&&
               input$comparison_interval1){
            intervals = c(intervals,"comp")
          }
        }else if(!is.null(input$inference_type1)&&
                   input$inference_type1%in%"Bootstrap"){
          graphical.par$bs.inference = T
          if(!is.null(input$confidence_interval1)&&
               input$confidence_interval1){
            intervals = c(intervals,"conf")
          }
          if(!is.null(input$comparison_interval1)&&
               input$comparison_interval1){
            intervals = c(intervals,"comp")
          }
        }
      }else if(!is.null(input$inference_parameter1)&&
                 input$inference_parameter1%in%"Median"&&
                 ((!is.null(input$confidence_interval1)&&
                     input$confidence_interval1)|
                    (!is.null(input$comparison_interval1)&&
                       input$comparison_interval1))){
        graphical.par$inference.par = "median"
        intervals = c(intervals,"conf")
        graphical.par$bs.inference = F
        if(input$inference_type2%in%"Bootstrap"&&
             ((!is.null(input$confidence_interval1)&&
                input$confidence_interval1)|
                (!is.null(input$comparison_interval1)&&
                   input$comparison_interval1))){
          intervals = NULL
          if(!is.null(input$confidence_interval1)&&
               input$confidence_interval1){
            intervals = c(intervals,"conf")
          }
          if(!is.null(input$comparison_interval1)&&
               input$comparison_interval1){
            intervals = c(intervals,"comp")
          }
          graphical.par$bs.inference = T
        }else if(input$inference_type2%in%"Bootstrap"){
          graphical.par$bs.inference = T
          intervals=NULL
        }
      }else if(!is.null(input$inference_parameter1)&&
                 input$inference_parameter1%in%"Median"){
        if(input$inference_type2%in%"Year 12"){
          graphical.par$inference.par = "median"
          intervals = c(intervals,"conf")
          graphical.par$bs.inference = F
        }
      }
    }
    graphical.par$inference.type = intervals
  })
})


# Advanced options panel -> 
output$plot.appearance.panel = renderUI({
  get.data.set()
  ret=NULL
  input$vari1
  input$vari2
  input$select.plot.type
  plot.par$design
  isolate({
    # barplot with one factor variable the other one not specified
    cols1 = colors()[c(354, 1,3,16,19,63,87,109,259,
                       399,419,558,600,626,647)]
    cols2 = colors()[c(81,73,84,107,371,426,517,617)]
    cols3 = colors()[c(203,73,81,84,107,371,425,517,617)]
    
    
    general.appearance.title = h5(strong("General Appearance"))
    
    bar.colour.title = h5(strong("Bar Colour"))
    
    point.size.title = checkboxInput(inputId = "point_size_title",
                                     label = strong("Point Size"),
                                     value = FALSE)
    
    point.colour.title = checkboxInput(inputId = "point_colour_title",
                                       label = strong("Point Colour"),
                                       value = FALSE)
    
    select.bg.object = fixedRow(column(3, h5("Background colour:")),
                                column(6, selectInput(inputId="select.bg1",
                                                      label=NULL,
                                                      choices=cols1,
                                                      selected=graphical.par$bg)))
    
    select.plot.type.object = NULL
    
    select.barcolor.object = conditionalPanel(
      condition = "input.color_by_select == ' '",
      
      fixedRow(column(3, h5("Bar Colour:")),
               column(6, selectInput(inputId="select.barcolor",label=NULL,
                                     choices=cols2,
                                     selected=graphical.par$bar.fill)))
    )
      
#    select.barcolor.object = fixedRow(column(3, h5("Bar Colour:")),
#                                      column(6, selectInput(inputId="select.barcolor",label=NULL,
#                                                            choices=cols2,
#                                                            selected=graphical.par$bar.fill)))
      
    select.dotcolor.object = conditionalPanel(condition = "input.point_colour_title == true & input.color_by_select == ' '",
                                              fixedRow(column(3, h5("Point Colour:")),
                                                       column(6, selectInput(inputId="select.dotcolor",label=NULL,
                                                                             choices=cols3,
                                                                             selected=graphical.par$col.pt))))  
#    select.dotcolor.object = fixedRow(column(3, h5("Point Colour:")),
#                                      column(6, selectInput(inputId="select.dotcolor",label=NULL,
#                                                            choices=cols3,
#                                                            selected=graphical.par$col.pt)))
    color.interior = conditionalPanel(condition = "input.point_colour_title == true",
                                      fixedRow(column(3),
                                               column(6, checkboxInput(inputId="color.interior",label = "Colour interior",
                                                                       value = FALSE))))
#    color.interior = checkboxInput(inputId="color.interior",label = "Colour interior",
#                                   value = FALSE)
    
#    select.dotcolor.interior.object = fixedRow(column(5, select.dotcolor.object),
#                                               column(4, color.interior))
    if(is.null(graphical.par$cex.dotpt)){
      graphical.par$cex.dotpt = 0.5
    }
    
    adjust.size.scale.object = fixedRow(column(3, h5("Overall size scale:")),
                                        column(6, sliderInput("adjust.size.scale", 
                                                              label = NULL, 
                                                              min = 0.5, 
                                                              max = 2, 
                                                              value = 1, step = .05, ticks = FALSE)))
    
    
    adjust.size.points.dot.object = conditionalPanel(condition = "input.point_size_title == true",
                                                     fixedRow(column(3, h5("Point size:")),
                                                              column(6, sliderInput("adjust.size.points.dot", 
                                                                                    label = NULL, 
                                                                                    min = 0.1, 
                                                                                    max = 3.5, 
                                                                                    value=graphical.par$cex.dotpt,
                                                                                    step=.05, 
                                                                                    ticks = FALSE))))
    
#    adjust.size.points.dot.object = fixedRow(column(3, h5("Point size:")),
#                                              column(6, sliderInput("adjust.size.points.dot", 
#                                                                    label = NULL, 
#                                                                    min = 0.1, 
#                                                                    max = 3.5, 
#                                                                    value=graphical.par$cex.dotpt,
#                                                                    step=.05, 
#                                                                    ticks = FALSE)))
    
    adjust.size.points.scatter.object = conditionalPanel(condition = "input.point_size_title == true",
                                                         fixedRow(column(3, h5("Point size:")),
                                                                  column(6, sliderInput("adjust.size.points.scatter", 
                                                                                        label = NULL, 
                                                                                        min = 0.1, 
                                                                                        max = 3.5, 
                                                                                        value=graphical.par$cex.dotpt,
                                                                                        step=.05, 
                                                                                        ticks = FALSE))))
    
#    adjust.size.points.scatter.object = fixedRow(column(3, h5("Point size:")),
#                                                 column(6, sliderInput("adjust.size.points.scatter", 
#                                                                       label = NULL, 
#                                                                       min = 0.1, 
#                                                                       max = 3.5, 
#                                                                       value=graphical.par$cex.dotpt,
#                                                                       step=.05, 
#                                                                       ticks = FALSE)))
    
    
    adjust.grid.size.title = h5(strong("Size"))
    adjust.grid.size.object = fixedRow(column(3, h5("Grid size (n x n):")),
                                       column(6, sliderInput("adjust.grid.size", 
                                                             label = NULL, 
                                                             min = 10, max = 250, 
                                                             value=graphical.par$scatter.grid.bins,
                                                             step=1, 
                                                             ticks = FALSE))) 
                                                    
    adjust.min.count.grid.object = fixedRow(column(3, h5("Min-count colour (% grey):")),
                                            column(6, sliderInput("adjust.min.count.grid", 
                                                                  label = NULL, 
                                                                  min = 0, 
                                                                  max = 100, 
                                                                  value=convert.to.percent(graphical.par$alpha),
                                                                  step=1, 
                                                                  ticks = FALSE)))
      
      
      
    if(is.null(graphical.par$alpha)){
      graphical.par$alpha = 1
    }
    adjust.transparency.object = conditionalPanel(condition = "input.point_colour_title == true",
                                                  fixedRow(column(3, h5("Transparency:")),
                                                           column(6, sliderInput("adjust.transparency", 
                                                                                 label = NULL, min = 0, 
                                                                                 max = 100, 
                                                                                 value=convert.to.percent(graphical.par$alpha),
                                                                                 step=1, ticks = FALSE))))
#    adjust.transparency.object = fixedRow(column(3, h5("Transparency:")),
#                                          column(6, sliderInput("adjust.transparency", 
#                                                                label = NULL, min = 0, 
#                                                                max = 100, 
#                                                                value=convert.to.percent(graphical.par$alpha),
#                                                                step=1, ticks = FALSE)))
      
    if(is.null(graphical.par$hex.bins)){
      graphical.par$hex.bins = 20
    }
    adjust.hex.bins.title = h5(strong("Size"))
    adjust.hex.bins.object = fixedRow(column(3, h5("Hexagon size (number of bins to use):")),
                                      column(6, sliderInput("adjust.hex.bins", 
                                                            label = NULL, min = 2, 
                                                            max = 70, 
                                                            value=graphical.par$hex.bins,
                                                            step=1, ticks = FALSE)))
    
    
    adjust.num.bins.object = NULL
    if((!is.null(input$vari1)&
         !is.null(input$vari2))&&
         (input$vari1%in%colnames(get.data.set())&&
            (input$vari2%in%colnames(get.data.set())|
               input$vari2%in%"none"))){
      temp = list()
      temp$x = get.data.set()[,input$vari1]
      if(input$vari2%in%'none'){
        temp$y = NULL
      }else{
        temp$y = get.data.set()[,input$vari2]
      }
      temp$plot = F
      tester = try(do.call(iNZightPlots:::iNZightPlot,temp))
#####################################################################
#      large.sample = T
      large.sample = search.name(tester,"largesample")[[1]]
      if(is.null(large.sample)){
        large.sample = F
      }
#####################################################################
      # bar plot with one factor variable
      # vari1 = factor , vari2 = none
      if(input$vari2%in%"none"&&
           (class(get.data.set()[,input$vari1])%in%"factor"|
              class(get.data.set()[,input$vari1])%in%"character")){
        ret = list(general.appearance.title,
                   select.bg.object,
                   adjust.size.scale.object,
                   bar.colour.title,
                   select.barcolor.object
        )
      # bar plot with two factor variables
      # vari1 = factor , vari2 = factor
      }else if(!input$vari2%in%"none"&&
                 ((class(get.data.set()[,input$vari1])%in%"factor"|
                     class(get.data.set()[,input$vari1])%in%"character")&&
                    (class(get.data.set()[,input$vari2])%in%"factor"|
                       class(get.data.set()[,input$vari2])%in%"character"))){
        select.bg.object = fixedRow(column(3, h5("Background colour:")),
                                    column(6, selectInput(inputId="select.bg1",label=NULL,
                                                          choices=cols1,
                                                          selected=graphical.par$bg)))
        ret = list(general.appearance.title,
                   select.bg.object,
                   adjust.size.scale.object,
                   bar.colour.title)
      # dotplot or histogram for numeric varible in x or 
      # dotplot or histogram for one numeric one factor variable
      # vari1 = numeric , vari2 = none
      # vari1 = factor , vari2 = numeric or
      # vari1 = numeric , vari2 = factor
      }else if((input$vari2%in%"none"&&
                     (class(get.data.set()[,input$vari1])%in%"numeric"|
                        class(get.data.set()[,input$vari1])%in%"integer"))||
                    (!input$vari2%in%"none"&&
                       ((class(get.data.set()[,input$vari1])%in%"factor"|
                          class(get.data.set()[,input$vari1])%in%"character")&&
                       (class(get.data.set()[,input$vari2])%in%"integer"|
                          class(get.data.set()[,input$vari2])%in%"numeric")))||
                    (!input$vari2%in%"none"&&
                       ((class(get.data.set()[,input$vari1])%in%"integer"|
                          class(get.data.set()[,input$vari1])%in%"numeric")&
                       (class(get.data.set()[,input$vari2])%in%"character"|
                          class(get.data.set()[,input$vari2])%in%"factor")))){
        select.plot.type.object = fixedRow(column(3, h5("Plot type:")),
                                           column(6, selectInput(inputId = "select.plot.type",
                                                                 label = NULL,
                                                                 choices=c("default",
                                                                           "dot plot",
                                                                           "histogram"),
                                                                 selected=input$select.plot.type)))
        ret = list(general.appearance.title,
                   select.plot.type.object,
                   select.bg.object,
                   adjust.size.scale.object,
                   point.size.title,
                   adjust.size.points.dot.object,
                   point.colour.title,
                   select.dotcolor.object,
                   color.interior,
#                   select.dotcolor.interior.object,
                   adjust.transparency.object)
        if((!is.null(input$select.plot.type)&&
             (input$select.plot.type%in%"histogram"||
             (large.sample&&input$select.plot.type%in%"default")))||
             !is.null(plot.par$design)){
          isolate({
            temp = vis.par()
          })
          temp$plot = F
          nbins=NULL
          if(is.null(get.nbins())){
            nbins  = search.name(tester,"hist.bins")[[1]][1]
  #           nbins = nbins[1]
          }else{
            nbins = get.nbins()
          }
          if(is.null(nbins)|is.na(nbins)){
            nbins=50
          }
          m = length(unique(get.data.set()[,input$vari1]))
          if(!is.null(input$vari2)&&
               !input$vari2%in%"none"&&
               input$vari2%in%colnames(get.data.set())){
            m = max(c(length(unique(get.data.set()[,input$vari1])),
                      length(unique(get.data.set()[,input$vari2]))))
          }
          if(m<nbins){
            m=nbins
          }
          adjust.num.bins.title = h5(strong("Size"))
          adjust.num.bins.object = fixedRow(column(3, h5("Number of bars:")),
                                            column(6, sliderInput("adjust.num.bins", label = NULL, min = 1, 
                                                                  max = m, value=nbins,step=1, ticks = FALSE)))
          if(is.null(plot.par$design)){
            ret=list(general.appearance.title,
                     select.plot.type.object,
                     select.bg.object,
                     adjust.size.scale.object,
                     bar.colour.title,
                     select.barcolor.object,
                     adjust.num.bins.title,
                     adjust.num.bins.object)
          }else{
            ret=list(general.appearance.title,
                     select.bg.object,
                     adjust.size.scale.object,
                     bar.colour.title,
                     select.barcolor.object,
                     adjust.num.bins.title,
                     adjust.num.bins.object)
          }
        }
      # scatter plot
      # vari1 = numeric , vari2 = numeric
      }else if(!input$vari2%in%"none"&&
                 ((class(get.data.set()[,input$vari1])%in%"numeric"|
                     class(get.data.set()[,input$vari1])%in%"integer")&&
                    (class(get.data.set()[,input$vari1])%in%"numeric"|
                       class(get.data.set()[,input$vari1])%in%"integer"))){
        select.plot.type.object = fixedRow(column(3, h5("Plot type:")),
                                           column(6, selectInput(inputId = "select.plot.type",
                                                                 label = NULL,
                                                                 choices=c("default",
                                                                           "scatter plot",
                                                                           "hexbin plot-size",
                                                                           "hexbin plot-alpha",
                                                                           "grid-density plot"),
                                                                 selected=input$select.plot.type)))
        resize.by.object = conditionalPanel(condition = "input.point_size_title == true",
                                            fixedRow(column(3, h5("Resize points by:")),
                                                     column(6, selectInput("resize.by.select",
                                                                           label=NULL,
                                                                           choices=c(" ",get.numeric.column.names(vis.data())),
                                                                           selected = "input$resize.by.select"))))
#                resize.by.object = fixedRow(column(3, h5("Resize points by:")),
#                                    column(6, selectInput("resize.by.select",
#                                                          label=NULL,
#                                                          choices=c(" ",get.numeric.column.names(vis.data())),
#                                                          selected = "input$resize.by.select")))
        ret = list(general.appearance.title,
                   select.plot.type.object,
                   select.bg.object,
                   adjust.size.scale.object,
                   point.size.title,
                   adjust.size.points.scatter.object,
                   resize.by.object,
                   point.colour.title,
                   select.dotcolor.object,
                   color.interior,
#                   select.dotcolor.interior.object,
                   adjust.transparency.object)
        if(!is.null(input$select.plot.type)&&
             (input$select.plot.type%in%"grid-density plot"||
                (large.sample&&input$select.plot.type%in%"default"))){
          ret = list(general.appearance.title,
                     select.plot.type.object,
                     select.bg.object,
                     adjust.size.scale.object,
                     adjust.grid.size.title,
                     adjust.grid.size.object)
        }else if(!is.null(input$select.plot.type)&&
                   input$select.plot.type%in%"hexbin plot-size"){
          ret = list(general.appearance.title,
                     select.plot.type.object,
                     select.bg.object,
                     adjust.size.scale.object,
                     adjust.hex.bins.title,
                     adjust.hex.bins.object,
                     point.colour.title,
                     select.dotcolor.object)
        }
        else if(!is.null(input$select.plot.type)&&
                input$select.plot.type%in%"hexbin plot-alpha") {
          ret = list(general.appearance.title,
                     select.plot.type.object,
                     select.bg.object,
                     adjust.size.scale.object,
                     adjust.hex.bins.title,
                     adjust.hex.bins.object,
                     point.colour.title,
                     select.dotcolor.object)
        }
      }
    }
  })
  ret
})

# observe the plot type and change 'Advanced options' select input
observe({
  input$select.plot.type
  if(!is.null(input$vari1)&!is.null(input$vari2)){
    isolate({
      if(input$vari1%in%colnames(get.data.set())&&
           (input$vari2%in%colnames(get.data.set())||
              input$vari2%in%"none")){
        temp = list()
        temp$x = get.data.set()[,input$vari1]
        if(input$vari2%in%'none'){
          temp$y = NULL
        }else{
          temp$y = get.data.set()[,input$vari2]
        }
        temp$plot = F
        tester = try(do.call(iNZightPlots:::iNZightPlot,temp))
####################################################################
#        large.sample = T
        large.sample = search.name(tester,"largesample")[[1]]
        if(is.null(large.sample)){
          large.sample = F
        }
####################################################################
        if(!is.null(input$advanced_options)){
          sel = input$advanced_options
          ch = NULL
          # vari1 = factor, vari2 = none
          if((class(get.data.set()[,input$vari1])%in%"factor"|
                class(get.data.set()[,input$vari1])%in%"character")&
               input$vari2%in%"none"){
            ch = c('Code more variables',
                   'Change plot appearance',
                   'Customize labels',
                   'Adjust number of Bars')
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
          # vari1 = factor, vari2 = factor
          }else if((class(get.data.set()[,input$vari1])%in%"factor"|
                      class(get.data.set()[,input$vari1])%in%"character")&
                     !input$vari2%in%"none"&&
                     (class(get.data.set()[,input$vari2])%in%"factor"|
                        class(get.data.set()[,input$vari2])%in%"character")){
            ch = c('Change plot appearance',
                   'Customize labels',
                   'Adjust number of Bars')
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
          # vari1 = numeric, vari2 = none or
          # vari1 = factor, vari2 = numeric or
          # vari1 = numeric, vari2 = factor
          }else if(((class(get.data.set()[,input$vari1])%in%"numeric"|
                       class(get.data.set()[,input$vari1])%in%"integer")&
                      input$vari2%in%"none")|
                     ((class(get.data.set()[,input$vari1])%in%"factor"|
                         class(get.data.set()[,input$vari1])%in%"character")&
                        !input$vari2%in%"none"&&
                        (class(get.data.set()[,input$vari2])%in%"integer"|
                           class(get.data.set()[,input$vari2])%in%"numeric"))|
                     ((class(get.data.set()[,input$vari1])%in%"integer"|
                         class(get.data.set()[,input$vari1])%in%"numeric")&
                        !input$vari2%in%"none"&&
                        (class(get.data.set()[,input$vari2])%in%"factor"|
                           class(get.data.set()[,input$vari2])%in%"character"))){
            ch = c('Code more variables',
                   'Change plot appearance',
                   'Identify points',
                   'Customize labels',
                   'Adjust axis limits')
            if(!is.null(input$select.plot.type)&&
                 (input$select.plot.type%in%"histogram"||
                 (large.sample&&
                    input$select.plot.type%in%"default"))){
              ch = c('Change plot appearance',
                     'Customize labels',
                     'Adjust axis limits')
            }
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
          # vari1 = numeric, vari2 = numeric
          }else if((class(get.data.set()[,input$vari1])%in%"numeric"|
                      class(get.data.set()[,input$vari1])%in%"integer")&
                     !input$vari2%in%"none"&&
                     (class(get.data.set()[,input$vari2])%in%"numeric"|
                        class(get.data.set()[,input$vari2])%in%"integer")){
            ch = c('Code more variables',
                   'Add trend curves',
                  'Add x=y line',
                  'Add a jitter',
                  'Add rugs',
                  'Join points by line',
                  'Change plot appearance',
                  'Identify points',
                  'Customize labels',
                  'Adjust axis limits')
            if(!is.null(input$select.plot.type)&&
                 ((input$select.plot.type%in%"grid-density plot"|
                    input$select.plot.type%in%"hexbin plot-size"|
                   input$select.plot.type%in%"hexbin plot-alpha")||
                    large.sample&&
                    input$select.plot.type%in%"default")){
              ch = c('Add trend curves',
                     'Add x=y line',
                     'Change plot appearance',
                     'Customize labels',
                     'Adjust axis limits')
            }
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
          }
          updateSelectInput(session,inputId = "advanced_options",
                            choices = ch,
                            selected = sel)
        }
      }
    })
  }
})


# select the colur palette

observe({
  input$colour.palette.reverse
  isolate({
    if(!is.null(input$colour.palette.reverse))
      graphical.par$reverse.palette = input$colour.palette.reverse
  })
})

# select colour ranks or not

observe({
  input$colour.use.ranks
  isolate({
    if(!is.null(input$colour.use.ranks) && input$colour.use.ranks == TRUE)
      graphical.par$col.method = "rank"
    else
      graphical.par$col.method = "linear"
  })
})

observe({
  input$select.colour.palette
  isolate({
    if(!is.null(input$select.colour.palette))
      if(input$select.colour.palette %in% names(graphical.par$colourPalettes$cat))
        graphical.par$col.fun = graphical.par$colourPalettes$cat[[input$select.colour.palette]]
      else if(input$select.colour.palette %in% names(graphical.par$colourPalettes$cont))
        graphical.par$col.fun = graphical.par$colourPalettes$cont[[input$select.colour.palette]]
  })
})


# select the plots background color.
observe({
  input$select.bg1
  isolate({
    if(!is.null(input$select.bg1)){
      graphical.par$bg = input$select.bg1
    }
  })
})

# select the bar color for bar plots
observe({
  input$select.barcolor
  isolate({
    if(!is.null(input$select.barcolor)){
      graphical.par$bar.fill = input$select.barcolor
    }
  })
})

# change the plot type
observe({
  input$select.plot.type
  isolate({
    if(!is.null(input$select.plot.type))
      if(input$select.plot.type%in%"dot plot"){
        graphical.par$plottype = "dot"
      }else if(input$select.plot.type%in%"histogram"){
        graphical.par$plottype = "hist"
      }else if(input$select.plot.type%in%"scatter plot"){
        graphical.par$plottype = "scatter"
      }else if(input$select.plot.type%in%"grid-density plot"){
        graphical.par$plottype = "grid"
      }else if(input$select.plot.type%in%"hexbin plot-size" || input$select.plot.type%in%"hexbin plot-alpha"){
        graphical.par$plottype = "hex"
      }else{
        graphical.par$plottype = "default"
      }
  })
})

# change whether the points interior is drawn.
observe({
  if(!is.null(input$color.interior)){
    isolate({
      if(!is.null(input$select.dotcolor)){
        if(input$color.interior){
#          graphical.par$pch = 19
          graphical.par$fill.pt = "fill"
#          graphical.par$alpha = 0.3
        }else{
#          graphical.par$pch = 1
          graphical.par$fill.pt = "transparent"
        }
      }
    })
  }
})

# select the dot color
observe({
  if(!is.null(input$select.dotcolor)){
    isolate({
      graphical.par$col.pt = input$select.dotcolor
    })
  }
})


# adjust the label size
observe({
  input$adjust.size.scale
  isolate({
    graphical.par$cex = input$adjust.size.scale
  })
})




# adjust the size of the points in dot plot
observe({
  input$adjust.size.points.dot
  isolate({
    if("dot"%in%get.plottype()){
      graphical.par$cex.dotpt = input$adjust.size.points.dot
    }
  })
})

# adjust the size of the points in scatter plot
observe({
  input$adjust.size.points.scatter
  isolate({
    if("scatter"%in%get.plottype()){
      graphical.par$cex.pt = input$adjust.size.points.scatter
    }
  })
})


# adjust the transparancy of the points
observe({
  input$adjust.transparency
  isolate({
#    if(!is.null(input$adjust.transparency)){
#      if(input$adjust.transparency==0){
#        graphical.par$pch=1
#      }else{
#        graphical.par$pch=19
#      }
#    }
    graphical.par$alpha = convert.to.percent(input$adjust.transparency,T)
  })
})

# adjust the number of bars in histogram
observe({
  input$adjust.num.bins
  isolate({
    graphical.par$hist.bins = input$adjust.num.bins
  })
})

# adjust the grid size of the grid-density plot
observe({
  input$adjust.grid.size
  isolate({
    graphical.par$scatter.grid.bins = input$adjust.grid.size
  })
})

# adjust the transparency in a grid-density plot to see lower density areas
observe({
  input$adjust.min.count.grid
  isolate({
    graphical.par$alpha = convert.to.percent(input$adjust.min.count.grid,T)
  })
})

# adjust the bins for the hex-grid plot
observe({
  input$adjust.hex.bins
  isolate({
    graphical.par$hex.bins = input$adjust.hex.bins
  })
})

# Customize labels UI
output$customize.labels.panel = renderUI({
  get.data.set()
  input$vari1
  input$vari2
  isolate({
    plot.par$xlab = NULL
    plot.par$varnames$xlab = NULL
    plot.par$ylab = NULL
    plot.par$varnames$ylab = NULL
    plot.par$main = NULL
    axis.label.title = h5(strong("Axis Labels"))
    main_title_text.object = fixedRow(column(4, h5("Main title:")),
                                      column(6, textInput(inputId="main_title_text",label=NULL)))
    x_axis_text.object = fixedRow(column(4, h5("X-axis label:")),
                                  column(6, textInput(inputId="x_axis_text",label=NULL)))
    y_axis_text.object = fixedRow(column(4, h5("Y-axis label:")),
                                  column(6, textInput(inputId="y_axis_text",label=NULL)))
    change.labels.button.object = fixedRow(column(4),
                                           column(6, actionButton(inputId="change.labels.button",label="Submit")))
    if(!is.null(vis.data())&&!is.null(input$vari1)&&!is.null(input$vari2)&&
         input$vari1%in%colnames(get.data.set())){
      if((class(vis.data()[,input$vari1])%in%"numeric"|
            class(vis.data()[,input$vari1])%in%"integer")&
           !is.null(input$vari2)&&!input$vari2%in%"none"&&
           (class(vis.data()[,input$vari2])%in%"numeric"|
           class(vis.data()[,input$vari2])%in%"integer")){
        list(axis.label.title,
             main_title_text.object,
             x_axis_text.object,
             y_axis_text.object,
             change.labels.button.object)
      }else{
        list(axis.label.title,
             main_title_text.object,
             x_axis_text.object,
             change.labels.button.object)
      }
    }
  })
})

# submit a new main titel or x axis label
observe({
  input$change.labels.button
  isolate({
    if(!is.null(input$change.labels.button)&&
         input$change.labels.button>0){
      if(!is.null(input$main_title_text)&&
        !input$main_title_text%in%""){
        plot.par$main = input$main_title_text
      }else{
        plot.par$main = NULL
      }
      if(!is.null(input$x_axis_text)&&
           !input$x_axis_text%in%""){
        plot.par$xlab = input$x_axis_text
        plot.par$varnames$xlab = input$x_axis_text
      }else{
        plot.par$xlab = NULL
        plot.par$varnames$xlab = NULL
      }
      if(!is.null(input$y_axis_text)&&
           !input$y_axis_text%in%""){
        plot.par$ylab = input$y_axis_text
        plot.par$varnames$ylab = input$y_axis_text
      }else{
        plot.par$ylab = NULL
        plot.par$varnames$ylab = NULL
      }
    }
  })
})




# "Code more variables" panel"
output$code.variables.panel = renderUI({
  get.data.set()
  ret = NULL
  input$vari1
  input$vari2
  input$select.plot.type
  input$color_by_select
  input$point_colour_title
  isolate({
    
    
    
    select.colour.palette.object = NULL
    colour.palette.reverse.object = NULL
    
    
    
    # vari1 = factor, vari2 = factor
    if(!input$vari2%in%"none"&&
       ((class(get.data.set()[,input$vari1])%in%"factor"|
         class(get.data.set()[,input$vari1])%in%"character")&&
        (class(get.data.set()[,input$vari2])%in%"factor"|
         class(get.data.set()[,input$vari2])%in%"character"))) {
      
      select.colour.palette.object = fixedRow(column(3, h5("Colour palette:")),
                                              column(6, selectInput(inputId="select.colour.palette",label=NULL,
                                                                    choices=names(graphical.par$colourPalettes$cat),
                                                                    selected = "Colourblind Friendly",
                                                                    size = 2,
                                                                    selectize = FALSE)))
      colour.palette.reverse.object = fixedRow(column(3),
                                               column(6, checkboxInput(inputId = "colour.palette.reverse", label = "Reverse palette",
                                                                       value = FALSE)))
      
      ret = list(
                 select.colour.palette.object,
                 colour.palette.reverse.object)
    }
      
    else {
      point.symbol.title = NULL
      symbol.object = NULL
      symbol.by.object = NULL
      symbol.linewidth.object = NULL
        
        
      color.by.object  = NULL
      
      color.use.ranks.object = NULL
      
      if((!is.null(input$vari1)&&
          !is.null(input$vari2))&&
         (input$vari1%in%colnames(get.data.set())&&
          (input$vari2%in%'none'||
           input$vari2%in%colnames(get.data.set())))){
        if((class(vis.data()[,input$vari1])%in%"factor"|
            class(vis.data()[,input$vari1])%in%"character")&&
           (is.null(input$vari2)|input$vari2%in%"none")){
          color.by.object = list(fixedRow(column(3, h5("Colour by:")),
                                          column(6, selectInput("color_by_select",
                                                                label=NULL,
                                                                choices = c(" ",get.categorical.column.names(vis.data())),
                                                                selected = input$color_by_select))),
                                 
                                 conditionalPanel("input.color_by_select != ' '",
                                                  fixedRow(column(3, h5("Colour palette:")),
                                                           column(6, selectInput(inputId="select.colour.palette",label=NULL,
                                                                                 choices=names(graphical.par$colourPalettes$cat),
                                                                                 selected = "Colourblind Friendly",
                                                                                 selectize=FALSE,
                                                                                 size = 2))),
                                 conditionalPanel("input.color_by_select != ' '",
                                                  fixedRow(column(3),
                                                           column(6, checkboxInput(inputId = "colour.palette.reverse",
                                                                                   label = "Reverse palette",
                                                                                   value = input$colour.palette.reverse))))))
          
    
          
        }else{
          point.symbol.title = checkboxInput(inputId = "point_symbol_title",
                                             label = strong("Point Symbol"),
                                             value = FALSE)
          
          
          color.by.object = list(conditionalPanel(condition = "input.point_colour_title == true",
                                                  fixedRow(column(3, h5("Colour by:")),
                                                           column(6, selectInput("color_by_select",
                                                                                 label=NULL,
                                                                                 choices=c(" ",colnames(vis.data())),
                                                                                 selected = input$color_by_select)))),

                                 conditionalPanel("input.color_by_select != ' ' & input.point_colour_title == true",
                                                  fixedRow(column(3, h5("Colour palette:")),
                                                           column(6, selectInput(inputId="select.colour.palette",
                                                                                 label=NULL,
                                                                                 
                                                                                 choices = switch(as.character(length(input$color_by_select) > 0 && input$color_by_select %in% get.numeric.column.names(vis.data())),
                                                                                                  "TRUE" = names(graphical.par$colourPalettes$cont),
                                                                                                  "FALSE" = names(graphical.par$colourPalettes$cat)),
                                                                                 selected = input$select.colour.palette,
                                                                                 selectize = FALSE,
                                                                                 size = 2))),
                                                  conditionalPanel("input.color_by_select != ' ' & input.point_colour_title == true",
                                                                   fixedRow(column(3),
                                                                            column(6, checkboxInput(inputId = "colour.palette.reverse",
                                                                                                    label = "Reverse palette",
                                                                                                    value = input$colour.palette.reverse))))
                                                  ))
          
          if(length(input$color_by_select) != 0 && 
             input$color_by_select %in% get.numeric.column.names(vis.data()) && 
             input$point_colour_title == TRUE)
            color.use.ranks.object = fixedRow(column(3),
                                              column(6, checkboxInput(inputId = "colour.use.ranks",
                                                                      label = "Use Ranks",
                                                                      value = input$colour.use.ranks)))
          
          symbol.object = conditionalPanel(condition = "input.point_symbol_title == true",
                                           fixedRow(column(3, h5("Symbol:")),
                                                    column(6, selectInput("point_symbol",
                                                                          label=NULL,
                                                                          choices = c("circle", "square", "diamond", "triangle", "inverted triangle"),
                                                                          selected = "circle"))))
          
#          symbol.object = fixedRow(column(3, h5("Point Symbol:")),
#                                   column(6, selectInput("point_symbol",
#                                                         label=NULL,
#                                                         choices = c("circle", "square", "diamond", "triangle", "inverted triangle"),
#                                                         selected = "circle")))
          
          
          symbol.by.object = conditionalPanel(condition = "input.point_symbol_title == true",
                                              fixedRow(column(3, h5("Symbol by:")),
                                                       column(6, selectInput("point_symbol_by",
                                                                             label=NULL,
                                                                             choices = c(" ", get.categorical.column.names(vis.data())),
                                                                             selected = " "))))
#          symbol.by.object = fixedRow(column(3, h5("Symbol by:")),
#                                      column(6, selectInput("point_symbol_by",
#                                                            label=NULL,
#                                                            choices = c(" ", get.categorical.column.names(vis.data())),
#                                                            selected = " ")))
          
          symbol.linewidth.object = conditionalPanel(condition = "input.point_symbol_title == true",
                                                     fixedRow(column(3, h5("Symbol line width:")),
                                                              column(6, sliderInput("symbol_linewidth", label = NULL, min = 1, 
                                                                                    max = 4, value=2, step=0.2, ticks = FALSE))))
#          symbol.linewidth.object = fixedRow(column(3, h5("Symbol line width:")),
#                                             column(6, sliderInput("symbol_linewidth", label = NULL, min = 1, 
#                                                                   max = 4, value=2, step=0.2, ticks = FALSE)))
        }
#        resize.by.object = NULL
#        if((class(vis.data()[,input$vari1])%in%"numeric"|
#            class(vis.data()[,input$vari1])%in%"integer")&&
#           (!input$vari2%in%"none"&&
#            (class(vis.data()[,input$vari2])%in%"numeric"|
#             class(vis.data()[,input$vari2])%in%"integer"))){
#          resize.by.object = fixedRow(column(3, h5("Resize points by:")),
#                                      column(6, selectInput("resize.by.select",
#                                                            label=NULL,
#                                                            choices=c(" ",get.numeric.column.names(vis.data())),
#                                                            selected = "input$resize.by.select")))
#        }
        if(length(input$select.plot.type) != 0 && 
           (input$select.plot.type %in% "histogram" ||
           input$select.plot.type %in% "grid-density plot" ||
           input$select.plot.type %in% "hexbin plot-size" ||
           input$select.plot.type %in% "hexbin plot-alpha"))
          ret = list(color.by.object)
        else
          ret = list(color.by.object,
                     color.use.ranks.object,
                     #                   resize.by.object,
                     point.symbol.title,
                     symbol.object,
                     symbol.by.object,
                     symbol.linewidth.object)
        

        
      }
      
      # vari1 = numeric , vari2 = none or
      # vari1 = numeric , vari2 = factor or
      # vari1 = factor , vari2 = numeric or
      # vari1 = numeric , vari2 = numeric
      if((input$vari2%in%"none"&&
          (class(get.data.set()[,input$vari1])%in%"numeric"|
           class(get.data.set()[,input$vari1])%in%"integer"))||
         (!input$vari2%in%"none"&&
          (class(get.data.set()[,input$vari1])%in%"factor"|
           class(get.data.set()[,input$vari1])%in%"character")&&
          (class(get.data.set()[,input$vari2])%in%"integer"|
           class(get.data.set()[,input$vari2])%in%"numeric"))||
         (!input$vari2%in%"none"&&
          (class(get.data.set()[,input$vari1])%in%"integer"|
           class(get.data.set()[,input$vari1])%in%"numeric")&&
          (class(get.data.set()[,input$vari2])%in%"character"|
           class(get.data.set()[,input$vari2])%in%"factor")) ||
         (!input$vari2%in%"none"&&
          ((class(get.data.set()[,input$vari1])%in%"numeric"|
            class(get.data.set()[,input$vari1])%in%"integer")&&
           (class(get.data.set()[,input$vari2])%in%"numeric"|
            class(get.data.set()[,input$vari2])%in%"integer")))) {
        
        temp = list()
        temp$x = get.data.set()[,input$vari1]
        if(input$vari2%in%'none'){
          temp$y = NULL
        }else{
          temp$y = get.data.set()[,input$vari2]
        }
        temp$plot = F
        temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
        ##################################################################    
        #    large.sample = T
        large.sample = search.name(temp,"largesample")[[1]]
        if(is.null(large.sample)){
          large.sample=F
        }
        ##################################################################
        
        if(large.sample)
          ret = NULL
      }
      
    }

  })
  ret
})

# The variable the points are colored by has changed
observe({
  input$color_by_select
  isolate({
    if(is.null(input$color_by_select)|
         (!is.null(input$color_by_select)&&
            input$color_by_select%in%" ")){
      plot.par$colby = NULL
      plot.par$varnames$colby = NULL
    }else{
      if(input$color_by_select%in%colnames(vis.data())){
        plot.par$colby = vis.data()[,input$color_by_select]
        plot.par$varnames$colby = input$color_by_select
      }
    }
  })
})

# the variable the points are resized by
observe({
  input$resize.by.select
  isolate({
    if(is.null(input$resize.by.select)|
         (!is.null(input$resize.by.select)&&
            input$resize.by.select%in%" ")){
      plot.par$sizeby = NULL
      plot.par$varnames$sizeby = NULL
    }else{
      plot.par$sizeby = vis.data()[ ,input$resize.by.select]
      plot.par$varnames$sizeby = input$resize.by.select
    }
  })
})


# the point symbol

observe({
  input$point_symbol
  isolate({
    if(length(input$point_symbol) == 0)
      graphical.par$pch = 21
    else
      graphical.par$pch = switch(input$point_symbol, 
                                 "circle" = 21,
                                 "square" = 22,
                                 "diamond" = 23,
                                 "triangle" = 24,
                                 "inverted triangle" = 25)
  })
})

# point symbol by the variable of:
observe({
  input$point_symbol_by
  isolate({
    if(is.null(input$point_symbol_by)|
       (!is.null(input$point_symbol_by)&&
        input$point_symbol_by == " ")){
      plot.par$symbolby = NULL
      plot.par$varnames$symbolby = NULL
    }else{
      plot.par$symbolby = vis.data()[ ,input$point_symbol_by]
      plot.par$varnames$symbolby = input$point_symbol_by
    }
  })
})


# the symbol line width
observe({
  input$symbol_linewidth
  isolate({
    if(length(input$symbol_linewidth) == 0)
      graphical.par$lwd.pt = 2
    else
      graphical.par$lwd.pt = input$symbol_linewidth
  })
})



# update checkbox to fit trend lines for every level
observe({
  input$color_by_select
  isolate({ 
    updateCheckboxInput(session,"each_level",
                        label=paste("Fit trend for every level of",
                                    input$color_by_select),
                        value=input$each_level)
  })
})

# add trends and curves 
output$trend.curve.panel = renderUI({
  get.data.set()
  isolate({
#    title.add.trend.curve = h5("Add trend curves")
    trend.curves.title = h5(strong("Trend Curves"))
    smoother.title = h5(strong("Smoother"))
    check.linear.object = checkboxInput("check_linear",label="linear",value = input$check_linear)
    check.quadratic.object = checkboxInput("check_quadratic",label="quadratic",value = input$check_quadratic)
    check.cubic.object = checkboxInput("check_cubic",label="cubic",value = input$check_cubic)
    check.smoother.object = checkboxInput("check_smoother",label="Add smoother",value = input$check_smoother)
    check.quantiles.object = checkboxInput("check_quantiles",label="Use Quantiles",value = input$check_quantiles)
    color.linear.select = selectInput("color.linear",label="",
                                      choices=c("blue", "red","black",
                                                "green4","yellow","pink",
                                                "grey","orange"),
                                      selected=input$color.linear)
    type.linear.select = selectInput("type.linear", label = "", 
                                     choices = c("solid", "dashed",
                                                 "dotted", "dotdash",
                                                 "longdash", "twodash"), 
                                     selected = input$type.linear)
    color.quadratic.select = selectInput("color.quadratic",label="",
                                         choices=c("red","black","blue",
                                                   "green4","yellow","pink",
                                                   "grey","orange"),
                                         selected=input$color.quadratic)
    type.quadratic.select = selectInput("type.quadratic", label = "", 
                                        choices = c("solid", "dashed",
                                                    "dotted", "dotdash",
                                                    "longdash", "twodash"),
                                        selected = input$type.quadratic)
    color.cubic.select = selectInput("color.cubic",label="",
                                     choices=c("green4","red","black","blue",
                                               "yellow","pink",
                                               "grey","orange"),
                                     selected=input$color.cubic)
    type.cubic.select = selectInput("type.cubic", label = "", 
                                    choices = c("solid", "dashed",
                                                "dotted", "dotdash",
                                                "longdash", "twodash"), 
                                    selected = input$type.cubic)
    color.smoother.select = selectInput("color.smoother",label="",
                                        choices=c("red","black","blue",
                                                  "green4","yellow","magenta",
                                                  "grey","orange"),
                                        selected="magenta")
    smoother.smooth.slider = sliderInput("smoother.smooth",
                                         label="",min=0.01,max=1,value=0.7,
                                         step=0.01,ticks=F)
    
    each_level.check = checkboxInput("each_level",
                                     label=paste("Fit trend for every level of",
                                                 input$color_by_select))
    each_level_seperate.check = checkboxInput("each_level_seperate",
                                              label="Fit paralell trend lines",
                                              value=T)
    line.width.multiplier.object = fixedRow(column(width = 3, "Line Width Multiplier:"),
                                            column(width = 6, sliderInput("line.width.multiplier", 
                                                                          label = NULL, 
                                                                          min = 1, 
                                                                          max = 2, 
                                                                          value = input$line.width.multiplier, step = 0.5, ticks = FALSE)))
    list(trend.curves.title,
         fixedRow(column(width=3),
                  column(width=4,"Line colour"),
                  column(width=4,"Line type")),
         fixedRow(column(width=3,check.linear.object),
                  column(width=4,color.linear.select),
                  column(width=4,type.linear.select)),
         fixedRow(column(width=3,check.quadratic.object),
                  column(width=4,color.quadratic.select),
                  column(width=4,type.quadratic.select)),
         fixedRow(column(width=3,check.cubic.object),
                  column(width=4,color.cubic.select),
                  column(width=4,type.cubic.select)),
         line.width.multiplier.object,
         smoother.title,
         fixedRow(column(width=3,check.smoother.object),
                  column(width=6,color.smoother.select)),
         conditionalPanel("input.check_smoother",
                          fixedRow(
                            column(3, check.quantiles.object),
                            column(6, smoother.smooth.slider)
                          )),
                          
#                          fixedRow(width=12,check.quantiles.object),
#                          fixedRow(width=12,smoother.smooth.slider)),
         conditionalPanel("input.color_by_select != ' ' &                    
                          (input.check_linear | input.check_quadratic |
                          input.check_cubic | input.check_smoother) &
                          !input.check_quantiles",
                          each_level.check),
         conditionalPanel("input.each_level",
                          each_level_seperate.check))
  })
})

# update whether trend curves are parallel or not
observe({
  input$each_level_seperate
  isolate({
    graphical.par$trend.parallel = input$each_level_seperate
  })
})

# update the quantile smother
observe({
  input$check_quantiles
  isolate({
    if(!is.null(input$check_quantiles)&&input$check_quantiles){
      updateCheckboxInput(session,"each_level",value=F)
      graphical.par$quant.smooth = c(0.25, 0.5, 0.75)
    }else{
      graphical.par$quant.smooth = NULL
    }
  })
})

# change whether trend lines are drawn for 
# every selected level
observe({
  input$each_level
  isolate({
    if(!is.null(input$each_level)){
      graphical.par$trend.by = input$each_level
    }
  })
})

# observe linear trend
observe({
  input$check_linear
  input$color.linear
  input$type.linear
  isolate({
#    graphical.par$bs.inference = F
#    graphical.par$inference.type = NULL
    if(!is.null(input$check_linear)){
      if(input$check_linear){
        if(length(which(graphical.par$trend%in%"linear"))==0){
          graphical.par$trend=c(graphical.par$trend,"linear")
        }
        graphical.par$col.trend[["linear"]] = input$color.linear
        graphical.par$lty.trend[["linear"]] = switch(input$type.linear, 
                                                     "solid" = 1, "dashed" = 2,
                                                     "dotted" = 3, "dotdash" = 4,
                                                     "longdash" = 5, "twodash" = 6)
      }else{
        if(length(which(graphical.par$trend%in%"linear"))>0){
          graphical.par$trend=graphical.par$trend[-which(graphical.par$trend%in%"linear")]
          if(length(graphical.par$trend)==0){
            graphical.par$trend=NULL
          }
        }
      }
    }
  })
})

# observe quadratic trend
observe({
  input$check_quadratic
  input$color.quadratic
  input$type.quadratic
  isolate({
#    graphical.par$bs.inference = F
    if(!is.null(input$check_quadratic)){
      if(input$check_quadratic){
        if(length(which(graphical.par$trend%in%"quadratic"))==0){
          graphical.par$trend=c(graphical.par$trend,"quadratic")
        }
        graphical.par$col.trend[["quadratic"]] = input$color.quadratic
        graphical.par$lty.trend[["quadratic"]] = switch(input$type.quadratic, 
                                                     "solid" = 1, "dashed" = 2,
                                                     "dotted" = 3, "dotdash" = 4,
                                                     "longdash" = 5, "twodash" = 6)
      }else{
        if(length(which(graphical.par$trend%in%"quadratic"))>0){
          graphical.par$trend=graphical.par$trend[-which(graphical.par$trend%in%"quadratic")]
          if(length(graphical.par$trend)==0){
            graphical.par$trend=NULL
          }
        }
      }
    }
  })
})

# observe cubic trend
observe({
  input$check_cubic
  input$color.cubic
  input$type.cubic
  isolate({
#    graphical.par$bs.inference = F
    if(!is.null(input$check_cubic)){
      if(input$check_cubic){
        if(length(which(graphical.par$trend%in%"cubic"))==0){
          graphical.par$trend=c(graphical.par$trend,"cubic")
        }
        graphical.par$col.trend[["cubic"]] = input$color.cubic
        graphical.par$lty.trend[["cubic"]] = switch(input$type.cubic, 
                                                        "solid" = 1, "dashed" = 2,
                                                        "dotted" = 3, "dotdash" = 4,
                                                        "longdash" = 5, "twodash" = 6)
      }else{
        if(length(which(graphical.par$trend%in%"cubic"))>0){
          graphical.par$trend=graphical.par$trend[-which(graphical.par$trend%in%"cubic")]
          if(length(graphical.par$trend)==0){
            graphical.par$trend=NULL
          }
        }
      }
    }
  })
})

# add a smoother
observe({
  input$check_smoother
  input$check.quantiles
  input$color.smoother
  input$smoother.smooth
  isolate({
    if(!is.null(input$check_smoother)&&input$check_smoother){
      graphical.par$smooth = input$smoother.smooth
      graphical.par$col.smooth = input$color.smoother
      if(!is.null(input$check.quantiles)&&input$check.quantiles){
        graphical.par$quant.smooth = "default"
      }else{
        graphical.par$quant.smooth = NULL
      }
    }else{
      graphical.par$smooth = 0
      graphical.par$quant.smooth = NULL
      graphical.par$col.smooth = ""
      updateCheckboxInput(session,"check.quantiles",value=F)
    }
  })
})

# add a x=y line
output$xy.line.panel = renderUI({
  get.data.set()
  ret = NULL
  
  isolate({
    
    if(!input$vari2%in%"none"&&
       ((class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer")&&
        (class(get.data.set()[,input$vari2])%in%"numeric"|
         class(get.data.set()[,input$vari2])%in%"integer"))) {
#      titel.xyline = h5("Add line of equality (x = y)")
      xyline.title = h5(strong("Trend Line Options"))
      check.xyline.object = checkboxInput("check.xyline",
                                          label="Add y=x line",
                                          value=F)
      color.xyline.select = selectInput("color.xyline",label="",
                                        choices=c("red","black","blue",
                                                  "green4","yellow","pink",
                                                  "grey","orange"),
                                        selected="black")
      
      ret = list(xyline.title,
                 fixedRow(column(width=3,check.xyline.object),
                          column(width=6,color.xyline.select)))
    }
  })
  ret
})




# check for changes in color or whether the x=y-line is drawn
observe({
  input$check.xyline
  input$color.xyline
  if(!is.null(input$check.xyline)&&
       input$check.xyline){
    graphical.par$LOE = T
    graphical.par$col.LOE = input$color.xyline
  }else{
    graphical.par$LOE = F
    graphical.par$col.LOE = NULL
  }
})


# trend line width
observe({
  input$line.width.multiplier
  if(!is.null(input$line.width.multiplier))
    graphical.par$lwd = input$line.width.multiplier
})

# add jitter to the plot
output$add.jitter.panel = renderUI({
  get.data.set()
  ret = NULL
  
  isolate({
    if(!input$vari2%in%"none"&&
       ((class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer")&&
        (class(get.data.set()[,input$vari2])%in%"numeric"|
         class(get.data.set()[,input$vari2])%in%"integer"))) {
#      title.jitter = h5("Add a jitter")
      axis.features.title = h5(strong("Axis Features"))
      check.jitter.x.object = checkboxInput("check.jitter.x",
                                            label="height",
                                            value=F)
      check.jitter.y.object = checkboxInput("check.jitter.y",
                                            label="rightfoot",
                                            value=F)
      ret = list(axis.features.title,
                 fixedRow(column(2, h5("Jitter:")),
                          column(width=4,check.jitter.x.object),
                          column(width=4,check.jitter.y.object)))
      
      temp = list()
      temp$x = get.data.set()[,input$vari1]
      if(input$vari2%in%'none'){
        temp$y = NULL
      }else{
        temp$y = get.data.set()[,input$vari2]
      }
      temp$plot = F
      temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
      ##################################################################    
      #    large.sample = T
      large.sample = search.name(temp,"largesample")[[1]]
      if(is.null(large.sample)){
        large.sample=F
      }
      ##################################################################
      
      if(large.sample)
        ret = NULL
    }
  })
  ret 
})

# observe jitter input
observe({
  input$check.jitter.x
  input$check.jitter.y
  isolate({
    graphical.par$jitter = ""
    if(!is.null(input$check.jitter.x)&&input$check.jitter.x&&
         !is.null(input$check.jitter.y)&&!input$check.jitter.y){
      graphical.par$jitter = "x"
    }else if(!is.null(input$check.jitter.x)&&!input$check.jitter.x&&
               !is.null(input$check.jitter.y)&&input$check.jitter.y){
      graphical.par$jitter = "y"
    }else if(!is.null(input$check.jitter.x)&&input$check.jitter.x&&
               !is.null(input$check.jitter.y)&&input$check.jitter.y){
      graphical.par$jitter = "xy"
    }
  })
})

# add rugs to plot
output$add.rugs.panel = renderUI({
  get.data.set()
  ret = NULL
  
  isolate({
    
    if(!input$vari2%in%"none"&&
       ((class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer")&&
        (class(get.data.set()[,input$vari2])%in%"numeric"|
         class(get.data.set()[,input$vari2])%in%"integer"))) {
#      title.rugs = h5("Add rugs")
      check.rugs.x.object = checkboxInput("check.rugs.x",
                                          label="height",
                                          value=F)
      check.rugs.y.object = checkboxInput("check.rugs.y",
                                          label="rightfoot",
                                          value=F)
      
      ret = list(
                 fixedRow(column(2, h5("Rugs:")),
                          column(width=4,check.rugs.x.object),
                          column(width=4,check.rugs.y.object)))
      
      temp = list()
      temp$x = get.data.set()[,input$vari1]
      if(input$vari2%in%'none'){
        temp$y = NULL
      }else{
        temp$y = get.data.set()[,input$vari2]
      }
      temp$plot = F
      temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
      ##################################################################    
      #    large.sample = T
      large.sample = search.name(temp,"largesample")[[1]]
      if(is.null(large.sample)){
        large.sample=F
      }
      ##################################################################
      
      if(large.sample)
        ret = NULL
    }
  })
  ret
  
})

# observe whether rugs should be added
observe({
  input$check.rugs.x
  input$check.rugs.y
  isolate({
    graphical.par$rugs = ""
    if(!is.null(input$check.rugs.x)&&input$check.rugs.x&&
         !is.null(input$check.rugs.y)&&!input$check.rugs.y){
      graphical.par$rugs = "x"
    }else if(!is.null(input$check.rugs.x)&&!input$check.rugs.x&&
               !is.null(input$check.rugs.y)&&input$check.rugs.y){
      graphical.par$rugs = "y"
    }else if(!is.null(input$check.rugs.x)&&input$check.rugs.x&&
               !is.null(input$check.rugs.y)&&input$check.rugs.y){
      graphical.par$rugs = "xy"
    }
  })
})

# join points panel
output$join.points.panel = renderUI({
  get.data.set()
  ret = NULL
  isolate({
    if(!input$vari2%in%"none"&&
       ((class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer")&&
        (class(get.data.set()[,input$vari2])%in%"numeric"|
         class(get.data.set()[,input$vari2])%in%"integer"))) {
      
#      title.join = h5("Join points by lines")
      join.points.title = h5(strong("Join points"))
      check.join.object = checkboxInput("check.join",
                                        label="Join points",
                                        value=F)
      color.join.select = selectInput("color.join",label="",
                                      choices=c("red","black","blue",
                                                "green4","yellow","pink",
                                                "grey","orange"),
                                      selected="blue")
      ret = list(join.points.title,
                 fixedRow(column(width=3,check.join.object),
                          column(width=6,color.join.select)))
      
      temp = list()
      temp$x = get.data.set()[,input$vari1]
      if(input$vari2%in%'none'){
        temp$y = NULL
      }else{
        temp$y = get.data.set()[,input$vari2]
      }
      temp$plot = F
      temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
      ##################################################################    
      #    large.sample = T
      large.sample = search.name(temp,"largesample")[[1]]
      if(is.null(large.sample)){
        large.sample=F
      }
      ##################################################################
      
      if(large.sample)
        ret = NULL
    }
  })
  ret
})

# observe whether points should be joined
observe({
  input$check.join
  input$color.join
  isolate({
    if(!is.null(input$check.join)){
      graphical.par$col.line = input$color.join
      graphical.par$join = input$check.join
    }
  })
})

# panel for wigets to adjust the x and y axis limits
output$adjust.axis.panel = renderUI({
  get.data.set()
  ret = NULL
  input$vari1
  input$vari2
#   plot.ret.para$parameters
  isolate({
    
    if((input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer"))||
       (!input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"factor"|
         class(get.data.set()[,input$vari1])%in%"character")&&
        (class(get.data.set()[,input$vari2])%in%"integer"|
         class(get.data.set()[,input$vari2])%in%"numeric"))||
       (!input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"integer"|
         class(get.data.set()[,input$vari1])%in%"numeric")&&
        (class(get.data.set()[,input$vari2])%in%"character"|
         class(get.data.set()[,input$vari2])%in%"factor")) ||
       (!input$vari2%in%"none"&&
       ((class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer")&&
        (class(get.data.set()[,input$vari2])%in%"numeric"|
         class(get.data.set()[,input$vari2])%in%"integer")))) {
      
      plot.par$xlim = NULL
      plot.par$ylim = NULL
      if((!is.null(input$vari1)&&
          !is.null(input$vari2))&&
         (input$vari1%in%colnames(get.data.set())&&
          (input$vari2%in%"none"||
           input$vari2%in%colnames(get.data.set())))){
        ret = list(h5(strong('Axis Limits')))
        temp = list()
        temp$x = get.data.set()[,input$vari1]
        if(input$vari2%in%'none'){
          temp$y = NULL
        }else{
          temp$y = get.data.set()[,input$vari2]
        }
        temp$plot = F
        tester = try(do.call(iNZightPlots:::iNZightPlot, temp))
        ###################################################################
        #      large.sample = T
        large.sample = search.name(tester,"largesample")[[1]]
#        limits.x = search.name(tester,"xlim")[[1]]
#        limits.y = search.name(tester,"ylim")[[1]]
        if(is.null(large.sample)){
          large.sample = F
        }
        ###################################################################
        if((input$vari2%in%"none"&&
           (class(get.data.set()[,input$vari1])%in%"numeric"|
           class(get.data.set()[,input$vari1])%in%"integer"))||
           (!input$vari2%in%"none"&&
           (class(get.data.set()[,input$vari1])%in%"integer"|
           class(get.data.set()[,input$vari1])%in%"numeric")&&
           (class(get.data.set()[,input$vari2])%in%"character"|
           class(get.data.set()[,input$vari2])%in%"factor"))){
          limits.x = range(temp$x, na.rm = TRUE)
          ret[[2]] = fixedRow(column(2,h5("x-axis:")),
                              column(4,textInput("x_axis_low_text",
                                                 label="",
                                                 value=limits.x[1])),
                              column(4,textInput("x_axis_hig_text",
                                                 label="",
                                                 value=limits.x[2])))
          
        }
        else if((!input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"factor"|
                class(get.data.set()[,input$vari1])%in%"character")&&
                (class(get.data.set()[,input$vari2])%in%"integer"|
                class(get.data.set()[,input$vari2])%in%"numeric"))) {
          limits.y = range(temp$y, na.rm = TRUE)
          ret[[2]] = fixedRow(column(2,h5("x-axis:")),
                              column(4,textInput("x_axis_low_text",
                                                 label="",
                                                 value=limits.y[1])),
                              column(4,textInput("x_axis_hig_text",
                                                 label="",
                                                 value=limits.y[2])))
        }
        else if((!input$vari2%in%"none"&&
                ((class(get.data.set()[,input$vari1])%in%"numeric"|
                class(get.data.set()[,input$vari1])%in%"integer")&&
                (class(get.data.set()[,input$vari2])%in%"numeric"|
                class(get.data.set()[,input$vari2])%in%"integer")))){
          limits.x = range(temp$x, na.rm = TRUE)
          limits.y = range(temp$y, na.rm = TRUE)
          ret[[2]] = fixedRow(column(2,h5("x-axis:")),
                              column(4,textInput("x_axis_low_text",
                                                 label="",
                                                 value=limits.y[1])),
                              column(4,textInput("x_axis_hig_text",
                                                 label="",
                                                 value=limits.y[2])))
          ret[[3]] = fixedRow(column(2,h5("y-axis:")),
                              column(4,textInput("y_axis_low_text",
                                                 label="",
                                                 value=limits.x[1])),
                              column(4,textInput("y_axis_hig_text",
                                                 label="",
                                                 value=limits.x[2])))
        }
        ret[[length(ret)+1]] = fixedRow(column(2),
                                        column(8, 
                                               actionButton("reset_axis_limits_button",
                                                            label='Reset')))
      }
    }
  })
  
  ret
})

# observe whether numeric input is used in x axis limit low and high
observe({
  input$x_axis_low_text
  input$x_axis_hig_text
  isolate({
    if(!is.null(input$x_axis_low_text)&&
         !is.null(input$x_axis_hig_text)){
      tryCatch({
        xlim = c(as.numeric(input$x_axis_low_text),
                 as.numeric(input$x_axis_hig_text))
        if(is.na(as.numeric(input$x_axis_low_text))){
          xlim[1] = 0
        }
        if(is.na(as.numeric(input$x_axis_hig_text))){
          xlim[2] = 0
        }
        plot.par$xlim = xlim
      }, warning = function(w) {
        if(is.na(suppressWarnings(as.numeric(input$x_axis_low_text)))){
          updateTextInput(session,"x_axis_low_text",
                          value = "")
        }
        if(is.na(suppressWarnings(as.numeric(input$x_axis_hig_text)))){
          updateTextInput(session,"x_axis_hig_text",
                          value = "")
        }
        plot.par$xlim = NULL
      }, error = function(e) {
        if(is.na(suppressWarnings(as.numeric(input$x_axis_low_text)))){
          updateTextInput(session,"x_axis_low_text",
                          value = "")
        }
        if(is.na(suppressWarnings(as.numeric(input$x_axis_hig_text)))){
          updateTextInput(session,"x_axis_hig_text",
                          value = "")
        }
        plot.par$xlim = NULL
      }, finally = {})
    }
  })
})

# observe whether numeric input is used in y axis limit low and high
observe({
  input$y_axis_low_text
  input$y_axis_hig_text
  isolate({
    if(!is.null(input$y_axis_low_text)&&
         !is.null(input$y_axis_hig_text)){
      tryCatch({
        ylim = c(as.numeric(input$y_axis_low_text),
                 as.numeric(input$y_axis_hig_text))
        if(is.na(as.numeric(input$y_axis_low_text))){
          ylim[1] = 0
        }
        if(is.na(as.numeric(input$y_axis_hig_text))){
          ylim[2] = 0
        }
        plot.par$ylim = ylim
      }, warning = function(w) {
        if(is.na(suppressWarnings(as.numeric(input$y_axis_low_text)))){
          updateTextInput(session,"y_axis_low_text",
                          value = "")
        }
        if(is.na(suppressWarnings(as.numeric(input$y_axis_hig_text)))){
          updateTextInput(session,"y_axis_hig_text",
                          value = "")
        }
        plot.par$ylim = NULL
      }, error = function(e) {
        if(is.na(suppressWarnings(as.numeric(input$y_axis_low_text)))){
          updateTextInput(session,"y_axis_low_text",
                          value = "")
        }
        if(is.na(suppressWarnings(as.numeric(input$y_axis_hig_text)))){
          updateTextInput(session,"y_axis_hig_text",
                          value = "")
        }
        plot.par$ylim = NULL
      }, finally = {})
    }
  })
})

# reset the x and y limits
observe({
  input$reset_axis_limits_button
  isolate({
    if(!is.null(input$reset_axis_limits_button)&&
         input$reset_axis_limits_button>0){
      plot.par$xlim = NULL
      plot.par$ylim = NULL
      temp = list()
      temp$y = get.data.set()[,input$vari1]
      if(input$vari2%in%'none'){
        temp$x = NULL
      }else{
        temp$x = get.data.set()[,input$vari2]
      }
#      temp$plot = F
#      temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
#      limits.x = search.name(temp,"xlim")[[1]]
#      limits.y = search.name(temp,"ylim")[[1]]
      if((!input$vari2%in%"none"&&
         ((class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer")&&
         (class(get.data.set()[,input$vari2])%in%"numeric"|
         class(get.data.set()[,input$vari2])%in%"integer")))) {
        limits.x = range(temp$x, na.rm = TRUE)
        limits.y = range(temp$y, na.rm = TRUE)
        updateTextInput(session,"x_axis_low_text",
                        value = limits.x[1])
        updateTextInput(session,"x_axis_hig_text",
                        value = limits.x[2])
        updateTextInput(session,"y_axis_low_text",
                        value = limits.y[1])
        updateTextInput(session,"y_axis_hig_text",
                        value = limits.y[2])
      }
      else if((input$vari2%in%"none"&&
              (class(get.data.set()[,input$vari1])%in%"numeric"|
              class(get.data.set()[,input$vari1])%in%"integer"))||
              (!input$vari2%in%"none"&&
              (class(get.data.set()[,input$vari1])%in%"integer"|
              class(get.data.set()[,input$vari1])%in%"numeric")&&
              (class(get.data.set()[,input$vari2])%in%"character"|
              class(get.data.set()[,input$vari2])%in%"factor"))) {
        limits.y = range(temp$y, na.rm = TRUE)
        updateTextInput(session,"x_axis_low_text",
                        value = limits.y[1])
        updateTextInput(session,"x_axis_hig_text",
                        value = limits.y[2])
      }
      else if((!input$vari2%in%"none"&&
              (class(get.data.set()[,input$vari1])%in%"factor"|
              class(get.data.set()[,input$vari1])%in%"character")&&
              (class(get.data.set()[,input$vari2])%in%"integer"|
              class(get.data.set()[,input$vari2])%in%"numeric"))) {
        limits.x = range(temp$x, na.rm = TRUE)
        updateTextInput(session,"x_axis_low_text",
                        value = limits.x[1])
        updateTextInput(session,"x_axis_hig_text",
                        value = limits.x[2])
      }
      
    }
  })
})

output$adjust.number.bars.panel = renderUI({
  get.data.set()
  input$vari1
  input$vari2
  ret = NULL
  isolate({
    
    if((input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"factor"|
         class(get.data.set()[,input$vari1])%in%"character")) ||
       (!input$vari2%in%"none"&&
        ((class(get.data.set()[,input$vari1])%in%"factor"|
          class(get.data.set()[,input$vari1])%in%"character")&&
         (class(get.data.set()[,input$vari2])%in%"factor"|
          class(get.data.set()[,input$vari2])%in%"character")))) {
      plot.par$zoombar = NULL
      
      if((!is.null(input$vari1)&&
          !is.null(input$vari2))&&
         (input$vari1%in%colnames(get.data.set())&&
          (input$vari1%in%colnames(get.data.set())||
           input$vari1%in%"none"))){
        if(length(levels(get.data.set()[,input$vari1]))>2){
          ret = list(sliderInput("num.bars.slider",
                                 label = "Number of Bars:",
                                 min=2,
                                 max=length(levels(get.data.set()[,input$vari1])),
                                 step=1,
                                 ticks=F,
                                 value=length(levels(get.data.set()[,input$vari1]))),
                     sliderInput("starting.bars.slider",
                                 label = "Starting Point:",
                                 min=1,
                                 max=length(levels(get.data.set()[,input$vari1]))-1,
                                 step=1,
                                 ticks=F,
                                 value=1),
                     actionButton("reset.zoombars","Reset"))
        }
      }
    }
    
  })
  ret
})

# observe the Number of bars slider
observe({
  input$num.bars.slider
  input$starting.bars.slider
  isolate({
    plot.par$zoombar = c(input$starting.bars.slider,input$num.bars.slider)
  })
})

#observe the reset button for adjusting bars
observe({
  input$reset.zoombars
  isolate({
    updateSliderInput(session,"num.bars.slider",
                      value=length(levels(get.data.set()[,input$vari1])))
    updateSliderInput(session,"starting.bars.slider",
                      value=1)
  })
})

# identify points panel
output$points.identify.panel = renderUI({
  get.data.set()
  ret = NULL
  input$vari1
  input$vari2
  isolate({
    
    if((input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"numeric"|
         class(get.data.set()[,input$vari1])%in%"integer"))||
       (!input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"factor"|
         class(get.data.set()[,input$vari1])%in%"character")&&
        (class(get.data.set()[,input$vari2])%in%"integer"|
         class(get.data.set()[,input$vari2])%in%"numeric"))||
       (!input$vari2%in%"none"&&
        (class(get.data.set()[,input$vari1])%in%"integer"|
         class(get.data.set()[,input$vari1])%in%"numeric")&&
        (class(get.data.set()[,input$vari2])%in%"character"|
         class(get.data.set()[,input$vari2])%in%"factor")) ||
       (!input$vari2%in%"none"&&
        ((class(get.data.set()[,input$vari1])%in%"numeric"|
          class(get.data.set()[,input$vari1])%in%"integer")&&
         (class(get.data.set()[,input$vari2])%in%"numeric"|
          class(get.data.set()[,input$vari2])%in%"integer")))) {
      plot.par$locate=NULL
      plot.par$locate.id=NULL
      plot.par$locate.col=NULL
      plot.par$locate.extreme=NULL
      plot.par.stored$locate.id=NULL
      identified.points$values=list()
      ret = list()
      ret[[1]] = fixedRow(column(11,h5(strong("How do you want to label points?"))))
      ret[[2]] = fixedRow(column(4,
                                 checkboxInput("label_observation_check",
                                               label="Text label",
                                               value=F)),
                          column(6,
                                 conditionalPanel("input.label_observation_check",
                                                  selectInput("label.select",
                                                              label="",
                                                              choices=c("id",
                                                                        colnames(get.data.set()))))))
      
      ret[[3]] = fixedRow(column(4,
                                 checkboxInput("color_points_check",
                                               label="Colour",
                                               value=F)),
                          column(6,
                                 conditionalPanel("input.color_points_check",
                                                  selectInput("color.select",
                                                              label="Select Colour",
                                                              choices=c("red",
                                                                        "blue",
                                                                        "green4")))))
      ret[[4]] = fixedRow(column(4,
                                 checkboxInput("same_level_of_check",
                                               label="With the same level of",
                                               value=F)),
                          column(6,
                                 conditionalPanel("input.same_level_of_check",
                                                  selectInput("same.level.of.select",
                                                              label="",
                                                              choices=colnames(get.data.set())))))
      ret[[5]] = radioButtons("select_identify_method",
                              label = h5(strong("Select method of selection")),
                              choices = c("Select by value",
                                          "Extremes",
                                          "Range of values"))
      if(!is.null(input$vari1)&&!is.null(input$vari2)){
        if(input$vari1%in%colnames(get.data.set())&&
           (input$vari2%in%"none"||
            input$vari2%in%colnames(get.data.set()))){
          ch = ""
          if(!is.null(input$by.value.column.select)){
            ch = c("none",sort(get.data.set()[,input$by.value.column.select]))
          }
          ret[[6]] = conditionalPanel("input.select_identify_method=='Select by value'&&
                                      (input.label_observation_check||input.color_points_check)",
                                      checkboxInput("single_vs_multiple_check",
                                                    label="Single value",
                                                    value=F),
                                      conditionalPanel("!input.single_vs_multiple_check",
                                                       fixedRow(column(6,
                                                                       selectInput("by.value.column.select",
                                                                                   label="Select a column",
                                                                                   choices=colnames(get.data.set()))),
                                                                column(4,
                                                                       selectInput("value.select",
                                                                                   label="Select multiple values",
                                                                                   choices=ch,
                                                                                   multiple=T,
                                                                                   selectize=F,
                                                                                   selected="none",
                                                                                   size=8)))),
                                      conditionalPanel("input.single_vs_multiple_check",
                                                       fixedRow(column(6,
                                                                       sliderInput("select.unique.value.slider",
                                                                                   label="Select single value",
                                                                                   min=0,
                                                                                   max=nrow(get.data.set()),
                                                                                   value=0,
                                                                                   step=1,
                                                                                   ticks=F)),
                                                                column(3,
                                                                       numericInput("specify.correct.numeric",
                                                                                    label="",
                                                                                    value=0,
                                                                                    min=0,
                                                                                    max=nrow(get.data.set()),
                                                                                    step=1)))))
          
          if(is.numeric(get.data.set()[,input$vari1])&&
             (!input$vari2%in%"none"&&
              is.numeric(get.data.set()[,input$vari2]))){
            ret[[7]] = conditionalPanel("input.select_identify_method=='Extremes'&&
                                        (input.label_observation_check||input.color_points_check)",
                                        sliderInput("extremes.slider",
                                                    label="Number of points",
                                                    min=0,
                                                    max=nrow(get.data.set()),
                                                    step=1,
                                                    value=0,
                                                    ticks=F))
          }else if((!input$vari2%in%"none"&&
                    ((is.factor(get.data.set()[,input$vari1])&&
                      is.numeric(get.data.set()[,input$vari2]))||
                     (is.numeric(get.data.set()[,input$vari1])&&
                      is.factor(get.data.set()[,input$vari2]))))||
                   (input$vari2%in%"none"&&
                    is.numeric(get.data.set()[,input$vari1]))){
            ret[[7]] = conditionalPanel("input.select_identify_method=='Extremes'&&
                                        (input.label_observation_check||input.color_points_check)",
                                        fixedRow(column(4,numericInput("extreme.lower",
                                                                       label="Lower",
                                                                       value=0,
                                                                       min=0,
                                                                       max=nrow(get.data.set()),
                                                                       step=1)),
                                                 column(4,numericInput("extreme.upper",
                                                                       label="Upper",
                                                                       value=0,
                                                                       min=0,
                                                                       max=nrow(get.data.set()),
                                                                       step=1))))
          }
          ret[[8]] = conditionalPanel("input.select_identify_method=='Range of values'&&
                                      (input.label_observation_check||input.color_points_check)",
                                      fixedRow(column(6,
                                                      sliderInput("range.values.slider", 
                                                                  label = "Select range", 
                                                                  min = 0, 
                                                                  max = nrow(get.data.set()), 
                                                                  value = c(0, 0),
                                                                  ticks=F)),
                                               column(5,
                                                      selectInput("range.column.select",
                                                                  label="Select column",
                                                                  choices=colnames(get.data.set())))))
          ret[[9]] = fixedRow(column(3,checkboxInput("show.stored.check",
                                                     label="Show stored",
                                                     value=T)),
                              column(4,
                                     actionButton("store.obs.button",
                                                  label="Store selected")),
                              column(4,
                                     actionButton("reset.obs.button",
                                                  label="Forget stored")))
        }
      }
    }
  })
  ret
})

# identify points per label
observe({
  input$label_observation_check
  input$label.select
  isolate({
    if(!is.null(input$label_observation_check)&&
         !is.null(input$label.select)&&
         (input$label.select%in%colnames(get.data.set())||
            input$label.select%in%"id")){
      if(input$label_observation_check){
        if(input$label.select%in%"id"){
          plot.par$locate=1:nrow(get.data.set())
        }else{
          plot.par$locate=get.data.set()[,input$label.select]
        }
      }else{
        plot.par$locate=NULL
      }
    }
  })
})

# identify points per color
observe({
  input$color_points_check
  input$color.select
  isolate({
    if(!is.null(input$color_points_check)&&
         !is.null(input$color.select)){
      if(input$color_points_check){
        plot.par$locate.col=input$color.select
      }else{
        plot.par$locate.col=NULL
      }
    }else{
      plot.par$locate.col=NULL
    }
  })
})

# reset the identify points widgets when the selection 
# method is changed.
observe({
  input$select_identify_method
  isolate({
    temp = NULL
    if(!is.null(input$select_identify_method)){
      if(input$select_identify_method%in%"Select by value"){
        if(input$single_vs_multiple_check){
          temp = input$select.unique.value.slider
          if(temp==0){
            temp=NULL
          }
        }else{
          temp = which(get.data.set()[,input$by.value.column.select]%in%input$value.select)
          if(length(temp)==0){
            temp=NULL
          }
        }
      }else if(input$select_identify_method%in%"Range of values"){
        range = input$range.values.slider
        if(!all(range%in%0)){
          range[which(range%in%0)] = 1
          temp = get.data.set()[,input$range.column.select]
          names(temp) = 1:length(temp)
          temp = sort(temp)
          temp = as.numeric(names(temp)[range[1]:range[2]])
          temp = which(get.data.set()[,input$range.column.select]%in%
                         get.data.set()[,input$range.column.select][temp])
        }else{
          temp = NULL
        }
      }
      if(!input$select_identify_method%in%"Extremes"){
        if(input$same_level_of_check){
          temp = which(get.data.set()[,input$same.level.of.select]%in%
                         get.data.set()[,input$same.level.of.select][temp])
        }
        if(input$show.stored.check){
          temp = unique(c(plot.par.stored$locate.id,temp))
        }
      }
      if(length(temp)==0){
        temp = NULL
      }
    }
    plot.par$locate.id = temp
    plot.par$locate.extreme=NULL
    updateSelectInput(session,
                      "by.value.column.select",
                      choices=colnames(get.data.set()),
                      selected=colnames(get.data.set())[1])
    ch = ""
    if(!is.null(input$by.value.column.select)){
      ch = c("none",sort(get.data.set()[,input$by.value.column.select]))
    }
    updateSelectInput(session,
                      "value.select",
                      choices=ch,
                      selected=ch[1])
    updateCheckboxInput(session,
                        "input.single_vs_multiple_check",
                        value=F)
    updateSliderInput(session,
                      "select.unique.value.slider",
                      max=length(ch),
                      value=0)
    updateNumericInput(session,
                       "specify.correct.numeric",
                       value=0,
                       max=length(ch))
    updateSliderInput(session,
                      "extremes.slider",
                      max=nrow(get.data.set()),
                      value=0)
    updateNumericInput(session,
                       "extreme.lower",
                       value=0,
                       max=nrow(get.data.set()))
    updateNumericInput(session,
                       "extreme.upper",
                       value=0,
                       max=nrow(get.data.set()))
    updateSliderInput(session,
                      "range.values.slider",
                      max = nrow(get.data.set()),
                      value = c(0, 0))
    updateSelectInput(session,
                      "range.column.select",
                      choices=colnames(get.data.set()),
                      selected = colnames(get.data.set())[1])
  })
})

# extreme slider for scatter plots
observe({
  if(!is.null(input$extremes.slider)){
    isolate({
      if(!is.null(input$select_identify_method)&&
           input$select_identify_method%in%"Extremes"){
        plot.par$locate.id = NULL
        if(input$extremes.slider == 0){
          plot.par$locate.extreme = NULL
        }else{
          plot.par$locate.extreme = input$extremes.slider
        }
      }
    })
  }
})

# observe the lower limit of extreme values in dot plots
observe({
  input$extreme.upper
  isolate({
    if(!is.null(input$select_identify_method)&&
         input$select_identify_method%in%"Extremes"){
      plot.par$locate.id = NULL
      if(!is.null(input$extreme.upper)){
        if(is.null(plot.par$locate.extreme)){
          plot.par$locate.extreme = c(0,0)
        }
        plot.par$locate.extreme[2] = input$extreme.upper
      }
    }
  })
})

# observe the lower limit of extreme values in dot plots
observe({
  input$extreme.lower
  isolate({
    if(!is.null(input$select_identify_method)&&
         input$select_identify_method%in%"Extremes"){
      plot.par$locate.id = NULL
      if(!is.null(input$extreme.lower)){
        if(is.null(plot.par$locate.extreme)){
          plot.par$locate.extreme = c(0,0)
        }
        plot.par$locate.extreme[1] = input$extreme.lower
      }
    }
  })
})

# With the same level of is checked
observe({
  if(!is.null(input$same_level_of_check)){
    isolate({
      temp = NULL
      if(input$select_identify_method%in%"Select by value"){
        if(input$single_vs_multiple_check){
          temp = input$select.unique.value.slider
          if(temp==0){
            temp=NULL
          }
        }else{
          temp = which(get.data.set()[,input$by.value.column.select]%in%input$value.select)
          if(length(temp)==0){
            temp=NULL
          }
        }
      }else if(input$select_identify_method%in%"Range of values"){
        range = input$range.values.slider
        if(!all(range%in%0)){
          range[which(range%in%0)] = 1
          temp = get.data.set()[,input$range.column.select]
          names(temp) = 1:length(temp)
          temp = sort(temp)
          temp = as.numeric(names(temp)[range[1]:range[2]])
          temp = which(get.data.set()[,input$range.column.select]%in%
                         get.data.set()[,input$range.column.select][temp])
        }
      }
      if(input$same_level_of_check){
        temp = which(get.data.set()[,input$same.level.of.select]%in%
                       get.data.set()[,input$same.level.of.select][temp])
        if(length(temp)==0){
          temp = NULL
        }
      }
      if(input$show.stored.check){
        temp = unique(c(plot.par.stored$locate.id,temp))
      }
      plot.par$locate.id = temp
    })
  }
})

# variable for with the same level of is changed
observe({
  if(!is.null(input$same.level.of.select)){
    isolate({
      temp = NULL
      if(input$select_identify_method%in%"Select by value"){
        if(input$single_vs_multiple_check){
          temp = input$select.unique.value.slider
          if(temp==0){
            temp=NULL
          }
        }else{
          temp = which(get.data.set()[,input$by.value.column.select]%in%input$value.select)
          if(length(temp)==0){
            temp=NULL
          }
        }
      }else if(input$select_identify_method%in%"Range of values"){
        range = input$range.values.slider
        if(!all(range%in%0)){
          range[which(range%in%0)] = 1
          temp = get.data.set()[,input$range.column.select]
          names(temp) = 1:length(temp)
          temp = sort(temp)
          temp = as.numeric(names(temp)[range[1]:range[2]])
          temp = which(get.data.set()[,input$range.column.select]%in%
                         get.data.set()[,input$range.column.select][temp])
        }else{
          temp = NULL
        }
      }
      temp = which(get.data.set()[,input$same.level.of.select]%in%
                     get.data.set()[,input$same.level.of.select][temp])
      if(length(temp)==0){
        temp = NULL
      }
      if(input$show.stored.check){
        temp = unique(c(plot.par.stored$locate.id,temp))
      }
      plot.par$locate.id = temp
    })
  }
})

# unique value slider
observe({
  if(!is.null(input$select.unique.value.slider)){
    isolate({
      if(!is.null(input$same.level.of.select)&&
           input$same.level.of.select%in%colnames(get.data.set())){
        temp = input$select.unique.value.slider
        if(input$same_level_of_check){
          temp = which(get.data.set()[,input$same.level.of.select]%in%
                         get.data.set()[,input$same.level.of.select][temp])
        }
        if(input$show.stored.check){
          plot.par$locate.id=unique(c(plot.par.stored$locate.id,temp))
        }else{
          plot.par$locate.id=temp
        }
      }
      updateNumericInput(session,"specify.correct.numeric",
                         value=input$select.unique.value.slider)
    })
  }
})

# unique numeric
observe({
  if(!is.null(input$specify.correct.numeric)){
    isolate({
      if(!is.null(input$same.level.of.select)&&
           input$same.level.of.select%in%colnames(get.data.set())){
        temp = input$specify.correct.numeric
        if(input$same_level_of_check){
          temp = which(get.data.set()[,input$same.level.of.select]%in%
                         get.data.set()[,input$same.level.of.select][temp])
        }
        if(input$show.stored.check){
          plot.par$locate.id=unique(c(plot.par.stored$locate.id,temp))
        }else{
          plot.par$locate.id=temp
        }
      }
      updateNumericInput(session,"select.unique.value.slider",
                         value=input$specify.correct.numeric)
    })
  }
})

# pick multiple values or just a single value
observe({
  input$single_vs_multiple_check
  isolate({
    plot.par$locate.id=NULL
    updateSelectInput(session,
                      "by.value.column.select",
                      choices=colnames(get.data.set()),
                      selected=colnames(get.data.set())[1])
    ch = ""
    if(!is.null(input$by.value.column.select)){
      ch = c("none",sort(get.data.set()[,input$by.value.column.select]))
    }
    updateSelectInput(session,
                      "value.select",
                      choices=ch,
                      selected=ch[1])
    updateSliderInput(session,
                      "select.unique.value.slider",
                      max=length(ch),
                      value=0)
    updateNumericInput(session,
                       "specify.correct.numeric",
                       value=0,
                       max=length(ch))
  })
})

# update the values for picking multiple values when the variable is changed
observe({
  if(!is.null(input$by.value.column.select)){
    isolate({
      plot.par$locate.id=NULL
      if(class(get.data.set()[,input$by.value.column.select])%in%"numeric"||
           class(get.data.set()[,input$by.value.column.select])%in%"integer"){
        temp = sort(get.data.set()[,input$by.value.column.select])
      }else{
        temp = sort(levels(get.data.set()[,input$by.value.column.select]))
      }
      updateSelectInput(session,
                        "value.select",
                        choices=c("none",temp),
                        selected=c("none",temp)[1])
    })
  }
})

# update locate.id for multiple values
observe({
  if(!is.null(input$value.select)){
    isolate({
      if(!is.null(input$same.level.of.select)&&
           input$same.level.of.select%in%colnames(get.data.set())){
        temp = which(get.data.set()[,input$by.value.column.select]%in%input$value.select)
        if(input$same_level_of_check){
          temp = which(get.data.set()[,input$same.level.of.select]%in%
                         get.data.set()[,input$same.level.of.select][temp])
        }
        if(input$show.stored.check){
          plot.par$locate.id=unique(c(plot.par.stored$locate.id,temp))
        }else{
          plot.par$locate.id=temp
        }
      }
    })
  }
})

# change whether the stored values are shown or not
observe({
  input$show.stored.check
  isolate({
    if(!is.null(input$select_identify_method)){
      if(input$select_identify_method%in%"Select by value"){
        if(!input$single_vs_multiple_check){
          temp = which(get.data.set()[,input$by.value.column.select]%in%input$value.select)
          if(input$same_level_of_check){
            temp = which(get.data.set()[,input$same.level.of.select]%in%
                           get.data.set()[,input$same.level.of.select][temp])
          }
          if(input$show.stored.check){
            plot.par$locate.id = unique(c(plot.par.stored$locate.id,temp))
          }else{
            plot.par$locate.id = temp
          }
        }else{
          if(input$select.unique.value.slider!=0&&
               input$specify.correct.numeric!=0){
            temp = input$specify.correct.numeric
            if(input$same_level_of_check){
              temp = which(get.data.set()[,input$same.level.of.select]%in%
                             get.data.set()[,input$same.level.of.select][temp])
            }
            if(input$show.stored.check){
              plot.par$locate.id = unique(c(plot.par.stored$locate.id,temp))
            }else{
              plot.par$locate.id = temp
            }
          }
        }
      }else if(input$select_identify_method%in%"Extreme"){
        # This needs work if the iNZight developer ever makes 
        # it possible to use locate.id and locate.extreme at 
        # the same time
        if(input$extremes.slider!=0){ 
          if(input$show.stored.check){
            plot.par$locate.id = NULL
            plot.par$locate.extreme = input$extremes.slider
          }else{
            plot.par$locate.id = NULL
            plot.par$locate.extreme = input$extremes.slider
          }
        }else if(input$extreme.lower!=0||
                   input$extreme.upper!=0){
          if(input$show.stored.check){
            plot.par$locate.id = NULL
            plot.par$locate.extreme = c(input$extreme.lower,input$extreme.upper)
          }else{
            plot.par$locate.id = NULL
            plot.par$locate.extreme = c(input$extreme.lower,input$extreme.upper)
          }
        }
      }else{
        if(any(input$range.values.slider>0)){
          if(input$range.column.select%in%colnames(get.data.set())){
            range = input$range.values.slider
            range[which(range%in%0)] = 1
            temp = get.data.set()[,input$range.column.select]
            names(temp) = 1:length(temp)
            temp = sort(temp)
            temp = as.numeric(names(temp)[range[1]:range[2]])
            temp = which(get.data.set()[,input$range.column.select]%in%
                           get.data.set()[,input$range.column.select][temp])
            if(input$same_level_of_check){
              temp = which(get.data.set()[,input$same.level.of.select]%in%
                             get.data.set()[,input$same.level.of.select][temp])
            }
            if(input$show.stored.check){
              plot.par$locate.id = unique(c(plot.par.stored$locate.id,temp))
            }else{
              plot.par$locate.id = temp
            }
          }
        }else{
          if(input$show.stored.check){
            plot.par$locate.id = plot.par.stored$locate.id
          }else{
            plot.par$locate.id = NULL
          }
        }
      }
    }
  })
})

# set a range of values
observe({
  if(!is.null(input$range.values.slider)&&
       !is.null(input$range.column.select)&&
       input$range.column.select%in%colnames(get.data.set())){
    isolate({
      range = input$range.values.slider
      if(length(which(range%in%0))<2){
        range[which(range%in%0)] = 1
        temp = get.data.set()[,input$range.column.select]
        names(temp) = 1:length(temp)
        temp = sort(temp)
        temp = as.numeric(names(temp)[range[1]:range[2]])
        temp = which(get.data.set()[,input$range.column.select]%in%
                       get.data.set()[,input$range.column.select][temp])
        if(input$same_level_of_check){
          temp = which(get.data.set()[,input$same.level.of.select]%in%
                         get.data.set()[,input$same.level.of.select][temp])
        }
        if(input$show.stored.check){
          plot.par$locate.id = unique(c(plot.par.stored$locate.id,temp))
        }else{
          plot.par$locate.id = temp
        }
      }else{
        if(input$show.stored.check){
          plot.par$locate.id = plot.par.stored$locate.id
        }else{
          plot.par$locate.id = NULL
        }
      }
    })
  }
})

# Store points to be visible in other plots
observe({
  input$store.obs.button
  isolate({
    if(!is.null(input$store.obs.button)&&
         input$store.obs.button>0){
      if((is.null(plot.par$locate.id)||
           length(plot.par$locate.id)==0)&&
           length(plot.par$locate.extreme)>0){
        temp = list()
        temp$x = get.data.set()[,input$vari1]
        if(input$vari2%in%'none'){
          temp$y = NULL
        }else{
          temp$y = get.data.set()[,input$vari2]
        }
        temp$locate.extreme = plot.par$locate.extreme
        temp$plot = F
        temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
        extreme.ids = search.name(temp,"extreme.ids")[[1]]
        plot.par.stored$locate.id = unique(c(plot.par.stored$locate.id,
                                             extreme.ids))
        
      }else if(length(plot.par$locate.id)>0){
        plot.par.stored$locate.id = unique(c(plot.par$locate.id,
                                             plot.par.stored$locate.id))
      }
    }
  })
})

# Remove all stored points
observe({
  input$reset.obs.button
  isolate({
    if(!is.null(input$reset.obs.button)&&
         input$reset.obs.button>0){
      plot.par.stored$locate.id = NULL
      temp = NULL
      if(input$select_identify_method%in%"Select by value"){
        if(input$single_vs_multiple_check){
          temp = input$select.unique.value.slider
          if(temp==0){
            temp=NULL
          }
        }else{
          temp = which(get.data.set()[,input$by.value.column.select]%in%input$value.select)
          if(length(temp)==0){
            temp=NULL
          }
        }
      }else if(input$select_identify_method%in%"Range of values"){
        range = input$range.values.slider
        if(!all(range%in%0)){
          range[which(range%in%0)] = 1
          temp = get.data.set()[,input$range.column.select]
          names(temp) = 1:length(temp)
          temp = sort(temp)
          temp = as.numeric(names(temp)[range[1]:range[2]])
          temp = which(get.data.set()[,input$range.column.select]%in%
                         get.data.set()[,input$range.column.select][temp])
        }else{
          temp = NULL
        }
      }
      if(input$same_level_of_check){
        temp = which(get.data.set()[,input$same.level.of.select]%in%
                       get.data.set()[,input$same.level.of.select][temp])
      }
      if(length(temp)==0){
        temp = NULL
      }
      if(input$show.stored.check){
        temp = unique(c(plot.par.stored$locate.id,temp))
      }
      plot.par$locate.id = temp
    }
  })
})




output$select_additions_panel = renderUI({
  get.data.set()
  ret = NULL
  input$vari1
  input$vari2
  
  isolate({
    
    temp = list()

    temp$x = get.data.set()[,input$vari1]
    if(input$vari2%in%'none'){
      temp$y = NULL
    }else{
      temp$y = get.data.set()[,input$vari2]
    }
    temp$plot = F
    temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
    
    ##################################################################    
    #    large.sample = T
    large.sample = search.name(temp, "largesample")[[1]]
    if(is.null(large.sample)){
      large.sample=F
    }
    ##################################################################
    if((!is.null(input$vari1)&&
        !is.null(input$vari2))&&
       (input$vari1%in%colnames(get.data.set())&&
        (input$vari2%in%"none"|
         input$vari2%in%colnames(get.data.set()))))
      
      # vari = factor, vari = none
      if(input$vari2%in%"none"&&
         (class(get.data.set()[,input$vari1])%in%"factor"|
          class(get.data.set()[,input$vari1])%in%"character")){
        ret = selectInput(inputId = "select_additions",
                          label = NULL,
                          choices = c('Customise Plot Appearance',
                                      'Axes and Labels',
                                      'Add Inference Information'),
                          selected = input$select_additions)
        
        # vari1 = factor, vari2 = factor
      }else if(!input$vari2%in%"none"&&
               ((class(get.data.set()[,input$vari1])%in%"factor"|
                 class(get.data.set()[,input$vari1])%in%"character")&&
                (class(get.data.set()[,input$vari2])%in%"factor"|
                 class(get.data.set()[,input$vari2])%in%"character"))){
        ret = selectInput(inputId = "select_additions",
                          label = NULL,
                          choices = c('Customise Plot Appearance',
                                      'Axes and Labels',
                                      'Add Inference Information'),
                          selected = input$select_additions)
        
        # vari1 = numeric , vari2 = none or
        # vari1 = numeric , vari2 = factor or
        # vari1 = factor , vari2 = numeric
      }else if((input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"numeric"|
                 class(get.data.set()[,input$vari1])%in%"integer"))||
               (!input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"factor"|
                 class(get.data.set()[,input$vari1])%in%"character")&&
                (class(get.data.set()[,input$vari2])%in%"integer"|
                 class(get.data.set()[,input$vari2])%in%"numeric"))||
               (!input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"integer"|
                 class(get.data.set()[,input$vari1])%in%"numeric")&&
                (class(get.data.set()[,input$vari2])%in%"character"|
                 class(get.data.set()[,input$vari2])%in%"factor"))){
        ret = selectInput(inputId = "select_additions",
                          label = NULL,
                          choices = c('Customise Plot Appearance',
                                      'Axes and Labels',
                                      'Identify Points',
                                      'Add Inference Information'),
                          selected = input$select_additions)
        
        if(large.sample){
          ret = selectInput(inputId = "select_additions",
                            label = NULL,
                            choices = c('Customise Plot Appearance',
                                        'Axes and Labels',
                                        'Add Inference Information'),
                            selected = input$select_additions)
        }
        
        # vari1 = numeric , vari2 = numeric
      }else if(!input$vari2%in%"none"&&
               ((class(get.data.set()[,input$vari1])%in%"numeric"|
                 class(get.data.set()[,input$vari1])%in%"integer")&&
                (class(get.data.set()[,input$vari2])%in%"numeric"|
                 class(get.data.set()[,input$vari2])%in%"integer"))){
        ret = selectInput(inputId = "select_additions",
                          label = NULL,
                          choices = c('Customise Plot Appearance',
                                      'Trend Lines and Curves',
                                      'Axes and Labels',
                                      'Identify Points',
                                      'Add Inference Information'),
                          selected = input$select_additions)
        
        if(large.sample){
          ret = selectInput(inputId = "select_additions",
                            label = NULL,
                            choices = c('Customise Plot Appearance',
                                        'Trend Lines and Curves',
                                        'Axes and Labels',
                                        'Add Inference Information'),
                            selected = input$select_additions)
        }
      }
  })
  list(ret) 
})


# save main plot;
output$saveplot = downloadHandler(
  filename = function() {
    if(input$saveplottype == "interactive html")
      paste("Plot.html")
    else
      paste("Plot", input$saveplottype, sep = ".")
  },
  
  content = function(file) {
    
    if(input$saveplottype == "jpg")
      jpeg(file)
    else if(input$saveplottype == "png")
      png(file)
    else if(input$saveplottype == "pdf")
      pdf(file, useDingbats = FALSE)
    else if(input$saveplottype == "interactive html") {
      create.html = function() {
        if (!is.null(vis.par())) {
          dafr = get.data.set()
          if(is.numeric(plot.par$x)&
             is.numeric(plot.par$y)){
            temp = vis.par()
            temp$trend.parallel = TRUE
            temp.x = temp$x
            temp$x=temp$y
            temp$y=temp.x
            temp.varnames.x = temp$varnames$x
            temp$varnames$x = temp$varnames$y
            temp$varnames$y = temp.varnames.x
            if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
               tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
              tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,temp)
              }, warning = function(w) {
                print(w)
              }, error = function(e) {
                print(e)
              }, finally = {})
            }else{
              plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
            }
          }else{
            if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
               tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
              tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,vis.par())
              }, warning = function(w) {
                print(w)
              }, error = function(e) {
                print(e)
              }, finally = {})
            }else{
              plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
            }
          }
        }
      }
      temp.dir = iNZightPlots:::exportHTML.function(create.html, file = file, width = 10, height = 6)
      #      file.dir = paste(temp.dir, "/index.html", sep="")
      workingdir = getwd()
      setwd(temp.dir)
      file.copy("index.html", file)
      #      file.remove("file.dir")
      setwd(workingdir)
    }
    
    if (!is.null(vis.par())) {
      dafr = get.data.set()
      if(is.numeric(plot.par$x)&
         is.numeric(plot.par$y)){
        temp = vis.par()
        temp$trend.parallel = TRUE
        temp.x = temp$x
        temp$x=temp$y
        temp$y=temp.x
        temp.varnames.x = temp$varnames$x
        temp$varnames$x = temp$varnames$y
        temp$varnames$y = temp.varnames.x
        if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
          tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,temp)
          }, warning = function(w) {
            print(w)
          }, error = function(e) {
            print(e)
          }, finally = {})
        }else{
          plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
        }
      }else{
        if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
           tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
          tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,vis.par())
          }, warning = function(w) {
            print(w)
          }, error = function(e) {
            print(e)
          }, finally = {})
        }else{
          plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
        }
      }
    }
    
    dev.off()
  })  

output$interactive.plot = renderUI({
  input$vari1
  input$vari2
  input$subs1
  input$subs2
  isolate({
    create.html = function() {
      if (!is.null(vis.par())) {
        dafr = get.data.set()
        if(is.numeric(plot.par$x)&
           is.numeric(plot.par$y)){
          temp = vis.par()
          temp$trend.parallel = TRUE
          temp.x = temp$x
          temp$x=temp$y
          temp$y=temp.x
          temp.varnames.x = temp$varnames$x
          temp$varnames$x = temp$varnames$y
          temp$varnames$y = temp.varnames.x
          if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
             tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
            tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,temp)
            }, warning = function(w) {
              print(w)
            }, error = function(e) {
              print(e)
            }, finally = {})
          }else{
            plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
          }
        }else{
          if(!is.null(parseQueryString(session$clientData$url_search)$debug)&&
             tolower(parseQueryString(session$clientData$url_search)$debug)%in%"true"){
            tryCatch({plot.ret.para$parameters = do.call(iNZightPlots:::iNZightPlot,vis.par())
            }, warning = function(w) {
              print(w)
            }, error = function(e) {
              print(e)
            }, finally = {})
          }else{
            plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
          }
        }
      }
    }
    temp.dir = iNZightPlots:::exportHTML.function(create.html, width = 10, height = 6)
#    setwd(temp.dir)
#    includeHTML("index.html")
#    file.dir = paste(temp.dir, "/index.html", sep="")
    addResourcePath("path", temp.dir)
    tags$iframe(
      seamless = "seamless",
      src = "path/index.html",
      height = 600, width = 800
      )
  })
})



observe({
  input$select.plot.type
  isolate({
    if(!is.null(input$select.plot.type) && input$select.plot.type == "hexbin plot-size")
      graphical.par$hex.style = "size"
    if(!is.null(input$select.plot.type) && input$select.plot.type == "hexbin plot-alpha")
      graphical.par$hex.style = "alpha"
    
  })
})


# add fitted values and residuals
# add trends and curves 
output$add.fitted.residuals.panel = renderUI({
  get.data.set()
  isolate({
    add.fitted.values.button = conditionalPanel("input.check_linear ||  input.check_quadratic || input.check_cubic || input.check_smoother",
                                                actionButton("store_fitted_values", "Store fitted values",
                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    add.residuals.button = conditionalPanel("input.check_linear ||  input.check_quadratic || input.check_cubic || input.check_smoother",
                                            actionButton("store_residuals", "Store residuals",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    
    
    
    
   
    list(fixedRow(column(width = 6, add.fitted.values.button),
                  column(width = 6, add.residuals.button))
         )
  })
})


observeEvent(input$store_fitted_values, {
  showModal(modalDialog(
    h5(strong("Specify names for the new variables")),
    
    conditionalPanel("input.check_linear",
                     fixedRow(column(2, h5("Linear:")),
                              column(6, textInput(inputId = "add_linear_fitted_values",
                                                  value = paste(input$vari1, ".predict.linear", sep = ""), 
                                                  label=NULL)))),
    conditionalPanel("input.check_quadratic",
                     fixedRow(column(2, h5("Quadratic:")),
                              column(6, textInput(inputId="add_quadratic_fitted_values",
                                                  value = paste(input$vari1, ".predict.quadratic", sep = ""), 
                                                  label=NULL)))),
    conditionalPanel("input.check_cubic",
                     fixedRow(column(2, h5("Cubic:")),
                              column(6, textInput(inputId="add_cubic_fitted_values",
                                                  value = paste(input$vari1, ".predict.cubic", sep = ""), 
                                                  label=NULL)))),
    conditionalPanel("input.check_smoother",
                     fixedRow(column(2, h5("Smoother:")),
                              column(6, textInput(inputId="add_smoother_fitted_values",
                                                  value = paste(input$vari1, ".predict.smoother", sep = ""), 
                                                  label=NULL)))),
    actionButton("store_fitted_values_ok", "OK"),
    textOutput("add_fitted_values_status"),
    title = "Store fitted values"
    
  ))
})


output$add_fitted_values_status = renderText({
  if(!is.null(input$store_fitted_values_ok) &&
     input$store_fitted_values_ok > 0)
    "Add succesful"
  else
    NULL
})


observeEvent(input$store_residuals, {
  showModal(modalDialog(
    h5(strong("Specify names for the new variables")),
    
    conditionalPanel("input.check_linear",
                     fixedRow(column(2, h5("Linear:")),
                              column(6, textInput(inputId="add_linear_residuals",
                                                  value = paste(input$vari1, ".residuals.linear", sep = ""), 
                                                  label=NULL)))),
    conditionalPanel("input.check_quadratic",
                     fixedRow(column(2, h5("Quadratic:")),
                              column(6, textInput(inputId="add_quadratic_residuals",
                                                  value = paste(input$vari1, ".residuals.quadratic", sep = ""), 
                                                  label=NULL)))),
    conditionalPanel("input.check_cubic",
                     fixedRow(column(2, h5("Cubic:")),
                              column(6, textInput(inputId="add_cubic_residuals",
                                                  value = paste(input$vari1, ".residuals.cubic", sep = ""), 
                                                  label=NULL)))),
    conditionalPanel("input.check_smoother",
                     fixedRow(column(2, h5("Smoother:")),
                              column(6, textInput(inputId="add_smoother_residuals",
                                                  value = paste(input$vari1, ".residuals.smoother", sep = ""), 
                                                  label=NULL)))),
    actionButton("store_resisuals_ok", "OK"),
    textOutput("add_residuals_status"),
    title = "Store residuals"
    
  ))
})


output$add_residuals_status = renderText({
  if(!is.null(input$store_resisuals_ok) &&
     input$store_resisuals_ok > 0)
    "Add succesful"
  else
    NULL
})

observe({
  input$store_resisuals_ok
  isolate({
    if(!is.null(input$store_resisuals_ok) &&
       input$store_resisuals_ok > 0) {
      linear_trend = FALSE
      quadratic_trend = FALSE
      cubic_trend = FALSE
      smoother_trend = FALSE
      temp1 = input$vari1
      temp2 = input$vari2
      temp = get.data.set()
      if("linear" %in% graphical.par$trend) {
        linear_trend = TRUE
        fit.linear = with(vis.par(), lm(x ~ y, na.action = na.exclude))
        resi.linear = data.frame(residuals(fit.linear))
        colnames(resi.linear) = input$add_linear_residuals
        temp = cbind(temp, resi.linear)
      }
      if("quadratic" %in% graphical.par$trend) {
        quadratic_trend = TRUE
        fit.quadratic = with(vis.par(), lm(x ~ y + I(y^2), na.action = na.exclude))
        resi.quadratic = data.frame(residuals(fit.quadratic))
        colnames(resi.quadratic) = input$add_quadratic_residuals
        temp = cbind(temp, resi.quadratic)
      }
      if("cubic" %in% graphical.par$trend) {
        cubic_trend = TRUE
        fit.cubic = with(vis.par(), lm(x ~ y + I(y^2) + I(y^3), na.action = na.exclude))
        resi.cubic = data.frame(residuals(fit.cubic))
        colnames(resi.cubic) = input$add_cubic_residuals
        temp = cbind(temp, resi.cubic)
      }
      if(graphical.par$smooth > 0) {
        temp3 = graphical.par$smooth
        smoother_trend = TRUE
        fit.smooth = with(vis.par(), loess(x ~ y, span = graphical.par$smooth, 
                                           family = "gaussian", degree = 1, na.action = "na.exclude"))
        resi.smooth = data.frame(residuals(fit.smooth))
        colnames(resi.smooth) = input$add_smoother_residuals
        temp = cbind(temp, resi.smooth)
        
      }
      updatePanel$datachanged = updatePanel$datachanged + 1
      values$data.set = temp
      updateCheckboxInput(session, "vari1", value = temp1)
      updateCheckboxInput(session, "vari2", value = temp2)
      if(linear_trend)
        updateCheckboxInput(session, "check_linear", value = T)
      if(quadratic_trend)
        updateCheckboxInput(session, "check_quadratic", value = T)
      if(cubic_trend)
        updateCheckboxInput(session, "check_cubic", value = T)
      if(smoother_trend) {
        updateCheckboxInput(session, "check_smoother",value = T)
        updateSliderInput(session, "smoother.smooth", value = temp3)
        
      }
    }
  })
})

observe({
  input$store_fitted_values_ok
  isolate({
    if(!is.null(input$store_fitted_values_ok) &&
       input$store_fitted_values_ok > 0) {
      linear_trend = FALSE
      quadratic_trend = FALSE
      cubic_trend = FALSE
      smoother_trend = FALSE
      temp1 = input$vari1
      temp2 = input$vari2
      temp = get.data.set()
      if("linear" %in% graphical.par$trend) {
        linear_trend = TRUE
        fit.linear = with(vis.par(), lm(x ~ y, na.action = na.exclude))
        pred.linear = data.frame(predict(fit.linear, newdata = data.frame(x = graphical.par$y)))
        colnames(pred.linear) = input$add_linear_fitted_values
        temp = cbind(temp, pred.linear)
      }
      if("quadratic" %in% graphical.par$trend) {
        quadratic_trend = TRUE
        fit.quadratic = with(vis.par(), lm(x ~ y + I(y^2), na.action = na.exclude))
        pred.quadratic = data.frame(predict(fit.quadratic, newdata = data.frame(x = graphical.par$y)))
        colnames(pred.quadratic) = input$add_quadratic_fitted_values
        temp = cbind(temp, pred.quadratic)
      }
      if("cubic" %in% graphical.par$trend) {
        cubic_trend = TRUE
        fit.cubic = with(vis.par(), lm(x ~ y + I(y^2) + I(y^3), na.action = na.exclude))
        pred.cubic = data.frame(predict(fit.cubic, newdata = data.frame(x = graphical.par$y)))
        colnames(pred.cubic) = input$add_cubic_fitted_values
        temp = cbind(temp, pred.cubic)
      }
      if(graphical.par$smooth > 0) {
        temp3 = graphical.par$smooth
        smoother_trend = TRUE
        fit.smooth = with(vis.par(), loess(x ~ y, span = graphical.par$smooth, 
                                           family = "gaussian", degree = 1, na.action = "na.exclude"))
        pred.smooth = data.frame(predict(fit.smooth, newdata = data.frame(x = graphical.par$y)))
        colnames(pred.smooth) = input$add_smoother_fitted_values
        temp = cbind(temp, pred.smooth)
        
      }
      updatePanel$datachanged = updatePanel$datachanged + 1
      values$data.set = temp
      updateCheckboxInput(session, "vari1", value = temp1)
      updateCheckboxInput(session, "vari2", value = temp2)
      if(linear_trend)
        updateCheckboxInput(session, "check_linear", value = T)
      if(quadratic_trend)
        updateCheckboxInput(session, "check_quadratic", value = T)
      if(cubic_trend)
        updateCheckboxInput(session, "check_cubic", value = T)
      if(smoother_trend) {
        updateCheckboxInput(session, "check_smoother",value = T)
        updateSliderInput(session, "smoother.smooth", value = temp3)
        
      }
    }
  })
})


observe({
  input$go.to.old
  if (!is.null(input$go.to.old) && input$go.to.old > 0) {
    isolate({
      output$visualize.panel <- renderUI({
        get.data.set()
        isolate({
          old.visualize.panel.ui(get.data.set())
        })
      })
    })
  }
})



observe({
  input$go.to.new
  if (!is.null(input$go.to.new) && input$go.to.new > 0) {
    if(!is.null(input$sub1_level_mini) && input$sub1_level_mini != 0) {
      updateSliderInput(session,"sub1_level_mini", value=0)
    }
    if(!is.null(input$sub2_level_mini) && input$sub2_level_mini != 0) {
      updateSliderInput(session,"sub2_level_mini", value=0)
    }
    if((is.null(input$sub1_level_mini) || input$sub1_level_mini == 0) &&
       (is.null(input$sub2_level_mini) || input$sub2_level_mini == 0)) {
      isolate({
        output$visualize.panel <- renderUI({
          get.data.set()
          isolate({
            visualize.panel.ui(get.data.set())
          })
        })
      })
    }
    
  }
})

output$old_add_inference = renderUI({
  get.data.set()
  input$vari1
  input$vari2
  ret = NULL
  isolate({
    dafr = get.data.set()
    add_inference.check = checkboxInput("add.inference",
                                        label="Add inference",
                                        value=input$add.inference)
    mean_median.radio = radioButtons("inference_parameter1",
                                     label="Parameter",
                                     choices=c("Mean","Median"),
                                     selected=input$inference_parameter1,
                                     inline=T)
    normal_bootstrap.radio = radioButtons("inference_type1",
                                          label="Type of inference",
                                          choices=c("Normal","Bootstrap"),
                                          selected=input$inference_type1,
                                          inline=T)
    confidence.interval.check = checkboxInput("confidence_interval1",
                                              label="Confidence interval",
                                              value=input$confidence_interval1)
    comparison.interval.check = checkboxInput("comparison_interval1",
                                              label="Comparison interval",
                                              value=input$comparison_interval1)
    year12_bootstrap.radio = radioButtons("inference_type2",
                                          label="Type of inference",
                                          choices=c("Year 12","Bootstrap"),
                                          selected=input$inference_type2,
                                          inline=T)
    intervals = NULL
    graphical.par$inference.par = NULL
    graphical.par$bs.inference = F
    if((!is.null(input$vari1)&&
        !is.null(input$vari2))&&
       (input$vari1%in%colnames(get.data.set())&&
        (input$vari2%in%colnames(get.data.set())||
         input$vari2%in%'none'))){
      if((!is.null(input$confidence_interval1)&&
          input$confidence_interval1)||
         (!is.null(input$comparison_interval1)&&
          input$comparison_interval1)){
        if(!is.null(input$confidence_interval1)&&
           input$confidence_interval1){
          intervals = c(intervals,"conf")
        }
        if(!is.null(input$comparison_interval1)&&
           input$comparison_interval1){
          intervals = c(intervals,"comp")
        }
        if(!is.null(input$inference_parameter1)&&
           input$inference_parameter1%in%"Mean"){
          graphical.par$inference.par = "mean"
        }else if(!is.null(input$inference_parameter1)&&
                 input$inference_parameter1%in%"Median"){
          graphical.par$inference.par = "median"
        }
        if((!is.null(input$inference_type1)&&
            input$inference_type1%in%"Bootstrap")||
           (!is.null(input$inference_type2)&&
            input$inference_type2%in%"Bootstrap")){
          graphical.par$bs.inference = T
        }else{
          graphical.par$bs.inference = F
        }
      }
      graphical.par$inference.type = intervals
      # vari1 = numeric; vari2 = numeric
      if(!input$vari2%in%"none"&&
         (class(dafr[,input$vari1])%in%"numeric"|
          class(dafr[,input$vari1])%in%"integer")&&
         (class(dafr[,input$vari2])%in%"numeric"|
          class(dafr[,input$vari2])%in%"integer")){
        ret = list(conditionalPanel("input.toggle_inference",
                                    conditionalPanel("input.check_linear||
                                                     input.check_quadratic||
                                                     input.check_cubic||
                                                     input.check_smoother",
                                                     add_inference.check)))
        # vari1 = numeric; vari2 = factor or 
        # vari1 = factor; vari2 = numeric
      }else if(!input$vari2%in%"none"&&
               (((class(dafr[,input$vari1])%in%"numeric"|
                  class(dafr[,input$vari1])%in%"integer")&&
                 (class(dafr[,input$vari2])%in%"factor"|
                  class(dafr[,input$vari2])%in%"character"))||
                ((class(dafr[,input$vari1])%in%"factor"|
                  class(dafr[,input$vari1])%in%"character")&&
                 (class(dafr[,input$vari2])%in%"numeric"|
                  class(dafr[,input$vari2])%in%"integer")))){
        ret = list(conditionalPanel("input.toggle_inference",
                                    mean_median.radio,
                                    conditionalPanel("input.inference_parameter1=='Mean'",
                                                     normal_bootstrap.radio),
                                    conditionalPanel("input.inference_parameter1=='Median'",
                                                     year12_bootstrap.radio),
                                    conditionalPanel("input.inference_parameter1=='Mean'||
                                                     (input.inference_parameter1=='Median'&&
                                                     input.inference_type2=='Bootstrap')",
                                                     h5("Type of interval"),
                                                     confidence.interval.check,
                                                     comparison.interval.check))
        )
        # vari1 = factor; vari2 = factor or vari1 = factor; vari2 = none
      }else if((!input$vari2%in%"none"&&
                ((class(dafr[,input$vari1])%in%"factor"|
                  class(dafr[,input$vari1])%in%"character")&&
                 (class(dafr[,input$vari2])%in%"factor"|
                  class(dafr[,input$vari2])%in%"character")))||
               (input$vari2%in%"none"&&
                (class(dafr[,input$vari1])%in%"factor"|
                 class(dafr[,input$vari1])%in%"character"))){
        ret = list(conditionalPanel("input.toggle_inference",
                                    h5("Parameter"),helpText("Proportions"),
                                    normal_bootstrap.radio,
                                    h5("Type of interval"),
                                    confidence.interval.check,
                                    conditionalPanel("input.inference_type1=='Normal'",
                                                     comparison.interval.check)))
        # var1 = numeric; vari2 = none
      }else if((input$vari2%in%"none"&&
                (class(dafr[,input$vari1])%in%"numeric"|
                 class(dafr[,input$vari1])%in%"integer"))){
        ret = list(conditionalPanel("input.toggle_inference",
                                    mean_median.radio,
                                    conditionalPanel("input.inference_parameter1=='Mean'",
                                                     normal_bootstrap.radio),
                                    conditionalPanel("input.inference_parameter1=='Median'",
                                                     year12_bootstrap.radio),
                                    conditionalPanel("input.inference_parameter1=='Mean'||
                                                     (input.inference_parameter1=='Median'&&
                                                     input.inference_type2=='Bootstrap')",
                                                     h5("Type of interval"),
                                                     confidence.interval.check)))
      }
    }
  })
  ret
  })

output$old_advanced_options_panel = renderUI({
  get.data.set()
  ret = NULL
  isolate({
    temp = list()
    temp$x = get.data.set()[,input$vari1]
    if(input$vari2%in%'none'){
      temp$y = NULL
    }else{
      temp$y = get.data.set()[,input$vari2]
    }
    temp$plot = F
    temp = try(do.call(iNZightPlots:::iNZightPlot,temp))
    ##################################################################    
    #    large.sample = T
    large.sample = search.name(temp,"largesample")[[1]]
    if(is.null(large.sample)){
      large.sample=F
    }
    ##################################################################
    if((!is.null(input$vari1)&&
        !is.null(input$vari2))&&
       (input$vari1%in%colnames(get.data.set())&&
        (input$vari2%in%"none"|
         input$vari2%in%colnames(get.data.set()))))
      # vari = factor, vari = none
      if(input$vari2%in%"none"&&
         (class(get.data.set()[,input$vari1])%in%"factor"|
          class(get.data.set()[,input$vari1])%in%"character")){
        ret = selectInput(inputId = "advanced_options",
                          label = "Options",
                          choices = c('Code more variables',
                                      'Change plot appearance',
                                      'Customize labels',
                                      'Adjust number of Bars'),
                          selected = 'Change plot appearance')
        # vari1 = factor, vari2 = factor
      }else if(!input$vari2%in%"none"&&
               ((class(get.data.set()[,input$vari1])%in%"factor"|
                 class(get.data.set()[,input$vari1])%in%"character")&&
                (class(get.data.set()[,input$vari2])%in%"factor"|
                 class(get.data.set()[,input$vari2])%in%"character"))){
        ret = selectInput(inputId = "advanced_options",
                          label = "Options",
                          choices = c('Change plot appearance',
                                      'Customize labels',
                                      'Adjust number of Bars'),
                          selected = 'Change plot appearance')
        # vari1 = numeric , vari2 = none or
        # vari1 = numeric , vari2 = factor or
        # vari1 = factor , vari2 = numeric
      }else if((input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"numeric"|
                 class(get.data.set()[,input$vari1])%in%"integer"))||
               (!input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"factor"|
                 class(get.data.set()[,input$vari1])%in%"character")&&
                (class(get.data.set()[,input$vari2])%in%"integer"|
                 class(get.data.set()[,input$vari2])%in%"numeric"))||
               (!input$vari2%in%"none"&&
                (class(get.data.set()[,input$vari1])%in%"integer"|
                 class(get.data.set()[,input$vari1])%in%"numeric")&&
                (class(get.data.set()[,input$vari2])%in%"character"|
                 class(get.data.set()[,input$vari2])%in%"factor"))){
        ret = selectInput(inputId = "advanced_options",
                          label = "Options",
                          choices = c('Code more variables',
                                      'Change plot appearance',
                                      'Identify points',
                                      'Customize labels',
                                      'Adjust axis limits'),
                          selected = 'Change plot appearance')
        if(large.sample){
          ret = selectInput(inputId = "advanced_options",
                            label = "Options",
                            choices = c('Change plot appearance',
                                        'Customize labels',
                                        'Adjust axis limits'),
                            selected = 'Change plot appearance')
        }
        # vari1 = numeric , vari2 = numeric
      }else if(!input$vari2%in%"none"&&
               ((class(get.data.set()[,input$vari1])%in%"numeric"|
                 class(get.data.set()[,input$vari1])%in%"integer")&&
                (class(get.data.set()[,input$vari2])%in%"numeric"|
                 class(get.data.set()[,input$vari2])%in%"integer"))){
        ret = selectInput(inputId = "advanced_options",
                          label = "Options",
                          choices = c('Code more variables',
                                      'Add trend curves',
                                      'Add x=y line',
                                      'Add a jitter',
                                      'Add rugs',
                                      'Join points by line',
                                      'Change plot appearance',
                                      'Identify points',
                                      'Customize labels',
                                      'Adjust axis limits'),
                          selected = 'Change plot appearance')
        if(large.sample){
          ret = selectInput(inputId = "advanced_options",
                            label = "Options",
                            choices = c('Add trend curves',
                                        'Add x=y line',
                                        'Change plot appearance',
                                        'Customize labels',
                                        'Adjust axis limits'),
                            selected = 'Change plot appearance')
        }
      }
  })
  list(ret) 
})



## switch variables selected
observe({
  input$switch1
  isolate({
    if(!is.null(input$vari2) && input$vari2 != "none") {
      
      var1.old = input$vari1
      var2.old = input$vari2
      
      updateSelectInput(session, "vari1", selected = var2.old)
      
      ch  = colnames(vis.data())
#      if(!is.null(input$vari1) && input$vari1 %in% ch){
        ch  = ch[-which(ch %in% var2.old)]
#      }
      ch = c("none", ch)

      updateSelectInput(session,"vari2", choices = ch, selected = var1.old)
    }
  })
})

observe({
  input$switch2
  isolate({
    if((!is.null(input$vari2) && input$vari2 != "none") ||
       (!is.null(input$subs1) && input$subs1 != "none")) {
      var2.old = input$vari2
      var3.old = input$subs1
      
      updateSelectInput(session, "vari2", selected = var3.old)
      
      ch  = colnames(vis.data())
      ch  = ch[-which(ch %in% input$vari1)]
      if(!is.null(var3.old) && var3.old != "none")
        ch  = ch[-which(ch %in% var3.old)]
      updateSelectInput(session, "subs1", choices = ch, selected = var2.old)
    }
  })
})

observe({
  input$switch3
  isolate({
    var3.old = input$subs1
    var4.old = input$subs2
    
    updateSelectInput(session, "subs1", selected = var4.old)
    updateSelectInput(session, "subs2", selected = var3.old)
  })
})















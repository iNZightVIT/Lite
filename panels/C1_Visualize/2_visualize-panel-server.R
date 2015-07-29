###-----------------------------------------------###
###  Server Functions for the "Visualize" Module  ###
###-----------------------------------------------###
###
###  Date Created   :   February 1, 2015
###  Last Modified  :   March 18, 2015
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

###  Then on the second day, he said let there be parameters for
###  iNZightPlot():
plot.par <- reactiveValues(
  x = NULL,
  y = NULL,
  varnames = list(x = NULL, y = NULL,
      xlab = NULL, ylab = NULL,
      g1 = NULL, g2 = NULL,
      colby=NULL,sizeby=NULL),
  g1 = NULL,
  g2 = NULL,
  g1.level = 0,
  g2.level = 0,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  colby=NULL,
  sizeby=NULL,
  data=NULL
  #largesample = FALSE
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
  bg = "white", #background colour
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
  pch = 1, # fill colour of points
  col.pt = "gray50",
#   fill.pt = "transparent",
  ##  Colours
  LOE = FALSE,
  col.LOE = "black",
  col.trend = 
  list(linear = "",
       quadratic = "",
       cubic =  ""),
  col.smooth = "",
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
  smooth = 0,
  szsym = 1,
  tpsym = 1,
  plottype="default",
  hist.bins=NULL,
  scatter.grid.bins=50,
  hex.bins=20,
  bs.inference=F
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
    if (input != "none" && input %in% names(vis.data())) {
        if (subs) {
            input.out = convert.to.factor(vis.data()[, input])
            factor.levels = nlevels(input.out)
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
            vis.par = modifyList(vis.par, temp)
        }
        vis.par = modifyList(reactiveValuesToList(graphical.par), vis.par)
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
    selectInput(inputId = "vari1",
                label = "Select first variable:",
                choices = colnames(vis.data()),
                selected = plot.par$varnames$x)
})

##  Update plot.par$x.
observe({
  if(!is.null(input$vari1)){
    vari1.par = handle.input(input$vari1)$input.out
    isolate({
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
  isolate({
    ch  = colnames(vis.data())
    if(!is.null(input$vari1)){
      ch  = ch[-which(ch%in%input$vari1)]
    }
    if(!is.null(input$vari2)&&input$vari2%in%ch){
      ch  = ch[-which(ch%in%input$vari2)]
    }
    if(!is.null(input$subs2)&&input$subs2%in%ch){
      ch  = ch[-which(ch%in%input$subs2)]
    }
  })
  selectInput(inputId = "subs1",
              label = "Subset by:",
              choices = c("none", ch),
              selected = plot.par$varnames$g1)
})

##  Update plot.par$g1.
observe({
  input$subs1
  isolate({
    subs1.par = handle.input(input$subs1, subs = TRUE)$input.out
    plot.par$g1 = subs1.par
    plot.par$varnames$g1 = input$subs1
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if(is.null(choices1)){
      choices1 = 1
    }
#     print(choices1)
#     print("--------")
    updateSliderInput(session,"sub1_level",
                      label = paste0("Subset '", input$subs1, "':"),
                      min = 0, max = choices1, value = 0,step=1)
  })
})

##  Update plot.par$g1.
observe({
  input$subs1
  isolate({
    subs1.par = handle.input(input$subs1, subs = TRUE)$input.out
    plot.par$g1 = subs1.par
    plot.par$varnames$g1 = input$subs1
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
  isolate({
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if (is.null(choices1)){
      choices1 = 1
    }
    sliderInput(inputId = "sub1_level",
                label = paste0("Subset '", input$subs1, "':"),
                min = 0, max = choices1, value = 0, step = 1,
                animate = TRUE,ticks=F)
  })
})

#  Subset level (Slider) for variable 1 (mini plot).
output$subs1_conditional_mini = renderUI({
  isolate({
    choices1 = handle.input(input$subs1, subs = TRUE)$factor.levels
    if (is.null(choices1)){
      choices1 = 1
    }
    sliderInput(inputId = "sub1_level_mini",
                label = paste0("Subset '", input$subs1, "':"),
                min = 0, max = choices1, value = 0, step = 1,
                animate = TRUE,ticks=F)
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
  })
})

observe({
  g1_level = input$sub1_level_mini
  isolate({
    if (is.null(g1_level) || g1_level == 0) {
      g1_level = NULL
    }
    plot.par$g1.level = g1_level
  })
})

# #  Clean up slider every time the subset variables change.
# observe({
#   input$subs1
#   isolate({
#     plot.par$g1.level = NULL
#     updateSliderInput(session,
#                       inputId = "sub1_level",
#                       value = 0)
#   })
# })

##  Variable 2  ##
##
##  Select variable 2.
output$vari2_panel = renderUI({
  isolate({
    ch = colnames(vis.data())[-which(colnames(vis.data())%in%input$vari1)]
    selectInput(inputId = "vari2",
                label = "Select second variable:",
                choices = c("none", ch),
                selected = plot.par$varnames$y)
  })
})

##  Update plot.par$y
observe({
    vari2.par = handle.input(input$vari2)$input.out
    isolate({
      if(!is.null(vis.data())){
        plot.par$y = vari2.par
        plot.par$varnames$y = input$vari2
        ch  = colnames(vis.data())
        if(!is.null(input$vari1)){
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
  isolate({
    ch = colnames(vis.data())
    if(!is.null(input$vari1)){
      ch  = ch[-which(ch%in%input$vari1)]
    }
    if(!is.null(input$vari2)&&input$vari2%in%ch){
      ch  = ch[-which(ch%in%input$vari2)]
    }
  })
  selectInput(inputId = "subs2",
              label = "Subset by:",
              choices = c("none", ch),
              selected = plot.par$varnames$g2)
})


##  Update plot.par$g2.
observe({
  subs2.par = handle.input(input$subs2, subs = TRUE)$input.out
  isolate({
    plot.par$g2 = subs2.par
    plot.par$varnames$g2 = input$subs2
    ch = colnames(vis.data())
    if(!is.null(input$vari1)){
      ch  = ch[-which(ch%in%input$vari1)]
    }
    if(!is.null(input$vari2)&&input$vari2%in%ch){
      ch  = ch[-which(ch%in%input$vari2)]
    }
#     print(ch)
    updateSelectInput(session,"subs1",choices=ch,selected=input$subs1)
  })
})

##  Subset level (Slider) for variable 2.
output$subs2_conditional = renderUI({
    choices2 = handle.input(input$subs2, subs = TRUE)$factor.levels
    if (is.null(choices2))
        choices2 = 2
    else
        choices2 = choices2 + 1
    sliderInput(inputId = "sub2_level",
                label = paste0("Subset '", input$subs2, "':"),
                min = 0, max = choices2, value = 0, step = 1,
                animate = TRUE,ticks=F)
})

##  Subset level (Slider) for variable 2.
output$subs2_conditional_mini = renderUI({
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
    if (is.null(g2_level) || g2_level == 0) {
        g2_level = NULL
    }
    g2.level.check = handle.input(input$subs2, subs = TRUE)$factor.levels + 1
    if (!is.null(g2_level) && 
          length(g2.level.check) == 1 && 
          g2_level == g2.level.check) {
        g2_level = "_MULTI"
    }
    plot.par$g2.level = g2_level
})

observe({
  g2_level = input$sub2_level_mini
  if (is.null(g2_level) || g2_level == 0) {
    g2_level = NULL
  }
  g2.level.check = handle.input(input$subs2, subs = TRUE)$factor.levels + 1
  if (!is.null(g2_level) && 
        length(g2.level.check) == 1 && 
        g2_level == g2.level.check) {
    g2_level = "_MULTI"
  }
  plot.par$g2.level = g2_level
})


# ##  Clean up slider every time the subset variables change.
# observe({
#     input$subs2
#     isolate({
#       plot.par$g2.level = NULL
#       updateSliderInput(session,
#                         inputId = "sub2_level",
#                         value = 0)
#     })
# })

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
    if(is.numeric(plot.par$x)&
         is.numeric(plot.par$y)){
      temp = vis.par()
      temp.x = temp$x
      temp$x=temp$y
      temp$y=temp.x
      temp.varnames.x = temp$varnames$x
      temp$varnames$x = temp$varnames$y
      temp$varnames$y = temp.varnames.x
      plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
    }else{
      plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
    }
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
    if(is.numeric(plot.par$x)&
         is.numeric(plot.par$y)){
      temp = vis.par()
      temp.x = temp$x
      temp$x=temp$y
      temp$y=temp.x
      temp.varnames.x = temp$varnames$x
      temp$varnames$x = temp$varnames$y
      temp$varnames$y = temp.varnames.x
      plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,temp))
    }else{
      plot.ret.para$parameters = try(do.call(iNZightPlots:::iNZightPlot,vis.par()))
    }
  }
})

output$visualize.inference = renderPrint({
  if(input$plot_selector%in%"Inference"){
    input$vari1
    input$vari2
    input$subs1
    isolate({
      if (is.null(plot.par$x)) {
          return(cat("Please select a variable"))
      }
      values.list = modifyList(reactiveValuesToList(plot.par),
                                reactiveValuesToList(graphical.par))
      values.list <- modifyList(
        values.list,
        list(bs.inference = T,
             summary.type = "inference",
             inference.type = "conf",
             inference.par = NULL)
      )
      try(cat(do.call(iNZightPlots:::getPlotSummary, values.list), sep = "\n"))
    })
  }
})

output$visualize.summary = renderPrint({
    if (is.null(plot.par$x)) {
        return(cat("Please select a variable"))
    }
    values.list = modifyList(reactiveValuesToList(plot.par),
                              reactiveValuesToList(graphical.par))
    try(cat(do.call(getPlotSummary, values.list), sep = "\n"))
})

##  Reset variable selection and graphical parameters.
observe({
  if (!is.null(input$reset.graphics)&&input$reset.graphics > 0) {
    isolate({
      graphical.par$alpha = 1
      updateSliderInput(session,"adjust.transparency",
                        value=1)
      graphical.par$bg = "white" #background colour
      updateSelectInput(session,"select.bg1",
                        selected="white")
      ##  Box
      graphical.par$box.col = "black"
      graphical.par$box.fill = "white" # fill colour for the boxplot
      ##  Bar
      graphical.par$bar.lwd = 1
      graphical.par$bar.col = "black" # colour for borders of bars in bar plot
      graphical.par$bar.fill = colors()[81] # colour for inside of bars in bar plot
      updateSelectInput(session,"select.barcolor",
                        selected=colors()[81])
      ##  Line
      graphical.par$lwd = 1
      graphical.par$lty = 1
      graphical.par$lwd.pt = 2
      graphical.par$col.line = "blue"
      graphical.par$join = FALSE
      updateCheckboxInput(session,"check.join",value=F)
      updateSelectInput(session,"color.join",selected="blue")
      ##  Point
      graphical.par$cex.pt = 0.5
      updateSliderInput(session,"adjust.size.points.scatter",
                        value=0.5)
      graphical.par$cex.dotpt = 0.5
      updateSliderInput(session,"adjust.size.points.dot",
                        value=0.5)
      graphical.par$pch = 1
      updateCheckboxInput(session,"color.interior",
                          value=F)
      graphical.par$col.pt = "gray50"
      updateSelectInput(session,"select.dotcolor",
                        selected="gray50")
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
  #    graphical.par$largesample = NULL
      graphical.par$lines.by = FALSE
      graphical.par$trend.by = FALSE
      updateCheckboxInput(session,"each_level",value=F)
      graphical.par$trend.parallel = T
      updateCheckboxInput(session,"each_level_seperate",value=T)
      graphical.par$smooth = 0
      graphical.par$szsym = 1
      graphical.par$tpsym = 1
      graphical.par$plottype="default"
      updateSelectInput(session,"select.plot.type",
                        selected="default")
      graphical.par$hist.bins=get.default.num.bins()
      updateSliderInput(session,"adjust.num.bins",
                        value=get.default.num.bins())
      graphical.par$scatter.grid.bins=50
      updateSliderInput(session,"adjust.grid.size",
                        value=50)
      graphical.par$hex.bins=20
      updateSliderInput(session,"adjust.hex.bins",
                        value=20)
      graphical.par$bs.inference=F
      graphical.par$varnames = list(x = NULL, y = NULL,
                                    xlab = NULL, ylab = NULL,
                                    g1 = NULL, g2 = NULL,
                                    colby=NULL,sizeby=NULL)
      plot.par$main=NULL
      updateTextInput(session,"main_title_text",
                      value="")
      plot.par$xlab=NULL
      updateTextInput(session,"x_axis_text",
                      value="")
      plot.par$ylab=NULL
      updateTextInput(session,"y_axis_text",
                      value="")
      plot.par$colby=NULL
      updateSelectInput(session,"color_by_select",
                        selected="")
      plot.par$sizeby=NULL
      updateSelectInput(session,"resize.by.select",
                        selected="")
      
    })
  }
})

output$add_inference = renderUI({
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
    if(input$vari1%in%colnames(get.data.set)){
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
    }
    graphical.par$inference.type = intervals
    # vari1 = numeric; vari2 = none
#     print(input$toggle_inference)
    if(!input$vari2%in%"none"&&
         ((class(dafr[,input$vari1])%in%"numeric"|
          class(dafr[,input$vari1])%in%"integer")&
         (class(dafr[,input$vari2])%in%"numeric"|
            class(dafr[,input$vari2])%in%"integer"))){
      ret = list(fixedRow(column(12,offset=11,
                                 checkboxInput("toggle_inference",
                                               label="Show Inference",
                                               value=input$toggle_inference))),
                 conditionalPanel("input.toggle_inference",
                                  conditionalPanel("input.check_linear||
                                                   input.check_quadratic||
                                                   input.check_cubic||
                                                   input.check_smoother",
                                                   add_inference.check)))
    # vari1 = numeric; vari2 = factor or vari1 = factor; vari2 = numeric
    }else if(!input$vari2%in%"none"&&
               (((class(dafr[,input$vari1])%in%"numeric"|
                class(dafr[,input$vari1])%in%"integer")&
               (class(dafr[,input$vari2])%in%"factor"|
                  class(dafr[,input$vari2])%in%"character"))|
               ((class(dafr[,input$vari1])%in%"factor"|
                   class(dafr[,input$vari1])%in%"character")&
                  (class(dafr[,input$vari2])%in%"numeric"|
                     class(dafr[,input$vari2])%in%"integer")))){
      ret = list(fixedRow(column(12,offset=11,
                                 checkboxInput("toggle_inference",
                                               label="Show Inference",
                                               value=input$toggle_inference))),
                 conditionalPanel("input.toggle_inference",
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
                class(dafr[,input$vari1])%in%"character")&
               (class(dafr[,input$vari2])%in%"factor"|
                  class(dafr[,input$vari2])%in%"character")))|
               ((class(dafr[,input$vari1])%in%"factor"|
               class(dafr[,input$vari1])%in%"character")&
                 input$vari2%in%"none")){
      ret = list(fixedRow(column(12,offset=11,
                                 checkboxInput("toggle_inference",
                                               label="Show Inference",
                                               value=input$toggle_inference))),
                 conditionalPanel("input.toggle_inference",
                                  h5("Parameter"),helpText("Proportions"),
                                  normal_bootstrap.radio,
                                  h5("Type of interval"),
                                  confidence.interval.check,
                                  conditionalPanel("input.inference_type1=='Normal'",
                                                   comparison.interval.check)))
    # var1 = numeric; vari2 = none
    }else if(input$vari2%in%"none"&&
               (class(dafr[,input$vari1])%in%"numeric"|
               class(dafr[,input$vari1])%in%"integer")){
      ret = list(fixedRow(column(12,offset=11,
                                 checkboxInput("toggle_inference",
                                               label="Show Inference",
                                               value=input$toggle_inference))),
                 conditionalPanel("input.toggle_inference",
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
    if(!is.null(input$vari1)&&
         input$vari1%in%colnames(get.data.set())&&
         is.numeric(get.data.set()[,input$vari1])&&
         !is.null(input$vari2)&&
         !input$vari2%in%colnames(get.data.set())){
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
    }else if((!is.null(input$vari1)&&
                input$vari1%in%colnames(get.data.set())&&
                (is.character(get.data.set()[,input$vari1])|
                   is.factor(get.data.set()[,input$vari1]))&&
                !is.null(input$vari2)&&input$vari2%in%"none")||
               ((!is.null(input$vari1)&&
                   input$vari1%in%colnames(get.data.set())&&
                   (is.factor(get.data.set()[,input$vari1])|
                      is.character(get.data.set()[,input$vari1])))&&
                  (!is.null(input$vari2)&&
                     input$vari2%in%colnames(get.data.set())&&
                     (is.factor(get.data.set()[,input$vari2])|
                        is.character(get.data.set()[,input$vari2]))))
             ){
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
                input$vari1%in%colnames(get.data.set())&&
                is.numeric(get.data.set()[,input$vari1]))&&
               (!is.null(input$vari2)&&
                  input$vari2%in%colnames(get.data.set())&&
                  is.numeric(get.data.set()[,input$vari2]))){
      graphical.par$bs.inference = input$add.inference
    # vari1 = numeric; vari2 = factor or vari1 = factor; vari2 = numeric
    }else if(((!is.null(input$vari1)&&
                 input$vari1%in%colnames(get.data.set())&&
                 (is.factor(get.data.set()[,input$vari1])|
                    is.character(get.data.set()[,input$vari1])))&&
                (!is.null(input$vari2)&&
                   input$vari2%in%colnames(get.data.set())&&
                   (is.numeric(get.data.set()[,input$vari2]))))||
               ((!is.null(input$vari1)&&
                   input$vari1%in%colnames(get.data.set())&&
                   (is.numeric(get.data.set()[,input$vari1])))&&
                  (!is.null(input$vari2)&&
                     input$vari2%in%colnames(get.data.set())&&
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
          intervals = NULL
        }
      }
    }
    graphical.par$inference.type = intervals
  })
})

# advanced option panel
output$advanced_options_panel = renderUI({
  ret = NULL
  isolate({
    if((class(get.data.set()[,input$vari1])%in%"factor"|
          class(get.data.set()[,input$vari1])%in%"character")&
         input$vari2%in%"none"){
      ret = selectInput(inputId = "advanced_options",
                        label = "Options",
                        choices = c('Code more variables',
                                    'Change plot appearance',
                                    'Customize labels'),
                        selected = 'Change plot appearance')
    }else if((class(get.data.set()[,input$vari1])%in%"factor"|
                class(get.data.set()[,input$vari1])%in%"character")&
               !input$vari2%in%"none"&&
               (class(get.data.set()[,input$vari2])%in%"factor"|
                  class(get.data.set()[,input$vari2])%in%"character")){
      ret = selectInput(inputId = "advanced_options",
                        label = "Options",
                        choices = c('Change plot appearance',
                                    'Customize labels'),
                        selected = 'Change plot appearance')
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
                  (class(get.data.set()[,input$vari2])%in%"character"|
                     class(get.data.set()[,input$vari2])%in%"factor"))){
      ret = selectInput(inputId = "advanced_options",
                        label = "Options",
                        choices = c('Code more variables',
                                    'Change plot appearance',
                                    'Identify points',
                                    'Customize labels'),
                        selected = 'Change plot appearance')
    }else if((class(get.data.set()[,input$vari1])%in%"numeric"|
                class(get.data.set()[,input$vari1])%in%"integer")&
               !input$vari2%in%"none"&&
               (class(get.data.set()[,input$vari1])%in%"numeric"|
                  class(get.data.set()[,input$vari1])%in%"integer")){
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
                                    'Customize labels'),
                        selected = 'Change plot appearance')
    }
  })
  list(ret) 
})

output$plot.appearance.panel = renderUI({
  ret=NULL
  input$vari1
  input$vari2
  input$select.plot.type
  isolate({
    # barplot with one factor variable the other one not specified
    cols1 = colors()[c(1,3,16,19,63,87,109,259,
                       399,419,558,600,626,647)]
    cols2 = colors()[c(81,73,84,107,371,426,517,617)]
    cols3 = colors()[c(203,73,81,84,107,371,425,517,617)]
    select.bg.object = selectInput(inputId="select.bg1",label="Select Background colour:",
                                   choices=cols1,
                                   selected=graphical.par$bg)
    select.barcolor.object = selectInput(inputId="select.barcolor",label="Bar Colour:",
                                         choices=cols2,
                                         selected=graphical.par$bar.fill)
    select.dotcolor.object = selectInput(inputId="select.dotcolor",label="Colour:",
                                         choices=cols3,
                                         selected=graphical.par$col.pt)
    select.plot.type.object = NULL
    color.interior = checkboxInput(inputId="color.interior",label = "Colour interior",
                                   value=graphical.par$pch==19)
    if(is.null(graphical.par$cex.dotpt)){
      graphical.par$cex.dotpt = 0.5
    }
    adjust.size.points.dot.object = sliderInput("adjust.size.points.dot", label = "Adjust size:", min = 0.05, 
                max = 3.5, value=graphical.par$cex.dotpt,step=.05)
    adjust.size.points.scatter.object = sliderInput("adjust.size.points.scatter", label = "Adjust size:", min = 0.05, 
                                                max = 3.5, value=graphical.par$cex.dotpt,step=.05)
    adjust.grid.size.object = sliderInput("adjust.grid.size", label = "Grid size (n x n):", min = 10, 
                                                    max = 250, value=graphical.par$scatter.grid.bins,step=1)
    adjust.min.count.grid.object = sliderInput("adjust.min.count.grid", label = "Min-count colour (% grey):", min = 0, 
                                          max = 1, value=graphical.par$alpha,step=0.01)
    if(is.null(graphical.par$alpha)){
      graphical.par$alpha = 1
    }
    adjust.transparency.object = sliderInput("adjust.transparency", label = "Transparency:", min = 0, 
                                            max = 1, value=graphical.par$alpha,step=.01)
    if(is.null(graphical.par$hex.bins)){
      graphical.par$hex.bins = 20
    }
    adjust.hex.bins.object = sliderInput("adjust.hex.bins", label = "Hex grid size:", min = 5, 
                                             max = 70, value=graphical.par$hex.bins,step=1)
    
    adjust.num.bins.object = NULL
    # bar plot with one factor variable
    if((class(get.data.set()[,input$vari1])%in%"factor"|
          class(get.data.set()[,input$vari1])%in%"character")&
         input$vari2%in%"none"){
      ret = list(h5("Change plot appearance"),
                 select.bg.object,
                 select.barcolor.object
      )
    # bar plot with two factor variables
    }else if((class(get.data.set()[,input$vari1])%in%"factor"|
              class(get.data.set()[,input$vari1])%in%"character")&
               !input$vari2%in%"none"&&
             (class(get.data.set()[,input$vari2])%in%"factor"|
                class(get.data.set()[,input$vari2])%in%"character")){
      select.bg.object = selectInput(inputId="select.bg1",label="Select Background colour:",
                                     choices=cols1,
                                     selected=graphical.par$bg)
      ret = list(h5("Change plot appearance"),
                 select.bg.object
      )
    # dotplot or histogram for one numeric variable
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
                  (class(get.data.set()[,input$vari2])%in%"character"|
                     class(get.data.set()[,input$vari2])%in%"factor"))){
      select.plot.type.object = selectInput(inputId = "select.plot.type",
                                            label = "Plot type:",
                                            choices=c("default",
                                                      "dot plot",
                                                      "histogram"),
                                            selected=input$select.plot.type)
      ret = list(h5("Change plot appearance"),
                 select.plot.type.object,
                 select.bg.object,
                 select.dotcolor.object,
                 color.interior,
                 adjust.size.points.dot.object,
                 adjust.transparency.object
                 )
      if(!is.null(input$select.plot.type)&&
           input$select.plot.type%in%"histogram"){
        # to be implemented
        isolate({
          temp = vis.par()
        })
        temp$plot = F
        if(is.null(get.nbins())){
          default.nbins = try(do.call(iNZightPlots:::iNZightPlot,temp))
          nbins = attr(default.nbins,"nbins")
          if(is.null(get.default.num.bins())){
            plot.ret.para$default.num.bins = nbins
          }
        }else{
          nbins = get.nbins()
        }
        m = length(unique(get.data.set()[,input$vari1]))
        if(input$vari2%in%colnames(get.data.set())){
          m = max(c(length(unique(get.data.set()[,input$vari1])),
                    length(unique(get.data.set()[,input$vari2]))))
        }
        if(m<nbins){
          m=nbins
        }
        adjust.num.bins.object = sliderInput("adjust.num.bins", label = "Number of bars:", min = 1, 
                                             max = m, value=nbins,step=1)
        ret=list(h5("Change plot appearance"),
                 select.plot.type.object,
                 select.bg.object,
                 select.barcolor.object,
                 adjust.num.bins.object)
      }
    }else if((class(get.data.set()[,input$vari1])%in%"numeric"|
                class(get.data.set()[,input$vari1])%in%"integer")&
               !input$vari2%in%"none"&&
               (class(get.data.set()[,input$vari1])%in%"numeric"|
                  class(get.data.set()[,input$vari1])%in%"integer")){
      select.plot.type.object = selectInput(inputId = "select.plot.type",
                                            label = "Plot type:",
                                            choices=c("default",
                                                      "scatter plot",
                                                      "grid-density plot",
                                                      "hexbin plot"),
                                            selected=input$select.plot.type)
      ret = list(h5("Change plot appearance"),
                 select.plot.type.object,
                 select.bg.object,
                 select.dotcolor.object,
                 color.interior,
                 adjust.size.points.scatter.object,
                 adjust.transparency.object)
      if(!is.null(input$select.plot.type)&&
           input$select.plot.type%in%"grid-density plot"){
        ret = list(h5("Change plot appearance"),
                   select.plot.type.object,
                   select.bg.object,
                   adjust.grid.size.object,
                   adjust.min.count.grid.object)
      }else if(!is.null(input$select.plot.type)&&
                 input$select.plot.type%in%"hexbin plot"){
        ret = list(h5("Change plot appearance"),
                   select.plot.type.object,
                   select.bg.object,
                   adjust.hex.bins.object)
      }
    }
  })
  ret
})

# observe the plot type
observe({
  input$select.plot.type
  if(!is.null(input$vari1)&!is.null(input$vari2)){
    isolate({
      if(input$vari1%in%colnames(get.data.set())){
        if(!is.null(input$advanced_options)){
          sel = input$advanced_options
          ch = NULL
          if((class(get.data.set()[,input$vari1])%in%"factor"|
                class(get.data.set()[,input$vari1])%in%"character")&
               input$vari2%in%"none"){
            ch = c('Code more variables',
                   'Change plot appearance',
                   'Customize labels')
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
          }else if((class(get.data.set()[,input$vari1])%in%"factor"|
                      class(get.data.set()[,input$vari1])%in%"character")&
                     !input$vari2%in%"none"&&
                     (class(get.data.set()[,input$vari2])%in%"factor"|
                        class(get.data.set()[,input$vari2])%in%"character")){
            ch = c('Change plot appearance',
                   'Customize labels')
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
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
                   'Customize labels')
            if(!is.null(input$select.plot.type)&&
                 input$select.plot.type%in%"histogram"&
                 input$advanced_options%in%'Change plot appearance'){
              ch = c('Change plot appearance',
                     'Customize labels')
            }
            if(!sel%in%ch){
              sel = 'Change plot appearance'
            }
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
                  'Customize labels')
            if(!is.null(input$select.plot.type)&&
                 (input$select.plot.type%in%"grid-density plot"|
                    input$select.plot.type%in%"hexbin plot")&
                 input$advanced_options%in%'Change plot appearance'){
              ch = c('Add trend curves',
                     'Add x=y line',
                     'Change plot appearance',
                     'Customize labels')
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
      }else if(input$select.plot.type%in%"hexbin plot"){
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
          graphical.par$pch = 19
        }else{
          graphical.par$pch = 1
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
    if(!is.null(input$adjust.transparency)){
      if(input$adjust.transparency==1){
        graphical.par$pch=1
      }else{
        graphical.par$pch=19
      }
    }
    graphical.par$alpha = input$adjust.transparency
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
    graphical.par$alpha = input$adjust.min.count.grid
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
  input$vari1
  input$vari2
  isolate({
    plot.par$xlab = NULL
    plot.par$varnames$xlab = NULL
    plot.par$ylab = NULL
    plot.par$varnames$ylab = NULL
    plot.par$main = NULL
    title.pane=h4("Customize labels")
    main_title_text.object = textInput(inputId="main_title_text",label="Main title:")
    x_axis_text.object = textInput(inputId="x_axis_text",label="X-axis label:")
    y_axis_text.object = textInput(inputId="y_axis_text",label="Y-axis label:")
    change.labels.button.object = actionButton(inputId="change.labels.button",label="Submit")
    if(!is.null(vis.data())&&!is.null(input$vari1)&&!is.null(input$vari2)){
      if((class(vis.data()[,input$vari1])%in%"numeric"|
            class(vis.data()[,input$vari1])%in%"integer")&
           !is.null(input$vari2)&&!input$vari2%in%"none"&&
           (class(vis.data()[,input$vari2])%in%"numeric"|
           class(vis.data()[,input$vari2])%in%"integer")){
        list(title.pane,
             main_title_text.object,
             x_axis_text.object,
             y_axis_text.object,
             change.labels.button.object)
      }else{
        list(title.pane,
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
  input$vari1
  input$vari2
  isolate({
    title.pane = h4("Code More Variables")
    color.by.object  = NULL
    if((class(vis.data()[,input$vari1])%in%"factor"|
          class(vis.data()[,input$vari1])%in%"character")&
         (is.null(input$vari2)|input$vari2%in%"none")){
      color.by.object = selectInput("color_by_select",
                                    label="Colour by levels of:",
                                    choices=c(" ",get.categorical.column.names(vis.data())),
                                    selected=input$color_by_select)
    }else{
      color.by.object = selectInput("color_by_select",
                                    label="Colour by levels of:",
                                    choices=c(" ",colnames(vis.data())),
                                    selected=input$color_by_select)
    }
    resize.by.object = NULL
    if((class(vis.data()[,input$vari1])%in%"numeric"|
          class(vis.data()[,input$vari1])%in%"integer")&
         (!is.null(input$vari2)&!input$vari2%in%"none")&&
         (class(vis.data()[,input$vari2])%in%"numeric"|
            class(vis.data()[,input$vari2])%in%"integer")){
      resize.by.object = selectInput("resize.by.select",
                                     label="Resize points proportional to:",
                                     choices=c(" ",get.numeric.column.names(vis.data())),
                                     selected=input$resize.by.select)
    }
    list(title.pane,
         color.by.object,
         resize.by.object)
  })
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
      plot.par$colby = vis.data()[,input$color_by_select]
      plot.par$varnames$colby = input$color_by_select
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
      plot.par$sizeby = vis.data()[,input$resize.by.select]
      plot.par$varnames$sizeby = input$resize.by.select
    }
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
  isolate({
    title.add.trend.curve = h4("Add trend curves")
    check.linear.object = checkboxInput("check_linear",label="linear",value = F)
    check.quadratic.object = checkboxInput("check_quadratic",label="quadratic",value = F)
    check.cubic.object = checkboxInput("check_cubic",label="cubic",value = F)
    check.smoother.object = checkboxInput("check_smoother",label="Draw a smoother",value = F)
    check.quantiles.object = checkboxInput("check_quantiles",label="Use Quantiles",value = F)
    color.linear.select = selectInput("color.linear",label="",
                                      choices=c("red","black","blue",
                                                "green4","yellow","pink",
                                                "grey","orange"),
                                      selected="blue")
    color.quadratic.select = selectInput("color.quadratic",label="",
                                      choices=c("red","black","blue",
                                                "green4","yellow","pink",
                                                "grey","orange"),
                                      selected="red")
    color.cubic.select = selectInput("color.cubic",label="",
                                         choices=c("red","black","blue",
                                                   "green4","yellow","pink",
                                                   "grey","orange"),
                                         selected="green4")
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
    list(title.add.trend.curve,
         fixedRow(column(width=6,check.linear.object),
                  column(width=6,color.linear.select)),
         fixedRow(column(width=6,check.quadratic.object),
                  column(width=6,color.quadratic.select)),
         fixedRow(column(width=6,check.cubic.object),
                  column(width=6,color.cubic.select)),
         fixedRow(column(width=6,check.smoother.object),
                  column(width=6,color.smoother.select)),
         conditionalPanel("input.check_smoother",
                          fixedRow(width=12,check.quantiles.object),
                          fixedRow(width=12,smoother.smooth.slider)),
         conditionalPanel("input.color_by_select!=null&&input.color_by_select!=''&&
                          (input.check_linear||input.check_quadratic||
                          input.check_cubic||input.check_smoother)&&
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
      graphical.par$quant.smooth = "default"
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
  isolate({
    if(!is.null(input$check_linear)&&input$check_linear){
      if(length(which(graphical.par$trend%in%"linear"))==0){
        graphical.par$trend=c(graphical.par$trend,"linear")
      }
      graphical.par$col.trend[["linear"]] = input$color.linear
    }else{
      if(length(which(graphical.par$trend%in%"linear"))>0){
        graphical.par$trend=graphical.par$trend[-which(graphical.par$trend%in%"linear")]
        if(length(graphical.par$trend)==0){
          graphical.par$trend=NULL
        }
      }
    }
  })
})

# observe quadratic trend
observe({
  input$check_quadratic
  input$color.quadratic
  isolate({
    if(!is.null(input$check_quadratic)&&input$check_quadratic){
      if(length(which(graphical.par$trend%in%"quadratic"))==0){
        graphical.par$trend=c(graphical.par$trend,"quadratic")
      }
      graphical.par$col.trend[["quadratic"]] = input$color.quadratic
    }else{
      if(length(which(graphical.par$trend%in%"quadratic"))>0){
        graphical.par$trend=graphical.par$trend[-which(graphical.par$trend%in%"quadratic")]
        if(length(graphical.par$trend)==0){
          graphical.par$trend=NULL
        }
      }
    }
  })
})

# observe cubic trend
observe({
  input$check_cubic
  input$color.cubic
  isolate({
    if(!is.null(input$check_cubic)&&input$check_cubic){
      if(length(which(graphical.par$trend%in%"cubic"))==0){
        graphical.par$trend=c(graphical.par$trend,"cubic")
      }
      graphical.par$col.trend[["cubic"]] = input$color.cubic
    }else{
      if(length(which(graphical.par$trend%in%"cubic"))>0){
        graphical.par$trend=graphical.par$trend[-which(graphical.par$trend%in%"cubic")]
        if(length(graphical.par$trend)==0){
          graphical.par$trend=NULL
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
  titel.xyline = h4("Add x=y line")
  check.xyline.object = checkboxInput("check.xyline",
                                      label="Plot x=y line",
                                      value=F)
  color.xyline.select = selectInput("color.xyline",label="",
                                    choices=c("red","black","blue",
                                              "green4","yellow","pink",
                                              "grey","orange"),
                                    selected="black")
  list(titel.xyline,
       fixedRow(column(width=6,check.xyline.object),
                column(width=6,color.xyline.select)))
})

# check for changes in color or whether the x=y-line is drawn
observe({
  input$check.xyline
  input$color.xyline
  if(!is.null(input$check.xyline)&&
       input$check.xyline){
    graphical.par$LOE = T
    graphical.par$col.LOE = input$color.xyline
  }
})

# add jitter to the plot
output$add.jitter.panel = renderUI({
  title.jitter = h4("Add a jitter")
  check.jitter.x.object = checkboxInput("check.jitter.x",
                                        label="Jitter x-variable",
                                        value=F)
  check.jitter.y.object = checkboxInput("check.jitter.y",
                                        label="Jitter y-variable",
                                        value=F)
  list(title.jitter,
       fixedRow(column(width=6,check.jitter.x.object),
                column(width=6,check.jitter.y.object)))
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
  title.rugs = h4("Add rugs")
  check.rugs.x.object = checkboxInput("check.rugs.x",
                                        label="Add x-rugs",
                                        value=F)
  check.rugs.y.object = checkboxInput("check.rugs.y",
                                        label="Add y-rugs",
                                        value=F)
  list(title.rugs,
       fixedRow(column(width=6,check.rugs.x.object),
                column(width=6,check.rugs.y.object)))
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
  title.join = h4("Join points by line")
  check.join.object = checkboxInput("check.join",
                                      label="Join points",
                                      value=F)
  color.join.select = selectInput("color.join",label="",
                                    choices=c("red","black","blue",
                                              "green4","yellow","pink",
                                              "grey","orange"),
                                    selected="blue")
  list(title.join,
       fixedRow(column(width=6,check.join.object),
                column(width=6,color.join.select)))
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

output$points.identify.panel = renderUI({
  input$vari1
  input$vari2
  isolate({
    identified.points$values=list()
    ret = list()
    ret[[1]] = fixedRow(column(3,
                               checkboxInput("label_observation_check",
                                             label="Label observations",
                                             value=F)),
                        column(5,
                               offset=1,
                               conditionalPanel("input.label_observation_check",
                               selectInput("label.select",
                                           label="Select column",
                                           choices=c("id",
                                                     colnames(get.data.set()))))))
                               
    ret[[2]] = fixedRow(column(3,
                               checkboxInput("color_points_check",
                                             label="Colour observations",
                                             value=F)),
                        column(5,offset=1,
                               conditionalPanel("input.color_points_check",
                               selectInput("color.select",
                                           label="Select Colour",
                                           choices=c("red",
                                                     "blue",
                                                     "green4")))))
    ret[[3]] = fixedRow(column(3,
                               checkboxInput("same_level_of_check",
                                             label="Identify Observations per level",
                                             value=F)),
                        column(5,offset=1,
                               conditionalPanel("input.same_level_of_check",
                                                selectInput("same.level.of.select",
                                                            label="Select Column",
                                                            choices=colnames(get.data.set())))))
    ret[[4]] = radioButtons("select_identify_method",
                            label = "Select method of selection",
                            choices = c("Select by value",
                                        "Extremes",
                                        "Range of values"))
    if(!is.null(input$vari1)&&!is.null(input$vari2)){
      if(input$vari1%in%colnames(get.data.set())&&
           input$vari2%in%colnames(get.data.set())){
        if(is.numeric(get.data.set()[,input$vari1])&&
             is.numeric(get.data.set()[,input$vari2])){
          ch = ""
          if(!is.null(input$by.value.column.select)){
            ch = get.data.set()[,input$by.value.column.select]
#             names(ch) = as.character(1:length(ch))
          }
          ret[[5]] = conditionalPanel("input.select_identify_method=='Select by value'&&
                                      (input.label_observation_check||input.color_points_check)",
                                      checkboxInput("single_vs_multiple_check",
                                                    label="Single value",
                                                    value=F),
                                      conditionalPanel("!input.single_vs_multiple_check",
                                                       fixedRow(column(6,
                                                                       selectInput("by.value.column.select",
                                                                                   label="Select a column",
                                                                                   choices=colnames(get.data.set()))),
                                                                column(6,
                                                                       selectInput("value.select",
                                                                                   label="Select multiple values",
                                                                                   choices=ch,
                                                                                   multiple=T,
                                                                                   selectize=F,
                                                                                   size=8)))),
                                      conditionalPanel("input.single_vs_multiple_check",
                                                       fixedRow(column(8,
                                                                       sliderInput("select.unique.value.slider",
                                                                                   label="Select single value",
                                                                                   min=1,
                                                                                   max=2,
                                                                                   value=0,
                                                                                   step=1,
                                                                                   ticks=F)),
                                                                column(4,
                                                                       numericInput("specify.correct.numeric",
                                                                                    label="",
                                                                                    value=1,
                                                                                    min=1,
                                                                                    max=2,
                                                                                    step=1)))))
          ret[[6]] = conditionalPanel("input.select_identify_method=='Extremes'&&
                                      (input.label_observation_check||input.color_points_check)")
          ret[[7]] = conditionalPanel("input.select_identify_method=='Range of values'&&
                                      (input.label_observation_check||input.color_points_check)")
        }else if((is.factor(get.data.set()[,input$vari1])&&
                   is.numeric(get.data.set()[,input$vari2]))||
                   is.numeric(get.data.set()[,input$vari1])&&
                   is.factor(get.data.set()[,input$vari2])){
          ret[[5]] = conditionalPanel("input.select_identify_method=='Select by value'&&
                                      (input.label_observation_check||input.color_points_check)")
          ret[[6]] = conditionalPanel("input.select_identify_method=='Extremes'&&
                                      (input.label_observation_check||input.color_points_check)")
          ret[[7]] = conditionalPanel("input.select_identify_method=='Range of values'&&
                                      (input.label_observation_check||input.color_points_check)")
        }
        ret[[8]] = fixedRow(column(5,offset=1,
                                   actionButton("store.obs.button",
                                                label="Store")),
                            column(5,offset=1,
                                   actionButton("reset.obs.button",
                                                label="Reset")))
      }
    }
    ret
  })
})

observe({
  if(!is.null(input$same_level_of_check)&!is.null(input$by.value.column.select)){
    namesCH = as.character(get.data.set()[,input$by.value.column.select])
    ch = 1:length(namesCH)
#     print(length(namesCH))
#     print(ch)
    names(ch) = namesCH
#     print(ch)
#     print(is.numeric(get.data.set()[,input$by.value.column.select]))
#     print(sort(get.data.set()[,input$by.value.column.select]))
    if(is.numeric(get.data.set()[,input$by.value.column.select])){
      ch = ch[as.character(sort(get.data.set()[,input$by.value.column.select]))]
    }
    updateSelectInput(session,"value.select",
                      choices=ch)
    if(input$same_level_of_check){
      updateSliderInput(session,"select.unique.value.slider",
                        min=1,
                        step=1,
                        max=length(unique(get.data.set()[,input$by.value.column.select])))
      updateNumericInput(session,"specify.correct.numeric",
                         min=1,
                         step=1,
                         max=length(unique(get.data.set()[,input$by.value.column.select])))
    }else{
      updateSliderInput(session,"select.unique.value.slider",
                        min=1,
                        step=1,
                        max=length(get.data.set()[,input$by.value.column.select]))
      updateNumericInput(session,"specify.correct.numeric",
                         min=1,
                         step=1,
                         max=length(get.data.set()[,input$by.value.column.select]))
    }
  }
})

observe({
  input$select.unique.value.slider
  isolate({
    updateNumericInput(session,"specify.correct.numeric",
                       value=input$select.unique.value.slider)
  })
})

observe({
  input$specify.correct.numeric
  isolate({
    updateNumericInput(session,"select.unique.value.slider",
                       value=input$specify.correct.numeric)
  })
})

observe({
#   print(input$value.select)
})
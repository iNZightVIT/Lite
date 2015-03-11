###-----------------------------------------------###
###  Server Functions for the "Visualize" Module  ###
###-----------------------------------------------###
###
###  Date Created   :   February 1, 2015
###  Last Modified  :   February 25, 2015
###
###  Please consult the comments before editing any code.
###
###  If you have any questions and/or suggestions, drop me an e-mail:
###  Chris Park <cpar137@aucklanduni.ac.nz>
###
###  * Note: This file is to be sourced locally within "server.R" * 

###  And on the first day of February, God said "Let there be data":
vis.data <- reactive({
    input$selector
    data
    vis.data <- data
})


###  Then on the second day, he said let there be parameters for
###  iNZightPlot():
plot.par <- reactiveValues(
    x = NULL,
    y = NULL,
    xlab = NULL,
    ylab = NULL,
    varnames = list(x = NULL, y = NULL,
        xlab = NULL, ylab = NULL,
        g1 = NULL, g2 = NULL,
        by = NULL, prop.size = NULL),
    g1 = NULL,
    g2 = NULL,
    g1.level = 0,
    g2.level = 0,
    by = NULL,
    prop.size = NULL,
    main = "",
    largesample = FALSE
)

##  Listen for clicks.
## observe({
##     if (is.null(input$click)) {
##         return()
##     }
##     isolate(plot.par$x <- c(plot.par$x, input$click$x))
##     isolate(plot.par$y <- c(plot.par$y, input$click$y))
## })


##  These are the list of parameters in inzPlotDefaults()
graphical.par <- reactiveValues(
    alpha = 1,
    bg = "white",
    ##  Box
    box.col = "gray60",
    box.fill = "lightskyblue3",
    ##  Bar
    bar.lwd = 1,
    bar.col = "gray60",
    bar.fill = "lightskyblue3",
    ##  Line
    lwd = 1,
    lty = 1,
    lwd.pt = 2,
    col.line = "blue",
    ##  Point
    cex.pt = .5,
    lwd.pt = .75,
    pch = 1,
    col.pt = "gray60",
    fill.pt = "gray85",
    ##  Colours
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
    largesample = NULL,
    LOE = FALSE,
    join = FALSE,
    lines.by = FALSE,
    trend.by = FALSE,
    smooth = 0,
    szsym = 1,
    tpsym = 1
)

##  Data handling
determine.class <- function(input) {
    if (is.null(input)) {
        return(NULL)
    }
    if (class(input) == "integer") {
        input.class <- "numeric"
    } else if (class(input) == "character") {
        input.class <- "factor"
    } else {
        input.class <- class(input)
    }
    input.class
}

##  Input Handling
handle.input <- function(input, subs = FALSE) {
    if (is.null(input)) {
        return()
    }
    if (input != "none" && input %in% names(vis.data())) {
        if (subs) {
            input.out <- convert.to.factor(vis.data()[, input])
            factor.levels <- nlevels(input.out)
        } else {
            input.out <- vis.data()[, input]
            factor.levels <- NULL
        }
    } else {
        input.out <- NULL
        factor.levels <- NULL
    }
    list(input.out = input.out, factor.levels = factor.levels)
}

x.class <- reactive({
    determine.class(plot.par$x)
})

y.class <- reactive({
    determine.class(plot.par$y)
})

determine.g <- reactive({
    xy.class <- c(x.class(), y.class())
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
    id <- !(xy.class %in% c("numeric", "factor"))
    if (id) {
        return(c("x of incorrect class", "y of incorrect class")[id])
    }
})

##  Then on the third, he declared the need for parameters for the "visualize" module:
vis.par <- reactive({
    vis.par <- reactiveValuesToList(plot.par)
    if (!is.null(vis.par$x) && plot.par$varnames$x != "") {
        validate(
            need(all(na.omit(vis.par$x) != ""),
                 "All values are empty.")
        )
        if (determine.g() == 6) {
            temp <- list(x = NULL, y = NULL,
                         varnames = list(x = "", y = ""))
            temp$x <- vis.par$x
            temp$y <- vis.par$y
            temp$varnames$x <- vis.par$varnames$x
            temp$varnames$y <- vis.par$varnames$y
            vis.par <- modifyList(vis.par, temp)
        }
        vis.par <- modifyList(reactiveValuesToList(graphical.par), vis.par)
    } else {
        NULL
    }
})

##  We write some UI outputs for variable selection and subsetting:
##
##  Variable 1
##
##  Select variable 1.
output$vari1_panel <- renderUI({
    selectInput(inputId = "vari1",
                label = "Select first variable:",
                choices = rev(colnames(vis.data())),
                selected = plot.par$varnames$x)
})

##  Update plot.par$x.
observe({
    vari1.par <- handle.input(input$vari1)$input.out
    plot.par$x <- vari1.par
    plot.par$varnames$x <- input$vari1
})


##  Subset variable 1.
output$subs1_panel <- renderUI({
    selectInput(inputId = "subs1",
                label = "Subset by:",
                choices = c("none", rev(colnames(vis.data()))),
                selected = plot.par$varnames$g1)
})

##  Update plot.par$g1.
observe({
    subs1.par <- handle.input(input$subs1, subs = TRUE)$input.out
    plot.par$g1 <- subs1.par
    plot.par$varnames$g1 <- input$subs1
})


##  Subset level (Slider) for variable 1.
output$subs1_conditional <- renderUI({
    choices1 <- handle.input(input$subs1, subs = TRUE)$factor.levels
    sliderInput(inputId = "sub1_level",
                label = paste0("Subset '", input$subs1, "':"),
                min = 0, max = choices1, value = 0, step = 1,
                animate = TRUE)
})


output$subs1_conditional_mini <- renderUI({
    choices1 <- handle.input(input$subs1, subs = TRUE)$factor.levels
    sliderInput(inputId = "sub1_level",
                label = paste0("Subset '", input$subs1, "':"),
                min = 0, max = choices1, value = 0, step = 1,
                animate = TRUE)
})


##  Update plot$g1.level
observe({
    g1_level <- input$sub1_level
    if (is.null(g1_level) || g1_level == 0) {
        g1_level <- NULL
    }
    plot.par$g1.level <- g1_level
})

##  Clean up slider every time the subset variables change.
observe({
    input$subs1
    plot.par$g1.level <- NULL
    updateSliderInput(session,
                      inputId = "subs1_level",
                      value = 0)
})



##  Variable 2  ##
##
##  Select variable 2.
output$vari2_panel <- renderUI({
    selectInput(inputId = "vari2",
                label = "Select second variable:",
                choices = c("none", rev(colnames(vis.data()))[-1]),
                selected = plot.par$varnames$y)
})

##  Update plot.par$y
observe({
    vari2.par <- handle.input(input$vari2)$input.out
    plot.par$y <- vari2.par
    plot.par$varnames$y <- input$vari2
})

##  Subset variable 2.
output$subs2_panel <- renderUI({
    selectInput(inputId = "subs2",
                label = "Subset by:",
                choices = c("none", rev(colnames(vis.data()))[-1]),
                selected = plot.par$varnames$g2)
})


##  Update plot.par$g2.
observe({
    subs2.par <- handle.input(input$subs2, subs = TRUE)$input.out
    plot.par$g2 <- subs2.par
    plot.par$varnames$g2 <- input$subs2
})

##  Subset level (Slider) for variable 2.
output$subs2_conditional <- renderUI({
    choices2 <- handle.input(input$subs2, subs = TRUE)$factor.levels + 1
    sliderInput(inputId = "sub2_level",
                label = paste0("Subset '", input$subs2, "':"),
                min = 0, max = choices2, value = 0, step = 1,
                animate = TRUE)
})

##  Subset level (Slider) for variable 2.
output$subs2_conditional_mini <- renderUI({
    choices2 <- handle.input(input$subs2, subs = TRUE)$factor.levels + 1
    sliderInput(inputId = "sub2_level",
                label = paste0("Subset '", input$subs2, "':"),
                min = 0, max = choices2, value = 0, step = 1,
                animate = TRUE)
})


##  Update plot.par$g2.level
observe({
    g2_level <- input$sub2_level
    if (is.null(g2_level) || g2_level == 0) {
        g2_level <- NULL
    }
    g2.level.check <- handle.input(input$subs2, subs = TRUE)$factor.levels + 1
    if (!is.null(g2_level) && g2_level == g2.level.check) {
        g2_level <- "_MULTI"
    }
    plot.par$g2.level <- g2_level
})


##  Clean up slider every time the subset variables change.
observe({
    input$subs2
    plot.par$g2.level <- NULL
    updateSliderInput(session,
                      inputId = "subs2_level",
                      value = 0)
})


output$visualize.plot <- renderPlot({
    if (!is.null(vis.par())) {
        do.call(iNZightPlots:::iNZightPlot, vis.par())
    }
})

output$mini.plot <- renderPlot({
    if (!is.null(vis.par())) {
        do.call(iNZightPlots:::iNZightPlot, vis.par())
    }
})


output$visualize.inference <- renderPrint({
    if (is.null(plot.par$x)) {
        return(cat("Please select a variable"))
    }
    values.list <- modifyList(reactiveValuesToList(plot.par),
                              reactiveValuesToList(graphical.par))
    ## if (determine.g() == 6) {
    cat(do.call(iNZightPlots:::getPlotInference, values.list), sep = "\n")
    ## } else {
    ##     cat("Meow")
    ## }
})

output$visualize.summary <- renderPrint({
    if (is.null(plot.par$x)) {
        return(cat("Please select a variable"))
    }
    values.list <- modifyList(reactiveValuesToList(plot.par),
                              reactiveValuesToList(graphical.par))
    cat(do.call(getPlotSummary, values.list), sep = "\n")
})

output$add.inference <- renderUI({
    ## if (!determine.g() %in% c(1, 4, 5, 6)) {
    ##     choices.option <- c("Proportions" = "proportion")
    ##     selected.option <- "proportion"
    ## }
    ## if (determine.g() %in% c(1, 4, 5)) {
    ##     choices.option <- c("Medians" = "median",
    ##                         "Means" = "mean")
    ##     selected.option <- "mean"
    ## }
    ## if (determine.g() == 6) {
    choices.option <- c("Medians" = "median",
                        "Means" = "mean",
                        "Proportions" = "proportion")
    selected.option = "mean"
    ## }
    radioButtons(inputId = "choose_parameter",
                 label = "Choose Paramater:",
                 choices = choices.option,
                 selected = selected.option)
    ## if (determine.g() != 6) {
    ##     radioButtons(inputId = "choose_interval",
    ##                  label = "Choose Interval:",
    ##                  choices =
    ##                      c("Comparison Intervals" = 1,
    ##                        "Confidence Intervals" = 2,
    ##                        "Both Intervals" = 3),
    ##                  selected = 3)
    ## radioButtons(inputId = "choose_method",
    ##              label = "Choose Method:",
    ##              choices =
    ##                  c("Bootstrap" = TRUE,
    ##                    "Normal" = FALSE),
    ##              selected = FALSE)
    ## }
})

##  Create a vector of colours.
colour.choices <- c(
    ## "Light Blue" = "lightskyblue3",
    "Blue" = "lightsteelblue1",
    "Purple" = "thistle2",
    "Orange" = "peachpuff1",
    "Green" = "darkseagreen1",
    "Gray" = "gray92",
    "Cream"  = "antiquewhite",
    "White" = "white"
)

dot.choices <- c(
    "Gray" = "gray60",
    "Blue 1" = "lightskyblue3",
    "Blue 2" = "darkblue",
    "Purple" = "magenta",
    "Orange" = "orange",
    "Green" = "darkseagreen"
)

##  Choose plot type
output$plot_type <- renderUI({
    selectInput(inputId = "choose_plot",
                label = "Graphical Object:",
                choices =
                    c("Bar" = 1,
                      "Dot" = 2,
                      "Box" = 3),
                selected = 1)
})


##  Adjust background colour: UI
output$background <- renderUI({
    selectInput(inputId = "choose_bg",
                label = "Background Colour:",
                choices = rev(colour.choices),
                selected = graphical.par$bg)
})

##  Adjust background colour: Update
observe({
    if (is.null(input$choose_bg) || input$choose_bg == "") {
        return()
    } else {
        bg.col <- input$choose_bg
        graphical.par$bg <- bg.col
    }
})


##  Adjust symbol colour: UI
output$symbol_colour <- renderUI({
    selectInput(inputId = "choose_symb_col",
                label = "Choose Dot Colour:",
                choices = dot.choices,
                selected = graphical.par$col.pt)
})

##  Adjust symbol colour: Update
observe({
    if (is.null(input$choose_symb_col) || input$choose_symb_col == "") {
        return()
    } else {
        symbol.col <- input$choose_symb_col
        graphical.par$col.pt <- symbol.col
    }
})


##  Adjust box colour: UI
output$box_colour <- renderUI({
    selectInput(inputId = "choose_box_col",
                label = "Choose Box Colour:",
                choices =
                    c("Light Blue" = "lightskyblue3",
                      colour.choices[-1]),
                selected = graphical.par$box.fill)
})

##  Adjust box colour: Update
observe({
    if (is.null(input$choose_box_col) || input$choose_box_col == "") {
        return()
    } else {
        box.col <- input$choose_box_col
        graphical.par$box.fill <- box.col
    }
})

##  Adjust bar colour: UI
output$bar_colour <- renderUI({
    selectInput(inputId = "choose_bar_col",
                label = "Choose Bar Colour:",
                choices =
                    c("Light Blue" = "lightskyblue3",
                      colour.choices[-1]),
                selected = graphical.par$bar.fill)
})

##  Adjust bar border colour: Update
observe({
    if (is.null(input$choose_bar_col) || input$choose_bar_col == "") {
        return()
    } else {
        bar.fill.col <- input$choose_bar_col
        graphical.par$bar.fill <- bar.fill.col
    }
})


##  Adjust xlab: Update.
observe({
    if (is.null(input$xlab)) {
        return()
    }
    if (input$xlab == "") {
        plot.par$xlab <- NULL
    } else {
        plot.par$xlab <- input$xlab
        updateTextInput(session,
                        inputId = "xlab",
                        value = plot.par$xlab)
    }
})
##  Adjust ylab: Update.
observe({
    if (is.null(input$ylab)) {
        return()
    }
    if (input$xlab == "") {
        plot.par$ylab <- NULL
    } else {
        plot.par$ylab <- input$ylab
        updateTextInput(session,
                        inputId = "ylab",
                        value = plot.par$ylab)
    }
})

##  Adjust title: Update.
observe({
    if (is.null(input$title)) {
        return()
    }
    if (input$title == "") {
        plot.par$main <- NULL
    } else {
        plot.par$main <- input$title
        updateTextInput(session,
                        inputId = "title",
                        value = plot.par$main)
    }
})

##  Adjust symbol transparency: UI.
output$symbol_transparency <- renderUI({
    sliderInput(inputId = "transparency",
                label = "Transparency:",
                min = 0.01, max = 1, step = 0.01,
                value = graphical.par$alpha)
})

##  Adjust symbol transparency: Update.
observe({
    if (is.null(input$transparency)) {
        return()
    } else {
        graphical.par$alpha <- input$transparency
    }
})

##  Adjust symbol size: UI.
output$symbol_size <- renderUI({
    sliderInput(inputId = "size",
                label  = "Dot Size:",
                min = 0.25, max = 4, step = 0.25,
                value = graphical.par$cex.pt)
})

##  Adjust symbol size: Update.
observe({
    if (is.null(input$size)) {
        return()
    } else {
        graphical.par$cex.pt <- input$size
    }
})

##  Adjust Bar Width: UI
output$bar_width <- renderUI({
    sliderInput(inputId = "bar_line_width",
                label  = "Bar Border Width:",
                min = 0.1, max = 3, step = 0.1,
                value = graphical.par$bar.lwd)
})


##  Adjust Bar Width: Update
observe({
    if (is.null(input$bar_line_width)) {
        return()
    } else {
        graphical.par$bar.lwd <- input$bar_line_width
    }
})


##  Adjust Bar Border Colour: UI
output$bar_border <- renderUI({
    selectInput(inputId = "bar_border_colour",
                label  = "Bar Border Colour:",
                choices = c("Light Gray" = "gray60", dot.choices),
                selected = graphical.par$bar.col)
})

##  Adjust Bar Border: Update
observe({
    if (is.null(input$bar_border_colour) || input$bar_border_colour == "") {
        return()
    } else {
        bar.border.col <- input$bar_border_colour
        graphical.par$bar.col <- bar.border.col
    }
})




##  Adjust Box Width: UI
output$bar_width <- renderUI({
    sliderInput(inputId = "box_line_width",
                label  = "Box Border Width:",
                min = 0.1, max = 3, step = 0.1,
                value = graphical.par$bar.lwd)
})


##  Adjust Box Width: Update
observe({
    if (is.null(input$box_line_width)) {
        return()
    } else {
        graphical.par$box.lwd <- input$bar_line_width
    }
})


##  Adjust Box Border Colour: UI
output$box_border <- renderUI({
    selectInput(inputId = "box_border_colour",
                label  = "Box Border Colour:",
                choices = c("Light Gray" = "gray60", dot.choices),
                selected = graphical.par$box.col)
})

##  Adjust Box Border: Update
observe({
    if (is.null(input$box_border_colour) || input$box_border_colour == "") {
        return()
    } else {
        box.border.col <- input$box_border_colour
        graphical.par$box.col <- box.border.col
    }
})

##-------------------##
##  "Add Colour" tab ##
##-------------------##
##
##  "Resize by" UI
output$resize.by <- renderUI({
    selectInput(inputId = "resize_by",
                label = "Resize relative to:",
                choices =
                    c("none", rev(colnames(vis.data()))),
                selected = plot.par$varnames$prop.size)
})

##  "Resize by" Update
observe({
    if (is.null(input$resize_by)) {
        return()
    }
    if (input$resize_by != "none" &&
        input$resize_by %in% colnames(vis.data())) {
        resize.input <- vis.data()[, input$resize_by]
    } else {
        resize.input <- NULL
    }
    plot.par$prop.size <- resize.input
    plot.par$varnames$prop.size <- input$resize_by
})

##  "Colour By" UI
output$colour.by <- renderUI({
    selectInput(inputId = "colour_by",
                label = "Colour by:",
                choices =
                    c("none", rev(colnames(vis.data()))),
                selected = plot.par$varnames$by)
})

##  "Colour by" Update
observe({
    if (is.null(input$colour_by)) {
        return()
    }
    if (input$colour_by != "none" &&
        input$colour_by %in% colnames(vis.data())) {
        colour.input <- vis.data()[, input$colour_by]
    } else {
        colour.input <- NULL
    }

    plot.par$by <- colour.input
    plot.par$varnames$by <- input$colour_by
})


##-----------------------##
##  "Add Inference" Tab  ##
##-----------------------##
##
##
output$add.inference <- renderUI({
    cat("boo")
})



##  Reset variable selection and graphical parameters.
observe({
    if (length(input$reset.graphics) > 0) {
            isolate({
                ##  Variable Selections
                updateSelectInput(session,
                                  inputId = "vari1",
                                  choices =
                                      c(rev(colnames(vis.data()))))
                updateSelectInput(session,
                                  inputId = "subs1",
                                  choices =
                                      c("none", rev(colnames(vis.data()))))
                updateSelectInput(session,
                                  inputId = "vari2",
                                  choices =
                                      c("none", rev(colnames(vis.data()))[-1]))
                updateSelectInput(session,
                                  inputId = "subs2",
                                  choices =
                                      c("none", rev(colnames(vis.data()))[-1]))
                updateSliderInput(session,
                                  inputId = "sub1_level")
                updateSliderInput(session,
                                  inputId = "sub2_level")
                

                ###  Graphical Parameters
                updateRadioButtons(session,
                                   inputId = "customize_plot",
                                   choices =
                                       c("Hide" = 1,
                                         "Show" = 2),
                                   selected = 1)

                graphical.par$bg = "white"                
                graphical.par$alpha = 1
                graphical.par$bg = "white"
                ##  Box
                graphical.par$box.col = "gray60"
                graphical.par$box.fill = "lightskyblue3"
                ##  Bar
                graphical.par$bar.lwd = 1
                graphical.par$bar.col = "gray60"
                graphical.par$bar.fill = "lightskyblue3"
                ##  Line
                graphical.par$lwd = 1
                graphical.par$lty = 1
                graphical.par$lwd.pt = 2
                graphical.par$col.line = "blue"
                ##  Point
                graphical.par$cex.pt = .5
                graphical.par$lwd.pt = .75
                graphical.par$pch = 1
                graphical.par$col.pt = "gray60"
                graphical.par$fill.pt = "gray85"
                ##  Colours
                graphical.par$col.LOE = "black"
                graphical.par$col.trend =
                    list(linear = "",
                         quadratic = "",
                         cubic =  "")
                graphical.par$col.smooth = ""
                ##  Jitter, rugs, and trend.
                graphical.par$jitter = ""
                graphical.par$rugs = ""
                graphical.par$trend = NULL
                ##  Others
                graphical.par$cex = 1
                graphical.par$quant.smooth = NULL
                graphical.par$inference.type = NULL
                graphical.par$largesample = NULL
                graphical.par$LOE = FALSE
                graphical.par$join = FALSE
                graphical.par$lines.by = FALSE
                graphical.par$trend.by = FALSE
                graphical.par$smooth = 0
                graphical.par$szsym = 1
                graphical.par$tpsym = 1                
                ## updateSelectInput(session,
                ##                   inputId = "choose_plot",
                ##                   choices =
                ##                       c("Bar" = 1,
                ##                         "Dot" = 2,
                ##                         "Box" = 3),
                ##                   selected = 1)
                ## updateSelectInput(session,
                ##                   inputId = "choose_bg",
                ##                   choices = rev(colour.choices))
                ## updateSelectInput(session,
                ##                   inputId = "choose_symb_col",
                ##                   choices = colour.choices)
                ## updateSelectInput(session,
                ##                   inputId = "choose_box_col",
                ##                   choices = colour.choices)
                ## updateSelectInput(session,
                ##                   inputId = "choose_bar_col",
                ##                   choices = colour.choices)
                ## updateTextInput(session,
                ##                 inputId = "xlab",
                ##                 value = "")
                ## updateTextInput(session,
                ##                 inputId = "ylab",
                ##                 value = "")
                ## updateTextInput(session,
                ##                 inputId = "main",
                ##                 value = "")
                ## updateSliderInput(session,
                ##                   inputId = "transparency",
                ##                   value = 1)
                ## updateSliderInput(session,
                ##                   inputId = "size",
                ##                   value = 0.5)
            })
        }
})

##  Other handling
observe({
    if (!is.null(input$vari2) && !input$vari2 %in% "none") {
        isolate({
            updateSelectInput(session, "subs2",
                              choices = c("none",
                                  colnames(vis.data())
                                  [which(!colnames(vis.data()) %in% input$vari1)]))
        })
    }
})

observe({
    if (!is.null(input$vari1)) {
        updateSelectInput(session,
                          inputId = "vari2",
                          choices = c("none",
                              colnames(vis.data())
                              [which(!colnames(vis.data())%in%input$vari1)]))
        updateSelectInput(session,
                          inputId = "subs2",
                          choices = c("none",
                              colnames(vis.data())
                              [which(!colnames(vis.data())%in%input$vari1)]))
    }
})

##  Server Functions for the "Visualize" Module
##
##  Date Created   : January 25, 2015.
##  Last Modified  : February 1, 2015.
##
##  Please consult the documentation for the Time Series module before
##  editing any code. If you have any questions and/or suggestions,
##  drop me an e-mail: Chris Park <cpar137@aucklanduni.ac.nz>
##
##  Note: This file is to be sourced locally within "server.R" *

vis.data <- reactive({
    input$selector
    data
    vis.data <- data
})

plot.par <- reactiveValues(
    x = NULL,
    y = NULL,
    varnames = list(x = NULL, y = NULL,
        g1 = NULL, g2 = NULL,
        by = NULL, prop.size = NULL),
    g1 = NULL,
    g2 = NULL,
    g1.level = NULL,
    g2.level = NULL,
    by = NULL,
    prop.size = NULL,
    main = NULL
)

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

determine.g <- reactive({
    x.class <- determine.class(plot.par$x)
    y.class <- determine.class(plot.par$y)
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
    id <- xy.class %in% c("numeric", "factor")
    return(c("x of incorrect class", "y of incorrect class")[!id])
})
        

##  These are the list of parameters in inzPlotDefaults()
graphical.par <- reactiveValues(
    alpha = 1,
    bg = "white",
    ##  Box
    box.col = "black",
    box.fill = "grey90",
    ##  Bar
    bar.lwd = 1,
    bar.col = "black",
    bar.fill = "slategrey", # me.
    ##  Line
    lwd = 1,
    lty = 1,
    lwd.pt = 2,
    col.line = "blue",
    ##  Colours
    col.LOE = "black",
    col.pt = "grey50", 
    col.trend =
        list(linear = " ",
             quadratic = " ",
             cubic = " "),
    col.smooth = " ",
    ##  Jitter, rugs, and trend.
    jitter = "",
    rugs = "",
    trend = NULL,
    ##  Others
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
        

vis.par <- reactive({
    vis.par <- reactiveValuesToList(plot.par)
    if (!is.null(vis.par$x) && plot.par$varnames$x != "") {
        validate(
            need(all(na.omit(vis.par$x) != "none"),
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
    
            


handle.data <- function(file) {
    ##  Compute the quantiles if the data is numerie, and coerce into
    ##  factors otherwise.
    if (is.numeric(file)) {
        ##  Compute quantiles and round to 2 decimal places.
        quants <- round(quantile(file, na.rm = TRUE), 2)
        ##  Create 25% quantile intervals.
        quant.ints <- paste(quants[1:4], quants[2:5], sep = "-")
        ##  Create a "choices" vector for the sliderInput.
        choices <- length(quant.ints)
        factors <- as.factor(quant.ints)
    } else {
        ##  Coerce into factors.
        fact <- as.factor(file)
        ##  Create a "choices" vector for the sliderInput.
        choices <- nlevels(fact)
        factors <- levels(fact)
    }
    
    list(choices = choices, factors = factors)
}



##-------------------------##
##  Panels for variable 1  ##
##-------------------------##
##
##  Select variable 1
output$vari1_panel <- renderUI({
    selectInput(inputId = "vari1",
                label = "Select first variable:",
                choices = rev(colnames(vis.data())),
                selected = plot.par$varnames$x)
})
                
##  Subset variable 1
output$subs1_panel <- renderUI({
    selectInput(inputId = "subs1",
                label = "Subset first variable:",
                choices = c("none", rev(colnames(vis.data()))),
                selected = plot.par$varnames$g1)
})
##  Subset level (Slider)
output$subs1_conditional <- renderUI({
    if (!is.null(input$subs1) && input$subs1 != "none") {
        ##  Create a local copy of the data.
        temp1 <- vis.data()[, which(colnames(vis.data()) %in% input$subs1)]
        ##  Call the "handle.data" function.
        choices1 <- handle.data(file = temp1)$choices
        ##  Create sliderInput.
        sliderInput(inputId = "sub1_level", label = "Subset Level:",
                    min = 0, max = choices1, value = 0, step = 1,
                    animate = TRUE)
    }
})

##-------------------------##
##  Panels for variable 2  ##
##-------------------------##
##
##  Select variable 2
output$vari2_panel <- renderUI({
    selectInput(inputId = "vari2",
                label = "Select second variable:",
                choices = c("none", rev(colnames(vis.data()))[-1]),
                selected = plot.par$varnames$y)
})
                
##  Subset variable 2
output$subs2_panel <- renderUI({
    selectInput(inputId = "subs2",
                label = "Subset second variable:",
                choices = c("none", rev(colnames(vis.data()))[-1]),
                selected = plot.par$varnames$g2)
})


output$subs2_conditional <- renderUI({
    if (!is.null(input$subs2) && input$subs2 != "none") {
        ##  Create a local copy of the data.
        temp2 <- vis.data()[, which(colnames(vis.data()) %in% input$subs2)]
        ##  Call the "handle.data" function.
        choices2 <- handle.data(file = temp2)$choices
        ##  Create sliderInput.
        sliderInput(inputId = "sub2_level", label = "Subset Level:",
                    min = 0, max = choices2, value = 0, step = 1,
                    animate = TRUE)
    }
})




##------------##
##  Observers ##
##------------##
observe({
    if (!is.null(input$reset.graphics)) {
        isolate({
            updateSelectInput(session,
                              inputId = "vari1",
                              ## choices = c("none", colnames(data)))
                              choices = c(rev(colnames(vis.data()))))
            updateSelectInput(session,
                              inputId = "subs1",
                              choices = c("none", rev(colnames(vis.data()))))
            updateSelectInput(session,
                              inputId = "vari2",
                              choices = c("none", rev(colnames(vis.data()))[-1]))
            updateSelectInput(session,
                              inputId = "subs2",
                              choices = c("none", rev(colnames(vis.data()))[-1]))
            updateSliderInput(session,
                              inputId = "sub1_level")
            updateSliderInput(session,
                              inputId = "sub2_level")
            updateCheckboxInput(session,
                                inputId = "add_plot",
                                value = FALSE)
        })
    }
})

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

##-------------------------##
##  Plot related functions ##
##-------------------------##
##  This functions converts raw data into a form that can be used by iNZightPlot()
handle.plot <-
    function(data, vari.input, subs.input, subs = TRUE) {
        ##  We subset the data that corresponds to the chosen variable.
        vari.cols <- which(colnames(vis.data()) %in% vari.input)
        vari.data <- vis.data()[, vari.cols]
        ##  If the variable is neither a numeric value nor a factor, then
        ##  we coerce it into a factor. This is because iNZightPlot() takes
        ##  factors as plot arguments.
        if (!is.numeric(vari.data) & !is.factor(vari.data)) {
            vari.data <- as.factor(vari.data)
        }
        ##  Next, we handle the data for the subsetting.
        if (subs) {
            ##  We subset the data the corresponds to the chosen subset.
            subs.cols <- which(colnames(vis.data()) %in% subs.input)
            subs.data <- vis.data()[, subs.cols]
            ##  If "subs.data" is neither a numeric value nor a factor, AND
            ##  if at least one variable has been chosen as a subsetting
            ##  variable for "vari1", then we perform a type coercion as above.
            if (!is.numeric(subs.data) && !is.factor(subs.data)
                && length(subs.cols) > 0) {
                subs.data <- as.factor(subs.data)
            }            
        } else {
            subs.data <- "No subset variable available."
        }
        ##  Return the results in a list.
        list(vari.data = vari.data, subs.data = subs.data)
    }

##  This function handles the "g" argument for iNZightPlot()
handle.g <- function(subset) {
    g <- character(length(subset))
    if (is.numeric(subset)) {
        ## g <- get.quantiles(subset)
        g <- handle.data(file = subset)$factors
    } else if (is.character(subset)) {
        g <- as.factor(subset)
    } else if (is.factor(subset)) {
        g <- subset
    } else {
        g <- NULL
    }
    list(g = g)
}
    
                  
###  "Visualize" panel plot.
output$visualize.plot = renderPlot({
    ##  "largesample" is an argument for iNZightPlots::iNZightPlot().
    ##  It simply specifies whether a plot should be displayed with
    ##  points or with bars, depending on the number of data points.
    ##  If set to NULL, iNZightPlot() determines this automatically.
    largesample = NULL
    
    ##--------------------------##
    ##  Only variable 1 chosen  ##
    ##--------------------------##
    ##
    ##  If only variable one has been selected, then...
    if (!is.null(input$vari1) && !is.null(input$vari2) && input$vari2 == "none") {
        ##  Handle variable data
        vari1.data <- handle.plot(data = vis.data(),
                                  vari.input = input$vari1,
                                  subs.input = input$subs1)$vari.data
        subs1.data <- handle.plot(data = vis.data(),
                                  vari.input = input$vari1,
                                  subs.input = input$subs1)$subs.data
        ##  If a variable HAS NOT been chosen as a subsetting variable, then...
        if (!is.null(input$subs1) && input$subs1 == "none") {
            iNZightPlots::iNZightPlot(x = vari1.data,
                                      xlab = input$vari1,
                                      largesample = largesample,
                                      main = "")
        }
        ##  If a variable HAS been chosen as a subsetting variable for
        ##  "vari1", then...
        else if (!is.null(input$subs1) && input$subs1 != "none") {
            g1 <- handle.g(subset = subs1.data)$g
            iNZightPlots::iNZightPlot(x = vari1.data,
                                      g1 = g1,
                                      g1.level = input$sub1_level,
                                      xlab = input$vari1,
                                      largesample = largesample,
                                      main = "")
        } else {
            NULL
        }
        
    }
    ##=================================##
    ##  Both variables 1 AND 2 chosen  ##
    ##=================================##
    ## 
    else if (input$vari1 != "none" && input$vari2 != "none") {
        ##--------------##
        ##  Variable 1  ##
        ##--------------##
        vari1.data <- handle.plot(data = vis.data(),
                                  vari.input = input$vari1,
                                  subs.input = input$subs1)$vari.data
        subs1.data <- handle.plot(data = vis.data(),
                                  vari.input = input$vari1,
                                  subs.input = input$subs1)$subs.data
        ##-------------##
        ##  Variable 2 ##
        ##-------------##
        vari2.data <- handle.plot(data = vis.data(),
                                  vari.input = input$vari2,
                                  subs.input = input$subs2)$vari.data
        subs2.data <- handle.plot(data = vis.data(),
                                  vari.input = input$vari2,
                                  subs.input = input$subs2)$subs.data
        ##  If there is no subsetting for both variables, then...
        if (input$subs1 == "none" && input$subs2 == "none") {
            iNZightPlots::iNZightPlot(x = vari1.data,
                                      y = vari2.data,
                                      xlab = input$vari1,
                                      ylab = input$vari2,
                                      largesample = largesample,
                                      main = "")
        }
        ## If there is subsetting for only variable 2, then...
        else if (input$subs1 == "none" && input$subs2 != "none") {
            g2 <- handle.g(subset = subs2.data)$g
            iNZightPlots::iNZightPlot(x = vari1.data,
                                      y = vari2.data,
                                      ## g2 = g2,
                                      ## g2.level = input$subs2_level,
                                      xlab = input$vari1,
                                      ylab = input$vari2,
                                      largesample = largesample,
                                      main = "")

        }
        ##  If there is subetting for variable 1 but not 2, then...
        else if (input$subs1 != "none" && input$subs2 == "none") {
            g1 <- handle.g(subset = subs1.data)$g
            iNZightPlots::iNZightPlot(x = vari1.data,
                                      y = vari2.data,
                                      g1 = g1,
                                      g1.level = input$sub1_level,
                                      xlab = input$vari1,
                                      ylab = input$vari2,
                                      largesample = largesample,
                                      main = "")
        }
        ##  If there is subsetting for both variables 1 and 2, then...
        else if (input$subs1 != "none" && input$subs2 != "none") {
            g1 <- handle.g(subset = subs1.data)$g
            g2 <- handle.g(subset = subs2.data)$g
            indices = 1:length(subs2.data)
            if (!is.null(input$sub2_level)) {
                indices = which(g2 %in% input$sub2_level)
            }
            iNZightPlots::iNZightPlot(x = vari1.data[indices],
                                      y = vari2.data[indices],
                                      g1 = g1[indices],
                                      g1.level = input$sub1_level,
                                      xlab = input$vari1,
                                      ylab = input$vari2,
                                      largesample = largesample,
                                      main = "")
        } else {
            NULL
        }
    } else {
        NULL
    }
})

            

output$visualize.inference <- renderPrint({    
    cat(runif(15), sep = "\n")
})


output$visualize.summary <- renderPrint({
    if (is.null(plot.par$x)) {
        return(cat("Please select a variable"))
    }
    values.list <- modifyList(reactiveValuesToList(plot.par),
                              reactiveValuesToList(graphical.par))
    cat(do.call(getPlotSummary, values.list), sep = "\n")
})

output$change.appearance <- renderUI({
})

output$add.inference.info <- renderUI({
})                                     



output$visualize.module = renderUI({
    input$selector
    visualize_panel()
})
                
    
    

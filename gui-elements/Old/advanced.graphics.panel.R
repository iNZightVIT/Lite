help.advanced.graphics = function() {
    helpModal('Advanced Plots',
              'advanced_plots',
              inclMD("gui-elements/notes/advanced.graphics.md"))
}


get.plot.section = function() {
    if(!is.null(data)) {
        list(
            plotOutput("advanced.plot")
            ## br(),
            ## fluidRow(
            ##     column(h6("Change plot appearance"),
            ##            tabsetPanel(id = "graph_selector", type = "pills",
            ##                        tabPanel("Add to plot", add.to.plot()),
            ##                        tabPanel("Change labels"),
            ##                        tabPanel("To be named")),
            ##            width = 12))
        )
    } else {
        ret = list(h4("No data available, please select or import a data set."))
    }
}

add.to.plot = function() {
    list(helpText("I'm here to help."))
}

get.graphics.section = function() {
    if (!is.null(data)) {
        list(
            hr(),
            selectInput(inputId = "vari1",
                        label = "Select first variable",
                        choices = rev(colnames(data))),
            selectInput(inputId = "subs1",
                        label = "Subset first variable",
                        choices = c("none", rev(colnames(data)))),
            conditionalPanel(condition = "input.subs1 != 'none'",
                             helpText("Select the level which should be plotted."),
                             sliderInput(inputId = "sub1_level",
                                         label = "Subset Level",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         step = 1,
                                         animate = TRUE)),
            ## selectInput(inputId = "sub1_level",
            ##             label = "Subset Level",
            ##             choices = "")),
            hr(),
            selectInput(inputId = "vari2",
            label = "Select second variable",
            choices = c("none", rev(colnames(data))[-1])),
            hr(),
            selectInput(inputId = " subs2",
                        label = "Subset second variable",
                        choices = c("none", rev(colnames(data))[-1])),
            conditionalPanel(condition = "input.subs2!='none'",
                             helpText("Select the filtering level for the values in the plot."),
                             selectInput(inputId = "sub2_level",
                                         label = "Subset Level",
                                         choices = "")),
            hr(),
            ## radioButtons(inputId = "graphics.style",
            ##              label = "Select style",
            ##              choices = c("Small", "Large"),
            ##              selected = "Small", inline = TRUE),
            ## br(),
            actionButton(inputId = "reset.graphics",
                         label = "Reset all"),
            ## br(),
            help.advanced.graphics()
            ## "  HELP",
            ## hr()
        )
    }
}

advanced.graphics.panel <- function() {
        fluidRow(
            column(2, get.graphics.section()),
            column(10, get.plot.section()))
    }

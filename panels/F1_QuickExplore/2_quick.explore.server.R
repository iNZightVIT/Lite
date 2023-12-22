#   Advanced --> Quick explore

output$quick.explore <- renderUI({
  updatePanel$doit
  isolate({
    quick.explore.panel(get.data.set())
  })
})

#   Advanced --> Quick explore --> Missing values

output$quick.missing.summary.side <- renderUI({
  get.quick.missing.summary.side(get.data.set(), get.data.name())
})

output$quick.missing.summary.main <- renderUI({
  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "Plot",
      plotOutput("quick.missing.summary.plot", height = "600px"),
      br(),
    ),
    tabPanel(
      title = "Table of row combinations",
      br(),
      br(),
      verbatimTextOutput("quick.missing.summary.out")
    )
  )
})

quick.par <- reactiveValues(combp = NULL)

output$quick.missing.summary.plot <- renderPlot({
  if (is.null(get.combinations(get.data.set(), T))) {
    plot.new()
    text(0.5, 0.5, "Data is clean of NA values", cex = 2)
  } else {
    quick.par$combp <- iNZightMR::plotcombn(get.data.set())
  }
})


output$quick.missing.summary.out <- renderPrint({
  quick.par$combp
})


#   Advanced --> Quick explore --> Data summary

output$quick.summary.side <- renderUI({
  get.quick.summary.sidebar(get.data.set())
})

output$quick.summary.main <- renderUI({
  get.quick.summary.main()
})

output$all.summary <- renderPrint({
  data.summary(get.data.set())
})

output$column.summary <- renderPrint({
  if (!is.null(input$select.column.sum)) {
    temp <- get.data.set()[
      ,
      which(colnames(get.data.set()) %in% input$select.column.sum)
    ]
    if (is.character(temp)) {
      print(as.factor(temp))
      cat("\n\t")
      print(summary(as.factor(temp)))
    } else {
      print(summary(temp))
    }
  } else {
    NULL
  }
})

#   Advanced --> Quick explore --> Single Column plot

output$single.column.plot.side <- renderUI({
  get.single.col.sidebar(get.data.set())
})

output$single.column.plot.main <- renderUI({
  get.single.col.main(get.data.set())
})

observe({
  input$single.backward
  isolate({
    if (!is.null(input$single.backward) && input$single.backward > 0) {
      index <- 1
      if (which(colnames(get.data.set()) %in% input$select.column.plot) == 1) {
        index <- ncol(get.data.set())
      } else {
        index <- which(
          colnames(get.data.set()) %in% input$select.column.plot
        ) - 1
      }
      updateSelectInput(session,
        inputId = "select.column.plot",
        choices = colnames(get.data.set()),
        selected = colnames(get.data.set())[index]
      )
      updateSliderInput(session, inputId = "single.play", value = index)
    }
  })
})

observe({
  input$single.forward
  isolate({
    if (!is.null(input$single.forward) && input$single.forward > 0) {
      index <- 1
      if (which(
        colnames(get.data.set()) %in% input$select.column.plot
      ) == ncol(get.data.set())) {
        index <- 1
      } else {
        index <- which(
          colnames(get.data.set()) %in% input$select.column.plot
        ) + 1
      }
      updateSelectInput(session,
        inputId = "select.column.plot",
        choices = colnames(get.data.set()),
        selected = colnames(get.data.set())[index]
      )
      updateSliderInput(session, inputId = "single.play", value = index)
    }
  })
})

observe({
  input$single.play
  isolate({
    if (!is.null(input$single.play)) {
      updateSelectInput(session,
        inputId = "select.column.plot",
        choices = colnames(get.data.set()),
        selected = colnames(get.data.set())[input$single.play]
      )
    }
  })
})

output$column.plot <- renderPlot({
  input$select.column.plot
  isolate({
    if (!is.null(get.data.set()) && !is.null(input$select.column.plot)) {
      index <- which(colnames(get.data.set()) %in% input$select.column.plot)
      if (length(index) == 0) {
        index <- 1
      }
      updateSliderInput(session, inputId = "single.play", value = index)
      temp <- get.data.set()[, index]
      if (is.character(temp)) {
        temp <- as.factor(temp)
      }
      iNZightPlot(temp, xlab = input$select.column.plot, main = get.data.name())
    }
  })
})

#   Advanced --> Quick explore --> Column pair plot

output$column.pair.plot.side <- renderUI({
  get.pair.plot.sidebar(get.data.set())
})

output$column.pair.plot.main <- renderUI({
  get.pair.plot.main(get.data.set())
})

observe({
  input$pair.player
  isolate({
    if (!is.null(input$pair.player)) {
      indMat <- rbind(
        1:(ncol(get.data.set()) * (ncol(get.data.set()) - 1)),
        rep(1:(ncol(get.data.set()) - 1), ncol(get.data.set())),
        ceiling(seq(
          from = 0.1, to = ncol(get.data.set()),
          by = 1 / (ncol(get.data.set()) - 1)
        ))
      )
      index1 <- indMat[3, input$pair.player]
      index2 <- indMat[2, input$pair.player]
      values$button <- T
      updateSelectInput(session,
        inputId = "select.column.plot1",
        selected = colnames(get.data.set())[index1],
        choices = colnames(get.data.set())
      )
      updateSelectInput(session,
        inputId = "select.column.plot2",
        selected = colnames(get.data.set())[-index1][index2],
        choices = colnames(get.data.set())[-index1]
      )
    }
  })
})

observe({
  input$pair.backward
  isolate({
    if (!is.null(input$pair.backward) && input$pair.backward > 0) {
      index1 <- which(colnames(get.data.set()) %in% input$select.column.plot1)
      index2 <- which(
        colnames(get.data.set())[-index1] %in% input$select.column.plot2
      )
      if (index2 == 1) {
        if (index1 == 1) {
          index1 <- ncol(get.data.set())
        } else {
          index1 <- index1 - 1
        }
        values$button <- T
        updateSelectInput(session,
          inputId = "select.column.plot1",
          selected = colnames(get.data.set())[index1],
          choices = colnames(get.data.set())
        )
        index2 <- ncol(get.data.set()) - 1
      } else {
        index2 <- index2 - 1
      }
      updateSelectInput(session,
        inputId = "select.column.plot2",
        selected = colnames(get.data.set())[-index1][index2],
        choices = colnames(get.data.set())[-index1]
      )
      matInd <- which(
        colnames(get.data.set()) %in% colnames(get.data.set())[-index1][index2]
      )
      updateSliderInput(session,
        inputId = "pair.player",
        value = matrix(c(unlist(lapply(
          seq(
            from = ncol(get.data.set()), by = ncol(get.data.set()),
            to = ncol(get.data.set()) * (ncol(get.data.set()) - 1)
          ), function(x, n) {
            c(0, (x - (n - 1)):x)
          },
          ncol(get.data.set())
        )), 0), nrow = ncol(get.data.set()))[matInd, index1]
      )
    }
  })
})

observe({
  input$pair.forward
  isolate({
    if (!is.null(input$pair.forward) && input$pair.forward > 0) {
      index1 <- which(colnames(get.data.set()) %in% input$select.column.plot1)
      index2 <- which(
        colnames(get.data.set())[-index1] %in% input$select.column.plot2
      )
      if (index2 == (ncol(get.data.set()) - 1)) {
        if (index1 == ncol(get.data.set())) {
          index1 <- 1
        } else {
          index1 <- index1 + 1
        }
        values$button <- T
        updateSelectInput(session,
          inputId = "select.column.plot1",
          selected = colnames(get.data.set())[index1],
          choices = colnames(get.data.set())
        )
        index2 <- 1
      } else {
        index2 <- index2 + 1
      }
      updateSelectInput(session,
        inputId = "select.column.plot2",
        selected = colnames(get.data.set())[-index1][index2],
        choices = colnames(get.data.set())[-index1]
      )
      matInd <- which(
        colnames(get.data.set()) %in% colnames(get.data.set())[-index1][index2]
      )
      updateSliderInput(session,
        inputId = "pair.player",
        value = matrix(c(unlist(lapply(
          seq(
            from = ncol(get.data.set()), by = ncol(get.data.set()),
            to = ncol(get.data.set()) * (ncol(get.data.set()) - 1)
          ), function(x, n) {
            c(0, (x - (n - 1)):x)
          },
          ncol(get.data.set())
        )), 0), nrow = ncol(get.data.set()))[matInd, index1]
      )
    }
  })
})

observe({
  input$select.column.plot1
  isolate({
    choice <- input$select.column.plot1
    if (!is.null(choice)) {
      choice2 <- input$select.column.plot2
      i <- 2
      ch <- colnames(
        get.data.set()
      )[-which(colnames(get.data.set()) %in% choice)]
      if (choice == choice2) {
        i <- which(colnames(get.data.set()) %in% choice2)
        if (length(ch) > 0 && i > length(ch)) {
          i <- 1
        }
      }
      if (!get.button()) {
        updateSelectInput(session, "select.column.plot2",
          choices = ch,
          selected = ch[i]
        )
      }
      values$button <- F
    }
  })
})

output$plot.column.pair <- renderPlot({
  if (!is.null(input$select.column.plot1) &&
    !is.null(input$select.column.plot2) &&
    !"" %in% input$select.column.plot1 && !"" %in% input$select.column.plot2) {
    index1 <- which(colnames(get.data.set()) %in% input$select.column.plot1)
    index2 <- which(colnames(get.data.set()) %in% input$select.column.plot2)
    if (length(index1) == 0) {
      index1 <- 1
    }
    if (length(index2) == 0) {
      if (index1 + 1 > ncol(get.data.set())) {
        index2 <- 1
      } else {
        index2 <- index1 + 1
      }
    }
    x <- get.data.set()[, index1]
    y <- get.data.set()[, index2]
    x.lab <- input$select.column.plot1
    y.lab <- input$select.column.plot2
    if (is.numeric(x) & is.numeric(y)) {
      temp <- x
      x <- y
      y <- temp
      temp <- x.lab
      x.lab <- y.lab
      y.lab <- temp
    }
    try(iNZightPlot(x, y,
      xlab = x.lab,
      ylab = y.lab,
      main = get.data.name()
    ))
  }
})

#   Advanced --> Quick explore --> Compare pairs

output$compare.pairs.main <- renderUI({
  get.matrix.main(get.data.set())
})

output$compare.pairs.side <- renderUI({
  get.matrix.sidebar(get.data.set())
})

output$plot.matrix <- renderPlot({
  choices <- input$select.matrix.plot
  if (is.null(choices) || length(choices) == 1) {
    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "You have to select more than 1 variable", cex = 2)
  } else {
    grpVar <- input$grpVar
    if (!is.null(grpVar) && !grpVar %in% " ") {
      try(suppressWarnings(ggpairs(get.data.set(),
        columns = which(colnames(get.data.set()) %in% choices),
        lower = list(
          continous = density,
          combo = "box"
        ),
        upper = list(
          continous = "points",
          combo = "dot"
        ),
        mapping = ggplot2::aes_string(colour = as.name(grpVar))
      )))
    } else {
      try(suppressWarnings(ggpairs(get.data.set(),
        columns = which(colnames(get.data.set()) %in% choices),
        lower = list(
          continous = density,
          combo = "box"
        ),
        upper = list(
          continous = "points",
          combo = "dot"
        )
      )))
    }
  }
})

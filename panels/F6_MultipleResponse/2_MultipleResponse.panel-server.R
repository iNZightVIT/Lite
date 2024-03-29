### -------------------------------------------------------###
###  Server Functions for the "Multiple Response" Module  ###
### -------------------------------------------------------###
###
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *



## initialize gui
output$multiple.response <- renderUI({
  MultipleResponse.panel.ui(get.data.set())
})

mr.par <- reactiveValues(
  plotSet = list(),
  objName = "response",
  guessName = TRUE,
  mrObject = NULL,
  combp = NULL
)


isBinary <- function(x) {
  ## NAs are ignored as they are handled by MR
  tab <- table(x, useNA = "no")[table(x) != 0]
  n <- length(names(tab))
  ## if not binary, return FALSE
  if (n != 2) {
    return(FALSE)
  }
  ## regular expressions for "yes, no, 0, 1, true, false"
  re1 <- "([Yy][Ee][Ss])|([Nn][Oo])|([Yy])|([Nn])"
  re2 <- "(0)|(1)"
  re3 <- "([Tt][Rr][Uu][Ee])|([Ff][Aa][Ll][Ss][Ee])|([Tt])|([Ff])"
  re <- paste(re1, re2, re3, sep = "|")
  ## do those patterns match?
  l <- grepl(re, names(tab))
  ## do BOTH binary values match the patterns?
  return(all(l))
}

getVars <- function(data) {
  which(apply(data, 2, function(x) isBinary(x)))
}

################
## left panel ##
################

output$mr.var <- renderUI({
  get.data.set()
  isolate({
    binaryVar <- getVars(get.data.set())
    vars <- names(get.data.set())
    if (length(binaryVar) == 0) {
      shinyalert(
        title = "No Binary Variables",
        text = "Unable to find any binary variables. Code any variables as: ['yes', 'no'] or [0,1] to use this module.",
        type = "error"
      )
    } else {
      list(
        h5(strong("Multiple Response")),
        selectInput("mr.select.var",
          label = "Select related variables: ",
          choices = vars[binaryVar],
          multiple = T,
          selectize = F,
          size = 18
        )
      )
    }
  })
})

output$mr.type <- renderUI({
  get.data.set()
  input$mr.select.var
  input$mr.select.sub.var1
  isolate({
    if (length(req(input$mr.select.var)) > 1 &&
      req(input$mr.select.sub.var1) == " ") {
      radioButtons("mr.result",
        label = NULL, choices = c("Summary", "Combinations"),
        selected = "Summary",
        inline = T
      )
    } else if (length(req(input$mr.select.var)) > 1 &&
      req(input$mr.select.sub.var1) != " ") {
      radioButtons("mr.result",
        label = NULL, choices = c("Summary"), selected = "Summary",
        inline = T
      )
    }
  })
})



output$mr.sub1 <- renderUI({
  get.data.set()
  input$mr.select.var
  isolate({
    if (length(req(input$mr.select.var)) > 1) {
      selectInput("mr.select.sub.var1",
        label = "Select subset variable 1:",
        choices = c(" ", names(get.data.set())),
        selectize = F
      )
    }
  })
})


output$mr.sub2 <- renderUI({
  get.data.set()
  input$mr.select.var
  input$mr.select.sub.var1
  isolate({
    if (length(req(input$mr.select.var)) > 1 &&
      req(input$mr.select.sub.var1) != " ") {
      selectInput("mr.select.sub.var2",
        label = "Select subset variable 2:",
        choices = c(" ", names(get.data.set())),
        selectize = F
      )
    }
  })
})


observe({
  if (req(input$mr.select.sub.var1) != " ") {
    isolate({
      ch <- names(get.data.set())
      ch <- c(" ", ch[-which(ch %in% input$mr.select.sub.var1)])
      sel <- input$mr.select.sub.var2
      if (!is.null(sel) && !sel %in% ch) {
        sel <- ch[1]
      }
      updateSelectInput(session, "mr.select.sub.var2",
        choices = ch, selected = sel
      )
    })
  }
})

output$mr.box <- renderUI({
  input$mr.select.var
  input$mr.select.sub.var1
  input$mr.select.sub.var2
  get.data.set()
  isolate({
    if (length(req(input$mr.select.var)) > 1 &&
      req(input$mr.select.sub.var1) != " " &&
      req(input$mr.select.sub.var2) != " ") {
      checkboxInput("mr.box.side",
        label = "Display subset variable 1 Side-by-side", value = FALSE
      )
    }
  })
})

##################
##  main panel  ##
##################


output$mr.ui.main <- renderUI({
  ret <- NULL
  get.data.set()
  input$mr.select.var
  input$mr.select.sub.var1
  input$mr.result
  isolate({
    plot.panel <- tabPanel(
      title = "Plot",
      plotOutput("mr.plot.out", height = "600px"),
      br(),
      fixedRow(
        column(
          width = 3,
          NULL
        ),
        column(
          width = 3,
          downloadButton(outputId = "mrsaveplot", label = "Download Plot")
        ),
        column(
          width = 3,
          radioButtons(
            inputId = "mr.save.plottype.out",
            label = strong("Select the file type"),
            choices = list("jpg", "png", "pdf"), inline = TRUE
          )
        )
      )
    )

    summary.panel <- tabPanel(
      title = "Summary",
      br(),
      br(),
      verbatimTextOutput("mr.summary.out")
    )

    comb.summary.panel <- tabPanel(
      title = "Combinations Summary",
      br(),
      br(),
      verbatimTextOutput("mr.comb.summary.out")
    )


    if (length(req(input$mr.select.var)) > 1 &&
      req(input$mr.result) == "Summary") {
      ret <- list(tabsetPanel(
        type = "pills",
        plot.panel,
        summary.panel
      ))
    } else if (length(req(input$mr.select.var)) > 1 &&
      req(input$mr.select.sub.var1) == " " &&
      req(input$mr.result) == "Combinations") {
      ret <- list(tabsetPanel(
        type = "pills",
        plot.panel,
        summary.panel,
        comb.summary.panel
      ))
    }
  })
})



### download Decomposed Plot
output$mrsaveplot <- downloadHandler(
  filename = function() {
    paste("MultipleResponsePlot",
      switch(input$mr.save.plottype.out,
        "jpg" = "jpg",
        "png" = "png",
        "pdf" = "pdf"
      ),
      sep = "."
    )
  },
  content = function(file) {
    if (input$mr.save.plottype.out %in% c("jpg", "png", "pdf")) {
      if (input$mr.save.plottype.out == "jpg") {
        jpeg(file)
      } else if (input$mr.save.plottype.out == "png") {
        png(file)
      } else if (input$mr.save.plottype.out == "pdf") {
        pdf(file, useDingbats = FALSE)
      }

      suppressWarnings(tryCatch(
        {
          if (req(input$mr.result) == "Summary") {
            setMRobj()
          } else {
            iNZightMR::plotcombn(mr.par$mrObject)
          }
        },
        error = function(e) {
          print(e)
        }, finally = {}
      ))
      dev.off()
    }
  }
)


#########################
##  update parameters  ##
#########################
observe({
  input$mr.box.side
  isolate({
    changePlotSettings(list(sidebyside = input$mr.box.side))
  })
})


observe({
  input$mr.select.sub.var1
  isolate({
    if (req(input$mr.select.sub.var1) != " ") {
      changePlotSettings(list(
        g1 = input$mr.select.sub.var1,
        g1.level = "_MULTI",
        varnames = list(
          g1 = input$mr.select.sub.var1
        )
      ))
    } else {
      changePlotSettings(list(
        g1 = NULL,
        g1.level = NULL,
        varnames = list(
          g1 = NULL
        )
      ), reset = TRUE)
      shinyjs::reset("mr.select.sub.var2")
      shinyjs::reset("mr.box.side")
    }
  })
})


observe({
  input$mr.select.sub.var2
  isolate({
    if (req(input$mr.select.sub.var2) != " ") {
      changePlotSettings(list(
        g2 = input$mr.select.sub.var2,
        g2.level = "_ALL",
        varnames = list(
          g2 = input$mr.select.sub.var2
        )
      ))
    } else {
      changePlotSettings(list(
        g2 = NULL,
        g2.level = NULL,
        varnames = list(
          g2 = NULL
        )
      ), reset = TRUE)
    }
  })
})



output$mr.plot.out <- renderPlot({
  input$mr.select.var
  input$mr.select.sub.var1
  input$mr.select.sub.var2
  input$mr.result
  input$mr.box.side
  isolate({
    if (req(input$mr.result) == "Summary") {
      setMRobj()
    } else {
      mr.par$combp <- iNZightMR::plotcombn(mr.par$mrObject)
    }
  })
})



output$mr.summary.out <- renderPrint({
  input$mr.select.sub.var1
  input$mr.select.var
  input$mr.select.sub.var2
  isolate({
    if (req(input$mr.select.sub.var1) == " ") {
      summary(iNZightMR::mroPara(mr.par$mrObject))
    } else if (req(input$mr.select.sub.var1) != " " &&
      req(input$mr.select.sub.var2) == " ") {
      summary(mr.par$byMRObject, "within")
    } else if (req(input$mr.select.sub.var1) != " " &&
      req(input$mr.select.sub.var2) != " ") {
      summary(mr.par$byMRObject, "between")
    }
  })
})


output$mr.comb.summary.out <- renderPrint({
  input$mr.result
  input$mr.select.var
  isolate({
    mr.par$combp
  })
})



## functions
changePlotSettings <- function(set, reset = FALSE) {
  mr.par$plotSet <- modifyList(mr.par$plotSet, set, keep.null = FALSE)
}
setMRobj <- function() {
  ## Get variables
  binaryVar <- getVars(get.data.set())
  vars <- names(get.data.set())
  responseID <- input$mr.select.var
  if (length(responseID) == 1) {
    mr.par$mrObject <- NULL
    updatePlot()

    return(NULL)
  }

  responseVars <- binaryVar[responseID]

  frm <- as.formula(paste(
    mr.par$objName, "~",
    paste(vars[responseVars], collapse = " + ")
  ))

  mr.par$mrObject <- iNZightMR::iNZightMR(frm,
    data = get.data.set(),
    Labels = substrsplit
  )
  if (mr.par$mrObject$Labels$Commonstr != mr.par$objName && mr.par$guessName) {
    if (!(mr.par$objName == "response" &&
      mr.par$mrObject$Labels$Commonstr == "")) {
      mr.par$objName <- ifelse(
        mr.par$mrObject$Labels$Commonstr == "", "response",
        mr.par$mrObject$Labels$Commonstr
      )
      setMRobj()
      return(NULL)
    }
  }
  updatePlot()
}



## create an MR object and plot it
updatePlot <- function() {
  if (is.null(mr.par$mrObject)) {
    return(NULL)
  }

  if (is.null(mr.par$plotSet$g1)) {
    mro <- iNZightMR::mroPara(mr.par$mrObject)
  } else if (is.null(mr.par$plotSet$g2)) {
    by.formula <- paste("~", mr.par$plotSet$g1)
    mro <- mr.par$byMRObject <- iNZightMR::byMRO(
      mr.par$mrObject, by.formula, mroPara
    )
  } else {
    by.formula <- paste("~", paste(mr.par$plotSet$g1, "+", mr.par$plotSet$g2))
    mro <- mr.par$byMRObject <- iNZightMR::byMRO(
      mr.par$mrObject, by.formula, mroPara
    )
    if (!is.null(mr.par$plotSet$sidebyside)) {
      if (mr.par$plotSet$sidebyside) {
        mro <- iNZightMR::between(mro)
      }
    }
  }
  iNZightMR::barplotMR(mro)
}

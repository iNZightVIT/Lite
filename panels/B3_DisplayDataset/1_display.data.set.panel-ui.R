current.data <- function() {
  ret <- list()
  ret[[1]] <- h4(textOutput("current.text"))
  if (!is.null(session$userData$LITE_VERSION) && session$userData$LITE_VERSION == "CAS") {
    ret[[2]] <- textOutput("data.sample.info")
    ret[[3]] <- br()
    ret[[4]] <- br()
  }
  ret[[5]] <- DTOutput(outputId = "current")
  ret[[6]] <- br()
  ret[[7]] <- br()
  ret[[8]] <- help.display(
    "Current data", "current_data",
    "panels/B3_DisplayDataset/3_display.data.set.panel-help.md"
  )
  ret[[9]] <- br()
  ret[[10]] <- HTML("&nbsp;")
  fluidPage(ret)
}

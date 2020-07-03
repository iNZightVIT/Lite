current.data = function(){
  ret = list()
  ret[[1]] = h4(textOutput("current.text"))
  ret[[2]] = DTOutput(outputId="current")
  ret[[3]] = br()
  ret[[4]] = br()
  ret[[5]] = help.display('Current data','current_data',
                          "panels/B3_DisplayDataset/3_display.data.set.panel-help.md")
  ret[[6]] = br()
  ret[[7]] = HTML("&nbsp;")
  fluidPage(ret)
}
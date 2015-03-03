# help.current = function(){
#   helpModal('Current data','current_data',inclMD("gui-elements/notes/current.data.md"))
# }

current.data = function(){
  ret = list()
  ret[[1]] = h4(textOutput("current.text"))
  ret[[2]] = dataTableOutput(outputId="current")
  ret[[3]] = br()
  ret[[4]] = br()
  ret[[5]] = help.display('Current data','current_data',"gui-elements/notes/current.data.md")
  ret[[6]] = br()
  ret[[7]] = HTML("&nbsp;")
  fluidPage(ret)
}
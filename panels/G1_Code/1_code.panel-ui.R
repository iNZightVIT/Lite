###-------------------###
###  code writing UI  ###
###-------------------###

code.mainPanel = function(){
  mainPanelUI = list(
    uiOutput("r.show.code")
  )## end of mainPanelUI
}

code.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/G1_Code/4_code.panel-null.md")
      )
    } else {
      fluidRow(
        column(12, code.mainPanel())
      )
    }
  )
}
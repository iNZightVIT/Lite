SamplingVariation.mainPanel = function(){
  mainPanelUI = list(
    uiOutput("web")
  )## end of mainPanelUI
}


VIT.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      
    } else {
      fluidRow(
        column(12, SamplingVariation.mainPanel())
      )
    }
  )
}
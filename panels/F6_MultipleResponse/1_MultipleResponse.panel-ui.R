###-----------------------------------------------------###
###  User Interface for the "Multiple Response" Module  ###
###-----------------------------------------------------###
###
###
###  The UI is divided into two parts:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: This is to be sourced within "server.R" *

###----------------###
###  Sidebar Panel ###
###----------------###


MultipleResponse.sidebarPanel <- function(){
  sidebarPanelUI = list(
    useShinyalert(),
    useShinyjs(),
    fluidRow(
      column(12,
             uiOutput("mr.var"),
             uiOutput("mr.type"),
             uiOutput("mr.sub1"),
             uiOutput("mr.sub2"),
             uiOutput("mr.box"))
    )
  )## end of list
}

### now, we set up the main panel
MultipleResponse.mainPanel = function(){
  mainPanelUI = list(
    uiOutput("mr.ui.main")
  )## end of mainPanelUI
}

###------------------------###
###  Multiple Response UI  ###
###------------------------###

###  We combine the 2 sidebarPanel() and 2 mainPanel() functions to
###  complete the UI for the Mixed Model module.

MultipleResponse.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fluidRow(
        includeMarkdown(
          "panels/F6_MultipleResponse/4_MultipleResponse-panel-null.md")
      )
    } else {
      fluidRow(
        column(3, MultipleResponse.sidebarPanel()),
        column(9, MultipleResponse.mainPanel())
      )
    }
  )
}






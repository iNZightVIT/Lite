###-------------------------------------------###
###  Server Functions for the "About" Module  ###
###-------------------------------------------###
###
###  Date Created   :   February 10, 2015
###  Last Modified  :   February 25, 2015
###
###  Please consult the comments before editing any code.
###

output$about.panel <- renderUI({
  get.vars = parseQueryString(session$clientData$url_search)
  if(length(get.vars)>0&&
       (any(names(get.vars)%in%"url")||
          any(names(get.vars)%in%"example"))){
    data.vals = NULL
    if(!is.null(get.vars$url)&&
         get.vars$url!=""){
      data.vals = get.data.from.URL(get.vars$url,get.data.dir.imported())
    }
    if(!is.null(get.vars$example)&&
         get.vars$example!=""){
      data.vals = load.data("data",get.vars$example)
    }
    if(!is.null(data.vals)){
      values$data.set = data.vals$data.set
      values$data.restore = get.data.set()
      values$data.name = data.vals$data.name
      if(!is.null(get.data.set())&&
           "land"%in%names(get.vars)&&
           get.vars$land!=""&&
           get.vars$land%in%"visualize"){
        updateTabsetPanel(session,"selector","visualize")
      }else if(!is.null(get.data.set())&&
                 "land"%in%names(get.vars)&&
                 get.vars$land!=""&&
                 get.vars$land%in%"timeSeries"){
        updateTabsetPanel(session,"selector","timeSeries")
      }else if(!is.null(get.data.set())&&
                 "land"%in%names(get.vars)&&
                 get.vars$land!=""&&
                 get.vars$land%in%"regression"){
        updateTabsetPanel(session,"selector","regression")
      }
    }
  }
  about.panel.ui(get.lite.version(),get.lite.update())
})

# pop up for change log
observeEvent(input$change_log_link, {
  change_log <- includeMarkdown("NEWS.md")

  showModal(modalDialog(
    title = "Change Log",
    renderUI(change_log),
    easyClose = TRUE
  ))

})

observeEvent(input$disconnect, {
  session$close()
})

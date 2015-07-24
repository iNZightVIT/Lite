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
       any(names(get.vars)%in%"url")&&
       !is.null(get.vars$url)&&
       get.vars$url!=""){
    data.vals = get.data.from.URL(get.vars$url,get.data.dir.imported())
    if(!is.null(data.vals)){
      values$data.set = data.vals$data.set
      values$data.restore = get.data.set()
      values$data.name = data.vals$data.name
      if(!is.null(get.data.set())){
        updateTabsetPanel(session,"selector","visualize")
      }
    }
  }
  about.panel.ui(get.lite.version(),get.lite.update())
})

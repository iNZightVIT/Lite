###-------------------------------------------###
###  Server Functions for the "About" Module  ###
###-------------------------------------------###
###
###  Date Created   :   February 10, 2015
###  Last Modified  :   February 25, 2015
###
###  Please consult the comments before editing any code.
###
library(openssl)
library(wkb)

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

      values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set)/4))
      values$sample.row = sample(1:nrow(values$data.set), values$sample.num)
      values$data.sample = as.data.frame(values$data.set[values$sample.row,])


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
  } else if (length(get.vars)>0&&
             (any(names(get.vars)%in%"filename")||
              any(names(get.vars)%in%"iv"))){
    data.vals = NULL

    f.name = rawToChar(aes_cbc_decrypt(base64_decode(get.vars$filename),
                                       hex2raw(Sys.getenv("LITE_KEY"))),
                                       hex2raw(get.vars$iv)))

    get.vars$url = paste0(Sys.getenv("LITE_URL"), f.name)
    data.vals = get.data.from.URL(get.vars$url,get.data.dir.imported())
    if(!is.null(data.vals)){
      values$data.set = as.data.frame(data.vals$data.set)
      values$data.restore = get.data.set()
      values$data.name = "data"

      values$sample.num = ifelse(nrow(values$data.set) > 2000, 500, round(nrow(values$data.set)/4))
      values$sample.row = sample(1:nrow(values$data.set), values$sample.num)
      values$data.sample = as.data.frame(values$data.set[values$sample.row,])

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

observe({
  if(!is.null(values$sample.row)){
    row.names(values$data.sample) = 1:length(values$sample.row)
    colnames(values$data.sample) = colnames(values$data.set)
  }
})

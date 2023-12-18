# puts together a list of shiny widgets to fill the sidebar
get.sidebar.switch = function(data.dir.global,data.dir.imported){
  choices1=get.data.dirs(data.dir.global)
  choices2=get.data.dirs(data.dir.imported)
  if(!data.dir.imported%in%data.dir.global){
    ret = list(selectInput(inputId="data_select",
                           label="Select Data set category",
                           choices=c(basename(choices1),basename(choices2)),
                           selected=1))
    for(i in 1:length(choices1)){
      radio.list = get.radio.list(choices1[i],"")
      if(!is.null(radio.list)){
        ret[[i+1]] = conditionalPanel(condition=paste0("input.data_select=='",
                                                       basename(choices1[i]),"'"),
                                      radio.list)
      }
    }
    radio.list = get.radio.list(choices2,"")
    if(!is.null(radio.list)){
      ret[[length(ret)+1]] = conditionalPanel(condition=paste0("input.data_select=='",
                                                               basename(choices2),"'"),
                                              radio.list)
    }
  }else{
    ret = list(selectInput(inputId="data_select",
                           label="Select Data set category",
                           choices=basename(choices1),
                           selected=1))
    for(i in 1:length(choices1)){
      radio.list = get.radio.list(choices1[i],"")
      if(!is.null(radio.list)){
        ret[[i+1]] = conditionalPanel(condition=paste0("input.data_select=='",
                                                       basename(choices1[i]),"'"),
                                      radio.list)
      }
    }
  }
  ret[[length(ret)+1]] = actionButton(inputId="change_set",label="Select Set")
  ret
}

get.switch.data.main = function(data.set){
  if(is.null(data.set)){
    list(div(class="page-divider"),
         h3(textOutput("data_name_show")),
         p(textOutput("row_dimension_show")),
         p(textOutput("col_dimension_show")),
         p(textOutput("col_names_show")),br(),
         div(class="page-divider"),
         h3(textOutput("set_to_change")),br(),
         DTOutput("temp_table"),
         div(class="page-divider"))
  }else{
    list(div(class="page-divider"),
         h3(textOutput("data_name_show")),
         p(textOutput("row_dimension_show")),
         p(textOutput("col_dimension_show")),
         p(textOutput("col_names_show")),br(),
         div(class="page-divider"),
         h3(textOutput("set_to_change")),
         DTOutput("temp_table"),
         div(class="page-divider"))
  }
}

#switch.data.panel creates reactive panel for input files
switch.data.panel = function(data.set,data.dir.global,data.dir.imported){
  sidebar.widgets = get.sidebar.switch(data.dir.global,data.dir.imported)
  sidebarLayout(
    sidebarPanel(sidebar.widgets,br(),br(),
                 help.display('Dataset Examples','switch_data',
                              "panels/B5_DatasetExamples/3_data.set.examples-help.md")
                 ,br(),
                 HTML("&nbsp;"),br()),
    mainPanel(get.switch.data.main(data.set)))
}

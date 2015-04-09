# help.switch = function(){
#   helpModal('Switch data','switch_data',inclMD("gui-elements/notes/switch.data.md"))
# }

# puts together a list of shiny widgets to fill the sidebar
get.sidebar.switch = function(){
  choices1=get.data.dirs(data.dir)
  ret = list(selectInput(inputId="data_select",
                         label="Select Data set category",
                         choices=basename(choices1),
                         selected=basename(choices1[1])))
  for(i in 1:length(choices1)){
    radio.list = get.radio.list(choices1[i],"")
    if(!is.null(radio.list)){
      ret[[i+1]] = conditionalPanel(condition=paste0("input.data_select=='",basename(choices1[i]),"'"),
                                    radio.list)
    }
  }
  ret[[length(ret)+1]] = actionButton(inputId="change_set",label="Select Set")
  ret
}

get.switch.data.main = function(has.input,data.set){
  if(is.null(data.set)){
    if(!has.input){
      list(div(class="page-divider"),
           h3(textOutput("data_name_show")),
           p(textOutput("row_dimension_show")),
           p(textOutput("col_dimension_show")),
           p(textOutput("col_names_show")),br(),
           div(class="page-divider"),
           h3(textOutput("set_to_change")),br(),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }else{
      list(div(class="page-divider"),
           h3(textOutput("data_name_show")),
           p(textOutput("row_dimension_show")),
           p(textOutput("col_dimension_show")),
           p(textOutput("col_names_show")),br(),
           div(class="page-divider"),
           h3(textOutput("set_to_change")),br(),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }
  }else{
    if(!has.input){
      list(div(class="page-divider"),
           h3(textOutput("data_name_show")),
           p(textOutput("row_dimension_show")),
           p(textOutput("col_dimension_show")),
           p(textOutput("col_names_show")),br(),
           div(class="page-divider"),
           h3(textOutput("set_to_change")),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }else{
      list(div(class="page-divider"),
           h3(textOutput("data_name_show")),
           p(textOutput("row_dimension_show")),
           p(textOutput("col_dimension_show")),
           p(textOutput("col_names_show")),br(),
           div(class="page-divider"),
           h3(textOutput("set_to_change")),
           dataTableOutput("temp_table"),
           div(class="page-divider"))
    }
  }
}

#switch.data.panel creates reactive panel for input files
switch.data.panel = function(data.set){
  sidebar.widgets = get.sidebar.switch()
  sidebarLayout(
    sidebarPanel(sidebar.widgets,br(),br(),
                 help.display('Switch data','switch_data',"gui-elements/notes/switch.data.md")
                 ,br(),
                 HTML("&nbsp;"),br()),
    mainPanel(get.switch.data.main(T,data.set)))
}

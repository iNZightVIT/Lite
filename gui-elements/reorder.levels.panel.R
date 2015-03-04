# help.reorder = function(){
#   helpModal('Reorder Levels','reorder_levels',inclMD("gui-elements/notes/reorder.levels.md"))
# }

get.reorder.sidebar =  function(choices2=c(),selected1=""){
  choices1 = c()
  if(!is.null(data)&&!is.null(ncol(data))&&ncol(data)>0){
    choices1 = colnames(data)[unlist(lapply(1:ncol(data),function(index,data){(is.factor(data[,index])|is.character(data[,index]))},data))]
  }else{
    choices2=c()
  }
  list(helpText("Select a column from the first dropdown menu. The second dropdown menu will be filled when a column is 
                selected. Select from the second dropdown menu in the desired order. Numeric values are ignored, 
                therefore columns of type numeric can not be selected. Please convert those to factors first. See 
                \"HELP\" below for more information."),
       selectInput("select.column","Select Column",choices=c("",choices1),multiple=F,selectize=F,selected=selected1),br(),
       selectInput("select.item", "Select in new Order",choices=choices2,multiple=T,selectize=T),br(),
       actionButton("reorder","Reorder"),br(),br(),
       help.display('Reorder Levels','reorder_levels',"gui-elements/notes/reorder.levels.md"),br(),HTML(""))
}

reorder.levels.panel =function(choices=c(),selected=""){
  if(is.null(data)){
    sidebarLayout(
      sidebarPanel(get.reorder.sidebar(choices)),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(get.reorder.sidebar(choices)),
      mainPanel(div(verbatimTextOutput("maintext.reorder")))
    )
  }
}
# categorical.main.panel = function(categorical_variables_select){
#   if(!is.null(categorical_variables_select)&&
#        categorical_variables_select%in%"Reorder levels"){
#     reorder.main.panel()
#   }else if(!is.null(categorical_variables_select)&&
#              categorical_variables_select%in%"Collapse levels"){
#     collapse.main.panel()
#   }else if(!is.null(categorical_variables_select)&&
#              categorical_variables_select%in%"Rename levels"){
#     rename.levels.main.panel()
#   }else if(!is.null(categorical_variables_select)&&
#              categorical_variables_select%in%"Combine categorical"){
#     combine.main.panel()
#   }
# }
# 
# categorical.side.panel = function(categorical_variables_select=NULL){
#   if(!is.null(categorical_variables_select)&&
#        categorical_variables_select%in%"Reorder levels"){
#     reorder.sidebar.panel()
#   }else if(!is.null(categorical_variables_select)&&
#              categorical_variables_select%in%"Collapse levels"){
#     collapse.sidebar.panel()
#   }else if(!is.null(categorical_variables_select)&&
#              categorical_variables_select%in%"Rename levels"){
#     rename.levels.sidebar.panel()
#   }else if(!is.null(categorical_variables_select)&&
#              categorical_variables_select%in%"Combine categorical"){
#     combine.sidebar.panel()
#   }
# }

get.categorical.variables =  function(categorical_variables_select=NULL,data.set){
  if(is.null(data.set)){
    list(list(help.display('Categorical variables','categorical_variables',
                           "gui-elements/notes/categorical.variables.md"),
              br(),HTML("")),
         h1("Please select or import a data set."))
  }else{
    choices=c("Reorder levels",
              "Collapse levels",
              "Rename levels",
              "Combine categorical")
    list(list(selectInput(inputId="categorical_variables_select",
                          label="Categorical variables",
                          choices=choices,
                          selectize=F,
                          selected=which(choices%in%categorical_variables_select)),
              uiOutput("categorical.side.panel")
              ),
         uiOutput("categorical.main.panel")
         )
  }
}

categorical.variables.panel = function(categorical_variables_select,data.set){
  sidebarLayout(
    sidebarPanel(get.categorical.variables(categorical_variables_select,data.set)[[1]]),
    mainPanel(get.categorical.variables(categorical_variables_select,data.set)[[2]])
  )
}
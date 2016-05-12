## Quick explore startup

get.quick.explore =  function(data.set){
  list(list(selectInput(inputId="quick_explore_select",
                        label="Quick explore",
                        choices=c("Missing values",
                                  "Data Summary",
                                  "All 1-variable plots",
                                  "Explore 2-variable plots",
                                  "Compare pairs"),
                        selectize=F),
            conditionalPanel("input.quick_explore_select=='Missing values'",
                             uiOutput("quick.missing.summary.side")),
            conditionalPanel("input.quick_explore_select=='Data Summary'",
                             uiOutput("quick.summary.side")),
            conditionalPanel("input.quick_explore_select=='All 1-variable plots'",
                             uiOutput("single.column.plot.side")),
            conditionalPanel("input.quick_explore_select=='Explore 2-variable plots'",
                             uiOutput("column.pair.plot.side")),
            conditionalPanel("input.quick_explore_select=='Compare pairs'",
                             uiOutput("compare.pairs.side"))),
            list(conditionalPanel("input.quick_explore_select=='Missing values'",
                                  uiOutput("quick.missing.summary.main")),
                 conditionalPanel("input.quick_explore_select=='Data Summary'",
                                  uiOutput("quick.summary.main")),
                 conditionalPanel("input.quick_explore_select=='All 1-variable plots'",
                                  uiOutput("single.column.plot.main")),
                 conditionalPanel("input.quick_explore_select=='Explore 2-variable plots'",
                                  uiOutput("column.pair.plot.main")),
                 conditionalPanel("input.quick_explore_select=='Compare pairs'",
                                  uiOutput("compare.pairs.main"))))
}

quick.explore.panel = function(data.set){
  if(is.null(data.set)){
    sidebarLayout(
      sidebarPanel(help.display('Quick explore',
                                'quick_explore',
                                "panels/F1_QuickExplore/3_quick.explore.help.md")),
      mainPanel(h1("Please select or import a data set."))
    )
  }else{
    sidebarLayout(
      sidebarPanel(get.quick.explore(data.set)[[1]]),
      mainPanel(get.quick.explore(data.set)[[2]])
    )
  }
}

# Missing values

get.quick.missing.summary.side = function(data.set,data.name){
  list(get.table.button(data.set,
                        "missing_values_table",
                        paste("Missing values combination of \"",
                              data.name,"\"")),
       br(),br(),
       help.display('Missing Values','missing_values',
                    "panels/F1_QuickExplore/4_missing.values.help.md"))
}

get.table.button = function(data.set,id,title){
  tex = " "
  tab = get.combinations(data.set,T)
  if(!is.null(tab)){
    if(!is.null(tab)&&nrow(tab)>0&&ncol(tab)>0){
      tex = "<table style='width: 500px; overflow-y: scroll;'><th><td>"
      tex = paste(tex,paste(paste(colnames(tab),collapse="</td><td>"),sep=""),sep="")
      tex = paste(tex,"</td></th>",sep="")
      for(row in 1:nrow(tab)){
        tex = paste(tex,"<tr><td></td><td>",paste(as.character(tab[row,]),collapse="</td><td>"),"</td></tr>",sep="")
      }
      tex = paste(tex,"</table>",sep="")
    }
    if(length(tex)==0){
      tex=" "
    }
  }else{
    tex = "No NA values in data."
  }
  HTML(paste("<div class='modal fade' id='", id, "' tabindex='-1' role='dialog' aria-labelledby='basicModal' aria-hidden='true'>
             <div class='modal-dialog'>
             <div class='modal-content'>
             <div class='modal-header'>
             <h4 class='modal-title' id='myModalLabel'>",title,"</h4>
             </div>
             <div class='modal-body'>
             <div style='overflow: auto'>",
             tex,
             "</div>
             </div>
             <div class='modal-footer'>
             </div>
             </div>
             </div>
             </div>
             <a href='#' class='btn btn-xs btn btn-primary' data-toggle='modal' data-target='#", id, "'>Get table of row combinations</a>", sep = ""))
}

get.quick.missing.summary.main = function(data.set){
  plotOutput("quick.missing.summary.plot")
}

# quick summary

get.quick.summary.sidebar =  function(data.set){
  choices1 = c()
  if(!is.null(data.set)&&!is.null(ncol(data.set))&&ncol(data.set)>0){
    choices1 = colnames(data.set)
  }else{
    choices2=c()
  }
  list(helpText("Select a column from the dropdown menu to display a short column summary."),
       selectInput("select.column.sum","Select Column",choices=choices1,multiple=F,
                   selectize=T,selected=choices1[1]),
       br(),br(),help.display('Quick summary','quick_summary',
                              "panels/F1_QuickExplore/5_quick.summary.help.md"),
       br(),HTML(""))
}
get.quick.summary.main = function(){
  list(mainPanel(verbatimTextOutput("all.summary"),
                 verbatimTextOutput("column.summary")))
}

# single column


get.single.col.sidebar <- function(data.set){
  choices1 = ""
  if (!is.null(data.set) && !is.null(ncol(data.set)) && ncol(data.set) > 0){
    choices1 = colnames(data.set)
  } else {
    choices2 = ""
  }
  list(helpText("Select a column from the dropdown menu to display a
                appropriate plot of the selected column."),
       selectInput("select.column.plot", "Select Column", choices = choices1,
                   multiple = FALSE, selectize = TRUE,
                   selected = choices1[1]),
       br(),
       help.display('Single column plot','single_column_plot',
                    "panels/F1_QuickExplore/6_single.column.plot.help.md"),
       br(),
       HTML(""))
}

get.single.col.main <- function(data.set){
  list(plotOutput("column.plot"),
       get.player(ID.backward="single.backward",
                  ID.player="single.play",
                  ID.forward="single.forward",
                  maxi=ncol(data.set)))
}

# 2 columns

get.pair.plot.sidebar =  function(data.set){
  choices1 = c()
  if(!is.null(data.set)&&!is.null(ncol(data.set))&&ncol(data.set)>0){
    choices1 = colnames(data.set)
  }else{
    choices1=c()
  }
  list(helpText("Select a column from the first and second 
                drop down menu to display a appropriate 
                plot of the selected columns."),
       selectInput("select.column.plot1","Select Column 1",
                   choices=choices1,multiple=F,selectize=T,
                   selected=choices1[1]),
       selectInput("select.column.plot2","Select Column 2",
                   choices=choices1[-1],multiple=F,selectize=T,
                   selected=choices1[-1][1]),
       br(),br(),help.display('Column pair plot',
                              'column_pair_plot',
                              "panels/F1_QuickExplore/7_column.pair.plot.help.md"),
       br(),HTML(""))
}

get.pair.plot.main = function(data.set){
  list(plotOutput("plot.column.pair"),
       get.player(ID.forward="pair.forward",
                  ID.player="pair.player",
                  ID.backward="pair.backward",
                  maxi=ncol(data.set)*(ncol(data.set)-1)))
}

# matrix plot

get.matrix.sidebar =  function(data.set){
  choices1 = c()
  if(!is.null(data.set)&&!is.null(ncol(data.set))&&ncol(data.set)>0){
    choices1 = colnames(data.set)
  }else{
    choices2=c()
  }
  list(helpText("Select a column from the dropdown 
                menu to display all posible pair 
                combination plots."),
       selectInput("select.matrix.plot","Select Columns",
                   choices=choices1,multiple=T,selectize=T,
                   selected=choices1[1]),
       br(),selectInput("grpVar",
                        label = "Select grouping variable",
                        choices=c(" ",get.categorical.column.names(data.set)),
                        selected=" "),
       br(),help.display('Pair matrix plot',
                         'pair_matrix_plot',
                         "panels/F1_QuickExplore/8_matrix.plot.help.md"),
       br(),HTML(""))  
}

get.matrix.main = function(data.set){
  tabPanel(NULL,plotOutput("plot.matrix", height = 700, width = 700))
}

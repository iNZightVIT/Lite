rank.numeric.sidebar = function(data.set){
  list(selectInput("rank.numeric.select.column",
                   label="Select one or more columns",
                   selected=NULL,multiple=T,
                   choices=get.numeric.column.names(data.set)),
       br(),
       actionButton("rank.numeric.submit",label="Rank Numeric"),
       br(),br(),
       help.display('Rank Numeric','rank_numeric',
                    "gui-elements/notes/rank.numeric.md"))
}

rank.numeric.main = function(){
  dataTableOutput("rank.numeric.table")
}
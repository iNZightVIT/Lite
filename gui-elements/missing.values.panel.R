get.quick.missing.summary.side = function(data.set,data.name){
  if(is.null(data.set)){
    list(help.display('Missing Values','missing_values',
                      "gui-elements/notes/missing.values.md"))
  }else{
    list(get.table.button(data.set,
                          "missing_values_table",
                          paste("Missing values combination of \"",
                                data.name,"\"")),
         br(),br(),
         help.display('Missing Values','missing_values',
                      "gui-elements/notes/missing.values.md"))
  }
}

get.table.button = function(data.set,id,title){
  tex = " "
  tab = get.combinations(data.set,T,T)
  if(!is.null(tab)){
    tab["total",] = unlist(lapply(1:ncol(tab),function(i,ta){
      if(is.numeric(ta[,i])){
        sum(ta[,i])
      }else if(is.character(ta[,i])){
        length(which(ta[,i]%in%"missing"))
      }else{
        0
      }
    },tab)) 
    tab[,"Freq"] = round(tab[,"counts"]/nrow(data.set),digits=3)
    if(!is.null(tab)&&nrow(tab)>0&&ncol(tab)>0){
      tex = "<table style='width:100%'><th><td>"
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
             <div class='modal-body'>",
             tex,
             "</div>
             <div class='modal-footer'>
             </div>
             </div>
             </div>
             </div>
             <a href='#' class='btn btn-xs btn btn-primary' data-toggle='modal' data-target='#", id, "'>Get table of row combinations</a>", sep = ""))
}

get.quick.missing.summary.main = function(data.set){
  if(is.null(data.set)){
    h1("Please select or import a data set.")
  }else{
    plotOutput("quick.missing.summary.plot")
  }
}
#' Creates a widget for moving through plots quickly.
#'
#' @param ID.forward inputID for the forward button in the player widget
#' @param ID.player inputID for the slider in the player widget
#' @param ID.backward inputID for the backward button in the player widget
#'
#' @author Christoph Knapp
get.player = function(ID.forward,ID.player,ID.backward,maxi){
    fixedRow(column(width=8,offset=2,
                div(class='player',
                    fixedRow(
                        column(width=1,offset=1,
                        div(class="seper",
                        actionButton(inputId=ID.backward,
                                    label="",icon=icon("backward")))),
                        column(width=6,offset=1,
                            sliderInput(inputId=ID.player,label="",min=1,max=maxi,step=1,
                                        animate=animationOptions(interval=500,loop=T,play=T),
                                        width="100%",value=1,ticks=F)),
                        column(width=1,offset=1,
                            div(class="seper",actionButton(inputId=ID.forward,label="",icon=icon("forward"))))
            ))))
}

change.factor.transform = function(temp,columns){
    temp = as.data.frame(temp)
    nums = unlist(lapply(1:ncol(temp),function(index,temp.data,columns){
      if(is.numeric(temp.data[,index])){
        columns[index]
      }else{
        NULL
      }
    },temp,columns))
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(temp),function(index,temp.data){
      if(is.numeric(temp.data[,index])){
        as.character(temp.data[,index])
      }else{
        NULL
      }
    },temp)),stringsAsFactors=T)
    if(!is.null(temp)&&ncol(temp)>0&&nrow(temp)>0){
      colnames(temp) = paste("factors",nums,sep="_")
      temp
    }else{
      NULL
    }
}

change.sign.transform = function(data,columns){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            as.matrix(data[,index]*(-1))
        }else{
            NULL
        }
    },data)))
    if(!is.null(temp)){
        colnames(temp) = paste("change_sign",columns[unlist(lapply(1:ncol(data),function(i,data){is.numeric(data[,i])},data))],sep=".")
    }
    temp
}

test.for.dates = function(){
    ret = F
    if(!is.null(data)){
        ret = unlist(lapply(
            1:ncol(data),function(index,data){
                tryCatch({
                    is.numeric(as.numeric(as.Date(data[,index], origin = "1900-01-01")))
                },
                         error=function(cond) {
                             ret = F
                         },
                         warning=function(cond) {
                             print(cond)
                         },
                         finally={})
            },data))
    }
    ret
}

copy.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = paste("copy",columns,sep=".")
    data
}

reverse.coding.transform = function(data,columns){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            min(data[,index],na.rm=T)+max(data[,index],na.rm=T)-data[,index]
        }else{
            NULL
        }
    },data)))
    if(!is.null(temp)){
        colnames(temp) = paste("reverse_coding",columns[unlist(lapply(1:ncol(data),function(i,data){is.numeric(data[,i])},data))],sep=".")
    }
    temp
}

median.split.transform = function(temp,columns){
    temp = as.data.frame(temp)
    nums = unlist(lapply(1:ncol(temp),function(index,temp){is.numeric(temp[,index])},temp))
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(temp),function(index,temp){
      if(is.numeric(temp[,index])){
        med = median(temp[,index],na.rm=T)
        ret = rep("high",length(temp[,index]))
        ret[which(temp[,index]<=med)] = "low"
        ret
      }else{
        NULL
      }
    },temp)),stringsAsFactors=T)
    colnames(temp) = paste("median_split",columns[nums],sep="_")
    temp
}

standardize.transform = function(data,columns){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            (data[,index]-mean(data[,index],na.rm=T))/sd(data[,index],na.rm=T)
        }else{
            (as.numeric(factor(data[,index]))-mean(as.numeric(factor(data[,index])),na.rm=T))/sd(as.numeric(factor(data[,index])),na.rm=T)
        }
    },data)))
    colnames(temp) = paste("standardize",columns,sep=".")
    temp
}

center.transform = function(data,columns){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            data[,index]-mean(data[,index])
        }else{
            as.numeric(factor(data[,index]))-mean(as.numeric(factor(data[,index])))
        }
    },data)))
    colnames(temp) = paste("center",columns,sep=".")
    temp
}

divide.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = columns
    if(is.null(data)){
        return(NULL)
    }else{
        if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))==1){
            temp = as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))])
            colnames(temp) = colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
        }else if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))>1){
            temp = as.data.frame(divide(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))
            colnames(temp) = paste0("divide.",paste(colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))],collapse="."))
        }else{
            return(NULL)
        }
    }
    temp
}

divide = function(data){
    data = data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
    data = as.data.frame(data)
    if(ncol(data)==1){
        data[,1]
    }else{
        start = data[,1]
        for(col in 2:ncol(data)){
            start = start/data[,col]
        }
        start
    }
}

multiply.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = columns
    if(is.null(data)){
        return(NULL)
    }else{
        if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))==1){
            temp = as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))])
            colnames(temp) = colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
        }else if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))>1){
            temp = as.data.frame(multiply(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))
            colnames(temp) = paste0("multiply.",paste(colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))],collapse="."))
        }else{
            return(NULL)
        }
    }
    temp
}

multiply = function(data){
    data = data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
    data = as.data.frame(data)
    if(ncol(data)==1){
        data[,1]
    }else{
        start = data[,1]
        for(col in 2:ncol(data)){
            start = start*data[,col]
        }
        start
    }
}

subtract.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = columns
    if(is.null(data)){
        return(NULL)
    }else{
        if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))==1){
            temp = as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))])
            colnames(temp) = colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
        }else if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))>1){
            temp = as.data.frame(subtract(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))
            colnames(temp) = paste0("subtract.",paste(colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))],collapse="."))
        }else{
            return(NULL)
        }
    }
    temp
}

subtract = function(data){
    data = data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
    data = as.data.frame(data)
    if(ncol(data)==1){
        data[,1]
    }else{
        start = data[,1]
        for(col in 2:ncol(data)){
            start = start-data[,col]
        }
        start
    }
}

add.transform  = function(temp,columns){
  temp = as.data.frame(temp)
  colnames(temp) = columns
  if(is.null(temp)){
    return(NULL)
  }else{
    ret = as.data.frame(temp[,unlist(lapply(1:ncol(temp),function(index,d)(is.numeric(d[,index])),temp))])
    if(ncol(ret)>1){
      ret = as.data.frame(apply(ret,1,function(row){sum(row)}))
      colnames(ret) = paste0("add_",paste(colnames(temp),collapse="_"))
    }else{
      return(NULL)
    }
  }
  ret
}

# returns the transformed columns and the original data as 
# dataframe (cbind(data,<transformed columns>)).
transform.perform = function(type,columns){
  temp = transform.get.temp(type,columns)
  if(!is.null(temp)){
    temp = cbind(data,temp)
  }
  temp
}

# returns the transformed columns and the original columns as 
# dataframe (cbind(<original columns>,<transformed columns>)).
transform.tempTable = function(type,columns){
  temp1 = as.data.frame(data[,which(colnames(data)%in%columns)])
  temp2 = transform.get.temp(type,columns)
  if(!is.null(temp2)){
    temp1 = cbind(temp1,temp2)
  }
  temp1
}

# transorms the columns named columns in data with the selected 
# type (type) of transformation.
transform.get.temp = function(type,columns){
  temp = NULL
  if (!is.null(columns) && type%in%"log"){
    temp = log.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"add"){
    temp = add.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"subtract"){
    temp = subtract.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"multiply"){
    temp = multiply.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"divide"){
    temp = divide.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"root"){
    temp = root.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"square"){
    temp = square.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"abs"){
    temp = abs.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"center"){
    temp = center.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"standardize"){
    temp = standardize.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"median split"){
    temp = median.split.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"reverse-coding"){
    temp = reverse.coding.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"copy"){
    temp = copy.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"change sign"){
    temp = change.sign.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%"change to factor"){
    temp = change.factor.transform(data[,columns],columns)
  } else if (!is.null(columns)&type%in%""){
    temp = NULL
  }
  temp
}

log.transform = function(tempdata,columns){
    tempdata = as.data.frame(tempdata)
    colnames(tempdata) = columns
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(tempdata),function(index,tempdata){
        if(is.numeric(tempdata[,index])){
            log(tempdata[,index])
        }else{
            NULL
        }
    },tempdata)))
    if(!is.null(temp)&&dim(temp)[1]>0&&dim(temp)[2]>0){
        colnames(temp) = unlist(lapply(1:ncol(tempdata),function(index,tempdata){
            if(is.numeric(tempdata[,index])){
                paste0("log.",colnames(tempdata)[index])
            }else{
                NULL
            }
        },tempdata))
        temp
    }else{
        NULL
    }
}

root.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = columns
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            sqrt(data[,index])
        }else{
            NULL
        }
    },data)))
    ##  temp = as.data.frame(temp)
    if(dim(temp)[1]>0&&dim(temp)[2]>0){
        colnames(temp) = unlist(lapply(1:ncol(data),function(index,data){
            if(is.numeric(data[,index])){
                paste0("root.",colnames(data)[index])
            }else{
                NULL
            }
        },data))
        temp
    }else{
        NULL
    }
}

square.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = columns
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            data[,index]^2
        }else{
            NULL
        }
    },data)))
    ##  temp = as.data.frame(temp)
    if(dim(temp)[1]>0&&dim(temp)[2]>0){
        colnames(temp) = unlist(lapply(1:ncol(data),function(index,data){
            if(is.numeric(data[,index])){
                paste0("square.",colnames(data)[index])
            }else{
                NULL
            }
        },data))
        temp
    }else{
        NULL
    }
}

abs.transform = function(data,columns){
    data = as.data.frame(data)
    colnames(data) = columns
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            abs(data[,index])
        }else{
            NULL
        }
    },data)))
                                        #   temp = as.data.frame(temp)
    if(dim(temp)[1]>0&&dim(temp)[2]>0){
        colnames(temp) = unlist(lapply(1:ncol(data),function(index,data){
            if(is.numeric(data[,index])){
                paste0("abs.",colnames(data)[index])
            }else{
                NULL
            }
        },data))
        temp
    }else{
        NULL
    }
}

delete.old.files = function(days){
    if(length(list.files(paste0(data.dir,"/Imported")))>0){
        unlink(list.files(paste0(data.dir,"/Imported"))[difftime(Sys.time(), file.info(list.files(paste0(data.dir,"/Imported"),full.name=T))[,"mtime"], units = "days")>days])
    }
}

###  A function for displaying help messages.

help.display = function(title, id, file) {
  HTML(paste("<div class='modal fade' id='", id, "' tabindex='-1' role='dialog' aria-labelledby='basicModal' aria-hidden='true'>
             <div class='modal-dialog'>
             <div class='modal-content'>
             <div class='modal-header'>
             <h4 class='modal-title' id='myModalLabel'>",title,"</h4>
             </div>
             <div class='modal-body'>",
             markdownToHTML(
                 file = file,
                 options = c(""),
                 stylesheet = "www/empty.css"),
             "</div>
             <div class='modal-footer'>
             </div>
             </div>
             </div>
             </div>
             <a href='#' class='btn btn-xs btn-success' data-toggle='modal' data-target='#", id, "'>Help</a>", sep = ""))
}

## reads a data set from a filename in the data directory
load.data = function(fileID=NULL,path=NULL){
    temp = NULL
    full.name = list.files(data.dir,full.names=T,recursive=T)
    if(!is.null(fileID)){
        if(is.null(path)){
            indexes = grep(paste(fileID,".",sep=""),full.name,fixed=T)
        }else if(!is.null(path)&file.exists(path)){
            full.name = path
            indexes = 1
        }else{
            return(list(NULL,NULL))
        }
        if(length(indexes[1])>0){
            ext = strsplit(full.name[indexes[1]],".",fixed=T)[[1]]
            ext = ext[length(ext)]
            if(!(tolower(ext)%in%c("rds","rda","rdata","csv","txt"))){
                ext = strsplit(fileID,".",fixed=T)[[1]]
                ext = ext[length(ext)]
            }
            if(!file.exists(full.name[indexes[1]])){
              return(list(NULL,NULL))
            }
            if(tolower(ext)%in%"rds"){
                temp = readRDS(file=full.name[indexes[1]])
            }else if(tolower(ext)%in%"rda"|tolower(ext)%in%"rdata"){
                name = load(full.name[indexes[1]])
                temp = get(name)
            }else if(tolower(ext)%in%"csv"){
                temp = read.csv(full.name[indexes[1]])
            }else if(tolower(ext)%in%"txt"){
                temp = read.delim(full.name[indexes[1]])
            }
        }
    }
    if(is.null(fileID)){
        list(NULL,temp)
    }else{
        list(basename(fileID),temp)
    }
}

## returns directories in the data directory
get.data.dirs = function(){
    list.files(data.dir,include.dirs=T,full.names=T)[file.info(paste(data.dir,list.files(data.dir),sep="/"))[,"isdir"]]
}

## returns a radioButton widget, for every filename in the dir.lable directory.
get.radio.list = function(dir.label,idlabel){
    files = c()
    files = list.files(dir.label,recursive=T,full.name=T)[!(file.info(list.files(dir.label,recursive=T,full.names=T))[,"isdir"])]
    temp.files = strsplit(files,"/")
    files = unlist(lapply(temp.files,
        function(x,label){
            paste(x[(which(x%in%label)+1):length(x)],collapse="==>")
        },strsplit(dir.label,"/",fixed=T)[[1]][length(strsplit(dir.label,"/",fixed=T)[[1]])]))
    ret=NULL
    if(length(files)>0){
        columns = lapply(1:length(files),
            function(i,ns){
                paste(strsplit(ns[i],".",fixed=T)[[1]][1:(length(strsplit(ns[i],".",fixed=T)[[1]])-1)],collapse=".")
            },
            basename(files))
        ret=radioButtons(inputId=paste(basename(dir.label),idlabel,sep=""), label=basename(dir.label), choices=columns, selected=columns[1])
    }
    ret
}

change.file.ext = function(name,new.ext){
    splity = strsplit(name,".",fixed=T)[[1]]
    if(length(splity)>1){
        splity = paste(paste(splity[1:(length(splity)-1)],collapse="."),new.ext,sep=".")
    }else{
        splity = paste0(splity,".",new.ext)
    }
    splity
}

get.vars = function(){
    lines = c()
    if(file.exists("VARS")){
        lines = scan("VARS",what="character",sep="\n",quiet=T)
    }
    if(length(lines)>0){
        invisible(lapply(lines,function(line){
            if(length(strsplit(line,"#")[[1]])>0){
                line=strsplit(line,"#")[[1]][1]
            }
            if(!""%in%line){
                variable = gsub("^\\s+|\\s+$", "", strsplit(line,"=")[[1]])
                if(length(variable)!=2){
                    message(paste("Format of variable:",paste(variable,collapse=" "),"\n is not valid.",sep=" "))
                }else{
                    if(variable[1]%in%vars&&!""%in%variable[2]){
                        if(variable[1]%in%"data.dir"){
                            data.dir<<-variable[2]
                        }else if(variable[1]%in%"version"){
                            version<<-variable[2]
                        }
                    }
                }
            }
        }))
    }
}

get.quantiles = function(subx){
    g1 = rep("",length(subx))
    if(is.numeric(subx)){
        quant = quantile(subx,na.rm=T)
        g1[which(subx>=quant[1]&subx<quant[2])] = paste(round(quant[1],2),round(quant[2],2),sep="-")
        g1[which(subx>=quant[2]&subx<quant[3])] = paste(round(quant[2],2),round(quant[3],2),sep="-")
        g1[which(subx>=quant[3]&subx<xquant[4])] = paste(round(quant[3],2),round(quant[4],2),sep="-")
        g1[which(subx>=quant[4]&subx<=quant[5])] = paste(round(quant[4],2),round(quant[5],2),sep="-")
        g1 = as.factor(g1)
    }
    g1
}

vars = c("data.dir","Version")
data.dir = "data"
lite.version = "iNZight Lite Version 0.9.7"
lite.update = "Last Updated: 18/03/15"
first.reorder = TRUE
#transform.text = ""
rawdata = load.data()
dataHasChanged = F
data.name = rawdata[[1]]
data = rawdata[[2]]
temp.data = ""
loaded = FALSE
get.vars()
single.play = F
button = F



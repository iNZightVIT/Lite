
change.factor.transform = function(data,names){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        as.data.frame(as.factor(as.vector(data[,index])))
    },data)))
    if(!is.null(temp)){
        colnames(temp) = paste("change_factor",names,sep=".")
    }
    temp
}

change.sign.transform = function(data,names){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            as.matrix(data[,index]*(-1))
        }else{
            NULL
        }
    },data)))
    if(!is.null(temp)){
        colnames(temp) = paste("change_sign",names[unlist(lapply(1:ncol(data),function(i,data){is.numeric(data[,i])},data))],sep=".")
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

copy.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = paste("copy",names,sep=".")
    data
}

reverse.coding.transform = function(data,names){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            min(data[,index],na.rm=T)+max(data[,index],na.rm=T)-data[,index]
        }else{
            NULL
        }
    },data)))
    if(!is.null(temp)){
        colnames(temp) = paste("reverse_coding",names[unlist(lapply(1:ncol(data),function(i,data){is.numeric(data[,i])},data))],sep=".")
    }
    temp
}

median.split.transform = function(data,names){
    data = as.data.frame(data)
    temp = lapply(1:ncol(data),function(index,data){
        med = median(data[,index],na.rm=T)
        if(is.numeric(data[,index])){
            ret = rep("high",length(data[,index]))
            ret[which(data[,index]<=med)] = "low"
            as.factor(ret)
        }else{
            NULL
        }
    },data)
    if(length(temp)>1){
        first = T
        tem = NULL
        for(i in 1:length(temp)){
            if(first&&!is.null(temp[[i]])){
                tem = as.data.frame(temp[[i]])
                first=F
            }else if(!is.null(temp[[i]])){
                tem = cbind(tem,temp[[i]])
            }
        }
        temp=tem
    }else{
        if(is.null(temp[[1]])){
            return(NULL)
        }else{
            temp = as.data.frame(temp[[1]])
        }
    }
    nums = unlist(lapply(1:ncol(data),function(index,data){is.numeric(data[,index])},data))
    colnames(temp) = paste("median_split",names[nums],sep=".")
    temp
}

standardize.transform = function(data,names){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            (data[,index]-mean(data[,index],na.rm=T))/sd(data[,index],na.rm=T)
        }else{
            (as.numeric(factor(data[,index]))-mean(as.numeric(factor(data[,index])),na.rm=T))/sd(as.numeric(factor(data[,index])),na.rm=T)
        }
    },data)))
    colnames(temp) = paste("standardize",names,sep=".")
    temp
}

center.transform = function(data,names){
    data = as.data.frame(data)
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            data[,index]-mean(data[,index])
        }else{
            as.numeric(factor(data[,index]))-mean(as.numeric(factor(data[,index])))
        }
    },data)))
    colnames(temp) = paste("center",names,sep=".")
    temp
}

divide.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
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

multiply.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
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

subtract.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
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

add.transform  = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
    if(is.null(data)){
        return(NULL)
    }else{
        if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))==1){
            temp = as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))])
            colnames(temp) = colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]
        }else if(ncol(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]))>1){
            temp = as.data.frame(apply(as.data.frame(data[,unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))]),1,function(row){sum(row)}))
            colnames(temp) = paste0("add.",paste(colnames(data)[unlist(lapply(1:ncol(data),function(index,d)(is.numeric(d[,index])),data))],collapse="."))
        }else{
            return(NULL)
        }
    }
    temp
}

log.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
    temp = as.data.frame(do.call(cbind,lapply(1:ncol(data),function(index,data){
        if(is.numeric(data[,index])){
            log(data[,index])
        }else{
            NULL
        }
    },data)))
    if(dim(temp)[1]>0&&dim(temp)[2]>0){
        colnames(temp) = unlist(lapply(1:ncol(data),function(index,data){
            if(is.numeric(data[,index])){
                paste0("log.",colnames(data)[index])
            }else{
                NULL
            }
        },data))
        temp
    }else{
        NULL
    }
}

root.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
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

square.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
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

abs.transform = function(data,names){
    data = as.data.frame(data)
    colnames(data) = names
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
        names = lapply(1:length(files),
            function(i,ns){
                paste(strsplit(ns[i],".",fixed=T)[[1]][1:(length(strsplit(ns[i],".",fixed=T)[[1]])-1)],collapse=".")
            },
            basename(files))
        ret=radioButtons(inputId=paste(basename(dir.label),idlabel,sep=""), label=basename(dir.label), choices=names, selected=names[1])
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
lite.update = "Last Updated: 11/03/15"
first.reorder = TRUE
transform.text = ""
rawdata = load.data()

data.name = rawdata[[1]]
data = rawdata[[2]]
temp.data = ""
loaded = FALSE
get.vars()



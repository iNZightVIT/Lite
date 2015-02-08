
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

helpModal <- function(title, link, content) {
    ## title: popup window head title
    ## link: HTML id attribution
    ## cotent: things inside
    html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <a title='Help' data-toggle='modal' href='#%s' class='icon-question-sign'>
                      <img src=\"images/question.png\" alt=\"HTML tutorial\" style=\"width:16px;height:16px;border:0\">
                   </a>", link, title, content, link)
    Encoding(html) <- 'UTF-8'
    HTML(html)
}

inclMD <- function(file){
    return(markdownToHTML(file=file, options = c(""), stylesheet="www/empty.css"))
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
    files = list.files(dir.label,recursive=T,full.name=T)[!(file.info(list.files(dir.label,full.names=T))[,"isdir"])]
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


library(iNZightTS)
###  File : utils.R
###  Modified by : Chris Park <cpar137@aucklanduni.ac.nz>
###  Description : Modified the "newdevice" function.
###  Last modified : December 16, 2014.

get.x <-
    function(tsObj) {
        ##  figure out the limits and step size along the x axis
        f = frequency(tsObj)
        s = start(tsObj)
        if (f == 1) {
            start.x = s[1]
            step.x = 1
            end.x = start.x + length(tsObj) - 1
        }
        else {
            step.x = 1/f
            start.x = s[1] + (s[2] - 1) * step.x
            end.x = start.x + step.x * (length(tsObj) - 1)
        }

        x = seq(start.x, end.x, by = step.x)
        x.units = unit(x, "native")
        list(x = x, x.units = x.units)
    }

get.x2 <-
    function(tsObj) {
        ##  figure out the limits and step size along the x axis
        f = frequency(tsObj)
        s = start(tsObj)
        if (f == 1) {
            start.x = s[1]
            step.x = 1
            end.x = start.x + length(tsObj) - 1
        }
        else {
            step.x = 1/f
            start.x = s[1] + (s[2] - 1) * step.x
            end.x = start.x + step.x * (length(tsObj) - 1)
        }

        x = seq(start.x, end.x, by = step.x)
        x = order(x)
        x = x/max(x)
        x.units = unit(x, "native")
        list(x = x, x.units = x.units)
    }

get.line.coords <-
    function(vars.decomp, vpName, lineGrobName) {
        decomp = vars.decomp$decompVars
        seekViewport(vpName)
        line = getGrob(decomp$tree, lineGrobName)
        line.y = convertUnit(line$y, attr(line$y[1], "unit"), valueOnly = TRUE)
        line.vp.yrange = current.viewport()$yscale
        line.y.npc = (line.y - line.vp.yrange[1]) / diff(line.vp.yrange)
        line.y.parent = switch(vpName,
            season = decomp$props["remainder"] +
            line.y.npc * decomp$props["seasonal"],
            random = line.y.npc * decomp$props["remainder"],
            trend = line.y.npc * decomp$props["trend"] +
            decomp$props["seasonal"] +
            decomp$props["remainder"])
        line.x = convertUnit(line$x, "native", valueOnly = TRUE)
        line.vp.xrange = current.viewport()$xscale
        line.x.npc = (line.x - line.vp.xrange[1]) / diff(line.vp.xrange)
        x.parent = line.x.npc

        list(line.y = line.y, line.vp.yrange = line.vp.yrange,
             line.y.npc = line.y.npc, line.y.parent = line.y.parent,
             line.x = line.x, line.vp.xrange = line.vp.xrange,
             line.x.npc = line.x.npc, x.parent = x.parent,
             line.col = line$gp$col)
    }



add.line.plots.vp <-
    function(vars.decomp, vpName, lineCol = "red3",
             name = paste(vpName, "copy", sep = ".")) {
        z = get.line.coords(vars.decomp, vpName, paste(vpName, "Line", sep = ""))
        lineCopy = linesGrob(unit(z$x.parent, "npc"),
            unit(z$line.y.parent, "npc"),
            name = name,
            vp = vpPath("parent", "plots"),
            gp = gpar(col = lineCol))
        updated.tree = addGrob(vars.decomp$decompVars$tree, lineCopy)
        vars.decomp$decompVars$tree = updated.tree
        vars.decomp
    }


###  If OS is windows, simply use the default graphics device.
###  If OS is non-windows, we perform a series of checks.
###  First, we check if "Acinonyx" or "cairoDevice" loads properly.
###  If they BOTH load correctly, we choose "Acinonyx".
###  If a package is installed but can't be loaded properly, a helpful
###  warning message is issued. Finally, if BOTH the packages cannot
###  be loaded properly, we use the X11(type = "cairo") device, as it
###  supports dev.hold() and dev.flush() which we will be using for
###  producing animations, as part of the puaseImage() function.

newdevice <-
    function(width, height, ...) {
            ##  Main function starts here.
            if (.Platform$OS.type == "windows") {
                dev.new(width = width, height = height, ...)
            } else {
                ##  See if 'Acinonyx' is installed properly.
                instAc <- "Acinonyx" %in% rownames(installed.packages())
                loadAc <- suppressWarnings(require(Acinonyx, quietly = TRUE,
                                                  warn.conflicts = TRUE))
                ##  See if 'cairoDevice' is installed properly.
                instCa <- "cairoDevice" %in% rownames(installed.packages())
                loadCa <- suppressWarnings(require(cairoDevice, quietly = TRUE,
                                                  warn.conflicts = TRUE))
                if (loadAc || (loadAc && loadCa))  {
                    width.in <- round(width * 90)
                    height.in <- round(height * 90)
                    Acinonyx::idev(width = width.in, height = height.in)
                }
                if (instAc && !loadAc) {
                    warning("The 'Acinonyx' package seems to have been
                             installed incorrectly. We suggest you re-install
                             the 'Acinonyx' package. If the issue persists,
                             please consult the package help page on CRAN.")
                }
                if (loadCa && !loadAc) {
                    cairoDevice::Cairo(width = width, height = height, ...)
                }
                if (instCa && !loadCa) {
                    warning("The 'cairoDevice' package seems to have been
                             installed incorrectly. We suggest you re-install
                             the 'cairoDevice' package. If the issue persists,
                             please consult the package help page on CRAN.")
                }
                if (!loadCa && !loadAc || !instCa && !instAc) {
                    X11(width = width, height = height, type = "cairo", ...)
                    warning("We recommend you download either the 'cairoDevice'
                            (for Linux/Windows users) or the 'Acinonyx' package
                            (for Mac OS X users) for better animations.")
                }
            }
        }


## hold.frame <-
##     function(frame = 1L) {
##         if (dev.interactive()) {
##             if (exists("dev.hold"))
##                 dev.hold(frame)
##         }
##     }

## flush.frame <-
##     function(frame = 1L, interval = .01, sleep = FALSE) {
##         if (dev.interactive()) {
##             if (exists("dev.flush")) {
##                 dev.flush(frame)
##             }
##             if (sleep) {
##                 Sys.sleep(interval)
##             }
##         }
##     }

drawImage <-
    function(image) {
        ##  if ("Acinonyx" %in% rownames(installed.packages()))
        ##  if "Acinonyx" is loaded, then use plot.new(.)
        ##  Unsure as to why this is necessary, but left as is
        ##  for now.
        ##  if ("package:Acinonyx" %in% search())
        ## plot.new()
        ##  Draws current image in device.
        dev.hold()
        grid.newpage()
        ##  On some devices (notably on Mac) we end up being unable to
        ##  see anything besides a single frame due to buffering.
        ##  dev.hold() and dev.flush() will force the device to hold
        ##  and flush currently buffered frames.
        grid.draw(image)
        dev.flush()
    }


###  The dev.hold() and dev.flush() functions hold and flush frames.
###  Moreover, they are part of the grDevices package, which is
###  included in R by default. For non-windows graphics devices
###  that may or may not come with "double buffering", holding
###  and flushing an image for every iteration allows us to overcome
###  the problem of "flickering".

pauseImage <-
    function(image, pause = 0.1) {
            drawImage(image)
            Sys.sleep(pause)
    }



rmGrobs <-
    function(image, grobs) {
        for (i in grobs) {
            if (i %in% childNames(image)) {
                image <- removeGrob(image, gPath(i))
            }
        }
        image
    }


onlineplot <-
    function(obj, multiplicative = FALSE, ylab = "", xlab = "",
             animate = FALSE, e = NULL) {

        ## the e argument to support animation stop
        if (is.null(e)) {
            e <- new.env()
            e$stopAnimation <- FALSE
        }

        if (any(grepl("^iNZightMTS$", class(data))))
            stop("Time-Series must be univariate")

        height = 5; width = 6

        ## x and y coordinates of the time series tsObj
        tsObj = obj$tsObj
        xlist = get.x(tsObj)
        x = xlist$x
        x.units = xlist$x.units
        y = tsObj@.Data
        y.units = unit(y, "native")

        ## We want a trend line, so do a decomposition
        if (frequency(tsObj) > 1) {
            decomp = decomposition(obj, ylab = "", multiplicative = multiplicative)$decompVars
            if (multiplicative)
                smooth = exp(log(decomp$components[,"trend"]))
            else
                smooth = decomp$components[,"trend"]
        } else {
            smooth = loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted
        }

        ## Height of the plotting viewport needs to be scale.factor times the height
        ## of the trend viewport in the decomposition plot
        ## browser()
        plotHeight = 2

        parent.vp = viewport(layout = grid.layout(3, 3,
                                 heights = unit(c(1, plotHeight, 1),
                                     c("null", "inches", "null")),
                                 widths = unit(c(1.2, 1, .2),
                                     c("inches", "null", "inches"))),
            name = "parent")
        head.vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2, name = "head")
        left.vp = viewport(layout.pos.row = 2, layout.pos.col = 1, name = "left")
        right.vp = viewport(layout.pos.row = 2, layout.pos.col = 3, name = "right")
        bottom.vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2, name = "bottom")

        plot.vp = viewport(name = "plot", layout.pos.row = 2, layout.pos.col = 2,
            xscale = extendrange(r = range(x)),
            yscale = extendrange(r = range(y, smooth)))
        plot.vptree = vpTree(parent.vp, vpList(head.vp, left.vp, plot.vp, right.vp,
            bottom.vp))

        ## The following creates a gTree which contains all of our grobs
        headtitle <- ifelse(ylab != "", ylab, "Time series plot")
        grobList = gList(rectGrob(vp = vpPath("parent", "plot"), name = "border"),
            linesGrob(x.units, y.units, vp = vpPath("parent", "plot"),
                      name = "line", gp = gpar(col = "black", lwd = 1)),
            linesGrob(x.units, unit(smooth, "native"), name = "smooth",
                      gp = gpar(col = "red"), vp = vpPath("parent", "plot")),
            yaxisGrob(vp = vpPath("parent", "plot"), name = "yAxis",
                      gp = gpar(cex = .8)),
            textGrob(ylab, x= 0, y = 0.5, vjust = -6,
                     rot = 90,
                     vp = vpPath("parent", "plot"), name = "yAxisLabel",
                     gp = gpar(cex = .8)),
            xaxisGrob(vp = vpPath("parent", "plot"), name = "xAxis",
                      gp = gpar(cex = .8)),
            textGrob(xlab, x= 0.5, y = 0, vjust = 5,
                     vp = vpPath("parent", "plot"), name = "xAxisLabel",
                     gp = gpar(cex = .8)),
            textGrob(paste(headtitle,"for", obj$currVar),
                     hjust = 0.5, vjust = -1.5, y = 0,
                     name = "topLabel",
                     vp = vpPath("parent", "head")))


        image = gTree(name = "image", children = grobList, childrenvp = plot.vptree)

        ##  newdevice - is this really necessary?
        ## newdevice(width = width, height = height)


        ##  Animations are useful for demonstrating theoretical concepts.
        ##  An animation is essentially a rapid display of a sequence of
        ##  static images that generates an illusion of movement, due to
        ##  a visual phenomenon called "persistence of vision".
        ##
        ##  While we can watch animation in R's graphics devices, some
        ##  devices under UNIX-based operating systems lack the capability
        ##  of double buffering, causing animations to flicker. Strangely
        ##  enough, relying on the supposedly "cross-platform" Cairo
        ##  device seems not to support double buffering under Windows -
        ##  clearly a bug. So we instead export animations from R for:
        ##
        ##  - better performance
        ##  - better portability
        ##
        if (animate) {
            ##  We require the animation package.
            require(animation)
            ani.options(nmax = 1000, interval = .2)
            par(mar = c(0, 0, 0, 0))

            ##  We set up grid graphics objects and compute the number of
            ##  points required.
            final.line <- getGrob(image, "line")
            final.smooth <- getGrob(image, "smooth")
            image <- removeGrob(image, "line")
            image <- removeGrob(image, "smooth")
            n.points <- length(final.line$x)
            p <- pointsGrob(x = final.line$x, y = final.line$y,
                            vp = vpPath("parent", "plot"), size = unit(2.5, "mm"),
                            pch = 19, name = "points", gp = gpar(col = "darkgray"))
            image <- addGrob(image, p)
            ##  We draw the initial image, which is a scatter plot with no lines.
            ## pauseImage(image, pause = 0)
            ## saveHTML2({
            saveGIF({
                ##  Change wd to tempdir() but store the current wd.
                ## currDir <- getwd()
                ## setwd(tempdir())
                ## print(getwd())
                ## setwd("~/Desktop")
                ## Set the plotting region.
                currDir <- getwd()
                for (i in 1:n.points) {
                    if ((get("stopAnimation", envir = e) && i < n.points))
                        next
                    ##  Draw initial image.
                    if (i == 1) {
                        dev.hold()
                        pauseImage(image)
                        dev.flush()
                        ani.pause(.001) # .01
                    }
                    ##  Set pause intervals.
                    ## if (i < 10) {
                    ##     pause = .5
                    ## } else if (i < 30) {
                    ##     pause = .2
                    ## } else {
                    ##     pause = .1
                    ## }
                    ##  Draw.
                    l <- linesGrob(x = final.line$x[1:i], y = final.line$y[1:i],
                                   vp = vpPath("parent", "plot"),
                                   name = "line", gp = gpar(col = 1, lwd = 1.5))
                    image <- addGrob(image, l)
                    dev.hold()
                    pauseImage(image)
                    dev.flush()
                    ani.pause(.001) # .01
                }
            }, ## img.name = "iNZight_TS_V1", ani.height = 400, ani.width = 1100, # 300 * 1000
                    ##        title = "iNZight_TS_Animation", outdir = ,
                    ##        htmlfile = "iNZight_TS.html", navigator = FALSE, verbose = FALSE,
                    ##        overwrite = TRUE)
                    movie.name = "animation.gif", ani.height = 400, ani.width = 800,
                    title = "iNZight Time Series")
        }
    }

vars = c("data.dir","version")
data.dir = "data"
version = "1.0"
first.reorder = TRUE
transform.text = ""
rawdata = load.data()

data.name = rawdata[[1]]
data = rawdata[[2]]
temp.data = ""
loaded = FALSE
get.vars()



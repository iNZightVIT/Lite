##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Reahapes the data that all columns are merged into two 
#' column with the variable names in the first column and 
#' the values in the second column.
#' 
#' @param dafr The data.frame to convert
#' 
#' @return The converted data.frame.
#' 
#' @author Christoph Knapp
get.reshape.data = function(dafr){
  temp = do.call(rbind,lapply(1:ncol(dafr),function(index,d){
    name = colnames(d)[index]
    data.frame(groups=name,d[,index])
  },dafr))
  colnames(temp)[2] = "variables"
  temp
}
##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Converts specified columns into binary factor variables 
#' which get added to the input data.frame.
#' 
#' @param dafr The input data.frame.
#' @param The column names or indexes to be converted.
#' 
#' @return A data.frame with the binary columns added.
#' 
#' @author Christoph Knapp  
get.missing.categorical = function(dafr,columns){
  temp = as.data.frame(dafr[,columns])
  colnames(temp) = columns
  new.dafr = data.frame(do.call(cbind,lapply(1:length(columns),
                                  function(index,d,c){
                                    te = rep("observed",nrow(d))
                                    te[is.na(d[,index])] = "missing"
                                    te
                                  },temp,columns)))
  colnames(new.dafr) = paste("missing",columns,sep=".")
  cbind(dafr,new.dafr)
}

#' Simplifies the input data.frame by keeping only 
#' columns where NA values are present. Function 
#' used in get.combinations
#' 
#' @param dafr A data.frame to be 
#' simplified such that all columns 
#' which do not contain NA values are 
#' removed.
#' 
#' @author Christoph Knapp
simplify.dafr = function(dafr){
  ies = c()
  for(col in 1:ncol(dafr)){
    if(!any(is.na(dafr[,col]))){
      ies = c(ies,col)
    }
  }
  dafr = dafr[,-ies]
  if(ncol(dafr)==0){
    dafr=NULL
  }
  dafr
}

#' Converts a data.frame to a format where all 
#' non NA are replaced by "observed" and all NA 
#' values are repalced by "missing".
#' 
#' @param dafr the data.frame to convert.
#' 
#' @return The converted data.frame.
#' 
#' @author Christoph Knapp
convert.dafr = function(dafr){
  if(!is.null(dafr)){
    temp = do.call(cbind,lapply(1:ncol(dafr),function(i,d){
      col = rep("observed",nrow(d))
      col[is.na(d[,i])] = "missing"
      col
    },dafr))
    colnames(temp) = colnames(dafr)
    temp
  }else{
    dafr
  }
}

#' Takes a data.frame as generated from the 
#' \texttt(get.missing.categorical) function and converts it 
#' into a data.frame of all unique rows and there counts in 
#' the original data. It is a modified version of the 
#' calmissing.data.frame method from the iNZightMR package.
#' 
#' @param dafr A data.frmae to convert
#' @param simplify Returns the smallest number of possible 
#' rows, ignoring columns which do not contain NA values if 
#' TRUE, otherwise it processes the whole input.
#' @param convert TRUE if the input data.frame should be 
#' converted into "missing" or observed format.   
#' 
#' @return A data.frame with all unique rows from dafr and 
#' their counts in the last column.
#' 
#' @author Christoph Knapp
get.combinations = function(dafr,simplify=F){
  dafr = data.frame(dafr)
  index.column = rep(T,ncol(dafr))
  rm.na <- function(variable) {
    sum(is.na(variable)) > 0
  }
  if(simplify){
    index.column <- sapply(dafr, rm.na)
  }
  x <- data.frame(dafr[,index.column])
  if(ncol(x)>0){
    x1 <- as.numeric(apply(x, 2, function(x) length(which(is.na(x)))))
    row4col.order <- order(x1) 
    x1 <- c(x1, nrow(x))
    z1 <- ifelse(is.na(x), "missing", "observed")
    tab <- table(apply(z1, 1, paste, collapse = ","))
    tab <- tab[order(names(tab), decreasing = TRUE)]
    tab <- data.frame(combination = names(tab), count = as.numeric(tab))
    tabp <- t(apply(tab, 1, function(x) {
      unlist(strsplit(x, ",", fixed = TRUE))
    }))
    tabp <- data.frame(tabp,stringsAsFactors=F)
    tabp <- tabp[,c(row4col.order, max(row4col.order)+1)]
    tabp <- rbind(tabp, x1[c(row4col.order, max(row4col.order)+1)])  #  x1[row4col.order] == numMiss
    names(tabp) <- c(names(x)[row4col.order], "Total")
    row.names(tabp) <- c(seq_len(nrow(tab)), "Total")
    
    tabfinal <- tabp[-nrow(tabp), ]
    tabfinal <- tabfinal[order(tabfinal$Total, decreasing = TRUE), ]
    tabfinal <- rbind(tabfinal, tabp[nrow(tabp), ])
    
    finaltable <- tabfinal
    
    Name <- names(finaltable)
    i <- nrow(finaltable)
    j <- ncol(finaltable)
    index <- order(x1[-j], decreasing = FALSE)
    numMiss <- x1[c(index, j)]
    percMiss <- round(numMiss / numMiss[j], 3)
    
    finaltable = rbind(finaltable,paste0(round(percMiss * 100, 2), "%"))
    colnames(finaltable)[j] <- "Freq"
    finaltable
  }else{
    NULL
  }
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Takes an input string of a formula involving colummn 
#' names in the input data set and tries to evaluate it. 
#' If this is not possible, NULL is returned and the error 
#' is printed to standard out.
#' 
#' @param dafr The data.frame containing the data needed 
#' to evaluate the expression.
#' @param new.formula The character string holding the 
#' expression to be evaluated.
#' 
#' @return Null if the expression could not be evaluated, 
#' otherwise the input data.frame with one additional 
#' column. This column contains the results of the 
#' expression.
#' 
#' @author Christoph Knapp
get.create.variables = function(dafr,new.formula,new.name=NULL){
  tryCatch({
    colu = eval(parse(text=new.formula),dafr)
    if(length(colu)>nrow(dafr)){
      colu = colu[1:nrow(dafr)]
    }
    temp = cbind(dafr,colu)
    if(is.null(new.name)||""%in%new.name){
      new.name = "new.name"
    }
    count=0
    while(new.name%in%colnames(dafr)){
      count = count+1
      new.name = paste(new.name,count,sep=".")
    }
    colnames(temp)[ncol(temp)] = new.name
    temp
  },error=function(cond) {
    #print(cond)
    return (NULL)
  },
  warning=function(cond) {
    #print(cond)
  },
  finally={
    
  })
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' This function is a wrapper function for the rank method.
#' 
#' @param dafr A dataframe with variables to be ranked.
#' @param columns Character of column names or column 
#' indices of the columns to be ranked.
#' 
#' @return A data.frame containig the original data plus 
#' the ranked variables.
#' 
#' @author Christoph Knapp
get.rank.numeric = function(dafr,columns){
  temp = data.frame(sapply(dafr[,columns], 
                           rank, 
                           ties.method = "min", 
                           na.last = "keep"))
  nams = paste(columns,"rank",sep=".")
  count = 0
  while(any(nams%in%colnames(dafr))){
    count = count + 1
    nams = paste(nams,count,sep=".")
  }
  colnames(temp) = nams
  cbind(dafr,temp)
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Form class intervals from a column specified by column 
#' name or column index from the a data.frame.
#' 
#' @param dafr The data frame the column can be retrieved from.
#' @param column The column name or index to produce class 
#' intervals from.
#' @param num.intervals The number of intervals the column 
#' should be separated in.
#' @param open.left.closed.right Logical variable specifying 
#' whether the output should be in the format 
#' [open left, closed right) or (closed left, open right] 
#' @param method The method used to generate the class 
#' intervals.
#' @param intervals If method="specified" this needs to be 
#' provided as a vector of numeric values. The length of the 
#' vector needs to be num.intervals-1 and the minimum and 
#' maximum needs to be within range of the minimum and maximum 
#' of the selected column.
#' @param labels Optional labels for the intervals. By default 
#' the range of the labels will be used.
#' 
#' @return The same data.frame as dafr except that the class 
#' intervals are added as additional column or the the unchanged 
#' dafr data.frame is returned if the input is wrong. Warnings 
#' are provided in this case.
#'  
#' @note This is a wrapper for the cut function. See \code{?cut}. 
#' 
#' @author Christoph Knapp 
get.form.class.interval = function(dafr,column,num.intervals,
                                   open.left.closed.right=T,
                                   method=c("equal.width",
                                            "equal.count",
                                            "specified"),
                                   intervals=NULL,
                                   labels=NULL){
  if(length(method)>1||!method%in%c("equal.width",
                                    "equal.count",
                                    "specified")){
    method = "equal.width"
  }
  if(!is.null(labels)&&(length(labels)!=num.intervals||
                          any(grepl("^\\s*$",intervals)))){
    warning("The labels not in the right format and are ignored.")
    labels=NULL
  }
  column.temp = dafr[,column]
  ret = dafr
  num.cols.old = ncol(ret)
  if(!is.null(intervals)&&method%in%"specified"){
    if(!is.numeric(intervals)||any(is.na(intervals))||
         is.null(intervals)||length(intervals)!=(num.intervals-1)||
         min(intervals,na.rm=T)<min(dafr[,column],na.rm=T)||
         max(intervals,na.rm=T)<max(dafr[,column],na.rm=T)){
      warning("The \"intervals\" variable is not in the right format.")
      return(dafr)
    }
    column.temp = dafr[,column]
    ret = cbind(dafr,cut(column.temp,
                         breaks=intervals,
                         labels=labels,
                         include.lowest=T,
                         right=open.left.closed.right))
  }else if(method%in%"equal.width"){
    ret = cbind(dafr,cut(x=column.temp, 
                         breaks=num.intervals, 
                         right=open.left.closed.right,
                         labels=labels,
                         include.lowest=TRUE))
  }else if(method%in%"equal.count"){
    ret = cbind(dafr,cut(x=column.temp,
                         breaks=quantile(column.temp, 
                                         probs=seq(0,1,1/num.intervals,),
                                         na.rm=TRUE),
                         include.lowest = TRUE,
                         right = open.left.closed.right,labels=labels))
  }
  if(num.cols.old<ncol(ret)){
    count = 1
    col.name = paste(column,method,count,sep=".")
    while(paste(column,method,count,sep=".")%in%colnames(ret)){
      count = count+1
      col.name = paste(column,method,count,sep=".")
    }
    colnames(ret)[ncol(ret)] = col.name
  }
  ret
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#'Combine the levels of factor variables.
#'
#' @param dafr The dataframe containing factor columns 
#' specified in columns.
#' @param columns The column names of the columns in dafr 
#' to combine.
#' 
#' @return A data.frame with one additional column as dafr,
#' which contains the combined levels.
#' 
#' @author Christoph Knapp
combine.levels = function(dafr,columns){
  new.column = do.call(paste,lapply(columns,function(name,d){
    d[,name]
  },dafr))
  dafr = cbind(dafr,gsub(" ",".",new.column))
  colnames(dafr)[ncol(dafr)] = paste(columns,collapse=".")
  dafr
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Renames the levels of a factor.
#'
#' @param dafr A data.frame of the data to change.
#' @param column The column name of the column to change.
#' @param new.levels A character variabel of the length of 
#' the number of factors of the column to change. This 
#' vector contains the new levels.
#'
#' @return A data.frame where the levels of the specified 
#' columns are changed.
#' 
#' @author Christoph Knapp
rename.levels = function(dafr,column,new.levels){
  temp = as.character(dafr[,column])
  for(i in 1:length(levels(dafr[,column]))){
    temp[which(dafr[,column]%in%levels(dafr[,column])[i])] = new.levels[i]
  }
  dafr[,column] = factor(temp,levels=new.levels)
  dafr
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' This function changes the order of levels in the data.
#' 
#' @param dafr The data to be changed
#' @param column the factor column where the order of 
#' levels should be changed.
#' @param levels.new A vector of all levels in the column 
#' specified by column in the order they should be 
#' ordered.
#' 
#' @return A data.frame with the levels of one column 
#' reordered
#' 
#' @author Christoph Knapp
reorder.levels = function(dafr,column,levels.new){
  dafr[,column] = factor(dafr[,column],levels=levels.new)
  dafr
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Collapses selected levels in factor vector
#' 
#' @param column the vector where levels should be 
#' collapsed into one.
#' @param to.collapse Vector of levels to collapse.
#' 
#' @note Levels in to.collapse which are not in column 
#' will be ignored.
#' 
#' @author Christoph Knapp
get.collapsed.column = function(column,to.collapse){
  column = as.character(column)
  new.level = paste(to.collapse,collapse=".")
  indices = which(column%in%to.collapse)
  if(length(indices)>0){
    column[indices] = new.level
  }
  as.factor(column)
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Stacks the selected columns onto the data.
#'
#' Multiplies the data.set by adding rows to the data for 
#' every selected column. The selected columns are stacked
#' onto each other and added as an additional column 
#' 
#' @param columns The columns to stack.
#' @param dafr a dataframe the stacking is performed on.
#' 
#' @author Christoph Knapp
stack.variables.perform = function(columns,dafr){
  stack = unlist(lapply(1:length(columns),function(index,d,c){
    d[,which(colnames(d)%in%c[index])]
  },dafr,columns))
  colstack = unlist(lapply(1:length(columns),function(index,d,c){
    rep(c[index],nrow(d))
  },dafr,columns))
  cbind(dafr,stack.columns=colstack,stack.variables=stack)
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' aggregates the data over selected factor columns
#' 
#' The dimensions of the df data frame will change so that 
#' all possible combinations of factors selected will be 
#' the number of rows and the number of all selected factor 
#' columns + a column for all methods selected will be the 
#' number of columns in the return data.frame. 
#' 
#' @param aggregate.over column names of factor variables 
#' in data to aggregate over
#' @param methods A set of methods which can be used to 
#' aggregate.
#' @param df A data.frame containing at least on factor 
#' column and one numeric column.
#' 
#' @return A data.frame with the results of the aggregation.
#' 
#' @author Christoph Knapp  
aggregate.data= function(aggregate.over,
                         methods=c("mean","median","sum","sd","IQR","count"),
                         dafr){
  if(is.null(aggregate.over)|is.null(methods)|length(methods)==0|
       length(aggregate.over)==0|is.numeric(aggregate.over)){
    stop("aggregate.data : Wrong input")
  }
  if(any(!as.character(aggregate.over)%in%colnames(dafr))){
    warning("aggregate.data : Some columns in aggregate.over are 
            not in the column names for df. They will be ignored.")
  }
  if(is.character(aggregate.over)){
    aggregate.over = as.factor(aggregate.over)
  }
  bys = lapply(1:length(aggregate.over),
               function(i,d,v){
                 d[,which(colnames(d)%in%v[i])]
               },dafr,aggregate.over)
  names(bys) = aggregate.over
  sets = lapply(1:length(methods),function(i,d,b,m){
    if("count"%in%m[i]){
      m[i] = "length"
      temp = aggregate(d[,unlist(lapply(1:ncol(d),
                                        function(j,da){
                                          is.numeric(da[,j])
                                        },d))], 
                       by=b, FUN=m[i],simplify = FALSE)
    }else{
      temp = aggregate(d[,unlist(lapply(1:ncol(d),
                                        function(j,da){
                                          is.numeric(da[,j])
                                        },d))], 
                       by=b, FUN=m[i],na.rm=T,simplify = FALSE)
    }
    
    colnames(temp)[(length(b)+1):ncol(temp)] = paste(m[i],
                                                     colnames(temp)[(length(b)+1):ncol(temp)],sep=".")
    temp
  },dafr,bys,methods)
  temp = sets[[1]]
  if(length(sets)>1){
    for(i in 2:length(sets)){
      temp = merge(temp,sets[[i]])
    }
  }
  temp
}

##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Sorts the df data frame after the input variables
#' 
#' Warnings will be given if the colnames in vars will not 
#' match the column in df. This is a wrapper function for 
#' sort. See \code{?sort} for more information.
#' 
#' @param vars The column names in the order the df data.frame
#' should be sorted.
#' @param sort.type A logical vector of the same length as 
#' vars. If the element in the vector is TRUE the corresponding 
#' element in vars will be sorted in increasing order.
#' @ param The data.fram or matrix to sort.
#' 
#' @return An ordered data.frame
#' 
#' @author Christoph Knapp  
sort.data = function(vars,sort.type,df){
  if(any(!vars%in%colnames(df))){
    warning("sort.data : Not all variables in vars could be 
            matched to column names in df.")
  }
  if(length(vars)!=length(sort.type)){
    stop("sort.data : vas and sort.type have different length")
  }
  z = lapply(1:length(vars),function(index,v,t,d){
    vec = d[,which(colnames(d)%in%v[index])]
    if(is.factor(vec)|is.character(vec)){
      vec = xtfrm(as.character(vec))
    }
    if(!t[index]){
      vec = -vec
    }
    vec
  },vars,sort.type,df)
  df[order.overwrite(z),]
}

#' The iNZight version of the order function which lets you pass 
#' in a list of vectors to order instead of the ... argument. It 
#' is shortened and might be therefore not as stable as the 
#' original order function.
#' 
#' @param z a sequence of numeric, complex, character or logical 
#' vectors, all of the same length, or a classed R object.
#' @param na.last for controlling the treatment of NAs. If TRUE, 
#' missing values in the data are put last; if FALSE, they are 
#' put first; if NA, they are removed (see ‘Note’.)
#' @param decreasing logical. Should the sort order be increasing 
#' or decreasing?
#' 
#' @note This function is only called in sort.data but needs to be 
#' available to sort.data
#' 
#' @author Christoph Knapp
order.overwrite = function (z, na.last = TRUE, decreasing = FALSE) {
  if (any(diff(l.z <- vapply(z, length, 1L)) != 0L)) 
    stop("argument lengths differ")
  ans <- vapply(z, is.na, rep.int(NA, l.z[1L]))
  ok <- if (is.matrix(ans)) 
    !apply(ans, 1, any)
  else !any(ans)
  if (all(!ok)) 
    return(integer())
  z[[1L]][!ok] <- NA
  ans <- do.call("order", c(z, decreasing = decreasing))
  keep <- seq_along(ok)[ok]
  ans[ans %in% keep]
}


##########################################################
#To be removed when the iNZight tools package is working##
##########################################################
#' Takes a sample of rows from a data.frame
#' 
#' This function samples rows from a data.frame with or 
#' without replacment. 
#' 
#' @param df A data.frame the sample is taken from.
#' @param sampleSize The size of the samples to be taken.
#' @param numSample The number of samples to be taken.
#' @param bootstrap TRUE if samples with replacement is 
#' desired, FALSE if no replacement.
#' 
#' @return A data.frame with the samples merged together. 
#' An additional column is added where the sampling 
#' iteration is stored. 
#' 
#' @author Christoph Knapp
#' 
#' @export
sample.data = function(df,sampleSize,numSample=1,bootstrap=F){
  if(sampleSize>nrow(df)){
    stop(paste0("This sample is to large. Only ",nrow(df)," samples available."))
  }
  if(sampleSize*numSample>nrow(df)&!bootstrap){
    stop(paste0("Not enough rows in data to sample that many times."))
  }
  colname = "num.sample"
  if("num.sample"%in%colnames(df)){
    count=1
    while(paste0("num.sample",count)%in%colnames(df)){
      count = count+1
    }
    colname = paste0("num.sample",count)
  }
  ret = NULL
  if(bootstrap){
    ret = do.call(rbind,lapply(1:numSample,function(index,d,size){
      cbind(d[sample(1:nrow(d),size),],rep(index,size))
    },df,sampleSize))
  }else{
    ret = do.call(rbind,lapply(1:numSample,function(index,d,size){
      s = sample(1:nrow(d),size)
      temp = cbind(d[s,],rep(index,size))
      d <<- d[-s,]
      temp
    },df,sampleSize))
  }
  colnames(ret)[ncol(ret)] = colname
  ret
}

#' Returns the names of all numeric columns in data 
#' 
#' @param dafr The input dataframe to be searched.
#' 
#' @author Christoph Knapp
get.numeric.column.names = function(dafr){
  colnames(dafr)[which(unlist(lapply(1:ncol(dafr),function(index,d){
    is.numeric(as.data.frame(d)[,index])
  },dafr)))]
}

#' Returns the column names of the currently selected data which
#' can be converted into factors.
#' 
#' @param dafr The input dataframe to be searched.
#' 
#' @author Christoph Knapp
get.categorical.column.names = function(dafr){
  colnames(dafr)[which(unlist(lapply(1:ncol(dafr),function(index,d){
    class(as.data.frame(d)[,index])%in%"factor"||class(as.data.frame(d)[,index])%in%"character"
  },dafr)))]
}


#' Returns TRUE if x can be converted to a numeric 
#' value, FALSE if not. 
#' 
#' @param x any oblect to be tested
#' 
#' @author Christoph Knapp
is.convertable.numeric = function(x){
  !suppressWarnings(is.na(as.numeric(x)))
}

#' Tests whether a character variable is convertable to an 
#' integer value.
#' 
#' @param x a character or numeric value to test for.
#' 
#' @author Christoph Knapp
is.convertable.integer = function(x){
  temp = is.convertable.numeric(x)
  temp[temp] = as.numeric(x[temp])%%1==0
  temp
}

#' Prints a summary of the currently selected data set.
#' 
#' @author Christoph Knapp
data.summary = function(dafr){  
  if(!is.null(dafr)){
    cat("Number of rows in data: ",nrow(dafr),"\n")
    cat("Number of columns in data: ",ncol(dafr),"\n")
    cat("\n")
    for(col in 1:length(colnames(dafr))){
      cat(colnames(dafr)[col],"\n")
      print(summary(dafr[,col]))
    }
  }
}

# #' Creates a widget for moving through plots quickly.
# #' 
# #' @param ID.forward inputID for the forward button in the player widget
# #' @param ID.player inputID for the slider in the player widget
# #' @param ID.backward inputID for the backward button in the player widget
# #' 
# #' @author Christoph Knapp
# get.player = function(ID.forward,ID.player,ID.backward,maxi){
#   fixedRow(column(width=8,offset=2,
#                   div(class='player',
#                       fixedRow(
#                         column(width=1,offset=1,
#                                div(class="seper",actionButton(inputId=ID.backward,label="",icon=icon("backward")))),
#                         column(width=6,offset=1,
#                                sliderInput(inputId=ID.player,label="",min=1,max=maxi,step=1,
#                                            animate=animationOptions(interval=500,loop=T,play=T),
#                                            width="100%",value=1,ticks=F)),
#                         column(width=1,offset=1,
#                                div(class="seper",actionButton(inputId=ID.forward,label="",icon=icon("forward"))))
#                       ))))
# }
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

#' changes numeric columns to factor columns
#' 
#' @param temp a matrix or data.frame with columns to change
#' @param columns a vector of column names for temp
#' 
#' @return a data.frame where all numeric columns are 
#' converted into character column
#' 
#' @author Christoph Knapp
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

#' change the sign of numeric columns
#' 
#' @param dafr a data frame with columns to transform
#' @param columns the column names of the columns in dafr
#' 
#' @author Christoph Knapp
change.sign.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,dafr){
    if(is.numeric(dafr[,index])){
      as.matrix(dafr[,index]*(-1))
    }else{
      NULL
    }
  },dafr)))
  if(!is.null(temp)){
    colnames(temp) = paste("change_sign",columns[unlist(lapply(1:ncol(dafr),function(i,d){
      is.numeric(d[,i])
    },dafr))],sep=".")
  }
  temp
}

test.for.dates = function(dafr){
  ret = F
  if(!is.null(dafr)){
    ret = unlist(lapply(
      1:ncol(dafr),function(index,d){
        tryCatch({
          is.numeric(as.numeric(as.Date(d[,index], origin = "1900-01-01")))
        },
        error=function(cond) {
          ret = F
        },
        warning=function(cond) {
          print(cond)
        },
        finally={})
      },dafr))
  }
  ret
}

copy.transform = function(dafr,columns){
  data = as.data.frame(dafr)
  colnames(dafr) = paste("copy",columns,sep=".")
  data
}

reverse.coding.transform = function(dafr,columns){
  data = as.data.frame(dafr)
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      min(d[,index],na.rm=T)+max(d[,index],na.rm=T)-d[,index]
    }else{
      NULL
    }
  },dafr)))
  if(!is.null(temp)){
    colnames(temp) = paste("reverse_coding",
                           columns[unlist(lapply(1:ncol(dafr),
                                                 function(i,d){
                                                   is.numeric(d[,i])
                                                 },dafr))],sep=".")
  }
  temp
}

median.split.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  nums = unlist(lapply(1:ncol(dafr),function(index,dafr){is.numeric(dafr[,index])},dafr))
  dafr = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      med = median(d[,index],na.rm=T)
      ret = rep("high",length(d[,index]))
      ret[which(d[,index]<=med)] = "low"
      ret
    }else{
      NULL
    }
  },dafr)),stringsAsFactors=T)
  colnames(dafr) = paste("median_split",columns[nums],sep="_")
  dafr
}

standardize.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  dafr = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      (d[,index]-mean(d[,index],na.rm=T))/sd(d[,index],na.rm=T)
    }else{
      (as.numeric(factor(d[,index]))
       -mean(as.numeric(factor(d[,index])),na.rm=T))/
        sd(as.numeric(factor(d[,index])),na.rm=T)
    }
  },dafr)))
  colnames(dafr) = paste("standardize",columns,sep=".")
  dafr
}

center.transform = function(dafr,columns){
  data = as.data.frame(dafr)
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      d[,index]-mean(d[,index])
    }else{
      as.numeric(factor(d[,index]))-mean(as.numeric(factor(d[,index])))
    }
  },dafr)))
  colnames(temp) = paste("center",columns,sep=".")
  temp
}

divide.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  if(is.null(dafr)){
    return(NULL)
  }else{
    if(ncol(as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
      is.numeric(d[,index])
    },dafr))]))==1){
      temp = as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))])
      colnames(temp) = colnames(data)[unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },data))]
    }else if(ncol(as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
      is.numeric(d[,index])
    },data))]))>1){
      temp = as.data.frame(divide(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])},dafr))]))
      colnames(temp) = paste0("divide.",
                              paste(colnames(dafr)[unlist(lapply(1:ncol(dafr),function(index,d){
                                is.numeric(d[,index])
                              },dafr))],collapse="."))
    }else{
      return(NULL)
    }
  }
  temp
}

divide = function(dafr){
  dafr = dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
    is.numeric(d[,index])
  },dafr))]
  dafr = as.data.frame(dafr)
  if(ncol(dafr)==1){
    dafr[,1]
  }else{
    start = dafr[,1]
    for(col in 2:ncol(dafr)){
      start = start/dafr[,col]
    }
    start
  }
}

multiply.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  if(is.null(dafr)){
    return(NULL)
  }else{
    if(ncol(as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
      is.numeric(d[,index])
    },dafr))]))==1){
      temp = as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))])
      colnames(temp) = colnames(dafr)[unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))]
    }else if(ncol(as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
      is.numeric(d[,index])
    },dafr))]))>1){
      temp = as.data.frame(multiply(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))]))
      colnames(temp) = paste0("multiply.",paste(colnames(dafr)[unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))],collapse="."))
    }else{
      return(NULL)
    }
  }
  temp
}

multiply = function(dafr){
  dafr = dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
    is.numeric(d[,index])
  },dafr))]
  dafr = as.data.frame(dafr)
  if(ncol(dafr)==1){
    dafr[,1]
  }else{
    start = dafr[,1]
    for(col in 2:ncol(dafr)){
      start = start*dafr[,col]
    }
    start
  }
}

subtract.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  if(is.null(dafr)){
    return(NULL)
  }else{
    if(ncol(as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
      is.numeric(d[,index])
    },dafr))]))==1){
      temp = as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))])
      colnames(temp) = colnames(dafr)[unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))]
    }else if(ncol(as.data.frame(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
      is.numeric(d[,index])
    },dafr))]))>1){
      temp = as.data.frame(subtract(dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))]))
      colnames(temp) = paste0("subtract.",paste(colnames(dafr)[unlist(lapply(1:ncol(dafr),function(index,d){
        is.numeric(d[,index])
      },dafr))],collapse="."))
    }else{
      return(NULL)
    }
  }
  temp
}

subtract = function(dafr){
  dafr = dafr[,unlist(lapply(1:ncol(dafr),function(index,d){
    is.numeric(d[,index])
  },dafr))]
  dafr = as.data.frame(dafr)
  if(ncol(dafr)==1){
    dafr[,1]
  }else{
    start = dafr[,1]
    for(col in 2:ncol(dafr)){
      start = start-dafr[,col]
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
    ret = as.data.frame(temp[,unlist(lapply(1:ncol(temp),function(index,d){
      is.numeric(d[,index])
    },temp))])
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
transform.perform = function(dafr,type,columns){
  temp = transform.get.temp(dafr,type,columns)
  if(!is.null(temp)){
    temp = cbind(dafr,temp)
  }
  temp
}

# returns the transformed columns and the original columns as 
# dataframe (cbind(<original columns>,<transformed columns>)).
transform.tempTable = function(dafr,type,columns){
  temp1 = as.data.frame(dafr[,which(colnames(dafr)%in%columns)])
  temp2 = transform.get.temp(dafr,type,columns)
  if(!is.null(temp2)){
    temp1 = cbind(temp1,temp2)
  }
  temp1
}

# transorms the columns named columns in data with the selected 
# type (type) of transformation.
transform.get.temp = function(dafr,type,columns){
  temp = NULL
  if (!is.null(columns) && type%in%"log"){
    temp = log.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"add"){
    temp = add.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"subtract"){
    temp = subtract.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"multiply"){
    temp = multiply.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"divide"){
    temp = divide.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"root"){
    temp = root.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"square"){
    temp = square.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"abs"){
    temp = abs.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"center"){
    temp = center.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"standardize"){
    temp = standardize.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"median split"){
    temp = median.split.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"reverse-coding"){
    temp = reverse.coding.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"copy"){
    temp = copy.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"change sign"){
    temp = change.sign.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%"convert to categorical"){
    temp = change.factor.transform(dafr[,columns],columns)
  } else if (!is.null(columns)&type%in%" "){
    temp = NULL
  }
  temp
}

log.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,dafr){
    if(is.numeric(dafr[,index])){
      log(dafr[,index])
    }else{
      NULL
    }
  },dafr)))
  if(!is.null(temp)&&dim(temp)[1]>0&&dim(temp)[2]>0){
    colnames(temp) = unlist(lapply(1:ncol(dafr),function(index,dafr){
      if(is.numeric(dafr[,index])){
        paste0("log.",colnames(dafr)[index])
      }else{
        NULL
      }
    },dafr))
    temp
  }else{
    NULL
  }
}

root.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      sqrt(d[,index])
    }else{
      NULL
    }
  },dafr)))
  ##  temp = as.data.frame(temp)
  if(dim(temp)[1]>0&&dim(temp)[2]>0){
    colnames(temp) = unlist(lapply(1:ncol(dafr),function(index,d){
      if(is.numeric(d[,index])){
        paste0("root.",colnames(d)[index])
      }else{
        NULL
      }
    },dafr))
    temp
  }else{
    NULL
  }
}

square.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      d[,index]^2
    }else{
      NULL
    }
  },dafr)))
  ##  temp = as.data.frame(temp)
  if(dim(temp)[1]>0&&dim(temp)[2]>0){
    colnames(temp) = unlist(lapply(1:ncol(dafr),function(index,d){
      if(is.numeric(d[,index])){
        paste0("square.",colnames(d)[index])
      }else{
        NULL
      }
    },dafr))
    temp
  }else{
    NULL
  }
}

abs.transform = function(dafr,columns){
  dafr = as.data.frame(dafr)
  colnames(dafr) = columns
  temp = as.data.frame(do.call(cbind,lapply(1:ncol(dafr),function(index,d){
    if(is.numeric(d[,index])){
      abs(d[,index])
    }else{
      NULL
    }
  },dafr)))
  #   temp = as.data.frame(temp)
  if(dim(temp)[1]>0&&dim(temp)[2]>0){
    colnames(temp) = unlist(lapply(1:ncol(dafr),function(index,d){
      if(is.numeric(d[,index])){
        paste0("abs.",colnames(d)[index])
      }else{
        NULL
      }
    },dafr))
    temp
  }else{
    NULL
  }
}

delete.old.files = function(data_dir,days){
  if(length(list.files(paste0(data_dir,"/Imported")))>0){
    unlink(list.files(paste0(data_dir,"/Imported"))
           [difftime(Sys.time(), 
                     file.info(list.files(
                       paste0(data_dir,"/Imported"),full.name=T))
                     [,"mtime"], units = "days")>days])
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
load.data = function(data_dir,fileID=NULL,path=NULL){
  temp = NULL
  full.name = list.files(data_dir,full.names=T,recursive=T)
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
      # catch possible problems with user data.
      tryCatch({
        if(tolower(ext)%in%"rds"){
          temp = readRDS(file=full.name[indexes[1]])
        }else if(tolower(ext)%in%"rda"|tolower(ext)%in%"rdata"){
          name = load(full.name[indexes[1]])
          temp = get(name)
        }else if(tolower(ext)%in%"csv"){
          temp = read.csv(full.name[indexes[1]],comment.char="#", na.strings = c("NULL","NA","N/A","#N/A","","<NA>"))
        }else if(tolower(ext)%in%"txt"){
          temp = read.delim(full.name[indexes[1]],comment.char="#", na.strings = c("NULL","NA","N/A","#N/A","","<NA>"))
        }
      }, warning = function(w) {
        print(w)
      }, error = function(e) {
        print(e)
      }, finally = {})
    }
  }
  if(is.null(fileID)){
    list(NULL,temp)
  }else{
    list(data.name=basename(fileID),data.set=temp)
  }
}

## returns directories in the data directory
get.data.dirs = function(data_dir){
  list.files(data_dir,
             include.dirs=T,
             full.names=T)[file.info(paste(data_dir,
                                           list.files(data_dir),
                                           sep="/"))[,"isdir"]]
}

## returns a radioButton widget, for every filename in the dir.lable directory.
get.radio.list = function(dir.label,idlabel){
  files = c()
  files = list.files(dir.label,
                     recursive=T,
                     full.name=T)[!(file.info(list.files(dir.label,
                                                         recursive=T,
                                                         full.names=T))[,"isdir"])]
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

get.vars = function(vars.path){
  lines = c()
  if(is.null(vars.path)){
    vars.path = "VARS"
  }
  if(file.exists(vars.path)){
    lines = scan(vars.path,what="character",sep="\n",quiet=T)
  }else{
    stop("The VARS file does not exist.")
  }
  if(length(lines)>0){
    ret = NULL
    for(line in lines){
      if(!grepl("^#",line)){
        if(grepl("#",line)){
          line = strsplit(line,"#")[[1]][1]
        }
        if(grepl("=",line)&&length(strsplit(line,"=")[[1]])>1){
          if(is.null(ret)){
            ret = list()
          }
          ret[[trim(strsplit(line,"=")[[1]][1])]] = trim(strsplit(line,"=")[[1]][2])
        }
      }
    }
    ret
  }
}

#' Tests whether a directory has read write execute permissions.
#' 
#' @param file The directory path to test
#' 
#' @return TRUE if writable and the file exists, otherwise FALSE
#' 
#' @note This is a Unix only function. On Windows and all other 
#' OS where \code{.Platform$OS.type} is not unix the function 
#' returns always TRUE. Extend this function if necessary. Only 
#' relevant permission types have been added. 
#' 
#' @author Christoph Knapp
file.writable = function(file,debug){
  tryCatch({
    if(file.exists(file)&&
         "unix"%in%.Platform$OS.type&&
         "Linux"%in%Sys.info()["sysname"]){
      grepl("777",strsplit(system(paste("stat -c \"%a %n\" ",file,sep=""),intern=T)," ")[[1]][1])||
        grepl("775",strsplit(system(paste("stat -c \"%a %n\" ",file,sep=""),intern=T)," ")[[1]][1])||
        grepl("755",strsplit(system(paste("stat -c \"%a %n\" ",file,sep=""),intern=T)," ")[[1]][1])
    }else{
      F
    }
  },error = function(e){
    return(F)
  },finally = {})
}

#' Wrapper function for \code{dir.create} which returns 
#' whether information whether the directory was created.
#' 
#' @param path a character vector containing a single 
#' path name. Tilde expansion (see ?path.expand) is done.
#' @param showWarnings logical; should the warnings on 
#' failure be shown?
#' @param recursive logical. Should elements of the path 
#' other than the last be created? If true, like the Unix 
#' command \code{mkdir -p}.
#' @param mode the mode to be used on Unix-alikes: it 
#' will be coerced by \code{?as.octmode}. For 
#' \code{Sys.chmod} it is recycled along paths.
#' 
#' @note This function does most likely not work on a 
#' windows System.
#' 
#' @return TRUE if the directory was created, FALSE if 
#' not.
#' 
#' @author Christoph Knapp
dir.create.logical = function(path,
                              showWarnings=TRUE,
                              recursive = FALSE,
                              mode="0777"){
  result = tryCatch({
    if(!file.exists(path)){
      dir.create(path,showWarnings,recursive,mode)
    }
    T
  }, warning = function(w) {
    return(F)
  }, error = function(e) {
    return(F)
  }, finally = {})
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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


#' Loads data from a specified URL
#' 
#' @param URL A valid URL pointing to a data set
#' @param data.dir.import The directory the data set 
#' should be downloaded to
#' 
#' @return A list of two elements. 
#'         data.set = A data.frame object containing the loaded data.set
#'         data.name = The name of the data set as retrieved from the URL
#'         
#' @note This method is using the function download.file and the wget method. 
#' This might not work in all possible cases.
#' 
#' @author Christoph Knapp
get.data.from.URL = function(URL,data.dir.import){
  ret = list()
  #URL = gsub(" ", "%20", URL)
  URL = URLencode(URL)
  print(URL)
  name = strsplit(URL,"/")[[1]]
  name = strsplit(name[length(name)],"?",fixed=T)[[1]][1]
  if (!file.exists(paste(data.dir.import,"/Imported",sep=""))&&
        file.writable(data.dir.import)) {
    dir.create(paste(data.dir.import,"/Imported",sep=""), recursive = TRUE)
  }
  tryCatch({
    if(Sys.info()["sysname"] %in% c("Windows", "Linux"))
      download.file(url=URL,destfile=paste0(data.dir.import,"/Imported/",name),method="auto")
    else
      download.file(url=URL,destfile=paste0(data.dir.import,"/Imported/",name),method="curl")
    
    temp = load.data(data.dir.import,fileID = name, path = paste0(data.dir.import,"/Imported/",name))
    if(!is.null(temp[[2]])){
      ret$data.set = temp[[2]]
      ret$data.name = name
    }else{
      return(NULL)
    }
    ret
  },error = function(e){
    if(file.exists(paste0(data.dir.import,"/Imported/",name))){
      unlink(paste0(data.dir.import,"Imported/",name))
    }
    print(e)
  },warning = function(w) {
    print(w)
  },finally = {})
}

#' Connerts transparency or alpha value to a 
#' percentage integer.
#' 
#' @param value Value to convert. Either a 
#' value between 0 and 1 in steps of 0.01 
#' (back = T) or a value between 0 and 100
#' (back = F).
#' @param back Wether to convert from 
#' percentage into fraction (back = T) or
#' from fraction to percentage (back = F). 
#' 
convert.to.percent = function(value,back=F){
  #percentage to numeric
  if(is.null(value)){
    if(!back){
      x = 0
    }else{
      x = 1
    }
  }else{
    if(back){
      x = (100-value)*0.01
    }else{
      x = (1-value)*100
    }
  }
  x
}

get.transformation.string = function(transform_select,
                                     transform_variable_select,
                                     arg3){
  transformation.string = ""
  if(transform_select%in%"log"){
    transformation.string = paste0("log(",
                                   transform_variable_select,
                                   ")")
  }else if(transform_select%in%"sqrt"){
    transformation.string = paste0("sqrt(",
                                   transform_variable_select,
                                   ")")
  }else if(transform_select%in%"by degree"&&
             !arg3%in%""){
    transformation.string = paste0("I(",
                                   transform_variable_select,
                                   "^",
                                   arg3,
                                   ")")
  }else if(transform_select%in%"polynomial of degree"&&
             !arg3%in%""){
    transformation.string = paste0("poly(",
                                   transform_variable_select,
                                   ",",arg3,")")
  }
  transformation.string
}

#' Searches a list recursivly for a name and
#' returns the value associated with the name
#' 
#' @param list.seach A list to search
#' @param search.name A name to be found in a list. 
#' If null, the whole simplified list is returned.
#' 
#' @return The value found in the list associated with
#' search.name, NULL if not there.
#' 
#' @note This function is used to change the 
#' everchanging plot output from iNZightPlot.
#' 
#' @author Christoph Knapp
search.name = function(list.search,search.name=NULL){
  list.out=list()
  search = function(input.list,nam=NULL){
    if("list"%in%class(input.list)||
         "inzplotoutput"%in%class(input.list)||
         "inzgrid"%in%class(input.list)||
         "inzpar.list"%in%class(input.list)||
         "inzdot"%in%class(input.list)||
         "inzhist"%in%class(input.list)||
         "inzscatter"%in%class(input.list)||
         "inzbar"%in%class(input.list)){
      for(i in 1:length(input.list)){
        if(!is.null(names(input.list)[i])){
          nam = names(input.list)[i]
        }else{
          nam = i
        }
        search(input.list[[nam]],nam)
      }
    }else{
      if(is.null(nam)){
        nam=length(list.out)+1
      }
      if(!is.null(input.list)){
        if(nam%in%names(list.out)){
          temp = list.out[[nam]]
          list.out[[nam]][[length(list.out[[nam]])+1]] <<- input.list
        }else{
          list.out[[nam]] <<- list(input.list)
        }
      }
    }
  }
  search(list.search)
  if(is.null(search.name)){
    list.out
  }else{
    list.out[[search.name]]
  }
}
































































































#######################################################################################

addBarInference <- function(inflist, center, opts, zi) {
  if (is.null(inflist[[1]]$lower))
    return(NULL)
  
  bs <- attr(inflist, "bootstrap")
  col1 <- ifelse(bs, opts$inf.col.comp[2], opts$inf.col.comp[1])
  col2 <- ifelse(bs, opts$inf.col.conf[2], opts$inf.col.conf[1])
  
  if (is.null(zi))
    zi <- 1:ncol(inflist[[1]]$lower)
  
  ## Add vertical lines to bars
  lapply(c("conf", "comp"), function(n) {
    if (!is.null(inflist[[n]])) {
      low <- c(inflist[[n]]$lower[, zi])
      upp <- c(inflist[[n]]$upper[, zi])
      cis <- c(rbind(low, upp))
      
      grid.polyline(x = unit(rep(center, each = 2), "native"),
                    y = unit(cis, "native"),
                    id = rep(1:length(center), each = 2),
                    gp =
                      gpar(col = switch(n, 'comp' = col1, 'conf' = col2),
                           lwd = switch(n,
                                        'comp' = opts$inf.lwd.comp / sqrt(nrow(inflist[[n]]$lower)),
                                        'conf' = opts$inf.lwd.conf / sqrt(nrow(inflist[[n]]$lower))
                           ),
                           lineend = "butt"))
    }
  })                   
}

addBarCompLines <- function(comp, bounds, phat, opts, zi) {
  
  ## Add horizontal comparison lines
  
  if (is.null(zi))
    zi <- 1:ncol(phat)
  
  n1 <- ncol(phat)
  n2 <- nrow(phat)
  
  
  
  if (n2 == 1) {
    x <- rep(range(bounds), n1 * 2)
  } else {
    x <- c(apply(apply(matrix(bounds, nrow = 2 * n2), 2, range),
                 2, rep, times = n2 * 2))
  }
  
  y <- rep(c(do.call(rbind, comp)[, zi]), each = 2)
  id <- rep(1:(n2 * n1 * 2), each = 2)
  
  grid.polyline(x = unit(x, "native"), y = unit(y, "native"),
                id = id, gp = gpar(lty = 3, col = "grey50", lwd = 0.5))
}




addXYsmoother <- function(obj, opts, col.args, xlim, ylim) {
  ## decide what x and y are:
  if ("svy" %in% names(obj)) {
    if (inherits(obj$svy, "survey.design")) {
      x <- obj$svy
      y <- NULL
    } else {
      # na's arent removed
      x <- obj$svy$x
      y <- obj$svy$y
      isna <- is.na(x) | is.na(y)
      x <- x[!isna]
      y <- y[!isna]
    }
  } else if ("args" %in% names(obj)) {
    x <- obj$args$df$x
    y <- obj$args$df$y
    isna <- is.na(x) | is.na(y)
    x <- x[!isna]
    y <- y[!isna]
  } else {
    x <- obj$x
    y <- obj$y
  }
  
  if (length(opts$quant.smooth) > 0) {
    if (inherits(x, "survey.design"))
      X <- x
    else
      X <- cbind(x, y)
    
    qs <- try(calcQSmooth(X, opts$quant.smooth, opts), TRUE)
    if (!inherits(qs, "try-error")) {
      qp <- qs$qp
      lty <- qs$lty
      lwd <- qs$lwd
      for (q in 1:length(qp))
        try(addQuantileSmoother(x, y, quantile = qp[q],
                                col = opts$col.smooth,
                                lty = lty[q], lwd = lwd[q]), TRUE)
    }
  } else if (!is.null(opts$smooth)) {
    # Smoothers
    if (opts$smooth != 0) {
      if (opts$smooth > 1) {
        warning("Smoothing value must be in the interval [0, 1]")
      } else {
        if (length(unique(obj$col)) == 1 | !opts$trend.by) {
          try(addSmoother(x, y, f = opts$smooth,
                          col = opts$col.smooth, bs = opts$bs.inference), TRUE)
        } else {
          byy <- as.factor(obj$col)  # pseudo-by-variable
          xtmp <- lapply(levels(byy), function(c) subset(x, obj$col == c))
          ytmp <- lapply(levels(byy), function(c) subset(y, obj$col == c))
          
          for (b in 1:length(levels(byy)))
            try(addSmoother(xtmp[[b]], ytmp[[b]],
                            f = opts$smooth,
                            col = darken(col.args$f.cols[b]),
                            bs = FALSE, lty = opts$smoothby.lty), TRUE)
        }
      }
    }
  }
}

addSmoother <-
  function(x, y = NULL, f, col, bs, lty = 1) {
    if (is.null(y) & inherits(x, "survey.design")) {
      sm <- svysmooth(y ~ x, design = x, method = "locpoly")[[1]]
    } else {
      sm <- lowess(x, y, f = f)
    }
    grid.lines(sm$x, sm$y,
               default.units = "native",
               gp = gpar(col = col, lwd = 2, lty = lty))
    
    if (bs) {
      for (i in 1:30) {
        # User wants bootstrap inference for the smoother:
        id <- sample(1:length(x), replace = TRUE)
        x2 <- x[id]
        y2 <- y[id]
        sm <- lowess(x2, y2, f = f)
        grid.lines(sm$x, sm$y,
                   default.units = "native",
                   gp = gpar(col = col, lwd = 1, lty = 3))
      }
    }
  }

addQuantileSmoother <-
  function(x, y = NULL, quantile, col, lty, lwd) {    
    # Draws quantiles on a plot.
    if (quantile < 0.5)  # symmetry
      quantile <- c(quantile, 1 - quantile)
    
    # Because we are using the `svysmooth()` function from the `survey` package,
    # we need to supply a design (here, everything is IID)
    if (is.null(y) & inherits(x, "survey.design"))
      des <- x
    else
      des <- suppressWarnings(svydesign(ids = ~1, data = data.frame(x = x, y = y)))
    
    invisible(sapply(quantile,
                     function(a) {
                       s <- svysmooth(y ~ x, design = des,
                                      method = "quantreg", quantile = a)$x
                       grid.lines(s$x, s$y, default.units = "native",
                                  gp = gpar(col = col, lty = lty, lwd = lwd))
                     }))
  }


calcQSmooth <- function(xy, q, opts) {
  if (inherits(xy, "survey.design")) {
    x <- xy$variables[, c("x", "y")]
    x <- x[!apply(x, 1, function(y) any(is.na(y))), ]
  } else {
    x <- xy[!apply(xy, 1, function(y) any(is.na(y))), ]
  }
  
  # check quantiles are correct:
  if (q[1] == "default") {
    qp <- 0.5
    if (nrow(x) > opts$quant.cutoff[1]) qp <- c(qp, 0.25)
    if (nrow(x) > opts$quant.cutoff[2]) qp <- c(qp, 0.1)
  } else {
    qp <- q
  }
  
  if (any(qp < 1 & qp > 0)) {
    qp <- qp[qp > 0 & qp < 1]  # remove invalid quantiles
    
    qp[qp > 0.5] <- qp[qp > 0.5] - 0.5  # symmetry!
    # incase user gives c(0.25, 0.75), remove duplicates
    qp <- sort(unique(qp), decreasing = TRUE)
    
    # Sort out the line type and width:
    nn <- length(qp)
    # bb: the base number of repeats for each unit
    bb <- rep(nn %/% 3, 3)
    be <- nn %% 3  # which units repeated once more
    q.reps <- bb
    if (be != 0) q.reps[1:be] <- q.reps[1:be] + 1
    lty <- rep(1:3, q.reps)
    
    # Line width (less complicated! ...)
    lwd <- rep(1, length(qp))
    lwd[1] <- 2
    if (length(x) > opts$large.sample.size)
      lwd <- lwd + 1
    
    qs <- list(qp = qp,
               lty = lty,
               lwd = lwd)
  } else {
    qs <- NULL
  }
  
  qs
}




addXYtrend <- function(obj, opts, col.args, xlim, ylim) {
  # Trend lines:
  # ------------------------------------------------------------- #
  # If the `by` variable has been set, then the points are        
  # coloured by the levels of `by`. Thus, there is more than one
  # level of `unique(col)`. In this case, we need to add the
  # trend lines for each level of by (i.e., each colour). The
  # colours of these lines are darker versions of the points.
  # ------------------------------------------------------------- #
  
  ## decide what x and y are:
  if ("svy" %in% names(obj)) {
    if (inherits(obj$svy, "survey.design")) {
      x <- obj$svy
      y <- NULL
    } else {
      # na's arent removed
      x <- obj$svy$x
      y <- obj$svy$y
      isna <- is.na(x) | is.na(y)
      x <- x[!isna]
      y <- y[!isna]
    }
  } else if ("args" %in% names(obj)) {
    x <- obj$args$df$x
    y <- obj$args$df$y
    isna <- is.na(x) | is.na(y)
    x <- x[!isna]
    y <- y[!isna]
  } else {
    x <- obj$x
    y <- obj$y
  }
  
  if (!is.null(opts$trend)) {
    if (length(unique(obj$col)) == 1 | !opts$trend.by) {
      lapply(opts$trend, function(o) {
        order = which(c("linear", "quadratic", "cubic") == o)  # gives us 1, 2, or 3
        addTrend(x, y, order = order, xlim = xlim,
                 col = opts$col.trend[[o]], bs = opts$bs.inference)
      })
    } else if (opts$trend.parallel) {
      byy <- as.factor(obj$col)
      lapply(opts$trend, function(o) {
        order = which(c("linear", "quadratic", "cubic") == o)
        addParTrend(x, y, byy, order = order, xlim = xlim,
                    cols = col.args$f.cols)
      })
    } else {
      byy <- as.factor(obj$col)  # pseudo-by-variable
      xtmp <- lapply(levels(byy), function(c) subset(x, obj$col == c))
      ytmp <- lapply(levels(byy), function(c) subset(y, obj$col == c))
      
      for (b in 1:length(levels(byy)))
        lapply(opts$trend, function(o) {
          order = which(c("linear", "quadratic", "cubic") == o)
          addTrend(xtmp[[b]], ytmp[[b]],
                   order = order, xlim = xlim,
                   col = col.args$f.cols[b],
                   bs = opts$bs.inference)
        })
    }
  }
}

addTrend <-
  function(x, y, order, xlim, col, bs) {
    xx <- seq(xlim[1], xlim[2], length = 1001)
    if (is.svy <- inherits(x, "survey.design")) {
      if(length(order)==1){
        svy <- x
        expr <- switch(order,
                       formula(y ~ x),
                       formula(y ~ x + I(x^2)),
                       formula(y ~ x + I(x^2) + I(x^3)))
      }
      yy <- try(predict(svyglm(expr, design = svy), data.frame(x = xx)),
                silent = TRUE)
    } else {
      yy <- try(c(predict(lm(y ~ poly(x, order)), data.frame(x = xx))),
                silent = TRUE)
    }
    
    # Sometimes, there might not be enough data points do run poly(),
    # so in this case simply don't draw.
    if (!inherits(yy, "try-error")) {
      grid.lines(xx, yy,
                 default.units = "native",
                 gp = gpar(col = col, lwd = 2, lty = order))
      
      if (bs) {
        bs.lines <- vector("list", 30)
        
        if (is.svy) {
          return(NULL)
        } else {
          for (i in 1:30) {
            ## User wants bootstrap inference for this line.
            id <- sample(1:length(x), replace = TRUE)
            x2 <- x[id]
            y2 <- y[id]
            
            yy <- try(predict(lm(y2 ~ poly(x2, order)), data.frame(x2 = xx)),
                      silent = TRUE)
            
            ## Some bootstraps can have less than `order` unique points:
            if (inherits(yy, "try-error")) next
            
            bs.lines[[i]] <- cbind(xx, yy, rep(i, length(yy)))
          }
        }
        
        all.lines <- do.call(rbind, bs.lines)
        grid.polyline(all.lines[, 1], all.lines[, 2], id = all.lines[, 3],
                      default.units = "native",
                      gp = gpar(col = col, lwd = 1, lty = 3))
      }
    }
  }

addParTrend <- function(x, y, by, order, xlim, cols) {
  xx <- rep(seq(xlim[1], xlim[2], length = 1001), length(lby <- levels(by)))
  byy <- rep(lby, each = 1001)
  if (inherits(x, "survey.design")) {
    if(length(order)==1){
      svy <- x
      expr <- switch(order,
                     formula(y ~ x + colby),
                     formula(y ~ x + I(x^2) + colby),
                     formula(y ~ x + I(x^2) + I(x^3) + colby))
      yy <- try(predict(LM <- svyglm(expr, design = svy), data.frame(x = xx, colby = byy)),
                silent = TRUE)
    }
  } else {
    yy <- try(c(predict(LM <- lm(y ~ poly(x, order) + by), data.frame(x = xx, by = byy))),
              silent = TRUE)
  }
  
  # Sometimes, there might not be enough data points do run poly(),
  # so in this case simply don't draw.
  if (!inherits(yy, "try-error")) {
    for (i in 1:length(lby)) {
      grid.lines(xx[byy == lby[i]], yy[byy == lby[i]],
                 default.units = "native",
                 gp = gpar(col = (cols[i]), lwd = 2, lty = order))
    }
  }
}


addUnivarInference <- function(inflist, i, opts) {
  bs <- attr(inflist, "bootstrap")
  col1 <- ifelse(bs, opts$inf.col.comp[2], opts$inf.col.comp[1])
  col2 <- ifelse(bs, opts$inf.col.conf[2], opts$inf.col.conf[1])
  
  ## We can only display one type of inference (mean OR median), so take the fist:
  inflist <- inflist[[1]]
  
  if (is.character(inflist)) return()
  
  lapply(rev(c("conf", "comp")), function(n) {
    if (!is.null(inflist[[n]])) {
      ci <- inflist[[n]][i, c("lower", "upper")]
      grid.lines(x = unit(ci, units = "native"),
                 y = unit(0.5, "npc"),
                 gp =
                   gpar(col = switch(n, 'comp' = col1, 'conf' = col2),
                        lwd = switch(n, 'comp' = 5, 'conf' = 2),
                        lineend = "butt"))
    }
  })
}

addUnivarCompLines <- function(inflist) {
  inflist <- inflist[[1]]
  guides <- unique(c(inflist$comp[, c("lower", "upper")]))
  if (!is.null(guides))
    grid.polyline(x = unit(rep(guides, each = 2), "native"),
                  y = rep(c(0, 1), length(guides)),
                  id = rep(1:length(guides), each = 2),
                  gp = gpar(lty = 3, col = "grey50", lwd = 0.5))
}



createPlot <- function(df, opts, xattr) {
  # This function takes a data.frame object and creates the necessary object which will have a
  # `plot` method.
  
  if (is.null(df))
    return(nullPlot(opts, xattr))
  
  large <- ifelse(is.null(lg <- opts$largesample),
                  nrow(df) > opts$large.sample.size, lg)
  wts <- xattr$class != "inz.simple"
  
  v <- xattr$v
  vt <- xattr$vartypes
  xfact <- vt$x == "factor"
  ynull <- ! "y" %in% v
  yfact <- if (ynull) FALSE else vt$y == "factor"
  xnum <- !xfact
  ynum <- if (ynull) FALSE else !yfact
  
  ## allow forcing the plot type:
  ## -- here, the switch takes the given plot type, checks the data types are correct,
  ## and if they aren't, just uses the default
  plottype <- gsub("plot", "", opts$plottype)  # remove `plot` from type, if specified
  type <- switch(plottype,
                 "bar" = ifelse(xfact & (ynull | yfact), plottype, "default"),
                 "hist" = ,
                 "dot" = ifelse((xnum & !ynum) | (!xnum & ynum), plottype, "default"),
                 "scatter" = ,
                 "grid" = ,
                 "hex" = ifelse(xnum & ynum, plottype, "default"),
                 "default")
  
  # throw a warning if they give an invalid type
  if (type != plottype)
    warning("The plot type specified does not match the supplied data.")
  
  if (type == "default") {
    if (ynull) {
      type <- ifelse(xfact, "barplot",
                     ifelse(large | wts, "histplot", "dotplot"))
    } else {
      type <- ifelse(xfact,
                     ifelse(yfact, "barplot",
                            ifelse(large | wts, "histplot", "dotplot")),
                     ifelse(yfact,
                            ifelse(large, "histplot", "dotplot"),
                            ifelse(large,
                                   ifelse(wts, "hexplot", "gridplot"),
                                   "scatterplot")))
    }
  } else {
    type <- paste0(type, "plot")
  }
  
  # Here, we create a class for the object to be plotted, then we use a generic function `create`
  # which will use the correct method, and create the required plot.
  
  pclass <- paste("inz", type, sep = ".")
  obj <- structure(.Data = list(df = df, opts = opts, xattr = xattr),
                   class = pclass)
  create(obj)
}

create <- function(obj)
  UseMethod("create")



drawAxes <- function(x, which = "x", main = TRUE, label = TRUE, opts, sub = 0, heightOnly = FALSE,
                     layout.only = FALSE) {
  if (is.numeric(x)) { 
    switch(which,
           "x" = {
             if (main) {
               grid.xaxis(gp = gpar(cex = opts$cex.axis), main = main, label = label)
             } else {
               xlim <- current.viewport()$xscale
               pushViewport(viewport(x = 0.5, y = 1, height = unit(sub, "in"), just = "bottom",
                                     xscale = xlim))
               grid.xaxis(gp = gpar(cex = opts$cex.axis), label = label, main = FALSE)
               upViewport()
             }
           }, "y" = {
             yax <- yaxisGrob(gp = gpar(cex = opts$cex.axis), main = main, label = label)
             if (label)
               yax <- editGrob(yax, edits =
                                 gEdit("labels", rot = ifelse(main, 90, 270),
                                       hjust = 0.5, vjust = ifelse(main, 0, -0.5)))
             grid.draw(yax)
           })
  } else {
    if (is.null(opts$ZOOM))
      x.lev <- levels(x)
    else {
      ZOOM <- opts$ZOOM
      ww <- ZOOM[1]:(sum(ZOOM) - 1)
      nl <- length(levels(x))
      ww <- ww - nl * (ww > nl)
      x.lev <- levels(x)[ww]
    }
    
    switch(which,
           "x" = {
             rot <- opts$rot
             labText <- textGrob(x.lev,
                                 x = unit((0:length(x.lev))[-1] - 0.5, "native"),
                                 y = if (rot) unit(-0.5, "mm") else unit(-1, "lines"),
                                 just = if (rot) c("right", "top") else "center",
                                 rot = ifelse(rot, 30, 0),
                                 gp = gpar(cex = opts$cex.axis * ifelse(rot, 0.8, 1)),
                                 name = "labelText")  # label is important!
             wm <- which.max(nchar(as.character(x.lev)))
             tt <- textGrob(levels(x)[wm])
             # save label widths
             labwid <- convertWidth(grobWidth(tt), "mm", valueOnly = TRUE)
             
             if (heightOnly) {
               return(grobHeight(labText))
             } else {
               grid.draw(labText)
             }
           }, "y" = {
             if (!is.null(x) & !layout.only) {
               labels <- levels(x)
               Nlab <- length(labels)
               for (i in 1:Nlab) {
                 seekViewport(paste0("VP:plotregion-", i))
                 grid.text(labels[i], x = unit(-0.5, "lines"), just = "right", gp = gpar(cex = opts$cex.axis))
                 upViewport()
               }
             }
           })
  }
}




drawLegend <-
  function(lab, col, pch = opts$pch, cex.mult = 1,
           title = "", any.missing = FALSE, opts = inzpar()) {
    
    legcex <- opts$cex.text * cex.mult
    
    title.grob <- textGrob(title, gp = gpar(cex = legcex * opts$cex.lab),
                           just = c("center"))
    if (title != "") {
      title.hgt <- convertHeight(grobHeight(title.grob), "in") * 2
      title.wd <- convertWidth(grobWidth(title.grob), "in", TRUE)
    } else {
      title.hgt <- unit(0, "in")
      title.wd <- 0
    }
    
    lab.width <- max(sapply(lab, function(x)
      convertWidth(grobWidth(textGrob(x, gp = gpar(cex = legcex))),
                   "in", TRUE)))
    lab.height <- convertHeight(grobHeight(textGrob(lab[1], gp = gpar(cex = legcex))),
                                "in", TRUE) * 2
    
    col2.wd <- max(title.wd - convertWidth(unit(2, "lines"), "in", TRUE), lab.width)
    
    ## want to create a layout with one row for each label, one for NA (if any are missing),
    ## a row for the title, and a line-height row between title and legend:
    if (any.missing) {
      lab <- append(lab, "missing")
      col <- append(col, opts$col.missing)
    }
    n <- length(lab)
    leg.layout <- grid.layout(n + 2, 3,
                              widths = unit.c(unit(2, "lines"), unit(col2.wd, "in"), unit(0.5, "lines")),
                              heights = unit.c(title.hgt, unit(0.5, "lines"),
                                               unit(rep(lab.height, n), "in")))
    
    fg <- frameGrob(layout = leg.layout)
    fg <- placeGrob(fg, title.grob, row = 1, col = 1:2)
    
    ## change pch into filled in one:
    if (pch %in% c(1, 2, 5, 6))
      pch <- switch(which(pch == c(1, 2, 5, 6)), 21, 24, 23, 25)
    
    for (i in 1:n) {
      fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = pch,
                                     gp =
                                       gpar(col = col[i], cex = legcex, lwd = opts$lwd.pt,
                                            fill = col[i])),
                      col = 1, row = i + 2)
      
      fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                   just = c("left", "center"),
                                   gp = gpar(cex = legcex)),
                      col = 2, row = i + 2)
    }
    
    fg
  }

drawContLegend <- function(var, title = "", height = NULL, cex.mult = 1,
                           any.missing = FALSE, opts = inzpar()) {
  
  legcex <- opts$cex.text * cex.mult
  
  title.grob <- textGrob(title, gp = gpar(cex = legcex * opts$cex.lab),
                         just = c("center"))
  title.hgt <- convertHeight(grobHeight(title.grob), "in") * 2
  
  vp <- viewport(yscale = range(var, na.rm = TRUE))
  yax <- yaxisGrob(main = FALSE, vp = vp, gp = gpar(cex = legcex * opts$cex.axis))
  
  ## need legend to fit the longest label
  var2 <- if (any.missing) c(var, "missing") else  var
  maxlablen <- convertWidth(grobWidth(textGrob(var2[which.max(nchar(var2))])), "in", TRUE)
  yax.wd <- (convertWidth(unit(1, "lines"), "in", TRUE) + maxlablen) * legcex * opts$cex.axis
  title.wd <- convertWidth(grobWidth(title.grob), "in", TRUE)
  rect.wd <- convertWidth(unit(2, "char"), "in", TRUE)
  col2.wd <- max(title.wd - rect.wd, yax.wd)
  
  legend.layout <- grid.layout(nrow = 5, ncol = 3,
                               widths = unit.c(unit(rect.wd, "in"), unit(col2.wd, "in"), unit(0.5, "lines")),
                               heights = unit.c(title.hgt, unit(0.5, "lines"), unit(height, "in"),
                                                unit(1, "lines"), unit(1, "lines")))
  
  ## vectorize the drawing of the scale to make it fast!
  xx <- rep(c(0, 1, 1, 0), 200)
  yy <- rep(0:200 / 200, each = 4)[1:800 + 2]
  id <- rep(1:200, each = 4)
  poly <- polygonGrob(xx, yy, id = id, gp = gpar(lty = 0, fill = (n.cols <- rainbow(200, start = 1/6))))
  
  fg <- frameGrob(layout = legend.layout)
  fg <- placeGrob(fg, poly, row = 3, col = 1)
  fg <- placeGrob(fg, rectGrob(width = unit(rect.wd, "in"), gp = gpar(fill = "transparent")),
                  col = 1, row = 3)
  fg <- placeGrob(fg, title.grob, row = 1, col = 1:2)
  fg <- placeGrob(fg, yax, row = 3, col = 1)
  
  if (any.missing) {
    fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = 21, gp =
                                     gpar(col = opts$col.missing, cex = legcex,
                                          lwd = opts$lwd.pt, fill = opts$col.missing)),
                    row = 5, col = 1)
    fg <- placeGrob(fg, textGrob("missing", x = unit(1, "lines"), y = 0.5, just = c("left", "center"),
                                 gp = gpar(cex = legcex * opts$cex.axis)),
                    col = 2, row = 5)
  }
  
  list(fg = fg, n.cols = n.cols)
}


drawLinesLegend <- function(x, opts = inzpar(), cex.mult = 1) {
  
  lines.list <- list()
  if (length(opts$trend) > 0) {
    if (all(opts$trend != FALSE)) {
      if (opts$trend.by) { #opts$trend.parallel) {
        for (i in 1:length(opts$trend)) {
          lines.list <- c(lines.list,
                          list(c(opts$trend[i], "black",
                                 which(opts$trend[i] == c("linear", "quadratic", "cubic")),
                                 opts$lwd)))
        }
      } else {
        for (i in 1:length(opts$trend)) {
          lines.list <- c(lines.list,
                          list(c(opts$trend[i],
                                 opts$col.trend[[opts$trend[i]]],
                                 opts$lty, opts$lwd)))
        }
      }
    }
  }
  if (length(opts$quant.smooth) > 0) {
    qs <- calcQSmooth(x, opts$quant.smooth, opts)
    if (!is.null(qs)) {
      for (i in 1:length(qs$qp)) {
        lines.list <- c(lines.list,
                        list(c(paste0(qs$qp[i] * 100,
                                      ifelse(qs$qp[i] != 0.5,
                                             paste0(" - ", (1 - qs$qp[i]) * 100),
                                             ""),
                                      "%"),
                               opts$col.smooth,
                               qs$lty[i], qs$lwd[i])))
      }
    }
  } else {
    if (!is.null(opts$smooth)) {
      if (opts$smooth != 0) {
        if (opts$trend.by) {
          lines.list <- c(lines.list,
                          list(c("smoother", "black",
                                 opts$smoothby.lty, opts$lwd)))
        } else {
          lines.list <- c(lines.list,
                          list(c("smoother", opts$col.smooth,
                                 opts$lty, opts$lwd)))
        }
      }
    }
  }
  if (opts$LOE) {
    lines.list <- c(lines.list,
                    list(c("x=y line", opts$col.LOE, opts$lty.LOE, opts$lwd)))
  }
  
  
  ## if there aren't any lines, return nothing, otherwise return legend grob
  if (length(lines.list) == 0)
    return(NULL)
  
  legcex <- opts$cex.text * cex.mult
  lab <- sapply(lines.list, function(x) x[1])
  col <- sapply(lines.list, function(x) x[2])
  lty <- as.numeric(sapply(lines.list, function(x) x[3]))
  lwd <- as.numeric(sapply(lines.list, function(x) x[4]))
  
  lab.width <- max(sapply(lab, function(x)
    convertWidth(grobWidth(textGrob(x, gp = gpar(cex = legcex))),
                 "in", TRUE)))
  lab.height <- convertHeight(grobHeight(textGrob(lab[1], gp = gpar(cex = legcex))),
                              "in", TRUE) * 2
  
  n <- length(lab)
  leg.layout <- grid.layout(n + 1, 3,
                            widths = unit.c(unit(2, "lines"), unit(lab.width, "in"), unit(0.5, "lines")),
                            heights = unit.c(unit(2, "lines"), unit(rep(lab.height, n), "in")))
  
  fg <- frameGrob(layout = leg.layout)
  
  for (i in 1:n) {
    fg <- placeGrob(fg, linesGrob(c(0.2, 0.8), 0.5,
                                  gp =
                                    gpar(col = col[i], lty = lty[i], lwd = lwd[i] * 2)),
                    col = 1, row = i + 1)
    
    fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                 just = c("left", "center"),
                                 gp = gpar(cex = legcex)),
                    col = 2, row = i + 1)
  }
  
  fg
}




##' Allows easy viewing of every variable in the data set.
##'
##' @title Explore all Univariate Plots
##' @param data a data frame
##' @return NULL
##' @author tell029
##' @export
exploreAllPlots <- function(data) {
  ## Runs iNZightPlot on all of the variables, with a click-for-next thing.
  
  grid.newpage()
  pushViewport(viewport())
  
  # Title window:
  grid.text(paste("Click the next button to see next plot."))
  
  oask <- devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
  for (i in 1:ncol(data)) {
    iNZightPlot(data[, i], varnames = list(x = colnames(data)[i]))
    dev.flush()
  }
  devAskNewPage(oask)
  grid.newpage()
}



##' Allows easy access to a summary for every variable in the data set.
##'
##' @title Explore all Univariate Summaries
##' @param data a data set
##' @return allSummaries object
##' @author tell029
##' @export
exploreAllSummaries <- function(data) {
  ## Runs getPlotSummary() on all variables.
  
  sums <- lapply(colnames(data),
                 function(x) {
                   paste(getPlotSummary(data[, x], varnames = list(x = x)),
                         collapse = "\n")
                 })
  
  tt <- paste(sums, collapse = "\n\n")
  
  class(tt) <- "allSummaries"
  tt
}

##' @export
print.allSummaries <- function(x, ...)
  cat(x)



rescale <- function(x) {
  r <- 4 * (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) + 0.5
  r[is.na(x)] <- NA
  r
}

Darken <- function(x = "#FFFFFF") {
  if (x %in% colours()) {
    x <- rgb(convertColor(t(col2rgb(x)), "sRGB", "Apple RGB"))
  }
  
  x <- gsub('#', '', x)
  if (nchar(x) == 3)
    x <- paste(rep(strsplit(x, '')[[1]], each = 2), collapse = '')
  
  ## catch any transparency added to the colours
  if (nchar(x) == 8) {
    alpha <- substr(x, 7, 8)
    x <- substr(x, 1, 6)
  } else {
    alpha <- ""
  }
  
  if (nchar(x) != 6)
    stop("Not a valid hexadecimal code!")
  
  # Now start the function!
  
  r <- substr(x, 1, 2)
  g <- substr(x, 3, 4)
  b <- substr(x, 5, 6)
  
  dark <- strtoi(c(r, g, b), base = 16) * 0.6 / 255
  
  paste0(rgb(dark[1], dark[2], dark[3]), alpha)
}

darken <- Vectorize(Darken)  # allow it to work on a vector of Xs


Shade <- function(x, light) {
  if (x %in% colours()) {
    x <- rgb(convertColor(t(col2rgb(x)), "sRGB", "Apple RGB"))
  }
  
  x <- gsub('#', '', x)
  if (nchar(x) == 3)
    x <- paste(rep(strsplit(x, '')[[1]], each = 2), collapse = '')
  
  ## catch any transparency added to the colours
  if (nchar(x) == 8) {
    alpha <- substr(x, 7, 8)
    x <- substr(x, 1, 6)
  } else {
    alpha <- ""
  }
  
  if (nchar(x) != 6)
    stop("Not a valid hexadecimal code!")
  
  rgb <- c(substr(x, 1, 2),
           substr(x, 3, 4),
           substr(x, 5, 6))
  rgb <- strtoi(rgb, base = 16)
  
  if (light > 1 | light < -1) {
    stop("light must be in [-1, 1]")
  }
  
  if (light < 0) {
    rgb <- (1 + light) * rgb
  } else {
    rgb <- (1 - light) * rgb + light * 255
  }
  
  rgb <- rgb / 255
  paste0(rgb(rgb[1], rgb[2], rgb[3]), alpha)
}

shade <- Vectorize(Shade)


##' Convert a numeric variable in to a factor with four levels.
##'
##' @title Convert to Factor
##' @param x a numeric vector
##' @return a factor vector
##' @author tell029
##' @export
convert.to.factor <-
  function(x) {
    if (is.factor(x)) {
      # to simplify coding elsewhere, allow convert to factor to simply return
      # the supplied x vector if it is already a factor.
      x.fact <- x
    } else {
      
      ## converts a 
      if (length(unique(x)) < 5)
        x.fact <- factor(x)
      else {  
        x.quantiles <- round((quantile(x, na.rm = TRUE)), 0)  
        x.fact <- try(cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                            x.quantiles[2:4],
                                            unique(x.quantiles[2:4])),
                               Inf)))
        
        if (inherits(x.fact, "try-error")) {
          eps <- .Machine$double.eps
          x.quantiles <- round((quantile(x, na.rm = TRUE)), 2) + eps * (0:10)
          x.fact <- cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                          x.quantiles[2:4],
                                          unique(x.quantiles[2:4])),
                             Inf))
        }
        
        if ((x.quantiles[2] == x.quantiles[3]) && (x.quantiles[3] == x.quantiles[4]))
          levels(x.fact) <-
          c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
            paste(c("(", x.quantiles[2], " - ", x.quantiles[5], "]"), collapse = ""))
        else if (x.quantiles[2] == x.quantiles[3])
          levels(x.fact) <-
          c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
            paste(c("(", x.quantiles[2], " - ", x.quantiles[4], "]"), collapse = ""),
            paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
        else if (x.quantiles[3] == x.quantiles[4])
          levels(x.fact) <-
          c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
            paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
            paste(c("(", x.quantiles[3], " - ", x.quantiles[5], "]"), collapse = ""))
        else
          levels(x.fact) <-
          c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
            paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
            paste(c("(", x.quantiles[3], " - ", x.quantiles[4], "]"), collapse = ""),
            paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
      }
    }
    
    # Remove any empty levels -_-
    factor(x.fact) 
  }


nullPlot <- function(opts, xattr) {
  # simply draw nothing!
  out <- list(xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))
  class(out) <- "inznull"
  out
}
plot.inznull <- function(...) {
  return(invisible(NULL))
}


genCols <- function(n) {
  # This allows us to simply edit the type of colour schemes used
  #rainbow(n, v = 0.7, start = 1/6)
  hcl((1:n) / n * 360, c = 80, l = 50)
}

colourPoints <- function(x, col.args, opts = inzpar()) {
  if (is.null(x))
    return(opts$col.pt)
  xclass <- ifelse(is.numeric(x), "numeric", "factor")
  switch(xclass,
         "numeric" = {
           xr <- col.args$n.range
           xm <- xr[1]
           xsc <- as.integer(199 * ((x - xm) / diff(xr)) + 1)
           ifelse(is.na(x), col.args$missing, col.args$n.cols[xsc])
         }, "factor" = {
           x <- as.character(x)
           x[is.na(x)] <- "missing"
           col.args$f.cols[x]
         })
}




inference <- function(object, ...)
  UseMethod("inference")


inference.inzdot <- function(object, bs, class, width, ...) {
  toplot <- object$toplot
  inf <- object$inference.info
  
  if (is.character(inf))
    return("Sample too small to get inference")
  
  if (is.null(inf[[1]]$conf))
    stop("Please specify `inference.type = conf` to get inference information.")
  
  mat <- inf$mean$conf[, c("lower", "mean", "upper"), drop = FALSE]
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, digits = 4)
  }), nrow = nrow(mat))
  
  ## Remove NA's and replace with an empty space
  mat[grep("NA", mat)] <- ""
  
  ## Text formatting to return a character vector - each row of matrix
  mat <- rbind(c("Lower", "Mean", "Upper"), mat)
  colnames(mat) <- NULL
  
  byFactor <- length(toplot) > 1
  
  if (byFactor)
    mat <- cbind(c("", names(toplot)), mat)
  rownames(mat) <- NULL
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, justify = "right")
  }), nrow = nrow(mat))
  
  out <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
  
  plural <- ifelse(byFactor, "s", "")
  bsCI <- ifelse(bs, " Percentile Bootstrap", "")
  out <- c(paste0(ifelse(byFactor,
                         "Group Means", "Mean"),
                  " with 95%", bsCI, " Confidence Interval", plural), "",
           out)
  
  if (bs) {
    ## BOOTSTRAP MEDIAN
    mat <- inf$median$conf[, c("lower", "mean", "upper"), drop = FALSE]
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, digits = 4)
    }), nrow = nrow(mat))
    
    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    
    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Lower", "Median", "Upper"), mat)
    colnames(mat) <- NULL
    
    byFactor <- length(toplot) > 1
    
    if (byFactor)
      mat <- cbind(c("", names(toplot)), mat)
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat))
    
    mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    
    out <- c(out, "",
             paste0(ifelse(byFactor,
                           "Group Medians", "Median"),
                    " with 95%", bsCI, " Confidence Interval", plural), "", mat)
    
    
    ## BOOTSTRAP INTERQUARTILE RANGE
    mat <- inf$iqr$conf[, c("lower", "mean", "upper"), drop = FALSE]
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, digits = 4)
    }), nrow = nrow(mat))
    
    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    
    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Lower", "IQR", "Upper"), mat)
    colnames(mat) <- NULL
    
    byFactor <- length(toplot) > 1
    
    if (byFactor)
      mat <- cbind(c("", names(toplot)), mat)
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat))
    
    mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    
    out <- c(out, "",
             paste0(ifelse(byFactor,
                           "Group Interquartile Ranges", "Interquartile Range"),
                    " with 95%", bsCI, " Confidence Interval", plural), "", mat)
  }
  
  
  
  ### NOTE: this doesn't account for SURVEY design yet.
  
  if (byFactor & !bs) {
    ## For x ~ factor, we also include an F test, and multiple comparisons
    ## (estimates, pvalues, and confidence intervals).
    
    dat <- do.call(rbind, lapply(names(toplot),
                                 function(t) {
                                   if (is.null(toplot[[t]]$x))
                                     NULL
                                   else
                                     data.frame(x = toplot[[t]]$x, y = t)
                                 }
    ))
    fit <- lm(x ~ y, data = dat)
    
    fstat <- summary(fit)$fstatistic
    
    Ftest <- c("Overall F-test", "",
               paste0("F: ", signif(fstat[1], 5), "   ", 
                      "df: ", fstat[2], " and ", fstat[3], "   ",
                      "p-value: ",
                      format.pval(pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE),
                                  digits = 2)))
    
    out <- c(out, "", Ftest, "",
             "", "   *** Differences between Group Means (column - row) ***", "")
    
    means <- predict(fit, newdata = data.frame(y = levels(dat$y)))
    names(means) <- LEVELS <- levels(dat$y)
    diffMat <- outer(means, means, function(x, y) y - x)
    diffMat <- formatTriMat(diffMat, LEVELS)
    
    out <- c(out,
             "Estimates", "",
             apply(diffMat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
    
    mc <- try(s20x::multipleComp(fit))
    if (!inherits(mc, "try-error")) {
      cimat <- triangularMatrix(LEVELS, mc, "ci")
      cimat <- formatMat(cimat)
      
      out <- c(out, "",
               "95% Confidence Intervals (adjusted for multiple comparisons)", "",
               apply(cimat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
      
      
      pmat <- triangularMatrix(LEVELS, mc, "p-values")
      pmat <- formatMat(pmat, 2)
      
      out <- c(out, "",
               "P-values", "",
               apply(pmat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
      
    } else {
      out <- c(out, "", "Unable to compute confidence intervals and p-values.")
    }
  }
  
  out
}

formatTriMat <- function(mat, names) {
  ## Formats a (lower) triangular matrix nicely for display:
  
  mat[!lower.tri(mat)] <- NA
  mat <- mat[-1, , drop = FALSE]
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, digits = 4)
  }), nrow = nrow(mat))
  
  mat[grep("NA", mat)] <- ""
  mat[grep("NaN", mat)] <- ""
  
  mat <- cbind(c("", names[-1]),
               rbind(names, mat))
  mat <- mat[, -ncol(mat)]
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, justify = "right")
  }), nrow = nrow(mat))
  
  mat
}

formatMat <- function(mat, digits = 4) {
  dn <- dimnames(mat)
  
  mat <- apply(mat, 1, function(x) suppressWarnings(as.numeric(x)))
  ## If the matrix has a single column, apply returns a vector rather than a matrix
  if (is.matrix(mat))
    mat <- t(mat)
  else
    mat <- matrix(mat, ncol = 1)
  
  mat <-  matrix(apply(mat, 2, function(col) {
    format(col, digits = digits)
  }), nrow = nrow(mat))
  mat[grep("NA", mat)] <- ""
  mat[grep("NaN", mat)] <- ""
  
  mat <- cbind(c("", dn[[1]]),
               rbind(dn[[2]], mat))
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, justify = "right")
  }), nrow = nrow(mat))
  
  mat
}
inference.inzhist <- function(object, bs, class, width, ...)
  inference.inzdot(object, bs, class, width, ...)



inference.inzbar <- function(object, bs, vn, nb, ...) {
  phat <- object$phat
  inf <- object$inference.info
  
  if (! "conf" %in% names(inf))
    stop("Please specify `inference.type = conf` to get inference information.")
  
  if (is.null(inf$conf))
    return("Unable to obtain inference information.")
  
  if (bs & sum(object$tab) < 10)
    return("Not enough data to perform bootstraps.")
  
  twoway <- nrow(phat) > 1
  
  if (twoway) {
    mat <- inf$conf$estimate
    dn <- dimnames(object$tab)
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, digits = 3)
    }), nrow = nrow(mat))
    
    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""
    
    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(dn[[2]], mat)
    colnames(mat) <- NULL
    
    mat <- cbind(c("", dn[[1]]), mat, c("Row sums", rep(1, nrow(phat))))
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat))
    
    out <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    out <- c("Estimated Proportions", "",  out)
    
    cis <- inf$conf
    cis <- rbind(cis$lower, cis$upper)
    cis <- cis[c(1:nrow(phat) * 2 - 1, 1:nrow(phat) * 2), ]
    
    cis <- matrix(apply(cis, 2, function(col) {
      format(col, digits = 3)
    }), nrow = nrow(cis))
    cis[grep("NA", cis)] <- ""
    cis[grep("NaN", cis)] <- ""
    
    cis <- rbind(dn[[2]], cis)
    colnames(cis) <- NULL
    
    cis <- cbind(c("", rbind(dn[[1]], "")), cis)
    colnames(cis) <- NULL
    
    cis <- matrix(apply(cis, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(cis))
    
    bsCI <- ifelse(bs, " Percentile Bootstrap", "")
    out <- c(out, "", paste0("95%", bsCI, " Confidence Intervals"), "",
             apply(cis, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
    
    out <- c(out, "", "",
             paste0("   *** Differences between proportions of ", vn$y, " for each level of ", vn$x, " ***"))
    
    if (bs) {
      tab <- object$tab
      dat <- data.frame(x = rep(colnames(tab), times = colSums(tab)),
                        y = rep(rep(rownames(tab), times = ncol(tab)), tab))
      
      b <- boot(dat, function(d, f) {
        tt <- t(table(d[f, "x"], d[f, "y"]))
        n1 <- nrow(tt)
        n2 <- ncol(tt)
        nn <- rowSums(tt)
        pp <- sweep(tt, 1, nn, "/")
        
        d <- c()
        for (ii in 1:n2)
          for (jj in 2:n1)
            d <- c(d, pp[jj, ii] - pp[jj - 1, ii])
        
        d  
      }, R = nb)
    }
    
    for (j in 1:ncol(phat)) {
      p <- phat[, j]
      n <- length(p)
      lev <- dn[[2]][j]
      sum <- rowSums(object$tab)
      
      if (bs) {
        ni <- nrow(tab) - 1
        wi <- 1:ni + (j - 1) * ni
        diff <- matrix(nrow = ni + 1, ncol = ni + 1)
        diff[lower.tri(diff)] <- colMeans(b$t, na.rm = TRUE)[wi]
        diff <- formatTriMat(diff, rownames(tab))
      } else {
        diff <- outer(p, p, function(p1, p2) p1 - p2)
        diff <- formatTriMat(diff, dn[[1]])
      }
      
      out <- c(out, "",
               paste0("### ", vn$x, " = ", lev), "",
               "Estimates", "",
               apply(diff, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
      
      cis <- matrix(NA, nrow = 2 * (n - 1), ncol = n - 1)
      for (k in 2:n) {
        for (l in 1:(k - 1)) {
          wr <- (k - 2) * 2 + 1
          cis[wr:(wr + 1), l] <-
            if (bs) quantile(b$t[, j], c(0.025, 0.975), na.rm = TRUE)
          else pDiffCI(p[k], p[l], sum[k], sum[l])
        }
      }
      colnames(cis) <- dn[[1]][-n]
      rownames(cis) <- c(rbind(dn[[1]][-1], ""))
      
      cis <- formatMat(cis, 3)
      
      out <- c(out, "",
               paste0("95% ", bsCI, " Confidence Intervals"), "",
               apply(cis, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
    }
  } else {
    mat <- t(rbind(inf$conf$lower, inf$conf$estimate, inf$conf$upper))
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, digits = 3)
    }), nrow = nrow(mat))
    
    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""
    
    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Lower", "Estimate", "Upper"), mat)
    colnames(mat) <- NULL
    
    LEVELS <- names(object$tab)
    mat <- cbind(c("", LEVELS), mat)
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat))
    
    out <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    
    bsCI <- ifelse(bs, " Percentile Bootstrap", "")
    out <- c(paste0("Estimated Proportion with 95%", bsCI, " Confidence Interval"), "",
             out)
    
    if (bs) {
      ## This is about the only place we do bootstrapping within the inference function, as no such
      ## method is applicable to the plots themselves.
      ## All other inferences are generated by the plotting function, which are then displayed on the plot
      ## and hence match the output from this function.
      
      dat <- data.frame(x = rep(names(object$tab), times = object$tab))
      b <- boot(dat, function(d, f) {
        tt <- table(d[f, ])
        ni <- length(tt)
        nn <- sum(tt)
        pp <- tt / nn
        
        d <- c()
        for (ii in 2:ni)
          for (jj in 1:(ii - 1))
            d <- c(d, pp[ii] - pp[jj])
        
        d                      
      }, R = nb)
      
      diffs <- matrix(nrow = length(LEVELS), ncol = length(LEVELS))
      diffs[lower.tri(diffs)] <- colMeans(b$t)
      diffs <- formatTriMat(diffs, LEVELS)
      
      cil <- ciu <- matrix(nrow = length(LEVELS), ncol = length(LEVELS))
      cil[lower.tri(cil)] <- apply(b$t, 2, quantile, probs = 0.025)
      ciu[lower.tri(ciu)] <- apply(b$t, 2, quantile, probs = 0.975)
      
      cil <- formatTriMat(cil, LEVELS)
      ciu <- formatTriMat(ciu, LEVELS)
      
      cis <- rbind(cil[-1, , drop = FALSE], ciu[-1, , drop = FALSE])
      cis <- cis[order(cis[, 1]), -1, drop = FALSE]
      
      rownames(cis) <- rbind(LEVELS[-1], "")
      colnames(cis) <- cil[1, -1]
      
      cis <- formatMat(cis)
      
    } else {
      diffs <- freq1way.edited(object$tab, "estimates")
      diffs <- formatMat(diffs)
      
      cis <- freq1way.edited(object$tab, "ci")
      cis <- formatMat(cis)
    }
    
    out <- c(out, "", "",
             "   *** Differences between Proportions (column - row) ***", "",
             "Estimates", "",
             apply(diffs, 1, function(x) paste0("   ", paste(x, collapse = "   "))),
             "",
             paste0("95%", bsCI, " Confidence Intervals"), "",
             apply(cis, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
  }
  
  if (!bs) {
    chi2 <- suppressWarnings(chisq.test(object$tab))
    out <- c(out, "", "",
             "   *** Chi-square test for equal proportions ***", "",
             paste0("   X-squared = ", format(signif(chi2$statistic, 5)), ", ",
                    "df = ", format(signif(chi2$parameter, 5)), ", ",
                    "p-value ", ifelse(chi2$p.value < 2.2e-16, "", "= "), format.pval(chi2$p.value, digits = 4)))
  }
  
  out
}

freq1way.edited <- function(tbl, inf.type = "estimates", conf.level = 0.95) {
  ## Before freq1way is called should output the variable name in the table
  level.names <- names(tbl)
  n <- sum(tbl)
  ncats <- length(tbl)
  ncatsC2 <- choose(ncats, 2)
  
  if (is.null(level.names)) level.names <- 1:ncats
  
  conf.pc <- conf.level * 100
  phat <- tbl / sum(tbl)
  
  qval <- abs(qnorm((1 - conf.level) / (2 * ncats)))
  
  matw <- matrix(NA, ncats - 1, ncats - 1)
  
  dimnames(matw) <- list(level.names[-length(level.names)], level.names[-1])
  
  qval.adjusted <- abs(qnorm((1 - conf.level) / (2 * ncatsC2)))
  
  tempw <- ""
  
  if (inf.type == "estimates") {
    for(i1 in 1:(ncats - 1)) {
      for(i2 in 2:ncats) {
        tempw <- phat[i1] - phat[i2]
        tempw <- signif(tempw, 5)
        matw[i1, i2 - 1] <- ifelse((i1 < i2), tempw , " ")
      }
    }
    t(matw)
  } else {
    testMatrix <- matrix(" ", ncats - 1, 2 * ncats - 2)
    count <- 1
    count.2 <- 0
    for(i1 in 1:(ncats - 1)) {
      count <- 0
      for(i2 in 2:ncats) {
        tempw <- phat[i1] - phat[i2] +
          abs(qnorm((1 - conf.level) / (2 * ncatsC2))) * c(-1, 1) *
          sqrt(((phat[i1] + phat[i2]) - ((phat[i1] - phat[i2])^2)) / n)
        tempw <- signif(tempw, 5)
        matw[i1, i2 - 1] <- ifelse((i1 < i2),
                                   paste("(", tempw[1], ",", tempw[2], ")", sep = ""),
                                   " ")
        
        if(i2 == 2)
          count <- i2 - 2
        if(i1 < i2) {
          testMatrix[i1, count + 1 + count.2] <- tempw[1]
          testMatrix[i1, (count = count + 2) + count.2] <- tempw[2]
        }
      }
      count.2 <- count.2 + 2
    }
    
    rowNames <- rep("", ncats * 2)
    temp <- 1:(ncats * 2)
    rowNames[(temp %% 2 != 0)] <- level.names
    
    testMatrix <- t(testMatrix)
    rownames(testMatrix) <- rowNames[-c(1, 2)]
    colnames(testMatrix) <- level.names[-ncats]
    
    testMatrix
  }
}

pDiffCI <- function(p1, p2, n1, n2, z = 1.96) {
  p <- p1 - p2
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  
  p + c(-1, 1) * z * se
}


inference.inzscatter <- function(object, bs, nboot, vn, ...) {
  d <- data.frame(x = object$x, y = object$y)
  trend <- object$trend
  
  if (is.null(trend))
    return("Please specify a trend line to obtain inference information.")
  
  if (bs & nrow(d) < 10)
    return("Not enough observations to perform bootstrap simulation.")
  
  ## Ensure the order is linear/quad/cubic
  allT <- c("linear", "quadratic", "cubic")
  tr <- (1:3)[allT %in% trend]
  
  out <- character()
  
  for (t in tr) {
    if (nrow(d) <= t + 1) {
      out <- c(out, "",
               paste0("Not enough observations (n = ", nrow(d), ") to fit ",
                      switch(t, "Linear", "Quadratic", "Cubic"), " trend"))
      break
    } else {
      if (bs) {
        b <- boot(d,
                  function(dat, f) {
                    fit <- switch(t,
                                  lm(y ~ x, data = dat[f, ]),
                                  lm(y ~ x + I(x^2), data = dat[f, ]),
                                  lm(y ~ x + I(x^2) + I(x^3), data = dat[f, ]))
                    c(coef(fit),
                      if (t == 1) cor(d[f, "x"], d[f, "y"]))
                  }, R = object$n.boot)
        mat <-
          cbind(sprintf("%.5g", colMeans(b$t, na.rm = TRUE)),
                sprintf("%.5g", apply(b$t, 2, quantile, probs = 0.025, na.rm = TRUE)),
                sprintf("%.5g", apply(b$t, 2, quantile, probs = 0.975, na.rm = TRUE)))
        
      } else {
        fit <- switch(t,
                      lm(y ~ x, data = d),
                      lm(y ~ x + I(x^2), data = d),
                      lm(y ~ x + I(x^2) + I(x^3), data = d))
        
        cc <- summary(fit)$coef
        ci <- confint(fit)
        
        mat <-
          cbind(sprintf("%.5g", cc[, 1]),
                sprintf("%.5g", ci[, 1]),
                sprintf("%.5g", ci[, 2]),
                format.pval(cc[, 4], digits = 2))
      }
      
      if (bs & t == 1) {
        covMat <- mat[3, ]
        mat <- mat[1:2, ]
      }
      
      mat <- rbind(c("Estimate", "Lower", "Upper", if (!bs) "p-value"), mat)
      
      rn <- paste0(vn$x, c("", "^2", "^3"))
      mat <- cbind(c("", "Intercept", rn[1:t]), mat)
      if (bs & t == 1) {
        mat <- rbind(mat, "", c("correlation", covMat))
      }
      mat <- apply(mat, 2, function(x) format(x, justify = "right"))
      
      out <- c(out, "",
               paste0(switch(t, "Linear", "Quadratic", "Cubic"), " Trend Coefficients with 95% ",
                      ifelse(bs, "Percentile Bootstrap ", ""), "Confidence Intervals"), "",
               apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
      
      ## add a 'key' to the end of the output
      if (t == max(tr) & !bs) 
        out <- c(out, "", "",
                 "   p-values for the null hypothesis of no association, H0: beta = 0")
    }
  }
  
  out
}
inference.inzgrid <- function(object, bs, nboot, vn, ...)
  inference.inzscatter(object, bs, nboot, vn, ...)

inference.inzhex <- function(object, bs, nboot, vn, ...)
  inference.inzscatter(object, bs, nboot, vn, ...)



create.inz.barplot <- function(obj) {
  # take the dataframe and settings from the object
  df <- obj$df
  opts <- obj$opts
  xattr <- obj$xattr
  
  ZOOM <- xattr$zoom
  
  inf.type <- opts$inference.type
  inf.par <- "proportion"
  bs <- opts$bs.inference
  
  if (xattr$class == "inz.survey")
    df <- df$variables
  
  ynull <- !"y" %in% colnames(df)
  
  # first need to remove missing values
  missing <- is.na(df$x)
  if ("y" %in% colnames(df)) {
    y.levels <- levels(df$y) # need to save these before we remove missing ...
    missing <- missing | is.na(df$y)
  }
  
  n.missing <- sum(missing)
  df <- df[!missing, , drop = FALSE]
  
  svy <- switch(xattr$class,
                "inz.survey" = {
                  eval(parse(text = modifyData(obj$df$call, "df")))
                }, "inz.freq" = {
                  svydesign(ids=~1, weights = ~freq, data = df)
                }, "inz.simple" = {
                  NULL
                })
  
  SEG <- FALSE
  if (ynull) {
    if (is.null(svy)) {
      tab <- table(df$x)
      phat <- matrix(tab / sum(tab), nrow = 1)
    } else {
      tab <- svytable(~x, design = svy)
      phat <- matrix(svymean(~x, design = svy), nrow = 1)
    }
    
    widths <- rep(1, length(tab))
    edges <- c(0, 1)
    
    ## colby: (segmented bar plot)
    SEG <- "colby" %in% colnames(df)
    if (SEG & !is.factor(df$colby))
      SEG <- FALSE
    
    if (SEG) {
      tab2 <-
        if (is.null(svy))
          table(df$colby, df$x)
      else
        svytable(~colby + x, design = svy)
      p2 <- sweep(tab2, 2, colSums(tab2), "/")
    }
  } else {
    if (is.null(svy)) {
      tab <- table(df$y, df$x)
      phat <- sweep(tab, 1, nn <- rowSums(tab), "/")
    } else {
      tab <- svytable(~y + x, design = svy)
      phat <- svyby(~x, by = ~y, svy, FUN = svymean)[, 1 + 1:ncol(tab)]
      nn <- rowSums(tab)
    }
    
    widths <- nn / sum(nn)
    edges <- c(0, cumsum(widths))
  }
  
  ## Cannot have inference on segmented plot (too complicated for now)
  inflist <- if (!SEG) barinference(obj, tab, phat) else NULL
  
  ymax <- max(phat)
  if (!is.null(ZOOM)) {
    if (ZOOM[1] > ncol(phat))
      next
    
    ww <- ZOOM[1]:(sum(ZOOM) - 1)
    ww <- ww - ncol(phat) * (ww > ncol(phat))
    
    phat <- phat[, ww, drop = FALSE]
    if (ynull) {
      tab <- tab[ww]
      widths <- widths[ww]
    } else {
      tab <- tab[, ww, drop = FALSE]
    }
  }
  
  out <- list(phat = phat, tab = tab, widths = widths, edges = edges, nx = ncol(phat),
              full.height = opts$full.height, inference.info = inflist,
              xlim = c(0, if (ynull) length(tab) else ncol(tab)),
              ylim = c(0, max(ymax, if (!is.null(inflist)) attr(inflist, "max"), na.rm = TRUE)))
  if (SEG) out$p.colby <- p2[nrow(p2):1, ]
  if (!is.null(ZOOM)) out$zoom.index <- ww
  
  class(out) <- "inzbar"
  
  out
}

plot.inzbar <- function(obj, gen) {
  opts <- gen$opts
  p <- obj$phat
  nx <- obj$nx
  
  inflist <- obj$inference.info
  
  if (SEG <- !is.null(obj$p.colby)) {
    seg.cols <- gen$col.args$f.cols
  }
  
  edges <- rep(obj$edges * 0.9 + 0.05, each = 4)
  edges <- edges[3:(length(edges) - 2)]
  xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))
  
  if (SEG) {
    xx <- rep(xx, length(seg.cols))
    tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
    
    yy <- rep(tops, length(seg.cols))
    ps <- rep(c(t(obj$p.colby)), each = 4)
    pT <- rep(c(t(apply(obj$p.colby, 2, cumsum))), each = 4)
    yy <- yy * pT
    
    ## reverse the order, so the short ones are drawn last!       
    id <- rev(rep(1:prod(dim(obj$p.colby)), each = 4))
    colz <- rep(seg.cols, each = nx)
    
    grid.polygon(unit(xx, "native"), unit(yy, "native"), id = id,
                 gp =
                   gpar(fill = colz, col = "transparent",
                        lwd = 0))
    
    ## separating lines
    mat <- apply(sweep(obj$p.colby, 2, tops[2, ], "*"), 2, cumsum)
    mat <- mat[-nrow(mat), , drop = FALSE]  # drop the last one
    
    yl <- rep(c(mat), each = 2)
    xl <- rep(edges[2:3], length = length(yl)) + rep(1:nx - 1, each = 2 * nrow(mat))
    id <- rep(1:length(c(mat)), each = 2)
    
    grid.polyline(xl, yl, default.units = "native", id = id,
                  gp = gpar(col = opts$bar.col, lwd = opts$bar.lwd))                          
  }
  
  xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))
  tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
  yy <- c(tops)
  
  id <- rep(1:prod(dim(p)), each = 4)
  colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill else rep(gen$col.args$b.cols, nx)
  
  grid.polygon(unit(xx, "native"), unit(yy, "native"), id = id,
               gp =
                 gpar(fill = if (SEG) "transparent" else colz, col = opts$bar.col,
                      lwd = opts$bar.lwd))    
  
  center <- apply(matrix(xx, ncol = 4, byrow = TRUE), 1, function(x) x[2] + (x[3] - x[2]) / 2)
  bounds <- apply(matrix(xx, ncol = 4, byrow = TRUE), 1, function(x) x[2:3])
  
  if (!is.null(inflist)) {
    addBarInference(inflist, center, opts, obj$zoom.index)
    if (!is.null(inflist$comp))
      addBarCompLines(inflist$comp, bounds, p, opts, obj$zoom.index)
  }
}

barinference <- function(obj, tab, phat) {
  ## obj: list of data broken down by subsets
  ## opts: various options (inzpar)
  
  opts <- obj$opts
  xattr <- obj$xattr
  inf.par <- "proportion"
  inf.type <- opts$inference.type
  bs <- opts$bs.inference
  dat <- obj$df
  
  
  if (is.null(inf.type)) {
    return(NULL)
  }
  
  twoway <- length(dim(tab)) == 2  # two way comparison (two factors ...)
  svy <- obj$xattr$class != "inz.simple"
  
  if (length(dim(tab)) == 1) {
    twoway <- FALSE
    tab <- t(tab)
    phat <- t(phat)
  } else {
    twoway <- TRUE
  }
  
  lapply(inf.type, function(type) {
    switch(type,
           "conf" = {
             if (bs) {
               if (svy) {
                 NULL                           
               } else {
                 if (twoway) {
                   ## For now, we will just all over and not return intervals
                   ## IN FUTURE: might want to bootstrap some other way?
                   inf <- try({
                     n <- rowSums(tab)
                     b <- boot(dat, function(d, f) {
                       tt <- t(table(d[f, 1], d[f, 2]))
                       sweep(tt, 1, n, "/")
                     }, R = 1000)
                     cis <- apply(b$t, 2, function(x) c(quantile(x, probs = c(0.025, 0.975)), mean(x)))
                   }, silent = TRUE)
                   if (inherits(inf, "try-error"))
                     NULL
                   else
                     list(lower = matrix(cis[1, ], nrow = nrow(tab), byrow = FALSE),
                          upper = matrix(cis[2, ], nrow = nrow(tab), byrow = FALSE),
                          estimate = matrix(cis[3, ], nrow = nrow(tab), byrow = FALSE))
                 } else {
                   n <- sum(tab)
                   if (n == 0) return(NULL)
                   b <- boot(dat, function(d, f) table(d[f, 1]) / n, R = opts$n.boot)
                   cis <- apply(b$t, 2, function(x) c(quantile(x, probs = c(0.025, 0.975)), mean(x)))
                   list(lower = cis[1, , drop = FALSE], upper = cis[2, , drop = FALSE], estimate = cis[3, , drop = FALSE])
                 }
               }
             } else {
               if (svy) {
                 if (twoway) {
                   est <- svyby(~x, by = ~y, obj$df, FUN = svymean, vartype = "ci")[, -1]
                   nc <- length(levels(obj$df$variables$x))
                   list(lower = as.matrix(est[, nc + 1:nc]),
                        upper = as.matrix(est[, 2 * nc + 1:nc]),
                        estimate = as.matrix(est[, 1:nc]))
                 } else {
                   ci <- t(confint(svymean(~x, obj$df)))
                   list(lower = ci[1, , drop = FALSE],
                        upper = ci[2, , drop = FALSE],
                        estimate = t(phat))
                 }
               } else {
                 ## Standard confidence interval:
                 t(apply(tab, 1, function(x) {
                   n <- sum(x)
                   p <- ifelse(x >= opts$min.count, x / n, NA)
                   se <- sqrt(p * (1 - p) / n)
                   se * 1.96
                 })) -> size
                 if (!twoway) phat <- t(phat)
                 list(lower = phat - size, upper = phat + size, estimate = phat)
               }
             }
           },
           "comp" = {
             if (bs) {
               if (svy) {
                 NULL
               } else {
                 NULL
               }
             } else {
               if (svy) {
                 NULL
               } else {
                 if (twoway) {
                   ## several ways in which this can fall over; rather than testing for
                   ## each, just try and iNZightMR will fail with an error
                   int <- try({
                     n <- rowSums(tab)
                     
                     ## ii - only use rows that have at least 1 count
                     ## (due to subsetting, can be 0 counts for a row)
                     ii <- n > 0
                     
                     lapply(1:ncol(tab), function(i) {
                       if(sum(tab[ii, ] > 0) < 2) return(list(compL = NA, compU = NA))
                       
                       suppressWarnings(
                         moecalc(
                           seBinprops(n[ii], phat[ii, i]), est = phat[ii, i]
                         )
                       )
                     }) -> out
                     
                     low <- upp <- phat * 0
                     low[ii, ] <- sapply(out, function(x) x$compL)
                     upp[ii, ] <- sapply(out, function(x) x$compU)
                   }, silent = TRUE)
                   if (inherits(int, "try-error"))
                     NULL
                   else
                     list(lower = low, upper = upp)
                 } else {
                   if (sum(tab) == 0) return(NULL)
                   phat <- c(phat) # don't want matrix
                   with(suppressWarnings(moecalc(seMNprops(sum(tab), phat), est = phat)),
                        list(lower = t(compL), upper = t(compU))) -> res
                   lapply(res, function(r) {
                     colnames(r) <- colnames(tab)
                     r[tab < opts$min.count] <- NA
                     r
                   })
                 }
               }
             }
           })
  }) -> result
  names(result) <- inf.type
  
  attr(result, "bootstrap") <- bs
  attr(result, "max") <- max(sapply(result, function(r)
    if (is.null(r)) 0 else max(sapply(r, function(x)
      if(is.null(x)) 0 else max(c(0, x), na.rm = TRUE)),
      na.rm = TRUE)
  ),
  na.rm = TRUE)
  
  result    
}





inzDataframe <- function(m, data = NULL, names = list(), g1.level, g2.level, env) {
  
  # This function takes the given arguments and converts them into a
  # data frame for easy use by iNZightPlot.
  # It returns an object with a class: `inz.(simple|freq|survey)`
  
  if ("g2" %in% names(m) & (!("g1" %in% names(m)) | is.null(m$g1))) {
    if (!is.null(m$g2)) {
      if (g2.level == "_ALL") {
        m$g1 <- NULL
        m$g2 <- NULL
        
        m$g1.level <- NULL
        m$g2.level <- NULL
        
        g1.level <- NULL
        g2.level <- NULL
        
        names$g1 <- NULL
        names$g2 <- NULL
      } else {
        m$g1.level <- NULL
        names(m) <- gsub("^g2", "g1", names(m))
        
        g1.level <- g2.level
        g2.level <- NULL
        
        names$g1 <- names$g2
        names$g2 <- NULL
      }
    }
  }
  
  names <- names[!sapply(names, is.null)]
  
  # the variables we want to look for in argument list (m)
  vars <- c("", "x", "y", "g1", "g2", "colby", "sizeby", "locate")
  mw <- names(m) %in% vars
  mw[1] <- FALSE  # the function name
  mw <- mw & !sapply(as.list(m), is.null)
  
  # take the names and replace if specified
  names <- names[names != ""]
  varnames <- modifyList(as.list(m[mw]), names)
  
  df <- list()  # initialise the object
  
  ## ----- DATA TYPES:
  # here, it is possible to add new data types (add the necessary conditions, etc,
  # and then simply add the appropriate class)
  # e.g., in future we might want to add a TimeSeries data type ...
  
  if (inherits(data, "survey.design")) {
    df$data <- as.data.frame(lapply(m[mw], eval, data$variables, env))
    df$design <- eval(data, env)
    class(df) <- "inz.survey"
  } else if ("freq" %in% names(m)) {
    df$data <- as.data.frame(lapply(m[mw & names(m) != "sizeby"], eval, data, env))
    df$freq <- eval(m$freq, data, env)
    df$max.freq <- max(df$freq)
    class(df) <- "inz.freq"
  } else {
    df$data <- as.data.frame(lapply(m[mw], eval, data, env))
    class(df) <- "inz.simple"
  }
  
  if (!is.null(df$data$sizeby)) {
    df$data$sizeby <- rescale(df$data$sizeby)
  }
  
  if (!is.null(m$locate.id)) {
    label <- character(nrow(df$data))
    if (is.null(df$data$locate)) {
      if (is.null(m$locate.col))
        locCol <- "default"
      else
        locCol <- m$locate.col
      label[eval(m$locate.id)] <- paste(" ")
    } else {
      locVar <- as.character(df$data$locate)
      locVar[is.na(locVar)] <- "missing"
      label[eval(m$locate.id)] <- locVar[eval(m$locate.id)]
    }
    df$data$locate <- label
  } else if (!is.null(m$locate.extreme)) {
    label <- character(nrow(df$data))
    if (!is.null(df$data$locate)) {
      locVar <- as.character(df$data$locate)
      locVar[is.na(locVar)] <- "missing"
      label <- locVar
    } else {
      label <- rep(" ", nrow(df$data))
    }
    df$data$extreme.label <- label
    df$data$pointIDs <- 1:nrow(df$data)
  }
  
  if (!is.null(m$highlight)) {
    df$data$highlight <- (1:nrow(df$data)) %in% m$highlight
  }
  
  
  
  # convert anything that isn't a numeric variable to a factor
  # NOTE: this is just precautionary; as.data.frame should set any
  # character strings to factors by default.
  makeF <- sapply(df$data, function(x) !is.numeric(x) & !is.factor(x))
  if (any(makeF))
    for (i in colnames(df$data)[makeF])
      df$data[[i]] <- as.factor(df$data[[i]])
  
  # convert any -Inf/Inf values to NA's
  # this is likely to occur if the user supplies a transformed value such as 1 / x, or
  # log(x) (which give Inf and -Inf respectively). Because we can't plot these values,
  # it is easier just to replace them with missing.
  for (i in colnames(df$data))
    df$data[[i]][is.infinite(df$data[[i]])] <- NA
  
  # convert numeric grouping variables to factors
  if ("g2" %in% colnames(df$data))
    if (is.numeric(df$data$g2))
      df$data$g2 <- convert.to.factor(df$data$g2)
  if ("g1" %in% colnames(df$data)) {
    if (is.numeric(df$data$g1))
      df$data$g1 <- convert.to.factor(df$data$g1)
  } else {
    if (!is.null(g2.level)) {
      # g2.level can only be of length 1
      if (length(g2.level) > 1)
        stop("g2.level must be of length 1 or NULL")
      
      if (g2.level %in% c(length(levels(df$data$g2)) + 1, "_MULTI")) {
        # need to replace g1 with g2
        varnames$g1 <- varnames$g2
        varnames$g2 <- NULL
        df$data$g1 <- df$data$g2
        df$data$g2 <- NULL
        g1.level <- g2.level
        g2.level <- NULL
      }
    }
  }
  
  ## WE TURN THIS OFF - leave it up to users to decide if there are too many levels.
  
  ## another check that there aren't too many levels of colby:
  #if ("colby" %in% colnames(df$data)) {
  #    if (is.factor(df$data$colby)) {
  #        if (length(levels(df$data$colby)) > 20) {
  #            warning("Ignoring colby argument: too many factor levels.")
  #            df$data$colby <- NULL
  #            varnames$data$colby <- NULL
  #        }
  #    }
  #}
  if ("colby" %in% colnames(df$data)) {
    if (is.factor(df$data$colby)) {
      if (length(levels(df$data$colby)) == 1)
        df$data$colby <- varnames$data$colby <- NULL                
    } else {
      if (length(unique(df$data$colby)) == 1)
        df$data$colby <- varnames$data$colby <- NULL
    }
  }
  
  # fix a bug that ensures colby grouping variable is the same as g2 if both specified
  if ("g2" %in% colnames(df$data) & "colby" %in% colnames(df$data))
    if (varnames$g2 == varnames$colby)
      df$data$colby <- df$data$g2
  
  df$varnames <- sapply(varnames,
                        function(x) ifelse(!is.character(x), deparse(x), x))
  df$glevels <- list(g1.level = g1.level, g2.level = g2.level)
  
  df
}





inzDataList <- function(d, x) {
  
  if (x != "all") {
    # for some reason, the subset() function gives errors
    w <- d$g1 == x
    df <- d[w & !is.na(w), , drop = FALSE]
  } else {
    df <- d
  }
  
  df <- df[, colnames(df) != "g1", drop = FALSE]
  df
}



create.inz.dotplot <- function(obj, hist = FALSE) {
  df <- obj$df
  opts <- obj$opts
  xattr <- obj$xattr
  
  boxplot <- opts$boxplot
  
  v <- colnames(df)
  vn <- xattr$varnames
  
  #trimX <- xattr$trimX
  
  if (xattr$class != "inz.simple")
    hist <- TRUE
  
  if (xattr$class == "inz.survey")
    df <- df$variables
  
  # May need to switch around X and Y:
  if (all(c("x", "y") %in% v)) {
    if (is.factor(df$x) & is.numeric(df$y)) {
      X <- df$y
      df$y <- df$x
      df$x <- X
      Xn <- vn$y
      vn$y <- vn$x
      vn$x <- Xn
      xattr$xrange <- xattr$yrange
      xattr$yrange <- NULL
      
      obj$df <- df
      xattr$varnames <- vn
    }
  }
  
  # 'trim' X values
  #if (!is.null(trimX)) {
  #    df <- df[df$x > min(trimX) & df$x < max(trimX), , drop = FALSE]
  #}
  
  # first need to remove missing values
  missing <- is.na(df$x)
  if ("y" %in% colnames(df)) {
    y.levels <- levels(df$y) # need to save these before we remove missing ...
    missing <- missing | is.na(df$y)
  }
  
  n.missing <- sum(missing)
  
  if ("y" %in% colnames(df)) {
    attr(n.missing, "levels") <- tapply(missing, df$y, sum)
  }
  
  df <- df[!missing, , drop = FALSE]
  
  ## Return a LIST for each level of y
  if ("y" %in% colnames(df)) {
    out <- vector("list", length(levels(df$y)))
    names(out) <- levels(df$y)
    id <- df$y
  } else {
    out <- list(all = NULL)
    id <- rep("all", nrow(df))
  }
  
  for (i in unique(id)) {
    dfi <- subset(df, id == i)
    dfi$y <- NULL
    
    if (xattr$class == "inz.freq")
      di <- svydesign(ids=~1, weights = dfi$freq, data = dfi)
    else if (xattr$class == "inz.survey") {
      di <- eval(parse(text = modifyData(obj$df$call, "dfi")))
    } else {
      di <- dfi
    }
    
    out[[i]] <- di
  }
  
  makeHist <- function(d, nbins, xlim, bins = NULL) {
    if (is.null(d)) return(NULL)
    
    if (is.null(nbins)) {
      range <- range(bins)
      cuts <- bins            
    } else {
      ## Create even cut points in the given data range:
      range <- xlim
      range <- extendrange(range, f = 0.01) ## is this necessary?
      cuts <- seq(range[1] - 0.1, range[2] + 0.1, length = nbins + 1)
    }
    
    bin.min <- cuts[-(nbins + 1)]
    bin.max <- cuts[-1]
    
    ## Cut the data and calculate counts:
    if (inherits(d, "survey.design")) {
      ## To do this, we will pretty much grab stuff from the `survey` package, however it
      ## cannot be used separately to produce the bins etc without plotting it; so copyright
      ## for the next few lines goes to Thomas Lumley.
      h <- hist(x <- d$variables$x, breaks = cuts, plot = FALSE)
      
      ## We can run into problems with PSUs have single clusters, so:
      oo <- options()$survey.lonely.psu
      options(survey.lonely.psu = "certainty")
      probs <- coef(svymean(~cut(d$variables$x, h$breaks, include.lowest = TRUE,
                                 deff = FALSE, estimate.only = TRUE), d, na.rm = TRUE))
      options(survey.lonely.psu = oo)
      
      h$density <- probs / diff(h$breaks)
      h$counts <- probs * sum(weights(d))
    } else {
      x <- d$x
      h <- hist(x, breaks = cuts, plot = FALSE)
    }
    
    ret <- list(breaks = cuts,
                counts = as.numeric(h$counts),
                density = as.numeric(h$density),
                mids = h$mids,
                x = sort(x))
    
    if (!hist) {
      ret$y <- unlist(sapply(h$counts[h$counts != 0], function(c) 1:c))
      if ("colby" %in% colnames(d)) {
        ret$colby <- d$colby[order(x)]
      }
    }
    
    ret$extreme.ids <- NULL
    
    if ("extreme.label" %in% v) {
      eLab <- as.character(d$extreme.label)[order(x)]
      
      nx <- rep(xattr$nextreme, length = 2)
      if (sum(nx) >= nrow(d)) {
        text.labels <- eLab
        ret$extreme.ids <- d$pointIDs[order(x)]
      } else {
        min <- 1:nx[1]
        max <- (nrow(d) - nx[2] + 1):nrow(d)
        
        text.labels <- character(nrow(d))
        if (nx[1] > 0)
          text.labels[min] <- eLab[min]
        if (nx[2] > 0)
          text.labels[max] <- eLab[max]
        
        pointIDs <- d$pointIDs[order(x)]
        
        ret$extreme.ids <- pointIDs[text.labels != ""]
      }
    } else {
      text.labels <- as.character(d$locate)[order(x)]
    }
    
    ret$text.labels <- text.labels
    
    if ("highlight" %in% colnames(d))
      ret$highlight <- d$highlight[order(x)]
    
    attr(ret, "order") <- order(x)
    ret      
  }
  
  
  boxinfo <- if (boxplot & (!"mean" %in% opts$inference.par) & nrow(df) > 5)
    boxSummary(out, opts) else NULL
  
  nbins <- bins <- NULL
  if (hist) {
    ## some option here to adjust the number of bins (e.g., sample size < 100?)
    nbins <- if (is.null(opts$hist.bins)) {
      wd <- convertWidth(unit(1.2 * opts$cex.dotpt, "char"),
                         "npc", valueOnly = TRUE)
      floor(1 / (wd))
    } else {
      opts$hist.bins
    }
  } else {
    ## compute the smallest non-zero difference, and deduce if it is a common
    ## factor of all the differences:
    #diffs <- do.call(c, lapply(out, function(d) {
    #    if (is.null(d$x)) return(NULL)
    #    
    #    diffs <- diff(sort(d$x))
    #    diffs[diffs > 0]
    #}))
    
    #mdiff <- if (length(diffs) > 0) min(diffs) else 0
    #fdiff <- diffs / mdiff
    #isDiscrete <- all(round(fdiff) == fdiff)
    
    #xr <- diff(range(sapply(out, function(d) if (is.null(d$x)) 0 else range(d$x))))
    
    #mult.width <- ifelse(isDiscrete, 1, 1.2)
    
    dp <- xattr$dotplotstuff
    mdiff <- dp$mdiff
    xr <- dp$xr
    isDiscrete <- dp$isDiscrete
    mult.width <- dp$mult.width
    
    if ("symbol.width" %in% names(xattr))
      symbol.width <- xattr$symbol.width * xr
    else
      symbol.width <- convertWidth(unit(opts$cex.dotpt, "char"),
                                   "native", valueOnly = TRUE)
    
    if (symbol.width < mdiff) {
      ## If the symbols are smaller than the smallest differences,
      ## then just use all of the values as bins!
      xx <- unique(do.call(c, lapply(out, function(d) unique(d$x))))
      bins <- seq(min(xx) - 0.5 * mdiff, max(xx) + 0.5 * mdiff, by = mdiff)
    } else {
      wd <- xattr$symbol.width * 1.2
      
      nbins <- floor(1 / (wd))
      if (nbins == 0) {
        nbins <- if (is.null(opts$hist.bins)) {
          wd <- convertWidth(unit(1.2 * opts$cex.dotpt, "char"),
                             "npc", valueOnly = TRUE)
          floor(1 / (wd))
        } else {
          opts$hist.bins
        }
      }
    }
    
    

  }
  
  plist <- lapply(out, makeHist,
                  nbins = nbins, xlim = xattr$xrange, bins = bins)
  
  
  ## Generate a list of the inference information for each plot:
  inflist <- dotinference(obj)
  
  out <- list(toplot = plist, n.missing = n.missing, boxinfo = boxinfo, inference.info = inflist,
              nacol = if ("colby" %in% v) any(sapply(plist, function(T)
                if (is.null(T$colby)) FALSE else any(is.na(T$colby)))) else FALSE,
              xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
              ylim = c(0, max(sapply(plist, function(p) if (is.null(p)) 0 else max(p$counts)))),
              n.label = if (is.null(xattr$nextreme)) NULL else rep(xattr$nextreme, length = 2))
  
  class(out) <- ifelse(hist, "inzhist", "inzdot")
  
  out
}

plot.inzdot <- function(obj, gen, hist = FALSE) {
  # First step is to grab stuff:
  xlim <- current.viewport()$xscale
  ylim <- current.viewport()$yscale
  opts <- gen$opts
  mcex <- gen$mcex
  col.args <- gen$col.args
  boxplot <- opts$boxplot
  expand.points <- 1# if (is.null(opts$expand.points)) 1 else opts$expand.points
  
  toplot <- obj$toplot
  boxinfo <- obj$boxinfo
  inflist <- obj$inference.info
  
  nlev <- length(toplot)
  pushViewport(viewport(layout = grid.layout(nrow = nlev),
                        name = "VP:dotplot-levels", clip = "on"))
  Hgts <- if (boxplot) c(3, 1) else c(1, 0)
  dpLayout <- grid.layout(nrow = 2, heights = unit(Hgts, "null"))
  
  # we need to make the dots stack nicely, if they fit
  maxcount <- gen$maxcount
  seekViewport("VP:dotplot-levels")
  pushViewport(viewport(layout.pos.row = 1))
  pushViewport(viewport(layout = dpLayout))
  pushViewport(viewport(layout.pos.row = 1))  # this is where dots will go
  
  ht <- convertHeight(unit(opts$cex.dotpt, "char"),
                      "npc", valueOnly = TRUE)
  ny <- floor(convertHeight(unit(1, "npc"),
                            "npc", valueOnly = TRUE) / ht)
  
  maxdots <- max(ny, maxcount)
  ylim <- c(0, maxdots * 1.05)
  
  GROUP.names <- if (nlev > 1 & opts$internal.labels) names(toplot) else NULL
  
  for (i in 1:nlev) {
    pp <- toplot[[i]]
    
    seekViewport("VP:dotplot-levels")
    pushViewport(viewport(layout.pos.row = i))
    pushViewport(viewport(layout = dpLayout))
    
    pushViewport(viewport(layout.pos.row = 2, xscale = xlim, clip = "on"))
    if (boxplot) addBoxplot(boxinfo[[i]])
    if (!is.null(inflist)) addUnivarInference(inflist, i, opts)
    upViewport()
    
    vpname <- ifelse(nlev == 1, "VP:plotregion", paste0("VP:plotregion-", i))
    
    pushViewport(viewport(layout.pos.row = 1,
                          xscale = xlim, yscale = ylim,
                          name = vpname))
    
    ptCols <- colourPoints(pp$colby, col.args, opts)
    if (length(ptCols) == 1)
      ptCols <- rep(ptCols, length(pp$x))
    
    ptPch <- rep(opts$pch, length(pp$x))
    
    if (!is.null(pp$text.labels)) {
      locID <- which(pp$text.labels != "")
      if (!is.null(col.args$locate.col)) {
        ptCols[locID] <- col.args$locate.col
        ptPch[locID] <- 19
      }
    }
    
    if (length(pp$x) > 0) {
      NotInView <- pp$x < min(xlim) | pp$x > max(xlim)
      ptPch[NotInView] <- NA
      grid.points(pp$x, pp$y, pch = ptPch,
                  gp =
                    gpar(col = ptCols,
                         cex = opts$cex.dotpt / expand.points, lwd = opts$lwd.pt,
                         alpha = opts$alpha, fill = obj$fill.pt),
                  name = "DOTPOINTS")
      
      ## Highlighting:
      if (!is.null(pp$highlight) & length(ptCols) > 1) {
        hl <- as.logical(pp$highlight)
        if (sum(hl) > 0) {
          hcol <-
            if (opts$highlight.col == "shade")
              shade(ptCols[hl], 0.6)
          else
            opts$highlight.col
          
          grid.points(pp$x[hl], pp$y[hl], pch = 19, 
                      gp =
                        gpar(col = hcol,
                             cex = opts$cex.dotpt * 1.4, lwd = opts$lwd.pt))
          
          grid.points(pp$x[hl], pp$y[hl], pch = 19, 
                      gp =
                        gpar(col = ptCols[hl],
                             cex = opts$cex.dotpt, lwd = opts$lwd.pt))
        }
      }
    }
    
    ## Label extremes
    if (!is.null(pp$text.labels))
      if (sum(!is.na(pp$text.labels) > 0) & (length(locID) > 0))
        grid.text(paste0("  ", pp$text.labels[locID]), pp$x[locID], pp$y[locID],
                  default.units = "native", just = c("left"), rot = 45,
                  gp = gpar(cex = 0.6))
    
    ## Label group
    if (!is.null(GROUP.names))
      grid.text(GROUP.names[i], x = 0.01, y = 0.98, just = c("left", "top"),
                gp = gpar(cex = 0.8))
  }
  
  seekViewport("VP:dotplot-levels")
  upViewport()
  
  if (!is.null(inflist))
    if ("comp" %in% names(inflist[[1]]))
      addUnivarCompLines(inflist)
  
}





boxSummary <- function(obj, opts) {
  lapply(obj, function(o) {
    if (is.null(o))
      return(NULL)
    
    if (!inherits(o, "survey.design")) {
      quant <- quantile(o$x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      min <- min(o$x, na.rm = TRUE)
      max <- max(o$x, na.rm = TRUE)            
    } else {
      if (nrow(o$variables) < 5) return(NULL)
      svyobj <- o
      quant <- svyquantile(~x, svyobj, quantiles = c(0.25, 0.5, 0.75))
      min <- min(svyobj$variables$x, na.rm = TRUE)
      max <- max(svyobj$variables$x, na.rm = TRUE)
    }
    
    list(quantiles = quant, min = min, max = max, inference = FALSE, opts = opts)
  })
}



addBoxplot <- function(x) {
  opts <- x$opts
  if (is.null(x))
    return()
  
  xx <- rep(x$quantiles, each = 4)[3:10]
  yy <- rep(c(0.2, 0.8, 0.8, 0.2), 2)
  id <- rep(1:2, each = 4)
  grid.polygon(unit(xx, "native"), unit(yy, "npc"), id = id,
               gp = gpar(lwd = opts$box.lwd[1], fill = opts$box.fill))
  grid.polyline(unit(c(x$min, x$quantiles[1], x$quantiles[3], x$max), "native"),
                rep(0.5, 4), id = rep(1:2, each = 2),
                gp = gpar(lwd = opts$box.lwd[2]))
  
}



dotinference <- function(obj) {
  ## obj: list of data broken down by subsets
  ## opts: various options (inzpar)
  
  opts <- obj$opts
  xattr <- obj$xattr
  inf.par <- opts$inference.par
  inf.type <- opts$inference.type
  if (!is.null(inf.type) & is.null(inf.par))
    inf.par <- c("mean", "median", "iqr")
  bs <- opts$bs.inference
  
  if (is.null(inf.par) & is.null(inf.type)) {
    return(NULL)
  }
  
  if (nrow(obj$df) < opts$min.count) return(NULL)
  
  ## for simplicity, if no 'y' factor, just make all the same level for tapply later:
  if (obj$xattr$class == "inz.simple") {
    svy <- FALSE
    dat <- obj$df
    if (!"y" %in% colnames(dat)) {
      dat <- data.frame(dat, y = factor("all"))
      inf.type <- inf.type[inf.type != "comp"]
    } else if (bs) {
      dat <- dat[!is.na(dat$y), ]
    }
  } else {
    ## survey structure ...
    svy <- TRUE
    dat <- obj$df
    if (!"y" %in% colnames(dat$variables)) {
      inf.type <- inf.type[inf.type != "comp"]
    } else if (bs) {
      dat <- dat[!is.na(dat$y), ]
    }
  }
  
  if (!is.null(inf.par)) {
    lapply(inf.par, function(ip) {
      switch(
        ip,
        "mean" = {
          lapply(inf.type, function(type) {
            switch(
              type,
              "conf" = {
                if (bs) {
                  ## 95% bootstrap confidence interval
                  if (svy) {
                    if ("y" %in% colnames(dat$variables)) {
                      NULL
                    } else {
                      NULL
                    }
                  } else {
                    b <- boot(dat, strata = dat$y,
                              function(d, f) tapply(d[f, 1], d[f, 2], mean, na.rm = TRUE),
                              R = opts$n.boot)
                    ci <- cbind(t(apply(b$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)),
                                colMeans(b$t))
                    dimnames(ci) <- list(levels(dat$y),
                                         c("lower", "upper", "mean"))
                    ci
                  }
                } else {
                  ## 95% confidence interval (normal theory)
                  if (svy) {
                    if ("y" %in% colnames(dat$variables)) {
                      fit <- svyglm(x ~ y, design = dat)
                      ci <- confint(predict(fit, newdata = data.frame(y = levels(dat$variables$y))))
                      dimnames(ci) <- list(levels(dat$variables$y),
                                           c("lower", "upper"))
                    } else {
                      fit <- svyglm(x ~ 1, design = dat)
                      ci <- confint(fit)
                      dimnames(ci) <- list("all", c("lower", "upper"))
                    }
                    ci
                  } else {
                    n <- tapply(dat$x, dat$y, function(z) sum(!is.na(z)))
                    n <- ifelse(n < 5, NA, n)
                    wd <- qt(0.975, df = n - 1) * tapply(dat$x, dat$y, sd,
                                                         na.rm = TRUE) / sqrt(n)
                    mn <- tapply(dat$x, dat$y, mean, na.rm = TRUE)
                    cbind(lower = mn - wd, upper = mn + wd, mean = mn)
                  }
                }
              },
              "comp" = {
                if (bs) {
                  if (svy) {
                    NULL
                  } else {
                    b <- boot(dat, strata = dat$y,
                              function(d, f) tapply(d[f, 1], d[f, 2], mean, na.rm = TRUE),
                              R = opts$n.boot)
                    cov <- cov(b$t)
                    ses <- suppressWarnings(seCovs(cov))
                    ci <- suppressWarnings(moecalc(
                      ses, est = tapply(dat$x, dat$y, mean, na.rm = TRUE)
                    ))
                    cbind(lower = ci$compL, upper = ci$compU)
                  }
                } else {
                  if (svy) {
                    fit <- svyglm(x ~ y, design = dat)
                    est <- predict(fit, newdata = data.frame(y = levels(dat$variables$y)))
                    mfit <- suppressWarnings(moecalc(fit, factorname = "y", est = est))
                    cbind(with(mfit, cbind(lower = compL, upper = compU)) + coef(fit)[1], mean = est)
                  } else {
                    ## ########################################################## ##
                    ## This is the old method, an approximately 75% CI ...        ##
                    ## ########################################################## ##
                    ## n <- tapply(dat$x, dat$y, function(z) sum(!is.na(z)))      ##
                    ## wd <- tapply(dat$x, dat$y, sd, na.rm = TRUE) / sqrt(2 * n) ##
                    ## mn <- tapply(dat$x, dat$y, mean, na.rm = TRUE)             ##
                    ## cbind(lower = mn - wd, upper = mn + wd)                    ##
                    ## ########################################################## ##
                    
                    ## The new method uses iNZightMR:
                    
                    if(any(is.na(tapply(dat$x, dat$y, length)))) {
                      NULL
                    } else {
                      ycounts <- with(dat, tapply(x, y, function(x) sum(!is.na(x))))
                      if (any(ycounts < 5)) {
                        wi <- which(ycounts >= 5)
                        if (length(wi) == 1) {
                          NULL
                        } else {
                          ylevi <- levels(dat$y)[wi]
                          newdat <- dat[dat$y %in% ylevi, ]
                          newdat$y <- factor(newdat$y)
                          fit <- lm(x ~ y - 1, data = newdat)
                          est <- predict(fit, newdata = data.frame(y = levels(newdat$y)))
                          ses <- seIndepSes(summary(fit)$coef[, 2])
                          mfit <- suppressWarnings(moecalc(ses, est = est, base = FALSE))
                          coef.mat <- matrix(NA, ncol = 3, nrow = length(levels(dat$y)))
                          coef.mat[wi, ] <-
                            cbind(with(mfit, cbind(lower = compL, upper = compU)), mean = est)
                          dimnames(coef.mat) <- list(levels(dat$y), c("lower", "upper", "mean"))
                          coef.mat
                        }
                      } else {
                        fit <- lm(x ~ y - 1, data = dat)
                        est <- predict(fit, newdata = data.frame(y = levels(dat$y)))
                        ses <- seIndepSes(summary(fit)$coef[, 2])
                        mfit <- suppressWarnings(moecalc(ses, est = est, base = FALSE))
                        cbind(with(mfit, cbind(lower = compL, upper = compU)), mean = est)
                      }
                    }
                  }
                }
              })
          })
        },
        "median" = {
          lapply(inf.type, function(type) {
            switch(
              type,
              "conf" = {
                if (bs) {
                  if (svy) {
                    NULL
                  } else {
                    b <- boot(dat, strata = dat$y,
                              function(d, f) tapply(d[f, 1], d[f, 2], quantile, probs = 0.5, na.rm = TRUE),
                              R = 2 * opts$n.boot)
                    ci <- cbind(t(apply(b$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)),
                                colMeans(b$t))
                    dimnames(ci) <- list(levels(dat$y),
                                         c("lower", "upper", "mean"))
                    ci
                  }
                } else {
                  if (svy) {
                    NULL
                  } else {
                    ## YEAR 12 INTERVALS
                    #iqr <- 
                    
                    n <- tapply(dat$x, dat$y, function(z) sum(!is.na(z)))
                    n <- ifelse(n < 2, NA, n)
                    wd <- 1.5 * #qt(0.975, df = n - 1) *
                      tapply(dat$x, dat$y, function(z)
                        diff(quantile(z, c(0.25, 0.75), na.rm = TRUE))) / sqrt(n)
                    mn <- tapply(dat$x, dat$y, median, na.rm = TRUE)
                    cbind(lower = mn - wd, upper = mn + wd, mean = mn)
                  }
                }
              },
              "comp" = {
                if (bs) {
                  if (svy) {
                    NULL
                  } else {
                    b <- boot(dat, strata = dat$y,
                              function(d, f) tapply(d[f, 1], d[f, 2], median, na.rm = TRUE),
                              R = opts$n.boot)
                    cov <- cov(b$t)
                    ses <- suppressWarnings(seCovs(cov))
                    ci <- suppressWarnings(moecalc(ses, est = tapply(dat$x, dat$y, median, na.rm = TRUE)))
                    cbind(lower = ci$compL, upper = ci$compU)
                  }
                } else {
                  if (svy) {
                    NULL
                  } else {
                    n <- tapply(dat$x, dat$y, function(z) sum(!is.na(z)))
                    wd <- 1.5 * tapply(dat$x, dat$y, function(z) abs(diff(quantile(z, c(0.25, 0.75), na.rm = TRUE)))) / sqrt(n)
                    mn <- tapply(dat$x, dat$y, quantile, probs = 0.5, na.rm = TRUE)
                    cbind(lower = mn - wd, upper = mn + wd)
                  }
                }
              })
          })
        },
        "iqr" = {
          list(conf = 
                 if ("conf" %in% inf.type & bs) {
                   if (svy) {
                     NULL
                   } else {
                     b <- boot(dat, strata = dat$y,
                               function(d, f) tapply(d[f, 1], d[f, 2],
                                                     function(x) diff(quantile(x, probs = c(0.25, 0.75), na.rm = TRUE))),
                               R = 2 * opts$n.boot)
                     iqr <- cbind(t(apply(b$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)),
                                  colMeans(b$t))
                     dimnames(iqr) <- list(levels(dat$y),
                                           c("lower", "upper", "mean"))
                     iqr
                   }
                 } else {
                   NULL
                 }
          )
        }) -> result
      names(result) <- inf.type
      result
    }) -> result.list
    names(result.list) <- inf.par
    result.list
  } else {
    return(NULL)
  }
  
  attr(result.list, "bootstrap") <- bs
  result.list
}




create.inz.gridplot <- function(obj) {
  ## The original "grid" style plot for large sample sizes. This will only be used for
  ## simple iid data, and instead hexagonal binning will be used for frequency and
  ## survey data plots.
  
  df <- obj$df
  opts <- obj$opts
  xattr <- obj$xattr
  
  if (xattr$class == "inz.survey") {
    warning("The grid plot doesn't account for survey and frequency weighting. Better to use plottype = \"hex\".")
    df <- df$variables
  }
  
  v <- colnames(df)
  vn <- xattr$varnames    
  
  # first need to remove missing values
  missing <- apply(df[ , v %in% c("x", "y")], 1, function(x) any(is.na(x)))
  n.missing <- sum(missing)
  df <- df[!missing, ]
  
  ## Because this requires the xlimits to first be calculated, we can only create the
  ## plot AFTER we have done all the plots ...
  
  makeRects <- function(args, xlim, ylim) {
    df <- args$df
    opts <- args$opts
    xattr <- args$xattr
    
    # Set up the grid
    # (for now, we will scale the bin size by point size)
    Npt <- min(250, round(opts$scatter.grid.bins))  # / (opts$cex.pt * 2)))
    
    #  scatter.grid <- matrix(0, nrow = Npt, ncol = Npt)
    xbrk <- seq(xlim[1], xlim[2], length = Npt + 1)
    ybrk <- seq(ylim[1], ylim[2], length = Npt + 1)
    xx <- cut(df$x, xbrk)
    yy <- cut(df$y, ybrk)
    
    scatter.grid <- as.matrix(table(yy, xx))
    scatter.grid <- scatter.grid[Npt:1, ]
    
    # If a point has very high density, it dominates. So instead, shade by quantiles.
    nquants <- max(3, length(df$x) / 100)
    br <- unique(quantile(c(scatter.grid), seq(0, 1, length = nquants)))
    which.quant <- as.numeric(cut(c(scatter.grid), breaks = c(-1, br)))
    
    hcols <- hcl(0, 0, seq(100 * (1 - opts$alpha / 2), 0,
                           length = length(br)))
    shade <- matrix(hcols[which.quant], nrow = nrow(scatter.grid))
    
    is0 <- c(scatter.grid) == 0
    
    # centers of all grid boxes
    xv = (rep(1:Npt, each = Npt) - 0.5) / Npt
    yv = (rep(Npt:1, Npt) - 0.5) / Npt
    
    # We will attempt to use grid.polygon() to draw all of the
    # grid squares at the same time!
    wx <- 0.5 / Npt
    wy <- 0.5 / Npt
    xmat <- sapply(xv, function(x) x + c(-1, -1, 1, 1) * wx)
    ymat <- sapply(yv, function(y) y + c(-1, 1, 1, -1) * wy)
    id <- matrix(rep((1:Npt^2), each = 4), nrow = 4)
    
    # Remove zero-count cells:
    xmat <- xmat[, !is0]
    ymat <- ymat[, !is0]
    id <- id[, !is0]
    shade <- shade[!is0]
    
    list(xg = xmat, yg = ymat, id = id, col = shade)
  }
  
  out <- list(makeRects = makeRects, args = list(df = df, opts = opts, xattr = xattr),
              n.missing = n.missing, colby = df$colby, nacol = FALSE,
              xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
              ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf),
              x = df$x, y = df$y,
              trend = opts$trend, trend.by = opts$trend.by, smooth = opts$trend,
              n.boot = opts$n.boot)
  class(out) <- "inzgrid"
  
  out
}

plot.inzgrid <- function(obj, gen) {
  xlim <- current.viewport()$xscale
  ylim <- current.viewport()$yscale
  opts <- gen$opts
  mcex <- gen$mcex
  col.args <- gen$col.args
  
  gr <- obj$makeRects(obj$args, xlim, ylim)
  
  grid.polygon(gr$xg, gr$yg, id = gr$id, default.units = "npc",
               gp = gpar(fill = gr$col, col = gr$col))
  
  ## ---------------------------------------------------------------------------- ##
  ## Now that the main plot has been drawn, time to add stuff to it!
  
  # Line of Equality (LOE)
  if (opts$LOE) {
    xx <- c(min(xlim, ylim), max(xlim, ylim))
    grid.lines(xx, xx, default.units = "native",
               gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
  }
  
  # Add additional features to plot:
  addXYsmoother(obj, opts, col.args, xlim, ylim)
  addXYtrend(obj, opts, col.args, xlim, ylim)
  
  invisible(NULL)
}




create.inz.hexplot <- function(obj) {
  # make a plot using hexagonal binning
  
  # take the dataframe and settings from the object
  df <- obj$df
  opts <- obj$opts
  xattr <- obj$xattr
  
  if (xattr$class == "inz.survey")
    df <- df$variables
  
  v <- colnames(df)
  vn <- xattr$varnames
  
  # first need to remove missing values
  missing <- apply(df[ , v %in% c("x", "y")], 1, function(x) any(is.na(x)))
  n.missing <- sum(missing)
  df <- df[!missing, ]
  
  xbins <- opts$hex.bins
  #if (is.null(opts$hex.bins)) {
  #    if ((cpt <- opts$cex.pt) > 1) {
  #        (1 - (cpt - 1) / 2.5) * (25) + 5
  #    } else {
  #        (1 - (cpt - 0.05) / 0.95) * 70 + 30
  #    }
  #} else {
  #    opts$hex.bins
  #}
  
  ## hexbin returns an S4 object, so need to use the @ operator
  hb <- hexbin(df$x, df$y, IDs = TRUE, xbins = xbins)
  
  cellid <- hb@cID
  ## now manipulate the counts with the weight variable
  W <- switch(xattr$class,
              "inz.freq" = df$freq,
              "inz.survey" = weights(obj$df, "sampling")[!missing],
              "inz.simple" = rep(1, nrow(df)))
  
  hb@count <- as.vector(tapply(W, cellid, sum))
  hb@xcm <- as.vector(tapply(1:length(df$x), cellid,
                             function(i) weighted.mean(df$x[i], W[i])))
  hb@ycm <- as.vector(tapply(1:length(df$y), cellid,
                             function(i) weighted.mean(df$x[i], W[i])))
  
  out <- list(hex = hb, n.missing = n.missing, svy = obj$df,
              colby = df$colby, nacol = FALSE,
              xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
              ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf),
              x = df$x, y = df$y,
              trend = opts$trend, trend.by = opts$trend.by, smooth = opts$trend,
              n.boot = opts$n.boot)
  class(out) <- "inzhex"
  
  out
}

plot.inzhex <- function(obj, gen) {
  xlim <- current.viewport()$xscale
  ylim <- current.viewport()$yscale
  opts <- gen$opts
  mcex <- gen$mcex
  col.args <- gen$col.args
  
  grid.hexagons(obj$hex, style = "centroids", maxcnt = gen$maxcount)
  
  ## ---------------------------------------------------------------------------- ##
  ## Now that the main plot has been drawn, time to add stuff to it!
  
  # Line of Equality (LOE)
  if (opts$LOE) {
    xx <- c(min(xlim, ylim), max(xlim, ylim))
    grid.lines(xx, xx, default.units = "native",
               gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
  }
  
  addXYsmoother(obj, opts, col.args, xlim, ylim)
  addXYtrend(obj, opts, col.args, xlim, ylim)
  
  invisible(NULL)
}




create.inz.histplot <- function(obj) {
  create.inz.dotplot(obj, hist = TRUE)
}

plot.inzhist <- function(obj, gen) {
  # First step is to grab stuff:
  xlim <- current.viewport()$xscale
  ylim <- current.viewport()$yscale
  opts <- gen$opts
  mcex <- gen$mcex
  boxplot <- opts$boxplot
  
  toplot <- obj$toplot
  boxinfo <- obj$boxinfo
  inflist <- obj$inference.info
  
  nlev <- length(toplot)
  pushViewport(viewport(layout = grid.layout(nrow = nlev),
                        name = "VP:histplot-levels"))
  Hgts <- if (boxplot) c(3, 1) else c(1, 0)
  dpLayout <- grid.layout(nrow = 2, heights = unit(Hgts, "null"))
  
  ylim <- c(0, gen$maxcount * 1.05)
  
  GROUP.names <- if (nlev > 1 & opts$internal.labels) names(toplot) else NULL
  
  for (i in 1:nlev) {
    pp <- toplot[[i]]
    seekViewport("VP:histplot-levels")
    pushViewport(viewport(layout.pos.row = i))
    pushViewport(viewport(layout = dpLayout))
    
    pushViewport(viewport(layout.pos.row = 2, xscale = xlim))
    if (boxplot) addBoxplot(boxinfo[[i]])
    if (!is.null(inflist)) addUnivarInference(inflist, i, opts)
    upViewport()
    
    pushViewport(viewport(layout.pos.row = 1,
                          xscale = xlim, yscale = ylim,
                          name = paste0("VP:plotregion-", i)))
    
    if (length(pp$x) > 0) {
      bar.x <- rep(pp$breaks, each = 4)
      bar.x <- bar.x[-c(1:2, length(bar.x) - 0:1)]
      bar.y <- c(sapply(pp$counts, function(y) c(0, y, y, 0)))
      bar.id <- rep(1:length(pp$counts), each = 4)
      
      grid.polygon(bar.x, bar.y, bar.id, default.units = "native",
                   gp =
                     gpar(fill = opts$bar.fill, col = opts$bar.col,
                          lwd = opts$bar.lwd))
    }
    
    ## Label group
    if (!is.null(GROUP.names))
      grid.text(GROUP.names[i], x = 0.01, y = 0.98, just = c("left", "top"),
                gp = gpar(cex = 0.8))
  }
  
  seekViewport("VP:histplot-levels")
  upViewport()
  
  if (!is.null(inflist))
    if ("comp" %in% names(inflist[[1]]))
      addUnivarCompLines(inflist)
}



##' Plotting parameters for iNZight Plots
##'
##' A whole suite of parameters that can be used to fine-tune plots obtained from the
##' \code{iNZightPlot} function.
##' The parameters include both plot type, style, and appearance.
##' They are described below.
##'
##' \describe{
##' \item{'pch'}{the plotting symbol to be used; default is `1` (empty circle)}
##' \item{'col.pt'}{the colour of points. this can either be a single value, or a vector of
##' colours if \code{colby} is specified}
##' \item{'col.missing'}{the colour for missing values; default is a light grey}
##' \item{'cex'}{the overal scaling for the entire plot; values less than 1 will make the text and points
##' smaller, while values larger than 1 will magnify everything}
##' \item{'cex.pt'}{the scaling value for points}
##' \item{'cex.dotpt'}{the scaling value for points in a dotplot. Note, this is not multiplicative with
##' \code{'cex.pt'}}
##' \item{'cex.lab'}{the scaling value for the plot labels}
##' \item{'cex.axis'}{the scaling value for the axis labels}
##' \item{'cex.main'}{the scaling value for the main plot title}
##' \item{'cex.text'}{the scaling value for text on the plot}
##' \item{'alpha'}{transparency setting for points; default is 1, 0 is fully transparent}
##' \item{'bg'}{the background color for the plot}
##' \item{'fill.pt'}{the fill colour for points; default is \code{"transparent"}}
##' \item{'lwd'}{the line width of lines (for joining points)}
##' \item{'lty'}{the line type of lines (for joining points)}
##' \item{'lwd.pt'}{the line width used for points; default is 2}
##' \item{'col.line'}{the colour of lines used to join points}
##' \item{'col.sub'}{the background colour of subplot labels}
##' \item{'locate.col.def'}{the default colour for locating points}
##' \item{'jitter'}{the axes to add jitter to. Takes values \code{"x"}, \code{"y"},
##' or \code{"xy"} (default is en empty string, \code{""})}
##' \item{'rugs'}{the axes to add rugs to. Takes same values as \code{jitter}}
##' \item{'trend'}{a vector containing the trend lines to add to the plot. Possible values
##' are \code{c("linear", "quadratic", "cubic")}}
##' \item{'smooth'}{the smoothing (lowess) for the points. Takes a value between 0 and 1
##' (the default, 0, draws no smoother)}
##' \item{'smoothby.lty'}{the line type used for smoothers if \code{trend.by = TRUE}}
##' \item{'quant.smooth'}{if quantile smoothers are desired, they can be specified here as either
##' the quantiles to smooth over (e.g., \code{c(0.25, 0.5, 0.75)}), or \code{"default"}, which
##' uses the sample size to decide on an approprite set of quantile smoothers}
##' \item{'LOE'}{logical, if \code{TRUE}, then a 1-1 line of equality is drawn}
##' \item{'join'}{logical, if \code{TRUE}, then points are joined by lines}
##' \item{'lines.by}{logical, if \code{join = TRUE} and \code{colby} is specified, points are joined
##' by the specified variable}
##' \item{'col.trend'}{a named list of colors to be used for drawing the lines. The default is
##' \code{list(linear = "blue", quadratic = "red", cubic = "green4")}}
##' \item{'trend.by'}{logical, if \code{TRUE}, then trend lines are drawn separately for
##' each group specified by \code{colby}}
##' \item{'trend.parallel'}{logical, if \code{TRUE}, the trend lines by group are given the same slope;
##' otherwise they are fit independently}
##' \item{'col.smooth'}{the colour of the smoother}
##' \item{'col.LOE'}{the colour of the line of equality}
##' \item{'lty.LOE'}{the line type of the line of equality}
##' \item{'boxplot'}{logical, if \code{TRUE}, a boxplot is drawn with dotplots and histgrams}
##' \item{'box.lwd', 'box.col', 'box.fill'}{the line width, colour, and fill colour for
##' the box plot drawn}
##' \item{'bar.lwd', 'bar.col', 'bar.fill'}{the line width, colour, and fill colour of bars in a bar plot}
##' \item{'full.height'}{may no longer be necessary ...}
##' \item{'inf.lwd.comp', 'inf.lwd.conf'}{the line width of comparison and confidence intervals, respectively}
##' \item{'inf.col.comp', 'inf.col.conf'}{the colour of comparison and confidence intervals, respectively.
##' These take a length 2 vector, where the first element is used for normal inference, while the second
##' is used for bootstrap intervals}
##' \item{'inference.type'}{the type of inference added to the plot. Possible values
##' are \code{c("comp", "conf")}}
##' \item{'inference.par'}{the parameter which we obtain intervals for. For a dotplot or histogram,
##' this can be either \code{"mean"} or \code{"median"}; for bar plots it can be "proportion"}
##' \item{'bs.inference'}{logical, if \code{TRUE}, then nonparametric bootstrap simulation is used
##' to obtain the intervals}
##' \item{'min.count'}{the min count for barplots inference; counts less than this are ignored}
##' \item{'n.boot'}{the number of bootstrap simulations to perform}
##' \item{'large.sample.size'}{sample sizes over this value will use a large-sample plot variant
##' (i.e., scatter plots will become hex plots, dot plots become histograms)}
##' \item{'largesample'}{logical, if \code{TRUE}, then the large-sample plot variance is used}
##' \item{'scatter.grid.bins'}{the number, N, of bins to use for the scatter-grid plot,
##' producing an N x N matrix}
##' \item{'hex.bins'}{the number of bins to use for hexagonal binning}
##' \item{'quant.cutoff'}{if \code{quant.smooth = "default"}, these sample size values are used
##' to determine which quantiles are drawn}
##' \item{'plottype'}{used to override the default plot type. Possible values, depending on data type,
##' include \code{c("scatter"|"grid"|"hex"|"dot"|"hist")}}
##' \item{'matchplots'}{logical, if \code{TRUE}, then the type of plot is kept consistent between different
##' subsets}
##' \item{'match.limits'}{a vector of two values used to decide whether to use all small-sample or all
##' large-sample plots}
##' \item{'xlim'}{a vector defining the x axis limits (default NULL will use the data)}
##' \item{'ylim'}{a vector defining the y axis limits (default NULL will use the data)}
##' }
##' 
##' @title iNZight Plotting Parameters
##' @param ... If arguments are supplied, then these values are set. If left empty, then
##' the default list is returned.
##' @return an object of class \code{inzpar.list}
##' @author tell029
##' @export
inzpar <- function(...) {
  dots <- list(...)
  
  ip <- list(pch            = 1,
             col.pt         = "grey50",
             col.missing    = "#cccccc",
             cex            = 1,
             cex.pt         = 0.8,
             cex.dotpt      = 0.5,
             cex.lab        = 1,
             cex.axis       = 0.8,
             cex.main       = 1.2,
             cex.text       = 1,
             alpha          = 1,
             bg             = "white",
             fill.pt        = "transparent",
             lwd            = 1,
             lty            = 1,
             lwd.pt         = 2,
             col.line       = "blue",
             col.sub        = "wheat",
             locate.col.def = "red",
             highlight.col  = "shade",
             jitter         = "",
             rugs           = "",
             trend          = NULL,
             smooth         = 0,
             smoothby.lty   = 4,  # MUST be numeric
             quant.smooth   = NULL,
             LOE            = FALSE,
             join           = FALSE,
             lines.by       = TRUE,
             col.trend =
               list(linear = "blue",
                    quadratic = "red",
                    cubic ="green4"),
             trend.by       = FALSE,
             trend.parallel = TRUE,
             col.smooth     = c("magenta"),
             col.LOE        = "black",
             lty.LOE        = 2,
             boxplot        = TRUE,
             box.lwd        = c(2, 0.7),
             box.col        = "black",
             box.fill       = "grey90",
             bar.lwd        = 1,
             bar.col        = "black",
             bar.fill       = "darkgreen",
             full.height    = FALSE,
             inf.lwd.comp   = 4,
             inf.lwd.conf   = 2,
             inf.col.comp   = c("black", "green4"),
             inf.col.conf   = c("red", "orange"),
             inference.type = NULL,
             inference.par  = NULL,
             bs.inference   = FALSE,
             min.count      = 5,
             n.boot         = 1500,
             large.sample.size = 2000,
             largesample    = NULL,
             scatter.grid.bins = 50,
             hex.bins       = 20,
             hist.bins      = NULL,
             quant.cutoff   = c(200, 1000),
             plottype       = "default",
             matchplots     = TRUE,
             match.limits   = c(500, 10000),
             internal.labels= TRUE,
             xlim           = NULL,
             ylim           = NULL)
  
  # update any user has specified
  if (length(dots) > 0) {
    for (i in names(dots))
      ip[[i]] <- dots[[i]]
  }
  
  class(ip) <- "inzpar.list"
  ip
}






inzPlotDefaults <-
  function() {
    # Houses all of the default settings for plotting. Alternatively,
    # could edit them (i.e., points to a list in global environment)
    inzpar()
  }

nothing <- function() {
  list(pch            = 1,
       col            = "red",
       col.pt         = "grey50",
       cex            = 1,
       cex.pt         = 1.0,
       cex.lab        = 1,
       cex.axis       = 0.8,
       cex.main       = 1.2,
       cex.text       = 1,
       alpha          = 1,
       bg             = "white",
       fill.pt        = "transparent",
       lwd            = 1,
       lty            = 1,
       lwd.pt         = 2,
       col.line       = "blue",
       col.sub        = "wheat",
       jitter         = "",
       rugs           = "",
       trend          = NULL,
       smooth         = 0,
       quant.smooth   = NULL,
       LOE            = FALSE,
       join           = FALSE,
       lines.by       = TRUE,
       col.trend      = list(linear = "blue",
                             quadratic = "red",
                             cubic ="green4"),
       trend.by       = FALSE,
       col.smooth     = c("magenta"),
       col.LOE        = "black",
       lty.LOE        = 2,
       box            = TRUE,
       box.lwd        = c(2, 0.7),
       box.col        = "black",
       box.fill       = "grey90",
       bar.lwd        = 1,
       bar.col        = "black",
       bar.fill       = "darkgreen",
       inf.lwd.comp   = 4,
       inf.lwd.conf   = 2,
       inf.col.comp   = "black",
       inf.col.conf   = "red",
       inference.type = NULL,
       inference.par  = NULL,
       bs.inference   = FALSE,
       n.boot         = 1500,
       large.sample.size = 2000,
       largesample    = NULL,
       scatter.grid.bins = 200,
       hist.bins      = 100,
       quant.cutoff   = c(200, 1000))
}





create.inz.scatterplot <- function(obj) {
  # take the dataframe and settings from the object
  df <- obj$df
  opts <- obj$opts
  xattr <- obj$xattr
  
  if (xattr$class == "inz.survey") {
    df <- as.data.frame(cbind(df$variables,
                              weights = weights(df)))        
  }
  
  v <- colnames(df)
  vn <- xattr$varnames
  
  # first need to remove missing values
  missing <- apply(df[ , v %in% c("x", "y")], 1, function(x) any(is.na(x)))
  n.missing <- sum(missing)
  df <- df[!missing, ]
  
  # --- look through inzpar for settings
  
  # Jitter on x and y
  if ("x" %in% strsplit(opts$jitter, '')[[1]])
    df$x <- jitter(df$x)
  if ("y" %in% strsplit(opts$jitter, '')[[1]])
    df$y <- jitter(df$y)
  
  
  ## The plotting symbol:
  pch <- rep(ifelse(opts$alpha == 1,
                    ifelse(opts$fill.pt == "transparent", opts$pch, 21),
                    19), nrow(df))
  
  ## --- this is where FREQUENCY or SURVEY information is used to control sizes of points
  # size of points
  resize <- TRUE
  if ("freq" %in% v) {
    propsize <- df$freq / xattr$max.freq * 4 + 0.5
  } else if ("weights" %in% v) {
    if (length(unique(df$weights)) == 1) {
      resize <- FALSE
      propsize <- opts$cex.pt
    } else
      propsize <- df$weights       
  } else if ("sizeby" %in% v) {
    propsize <- df$sizeby
  } else {
    propsize <- opts$cex.pt
    resize <- FALSE
  }
  
  if (resize) {
    ## Try from half cex to 2 x cex
    cex.range <- opts$cex.pt * c(0.5, 4)
    propsize.range <- range(propsize, na.rm = TRUE)
    
    ## Calculate new sizes
    pr.size <- (propsize - propsize.range[1]) / (propsize.range[2] - propsize.range[1])
    propsize <- pr.size * (cex.range[2] - cex.range[1]) + cex.range[1]
  }
  
  pch[is.na(propsize)] <- 4
  propsize[is.na(propsize)] <- 0.6
  
  ext.ids <- NULL
  if ("extreme.label" %in% v) {
    eLab <- as.character(df$extreme.label)
    m <- cbind(df$x, df$y)
    if (sum(apply(m, 1, function(k) all(!is.na(k)))) > 0) {
      dist <- mahalanobis(m, colMeans(m, na.rm = TRUE), cov(m, use = "complete.obs"))
      o <- order(dist, decreasing = TRUE)
      text.labels <- eLab
      ext.ids <- o[1:min(sum(!is.na(dist)), xattr$nextreme)]
      text.labels[-ext.ids] <- ""
      ext.ids <- df$pointIDs[ext.ids]
    } else {
      text.labels <- character(length(eLab))
    }
  } else {
    text.labels <- as.character(df$locate)
  }
  
  # Combine everything together into a classed list which will have a `plot` method
  out <- list(x = df$x, y = df$y, colby = df$colby, propsize = propsize, pch = pch,
              fill.pt = opts$fill.pt, n.missing = n.missing,
              nacol = if ("colby" %in% v) any(is.na(df$colby)) else FALSE,
              nasize = if ("sizeby" %in% v) any(is.na(df$sizeby)) else FALSE,
              xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
              ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf),
              trend = opts$trend, trend.by = opts$trend.by, smooth = opts$trend,
              n.boot = opts$n.boot, text.labels = text.labels, extreme.ids = ext.ids)
  
  if (xattr$class == "inz.survey")
    out$svy <- obj$df
  if ("highlight" %in% colnames(df))
    out$highlight <- df$highlight
  
  class(out) <- "inzscatter"
  
  out
}

plot.inzscatter <- function(obj, gen) {
  xlim <- current.viewport()$xscale
  ylim <- current.viewport()$yscale
  opts <- gen$opts
  mcex <- gen$mcex
  col.args <- gen$col.args
  
  if (length(obj$x) == 0)
    return()
  
  ptCols <- colourPoints(obj$colby, col.args, opts)
  
  ## If locating points:
  locating <- FALSE
  if ("text.labels" %in% names(obj)) {
    if (sum(obj$text.labels != "", na.rm = TRUE) > 0) {
      locating <- TRUE
      
      labID <- which(obj$text.labels != "")
      
      if (!is.null(col.args$locate.col)) {
        newCol <- col.args$locate.col
        
        if (length(ptCols) == 1)
          ptCols <- rep(ptCols, length(obj$x))
        ptCols[labID] <- newCol
        
        ## make them solid:
        obj$pch[labID] <- 19
      }
    }
  }
  
  
  NotInView <- obj$x < min(xlim) | obj$x > max(xlim) | obj$y < min(ylim) | obj$y > max(ylim)
  obj$pch[NotInView] <- NA
  grid.points(obj$x, obj$y, pch = obj$pch,
              gp =
                gpar(col = ptCols,
                     cex = obj$propsize,
                     lwd = opts$lwd.pt, alpha = opts$alpha,
                     fill = obj$fill.pt),
              name = "SCATTERPOINTS")
  
  ## Highlighting:
  if (!is.null(obj$highlight) & length(ptCols) > 1) {
    hl <- as.logical(obj$highlight)
    if (sum(hl) > 0) {
      hcol <-
        if (opts$highlight.col == "shade")
          shade(ptCols[hl], 0.6)
      else
        opts$highlight.col
      
      grid.points(obj$x[hl], obj$y[hl], pch = 19, 
                  gp =
                    gpar(col = hcol,
                         cex = obj$propsize * 1.4,
                         lwd = opts$lwd.pt))
      
      grid.points(obj$x[hl], obj$y[hl], pch = 19, 
                  gp =
                    gpar(col = ptCols[hl],
                         cex = obj$propsize,
                         lwd = opts$lwd.pt))
    }
  }
  
  
  if (locating) {
    labs <- obj$text.labels[labID]
    labx <- unit(obj$x[labID], "native")
    laby <- unit(obj$y[labID], "native") +
      (grobHeight(textGrob("0", gp = gpar(cex = obj$propsize))) +
         grobHeight(textGrob("0", gp = gpar(cex = 0.6)))) *
      ifelse(obj$y[labID] < mean(ylim), 1, -1)
    
    grid.text(labs, labx, laby, gp = gpar(cex = 0.6))
  }
  
  # Connect by dots if they want it ...
  if (opts$join) {
    if (length(unique(obj$colby)) == 1 | !opts$lines.by) {
      grid.lines(obj$x, obj$y, default.units = "native",
                 gp =
                   gpar(lwd = opts$lwd, lty = opts$lty,
                        col = opts$col.line))
    } else {
      byy <- as.factor(obj$colby)  # pseudo-by-variable
      xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$colby == c))
      ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$colby == c))
      
      for (b in 1:length(levels(byy)))
        grid.lines(xtmp[[b]], ytmp[[b]], default.units = "native",
                   gp =
                     gpar(lwd = opts$lwd, lty = opts$lty,
                          col = col.args$f.cols[b]))
    }
  }
  
  ## add rugs --- these only make sense for a scatter plot
  if ("x" %in% strsplit(opts$rug, '')[[1]]) {
    # Add marks on the x-axis at the location of every data point
    grid.polyline(x = unit(rep(obj$x, each = 2), "native"),
                  y = unit(rep(c(0, 0.5), length(obj$x)), "char"),
                  id.lengths = rep(2, length(obj$x)))
  }
  if ("y" %in% strsplit(opts$rug, '')[[1]]) {
    # Same, but for the y-axis
    grid.polyline(y = unit(rep(obj$y, each = 2), "native"),
                  x = unit(rep(c(0, 0.5), length(obj$y)), "char"),
                  id.lengths = rep(2, length(obj$y)))
  }
  
  ## ---------------------------------------------------------------------------- ##
  ## Now that the main plot has been drawn, time to add stuff to it!
  
  # Line of Equality (LOE)
  if (opts$LOE) {
    xx <- c(min(xlim, ylim), max(xlim, ylim))
    grid.lines(xx, xx, default.units = "native",
               gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
  }
  
  if (opts$trend.by)
    if (is.null(col.args$f.cols))
      opts$trend.by <- FALSE
  
  # Add additional features to plot:
  addXYsmoother(obj, opts, col.args, xlim, ylim)
  addXYtrend(obj, opts, col.args, xlim, ylim)
  
  invisible(NULL)
}




makeTitle <- function(names, types, g1.level = NULL, g2.level = NULL) {
  # creates the title text for the plot
  varnames <- names(names)
  if (types$x == "factor") {
    # Need a different title for barplots:
    # Distribution of X by Y
    title1 <- paste0("Distribution of ", names$x)
    title2 <- ifelse(! "y" %in% varnames, '', paste0(" by ", names$y))
  } else {
    if (!"y" %in% varnames) {
      title1 <- ""
      title2 <- names$x
    } else if (types$y == "numeric") {
      title1 <- paste0(names$y, ' versus ')
      title2 <- names$x
    } else {
      title1 <- names$x
      title2 <- paste0(' by ', names$y)
    }
  }
  
  title3 <- ifelse(! "g1" %in% varnames, '',
                   paste0(' subset by ', names$g1))
  title4 <- ifelse(! "g2" %in% varnames, '',
                   ifelse(is.null(g2.level), '',
                          ifelse(g2.level == "_MULTI",
                                 ifelse("g1" %in% varnames, paste0(" and ", names$g2),
                                        paste0(" subset by ", names$g2)),
                                 paste0(', for ', names$g2, ' = ', g2.level))))
  
  if ("y" %in% varnames) {
    if (types$x == "numeric" & types$y == "numeric") {
      title5 <- ifelse(!"sizeby" %in% varnames, '',
                       paste0(' (size proportional to ', names$sizeby, ')'))
    } else {
      title5 <- ''
    }
  } else {
    title5 <- ''
  }
  title <- paste0(title1, title2, title3, title4, title5)
  
  title
}



## Default methods for data (will be for inz.simple and anything without a method)

gSubset <- function(df, g1.level, g2.level, df.vs, missing)
  UseMethod("gSubset")

gSubset.default <- function(df, g1.level, g2.level, df.vs, missing) {
  # subset the data by g2 (keep everything, so xlims can be calculated)
  # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
  
  matrix.plot <- FALSE
  if ("g2" %in% df.vs) {
    if (is.null(g2.level)) g2.level <- "_ALL"
    ng2 <- length(g2l <- if (is.null(g2.level)) "all" else levels(df$data$g2))
    
    # if g2 specified numerically, check the value is ok, and then convert it to
    # character level anyway
    if (is.numeric(g2.level)) {
      if (as.integer(g2.level) != g2.level)
        warning(paste0("g2.level truncated to ", g2.level, "."))
      
      if (g2.level == 0) {
        g2.level <- "_ALL"
      } else if (g2.level == ng2 + 1) {
        g2.level <- "_MULTI"
      } else if (g2.level > ng2 + 1) {
        stop(paste("g2.level must be a number between 0 and", ng2 + 1))
      } else {
        g2.level <- g2l[g2.level]
      }
    }
    
    # separate function for drawing the matrix version
    if (g2.level == "_ALL") {
      df1 <- list(all = df$data)
      g2.level <- NULL
    } else {
      if (g2.level == "_MULTI") {
        matrix.plot <- TRUE
      }                
      
      missing$g2 <- sum(is.na(df$data$g2))
      df1 <- lapply(g2l,
                    function(l) {
                      dft <- subset(df$data, df$data$g2 == l)
                      dft[, colnames(dft) != "g2"]
                    })
      names(df1) <- g2l
    }
  } else {
    g2l <- "all"
    df1 <- list(all = df$data)
  }
  
  
  # now, `df` is a list of data.frame of all levels of g2 (unless
  # g2.level = NULL/_ALL/0).  `missing` constains the number of
  # observations lost by subsetting g2 due to missing values of g2.
  
  if ("g1" %in% df.vs) {
    # take two methods of specifying g1.level (numeric or level names), and convert to a vector
    # of only character names to be plotted
    g1l <- levels(df$data$g1)  # all levels of variable
    if (is.null(g1.level)) g1.level <- "_MULTI"
    
    if (is.numeric(g1.level)) {
      if (any(g1.level > length(levels(df$data$g1)))) g1.level <- 0
      g1.level <- if (any(g1.level == 0)) "_MULTI" else levels(df$data$g1)[g1.level]
    }
    
    if (any(g1.level == "_MULTI"))
      g1.level <- levels(df$data$g1)
    
    # track missing values due to missingness in g1
    missing$g1 <- sum(is.na(df$data$g1))
  } else {
    g1l <- "all"
    g1.level <- "all"
  }
  
  # this converts each data.frame in the list to a list of data
  # frames for all levels of g1
  df.list <- lapply(df1, function(df2) {
    df3 <- lapply(g1l, function(x) inzDataList(df2, x))
    names(df3) <- g1l
    df3
  })
  
  ## sum up all of the missing values
  w.df <-
    if (is.null(g2.level)) "all"
  else if (g2.level == "_MULTI") 1:length(df.list)
  else g2.level
  
  missing$x <- sum(sapply(df.list[w.df], function(df)
    sum(sapply(df, function(d) sum(is.na(d$x))))))
  if ("y" %in% df.vs)
    missing$y <- sum(sapply(df.list[w.df], function(df)
      sum(sapply(df, function(d) sum(is.na(d$y))))))
  
  class(df.list) <- "inz.simple"
  
  list(df = df.list, matrix = matrix.plot, missing = missing,
       g1.level = g1.level, g2.level = g2.level)
}




## All of the methods only used for SIMPLE data types:

gSubset.inz.freq <- function(df, g1.level, g2.level, df.vs, missing) {
  # subset the data by g2 (keep everything, so xlims can be calculated)
  # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
  
  dd <- df$data
  dd$freq <- df$freq
  
  matrix.plot <- FALSE
  if ("g2" %in% df.vs) {
    if (is.null(g2.level)) g2.level <- "_ALL"
    ng2 <- length(g2l <- if (is.null(g2.level)) "all" else levels(dd$g2))
    
    # if g2 specified numerically, check the value is ok, and then convert it to
    # character level anyway
    if (is.numeric(g2.level)) {
      if (as.integer(g2.level) != g2.level)
        warning(paste0("g2.level truncated to ", g2.level, "."))
      
      if (g2.level == 0) {
        g2.level <- "_ALL"
      } else if (g2.level == ng2 + 1) {
        g2.level <- "_MULTI"
      } else if (g2.level > ng2 + 1) {
        stop(paste("g2.level must be a number between 0 and", ng2 + 1))
      } else {
        g2.level <- g2l[g2.level]
      }
    }
    
    # separate function for drawing the matrix version
    if (g2.level == "_ALL") {
      df1 <- list(all = dd)
      g2.level <- NULL
    } else {
      if (g2.level == "_MULTI") {
        matrix.plot <- TRUE
      }                
      
      missing$g2 <- sum(is.na(dd$g2))
      df1 <- lapply(g2l,
                    function(l) {
                      dft <- subset(dd, dd$g2 == l)
                      dft[, colnames(dft) != "g2"]
                    })
      names(df1) <- g2l
    }
  } else {
    g2l <- "all"
    df1 <- list(all = dd)
  }
  
  
  # now, `df` is a list of data.frame of all levels of g2 (unless
  # g2.level = NULL/_ALL/0).  `missing` constains the number of
  # observations lost by subsetting g2 due to missing values of g2.
  
  if ("g1" %in% df.vs) {
    # take two methods of specifying g1.level (numeric or level names), and convert to a vector
    # of only character names to be plotted
    g1l <- levels(dd$g1)  # all levels of variable
    if (is.null(g1.level)) g1.level <- "_MULTI"
    
    if (is.numeric(g1.level)) {
      if (any(g1.level > length(levels(dd$g1)))) g1.level <- 0
      g1.level <- if (any(g1.level == 0)) "_MULTI" else levels(dd$g1)[g1.level]
    }
    
    if (any(g1.level == "_MULTI"))
      g1.level <- levels(dd$g1)
    
    # track missing values due to missingness in g1
    missing$g1 <- sum(is.na(dd$g1))
  } else {
    g1l <- "all"
    g1.level <- "all"
  }
  
  # this converts each data.frame in the list to a list of data
  # frames for all levels of g1
  df.list <- lapply(df1, function(df2) {
    df3 <- lapply(g1l, function(x) inzDataList(df2, x))
    names(df3) <- g1l
    df3
  })
  
  ## sum up all of the missing values
  w.df <-
    if (is.null(g2.level)) "all"
  else if (g2.level == "_MULTI") 1:length(df.list)
  else g2.level
  
  missing$x <- sum(sapply(df.list[w.df], function(df)
    sum(sapply(df, function(d) sum(is.na(d$x))))))
  if ("y" %in% df.vs)
    missing$y <- sum(sapply(df.list[w.df], function(df)
      sum(sapply(df, function(d) sum(is.na(d$y))))))
  
  class(df.list) <- "inz.freq"
  
  list(df = df.list, matrix = matrix.plot, missing = missing,
       g1.level = g1.level, g2.level = g2.level)
}




gSubset.inz.survey <- function(df, g1.level, g2.level, df.vs, missing) {
  # subset the data by g2 (keep everything, so xlims can be calculated)
  # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
  
  dd <- df$design$variables
  dd <- cbind(dd, df$data)
  
  vn <- as.list(df$varnames)
  
  matrix.plot <- FALSE
  g1 <- g2 <- NULL
  if ("g2" %in% names(df$varnames)) {
    g2 <- df$varnames["g2"]
    
    if (is.null(g2.level)) g2.level <- "_ALL"
    ng2 <- length(g2l <- if (is.null(g2.level)) "all" else levels(dd$g2))
    
    # if g2 specified numerically, check the value is ok, and then convert it to
    # character level anyway
    if (is.numeric(g2.level)) {
      if (as.integer(g2.level) != g2.level)
        warning(paste0("g2.level truncated to ", g2.level, "."))
      
      if (g2.level == 0) {
        g2.level <- "_ALL"
      } else if (g2.level == ng2 + 1) {
        g2.level <- "_MULTI"
      } else if (g2.level > ng2 + 1) {
        stop(paste("g2.level must be a number between 0 and", ng2 + 1))
      } else {
        g2.level <- g2l[g2.level]
      }
    }
    
    # separate function for drawing the matrix version
    if (g2.level == "_ALL") {
      df1 <- list(all = dd)
      g2.level <- NULL
    } else {
      if (g2.level == "_MULTI") {
        matrix.plot <- TRUE
      }                
      
      missing$g2 <- sum(is.na(dd[, g2]))
      df1 <- lapply(g2l,
                    function(l) {
                      dft <- subset(dd, dd$g2 == l)
                      dft[, colnames(dft) != "g2"]
                    })
      names(df1) <- g2l
    }
  } else {
    g2l <- "all"
    df1 <- list(all = dd)
  }
  
  # now, `df` is a list of data.frame of all levels of g2 (unless
  # g2.level = NULL/_ALL/0).  `missing` constains the number of
  # observations lost by subsetting g2 due to missing values of g2.
  
  if ("g1" %in% names(df$varnames)) {
    g1 <- df$varnames["g1"]
    # take two methods of specifying g1.level (numeric or level names), and convert to a vector
    # of only character names to be plotted
    g1l <- levels(df$data$g1)  # all levels of variable
    if (is.null(g1.level)) g1.level <- "_MULTI"
    
    if (is.numeric(g1.level)) {
      if (any(g1.level > length(levels(dd[, g1])))) g1.level <- 0
      g1.level <- if (any(g1.level == 0)) "_MULTI" else levels(df$data$g1)[g1.level]
    }
    
    if (any(g1.level == "_MULTI"))
      g1.level <- levels(df$data$g1)
    
    # track missing values due to missingness in g1
    missing$g1 <- sum(is.na(df$data$g1))
  } else {
    g1l <- "all"
    g1.level <- "all"
  }
  
  # this converts each data.frame in the list to a list of data
  # frames for all levels of g1
  
  if (any(c(g1, g2) %in% colnames(df$design$cluster)))
    df$design <- eval(parse(text = modifyCall(df$design$call, "ids", "~1")))
  oldcall <- df$design$call
  
  df.list <- lapply(df1, function(df2) {
    df3 <- lapply(g1l, function(x) {
      if (x != "all") {
        w <- df2$g1 == x
        dfnew <- df2[w & !is.na(w), , drop = FALSE]
      } else {
        dfnew <- df2
      }
      if (is.null(g1)) {
        dfo <- dfnew
      } else {
        dfo <- dfnew[, colnames(dfnew) != "g1", drop = FALSE]
      }
      
      if (nrow(dfo) > 1) {
        # turn it into a svydesign:
        return(eval(parse(text = modifyData(df$design$call, "dfo"))))
      } else if (nrow(dfo) == 1) {
        return(list(variables = dfo))
      } else {
        return(NULL)
      }
    })
    names(df3) <- g1l
    df3
  })
  
  ## sum up all of the missing values
  w.df <-
    if (is.null(g2.level)) "all"
  else if (g2.level == "_MULTI") 1:length(df.list)
  else g2.level
  
  missing$x <- sum(sapply(df.list[w.df], function(df)
    sum(sapply(df, function(d)
      if (!is.null(d))
        sum(is.na(d$variables$x))  else 0))))
  if ("y" %in% df.vs)
    missing$y <- sum(sapply(df.list[w.df], function(df)
      sum(sapply(df, function(d)
        if (!is.null(d))
          sum(is.na(d$variables$y)) else 0))))
  
  class(df.list) <- "inz.survey"
  
  list(df = df.list, matrix = matrix.plot, missing = missing,
       g1.level = g1.level, g2.level = g2.level)
}


modifyData <- function(oldcall, data) {
  args <- names(oldcall)
  vals <- as.character(oldcall)
  vals[args == "data"] <- data
  newcall <- paste0(vals[1], "(", paste(args[-1], vals[-1],
                                        sep = " = ", collapse = ", "), ")", sep = "")
  newcall
}

modifyCall <- function(oldcall, arg, val) {
  args <- names(oldcall)
  vals <- as.character(oldcall)
  vals[args == arg] <- val
  newcall <- paste0(vals[1], "(", paste(args[-1], vals[-1],
                                        sep = " = ", collapse = ", "), ")", sep = "")
  newcall
}






params <- function(x = NULL) {
  ## A suite of parameters for controlling behaviour
  if (!is.null(x)) {
    params()[[x]]
  } else {
    list(max.levels = 101)
  }
}




resetPlot <- function()
  grid.newpage()





stopPlot <- function(msg) {
  grid.newpage()
  pushViewport(viewport())
  grid.text(msg)
}




summary.inzdot <- function(object, des, ...) {
  ## Generate summary information:
  
  ## Produce a matrix of the required summary:
  toplot <- object$toplot
  
  
  if (is.null(des)) {    
    do.call(rbind, lapply(names(toplot), function(p) {
      x <- toplot[[p]]$x
      
      s <- suppressWarnings(
        c(min(x),
          quantile(x, 0.25),
          quantile(x, 0.5),
          quantile(x, 0.75),
          max(x),
          mean(x), sd(x), length(x))
      )
      s[!is.finite(s)] <- NA
      s
    })) -> mat
  } else {
    dv <- des$variables
    if ("y" %in% colnames(dv))
      mat <- cbind(tapply(dv$x, dv$y, min),
                   svyby(~x, ~y, des, svyquantile, quantiles = c(0.25, 0.5, 0.75), keep.var = FALSE)[, -1],
                   tapply(dv$x, dv$y, max),
                   svyby(~x, ~y, des, svymean)[, "x"],
                   sqrt(svyby(~x, ~y, des, svyvar)[, "x"]),
                   as.vector(table(dv$y)),
                   svyby(~x, ~y, des, svytotal)[, "x"])
    else
      mat <- cbind(min(dv$x, na.rm = TRUE),
                   svyquantile(~x, des, quantiles = c(0.25, 0.5, 0.75)),
                   max(dv$x),
                   svymean(~x, des)[["x"]],
                   sqrt(svyvar(~x, des)[["x"]]),
                   nrow(dv),
                   svytotal(~x, des)[["x"]])
  }
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, digits = 4)
  }), nrow = nrow(mat))
  
  ## Remove NA's and replace with an empty space
  mat[grep("NA", mat)] <- ""
  
  ## Text formatting to return a character vector - each row of matrix
  rns <- c("Min", "25%", "Median", "75%", "Max", "Mean", "SD", "Sample Size")
  if (!is.null(des)) rns <- c(rns, "Population Size")
  mat <- rbind(rns,  mat)
  colnames(mat) <- NULL
  
  if (length(toplot) > 1) {
    mat <- cbind(c("", names(toplot)), mat)
  }
  rownames(mat) <- NULL
  
  mat <- matrix(apply(mat, 2, function(col) {
    format(col, justify = "right")
  }), nrow = nrow(mat))
  
  mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
  mat
}
summary.inzhist <- function(object, des, ...)
  summary.inzdot(object, des, ...)



summary.inzbar <- function(object, des, ...) {
  tab <- round(object$tab)
  perc <- object$phat * 100
  
  is.survey <- !is.null(des)
  
  twoway <- length(dim(tab)) == 2
  if (twoway) {
    tab <- as.matrix(tab)
    
    perc <- as.matrix(perc)
    perc <- t(apply(perc, 1, function(p) {
      if (all(is.finite(p)))
        paste0(c(format(p, digits = 3), "100"), "%")
      else
        rep("", length(p) + 1)
    }))
    
    mat1 <- rbind(c(colnames(tab), "Row Total"),
                  cbind(tab, rowSums(tab)))
    mat1 <- cbind(c("", rownames(tab)), mat1)
    
    mat1 <- matrix(apply(mat1, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat1))
    
    
    mat2 <- rbind(c(colnames(tab), "Total", "Row N"),
                  cbind(perc, rowSums(tab)))
    mat2 <- cbind(c("", rownames(tab)), mat2)
    
    mat2 <- matrix(apply(mat2, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat2))
    
    c("Table of Counts:", "",
      apply(mat1, 1, function(x) paste0("   ", paste(x, collapse = "   "))),
      "",
      "Table of Percentages:", "",
      apply(mat2, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
    
  } else {
    mat <- rbind(c(names(tab), "Total"),
                 c(tab, sum(tab)),
                 paste0(c(format(perc, digits = 4), "100"), "%"))
    
    mat <- cbind(c("", "Count", "Percent"), mat)
    
    mat <- matrix(apply(mat, 2, function(col) {
      format(col, justify = "right")
    }), nrow = nrow(mat))
    
    mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    mat
  }
}



summary.inzscatter <- function(object, vn, des, ...) {
  x <- object$x
  y <- object$y
  trend <- object$trend
  
  is.survey <- !is.null(des)
  
  out <- character()
  if ("linear" %in% trend) {
    beta <- try({
      if (is.survey)
        signif(coef(svyglm(y ~ x, design = des)), 4)
      else
        signif(coef(lm(y ~ x)), 4)
    }, silent = TRUE)
    
    if (inherits(beta, "try-error"))
      out <- "Unable to fit linear trend."
    else
      out <- c(out,
               "Linear trend:", "",
               paste0("    ",
                      vn$y, " = ",
                      beta[1], " + ",
                      beta[2], " * ", vn$x),
               paste0("    Linear correlation: ",
                      if (is.survey)
                        round(cov2cor(as.matrix(svyvar(y~x, design = des)))[1,2], 2)
                      else
                        round(cor(x, y), 2)),
               "")
  }
  if ("quadratic" %in% trend) {
    beta <- try({
      if (is.survey)
        signif(coef(svyglm(y ~ x + I(x^2), design = des)), 4)
      else
        signif(coef(lm(y ~ x + I(x^2))), 4)
    }, silent = TRUE)
    
    if (inherits(beta, "try-error"))
      out <- "Unable to fit quadratic trend."
    else
      out <- c(out,
               "Quadratic trend:", "",
               paste0("    ",
                      vn$y, " = ",
                      beta[1], " + ",
                      beta[2], " * ", vn$x, " + ",
                      beta[3], " * ", vn$x, "^2"), "")
  }
  if ("cubic" %in% trend) {
    beta <- beta <- try({
      if (is.survey)
        signif(coef(svyglm(y ~ x + I(x^2) + I(x^3), design = des)), 4)
      else
        signif(coef(lm(y ~ x + I(x^2) + I(x^3))), 4)
    }, silent = TRUE)
    
    if (inherits(beta, "try-error"))
      out <- "Unable to fit linear trend."
    else
      out <- c(out,
               "Cubic trend:", "",
               paste0("    ",
                      vn$y, " = ",
                      beta[1], " + ",
                      beta[2], " * ", vn$x, " + ",
                      beta[3], " * ", vn$x, "^2 + ",
                      beta[4], " * ", vn$x, "^3"), "")
  }
  
  rank.cor <- cor(x, y, method = "spearman")
  out <- c(out,
           paste0("Rank correlation: ", sprintf("%.2f", rank.cor),
                  "  (using Spearman's Rank Correlation)"))
  
}






##' A general plotting function that automatically detects variable type and draws the
##' appropriate plot.
##' It also provides facilities to add inference information to plots, colour- and
##' size-by variables, and can handle survey data.
##'
##' Some details here ...
##'
##' @title iNZight Plot
##' @param x a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object
##' @param y a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object
##' @param g1 a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object. This variable acts as a subsetting variable.
##' @param g1.level the name (or numeric position) of the level of \code{g1} that will be
##' used instead of the entire data set
##' @param g2 a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object. This variable acts as a subsetting variable, similar to
##' \code{g1}
##' @param g2.level same as \code{g1.level}, however takes the additional value \code{"_MULTI"},
##' which produces a matrix of \code{g1} by \code{g2}
##' @param varnames a list of variable names, with the list named using the appropriate arguments
##' (i.e., \code{list(x = "height", g1 = "gender")})
##' @param colby the name of a variable (numeric or factor) to colour points by. In the
##' case of a numeric variable, a continuous colour scale is used, otherwise each level of
##' the factor is assigned a colour
##' @param sizeby the name of a (numeric) variable, which controls the size of points
##' @param locate variable to label points
##' @param locate.id id of points (row numbers) to label
##' @param locate.col the colour to locate points if a variable is not specified
##' @param locate.extreme \code{numeric}, the number of extreme points to label (using Mahalanobis' distance)
##' @param highlight \code{numeric} vector consisting of the row numbers/IDs of points to highlight
##' @param data the name of a data set
##' @param design the name of a survey object, obtained from the \code{survey} package
##' @param freq the name of a frequency variable if the data are frequencies
##' @param missing.info logical, if \code{TRUE}, information regarding missingness is
##' displayed in the plot
##' @param xlab the text for the x-label
##' @param ylab the text for the y-label
##' @param new logical, used for compatibility
##' @param inzpars allows specification of iNZight plotting parameters over multiple plots
##' @param layout.only logical, if \code{TRUE}, only the layout is drawn (useful if a
##' custom plot is to be drawn)
##' @param plot logical, if \code{FALSE}, the plot is not drawn (used by \code{summary})
##' @param xlim specify the x limits of the plot
##' @param ylim specify the y limits of the plot
##' @param zoombars numeric, length 2; when drawing a bar plot, if the number of bars is too large,
##' the user can specify a subset. The first value is the starting point (1 is the first bar, etc),
##' while the second number is the number of bars to show.
##' @param df compatibility argument
##' @param env compatibility argument
##' @param ... additional arguments, see \code{inzpar}
##' @return An \code{inzightplotoutput} object, which contains the information displayed
##' in the plot
##'
##' @import grid boot s20x survey quantreg survey SparseM hexbin iNZightMR
##' @author tell029
##' @export
iNZightPlot_change <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                        g2 = NULL, g2.level = NULL, varnames = list(),
                        colby = NULL, sizeby = NULL,
                        locate = NULL, locate.id = NULL, locate.col = NULL,
                        locate.extreme = NULL, highlight = NULL,
                        data = NULL, design = NULL, freq = NULL,
                        missing.info = TRUE,
                        xlab = varnames$x, ylab = varnames$y,
                        new = TRUE,  # compatibility arguments
                        inzpars = inzpar(), layout.only = FALSE, plot = TRUE,
                        xlim = NULL, ylim = NULL, zoombars = NULL,
                        df, env = parent.frame(), ...) {
  
  # ------------------------------------------------------------------------------------ #
  #   iNZightPlots v2.0, written by Tom Elliott (2014, University of Auckland)
  #
  # This function will `attempt` to take a large variety of data configurations and
  # attempt to make a suitable plot. It can take into account the data structure (for
  # example, frequency or survey data), and make use of these in the final plot. It also
  # contains a suite of additional options such a trend lines and inference information
  # which can also be added to plots as required.
  #
  # A Summary and Inference method will be associated with the output of this file, so
  # users can easily get numerical information about any particular plot they produce. The
  # inference information will be either theoretically or bootstrap based.
  #
  # ------------------------------------------------------------------------------------ #
  # ++++++++ FOR (future) DEVELOPERS ++++++++
  #
  # First of all, welcome to the iNZight team!3
  # Second, have fun reading through all the code :D
  #
  # +++ How iNZightPlots works +++
  #
  # I'll write this later ...
  #
  # Have fun coding!
  # ------------------------------------------------------------------------------------ #
  #               Original author: Tom Elliott <tell029@aucklanduni.ac.nz>               #
  # ------------------------------------------------------------------------------------ #
  
  ########################################################################################
  ########################################################################################
  
  # ------------------------------------------------------------------------------------ #
  # 1. The data step
  # ----------------
  # grab the arguments and the data frame is supplied:
  m <- match.call(expand.dots = FALSE)
  
  ## getSummary and other wrappers will pass an inz.data object
  if (missing(df)) {
    if (!is.null(design)) {
      md <- eval(m$design, env)
    } else {
      md <- eval(m$data, env)
    }
    
    ## we now want to create a data object which contains *ALL* of the necessary
    ## information, including survey design, or frequency information:
    df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level, env = env)
  }
  
  total.missing <- sum(apply(df$data, 1, function(x) any(is.na(x))))
  total.obs <- nrow(df$data)
  
  ## df will have a class: inz.simple, inz.freq, inz.survey
  ## each of these classes will have appropriate methods for extracting the information
  
  varnames <- as.list(df$varnames)
  
  ## Any varnames supplied that AREN'T needed must be removed, otherwise errors:
  nullVars <- sapply(as.list(m)[names(varnames)], is.null)
  varnames[nullVars] <- NULL
  
  ## In some cases, arguments are removed and must be continued on other error
  ## (e.g., too many factor levels, etc)
  varnames[!names(varnames) %in% colnames(df$data)] <- NULL
  vartypes <- lapply(df$data[, names(varnames), drop = FALSE],
                     function(x) ifelse(is.factor(x), "factor", "numeric"))
  names(vartypes) <- unlist(varnames)
  df.vs <- colnames(df$data)
  missing <- list()  # a container to save missing value information
  
  ## ensure it matches what comes back from inzDataframe()
  g.level <- df$glevels
  g1.level <- g.level$g1.level
  g2.level <- g.level$g2.level
  
  # do some type checks
  xfact <- is.factor(df$data$x)
  ynull <- ! "y" %in% df.vs
  yfact <- if (ynull) NULL else is.factor(df$data$y)
  
  ## check the number of levels for a barchart:
  if (!is.null(zoombars))
    if (zoombars[2] == 0)
      zoombars <- NULL
  
  if (xfact) {
    if (ynull) {
      if (length(levels(df$data$x)) > params("max.levels") & is.null(zoombars)) {
        msg <- paste0("Too many levels in ", varnames$x,
                      " to draw a barchart.\n",
                      "(", varnames$x, " has ",
                      length(levels(df$data$x)), " levels.)")
        stopPlot(msg)
        plot <- FALSE
        #                return(invisible(NULL))
      }
    } else if (yfact) {
      if (length(levels(df$data$x)) * length(levels(df$data$y)) > params("max.levels") & is.null(zoombars)) {
        msg <- paste0("Too many levels in ", varnames$x, " and ",
                      varnames$y, " to draw a barchart.\n",
                      "(", varnames$x, " has ",
                      length(levels(df$data$x)), " levels, ",
                      varnames$y, " has ", length(levels(df$data$y)),
                      "levels.)")
        stopPlot("Too many levels in x and y to draw a barchart.")
        plot <- FALSE
        #                return(invisible(NULL))
      }
    }
  }
  
  # subset the data by g2 (keep everything, so xlims can be calculated)
  # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
  dfsub <- gSubset(df, g1.level, g2.level, df.vs, missing)
  matrix.plot <- dfsub$matrix
  missing <- dfsub$missing
  g1.level <- dfsub$g1.level
  g2.level <- dfsub$g2.level
  
  df.list <- dfsub$df
  
  # now, everything simply gets applied to the list of dataframes to
  # generate the necessary plots
  
  # ------------------------------------------------------------------------------------ #
  # 2. The plot setup step
  # ----------------------
  
  # The aim of this step is to produce a list of things to plot, each element pertaining to a
  # level of g1 and g2, containing the necessary information.
  
  dots <- list(...)  # capture the additional arguments
  opts <- inzpars
  wopt <- names(dots) %in% names(opts)  # which additional settings have been specified
  opts <- modifyList(opts, dots[wopt])
  
  if (!xfact) xx <- df$data$x
  if (!ynull) if (!yfact) yy <- df$data$y
  
  xattr <- list(class = class(df), v = colnames(df$data), varnames = as.list(df$varnames),
                vartypes = structure(vartypes, .Names = names(varnames)))
  
  ## HERE IS THE SWTICH FOR CHANGING FROM DIFFERENT TYPES OF DOT PLOT ZOOMING
  if (!xfact)
    #if (!is.null(xlim))
    #    xattr$xrange <- xlim
    #else
    xattr$xrange <- range(xx[is.finite(xx)])
  if (!ynull) if (!yfact) xattr$yrange <- range(yy[is.finite(yy)])
  if (!is.null(df$max.freq))
    xattr$max.freq <- df$max.freq
  if (!is.null(locate.extreme)) xattr$nextreme <- locate.extreme
  if (!is.null(zoombars)) xattr$zoom <- zoombars
  
  if (opts$matchplots) {
    # this is the case where the data is subset by g1/g2, but we want the plots to be the same
    # across all levels
    
    # we just need to go through all plots and test if they should be LARGESAMPLE or not:
    if (is.null(opts$largesample)) {
      sample.sizes <- do.call(c, lapply(df.list, function(df) sapply(df, function(a) {
        o <- nrow(a)
        if (is.null(o)) 0 else o
      })))
      smallest.sample <- min(sample.sizes, na.rm = TRUE)
      largest.sample <- max(sample.sizes, na.rm = TRUE)
      
      ## grab parameters
      N.LARGE <- opts$large.sample.size
      N.LIMITS <- opts$match.limits
      
      ## Do we need different plots?
      if (smallest.sample > N.LARGE) {
        ## all sample sizes are big enough
        opts$largesample <- TRUE
      } else if (largest.sample < N.LARGE) {
        ## all sample sizes are small enough
        opts$largesample <- FALSE
      } else if (smallest.sample > N.LIMITS[1]) {
        ## the smallest sample is bigger than the lower limit
        opts$largesample <- TRUE
      } else if (largest.sample < N.LIMITS[2]) {
        ## the largest sample is smaller than the upper limit
        opts$largesample <- FALSE
      } else {
        ## sample sizes range outside both upper and lower limits
        opts$largesample <- as.logical(round(mean(sample.sizes > N.LARGE)))
      }
      
      #            maxRow <- max(sapply(df.list, function(df) sapply(df, nrow)))
      #            opts$largesample <- maxRow > opts$large.sample.size  # essentially override the
      #                                                                 # largesample argument
    }
  }
  
  
  
  ## if creating a dot plot, must figure out the size of a symbol:
  itsADotplot <- FALSE
  
  if (ynull & !xfact)
    itsADotplot <- TRUE
    
  else if (!ynull) {
    if ((!xfact & yfact) | (xfact & !yfact))
      itsADotplot <- TRUE
      
  }
  
  if (itsADotplot)
    if (opts$plottype != "dot")
      if (opts$plottype != "default" | (opts$plottype == "default" & opts$largesample))
        itsADotplot <- FALSE
  
  #if (itsADotplot) {
  if(itsADotplot) {
    ## m2 <- match.call(expand.dots = TRUE)
    ## m2$plottype <- "hist"
    ## m2$layout.only <- TRUE
    ## m2$inference.type <- NULL
    ## m2$inference.par <- NULL
    
    ## X and Y axis limits:
    #if (!is.null(xlim)) {
    #    true.xr <- range(lapply(df.list, function(df)
    #        lapply(df, function(d) range(d$x, finite = TRUE))), finite = TRUE)
    #    
    #    opts$cex.dotpt <- opts$cex.dotpt * diff(true.xr) / diff(xlim)
    #    if (xlim[1] > true.xr[1] | xlim[2] < true.xr[2]) {
    #        opts$boxplot <- FALSE
    #        m2$boxplot <- FALSE
    #    }
    #}
    
    
    ## we will now attempt something slightly complicated/computationally dumb
    ## HOWEVER: it will give us pretty dotplots
    #        F <- "~/Desktop/file.pdf"#tempfile()
    #        S <- dev.size("px")
    #        pdf(F, width = S[1], height = S[2])  # create a NULL device with same dimensions
    
    
    ## ## Some complicated 'recursion' which we only want to happen when the plot dimensions change.
    ## if (!is.null(cur.symbol.width)) {
    ##     ## Check if the size of the points for the previous plot were sent,
    ##     ## and if so, check they are the same as current:
    
    ##     widths.match <- round(cur.symbol.width, 5) ==
    ##         round(convertWidth(unit(opts$cex.dotpt, "char"), "native", valueOnly = TRUE), 5)
    ## } else {
    ##     widths.match <- FALSE
    ## }
    
    ## print(widths.match)
    
    ## if (!widths.match) {
    ##     ## Only do recursion if needed!
    
    ##     p <- eval(m2, env)
    ## }
    
    
    ## ## This only happens on instrucion from the user (or, iNZight GUI):
    ## print(redraw.dotplots)
    ## if (redraw.dotplots) {
    ##     m2 <- match.call(expand.dots = TRUE)
    ##     m2$plottype <- "hist"
    ##     m2$layout.only <- TRUE
    ##     m2$inference.type <- NULL
    ##     m2$inference.par <- NULL
    
    ##     p <- eval(m2, env)
    ## }
    
    
      xattr$symbol.width <- convertWidth(unit(opts$cex.dotpt, "char"),
                                         "native", valueOnly = TRUE)
    
    
    
    

    
    #        dev.off()
    #        unlink(F)  ## delete the temp file
    
    ## sort out bin sizing:
    allX <- if(xfact) df$data$y else df$data$x
    allX <- allX[!is.na(allX)]
    diffs <- diff(sort(allX))
    diffs <- diffs[diffs > 0]
    mdiff <- min(diffs)
    fdiff <- diffs / mdiff
    isDiscrete <- all(round(fdiff) == fdiff)
    xr <- diff(range(allX, na.rm = TRUE))
    mult.width <- ifelse(isDiscrete, 1, 1.2)
    
    xattr$dotplotstuff <- list(mdiff = mdiff,
                               xr = xr, isDiscrete = isDiscrete,
                               mult.width = mult.width)
  }
  
  
  ## createPlot - uses various things such as "grobWidth" which causes a new device to open
  ## so create a NULL device and delete it afterwards ...
  if (plot) {
    ## --- The Main Viewport: this one is simply the canvas, and global CEX value
    dd <- dev.flush(dev.flush())  # flush everything ...
    
    dev.hold()
    grid.newpage()
    pushViewport(viewport(gp = gpar(cex = opts$cex), name = "container"))
    grid.rect(gp = gpar(fill = opts$bg, col = opts$bg))
  } 
  
  ##### AND ANOTHER METHOD FOR SWITCHING BETWEEN DOTPLOTS ZOOMS
  #    if (!is.null(xlim)) {
  #        opts$boxplot <- FALSE
  #        xattr$trimX <- xlim
  #    }
  
  plot.list <- lapply(df.list, function(df)
    lapply(df, createPlot, opts, xattr))
  
  
  
  xlim.raw <- range(sapply(plot.list, function(x) sapply(x, function(y) y$xlim)), finite = TRUE)
  ylim.raw <- range(sapply(plot.list, function(x) sapply(x, function(y) y$ylim)), finite = TRUE)
  
  if (is.null(xlim) | class(plot.list[[1]][[1]]) == "inzbar") 
    xlim <- xlim.raw
  
  #else if (itsADotplot) {
  #    ## Set up new ylimits
  #    ylim <- range(0, sapply(plot.list, function(x) sapply(x, function(y) sapply(y$toplot, function(FF)
  #        range(FF$y[FF$x > min(xlim) & FF$x < max(xlim)], finite = TRUE)))), finite = TRUE)
  #}
  if (is.null(ylim) | class(plot.list[[1]][[1]]) == "inzbar")
    ylim <- ylim.raw
  
  
  
  TYPE <- gsub("inz", "", class(plot.list[[1]][[1]]))
  if (!TYPE %in% c("bar")) xlim <- extendrange(xlim)
  ylim <-
    if (TYPE %in% c("scatter", "grid", "hex")) extendrange(ylim)
  else c(0, extendrange(ylim)[2])
  
  maxcnt <- NULL
  if (TYPE %in% c("grid", "hex")) {
    # if there is a `counts` need to get the max:
    maxcnt <- switch(TYPE,
                     "grid" = {
                       warning("Frequency density not constant scale across multiple plots yet.")
                     }, "hex" = {
                       max(sapply(plot.list, function(x) sapply(x, function(y) {
                         if (class(y) == "inzhex")
                           max(y$hex@count, na.rm = TRUE)
                         else 0
                       })))
                     })
  } else if (TYPE %in% c("dot", "hist")) {
    maxcnt <- ylim[2]#.raw[2]
  }
  
  if (class(plot.list[[1]][[1]]) %in% c("inzdot", "inzhist")) {
    if (class(plot.list[[1]][[1]]) == "inzhist") {
      nOutofview <- 0
    } else {
      nOutofview <-
        sum(sapply(plot.list, function(x) sapply(x, function(y) sapply(y$toplot, function(z) 
          sum(z$x < min(xlim) | z$x > max(xlim))))))
    }
  } else if (class(plot.list[[1]][[1]]) != "inzbar") {
    nOutofview <- sum(sapply(plot.list, function(x) sapply(x, function(z)
      sum(z$x < min(xlim) | z$x > max(xlim) | z$y < min(ylim) | z$y > max(ylim)))))
  } else {
    nOutofview <- 0
  }
  
  if (is.numeric(df$data$colby))
    opts$trend.by <- FALSE
  
  # Set up the plot layout
  
  if (plot) {
    PAGE.height <- convertHeight(current.viewport()$height, "in", TRUE)  # essentially the height of the window
    
    ## --- there will be some fancy stuff here designing and implementing a grid which adds titles,
    ## labels, and optionally legends
    
    ## --- first, need to make all of the labels/legends/etc:
    VT <- vartypes
    names(VT) <- names(varnames)
    
    if (all(c("x", "y") %in% names(VT))) {
      ## switch X/Y for dotplots
      
      if (VT$y == "numeric" & VT$x == "factor") {
        xn <- varnames$y
        varnames$y <- varnames$x
        varnames$x <- xn
        VT$x <- "numeric"
        VT$y <- "factor"
        
        my <- missing$y
        missing$y <- missing$x
        missing$x <- my
      }
    }
    
    if (is.null(xlab))
      xlab <- varnames$x
    if (is.null(ylab))
      ylab <- varnames$y
    
    titles <- list()
    titles$main <-
      if (!is.null(dots$main)) dots$main
    else makeTitle(varnames, VT, g1.level, g2.level)
    titles$xlab <- xlab
    if (!ynull) {
      titles$ylab <-
        if (xfact & yfact) "Proportion (%)" else ylab
    } else if (xfact) {
      titles$ylab <- "Proportion (%)"
    }
    if ("colby" %in% df.vs) titles$legend <- varnames$colby
    
    
    ## plot.list still contains all the levels of g1 that wont be plotted - for axis scaling etc
    ## so figure this one out somehow ...
    ng1 <- ifelse("g1" %in% names(df$data), length(g1.level), 1)
    ng2 <- ifelse("g2" %in% names(df$data), ifelse(matrix.plot,
                                                   ifelse(g2.level == "_MULTI",
                                                          length(plot.list), length(g2.level)), 1), 1)
    N <- ng1 * ng2  # length(plot.list) * length(g1.level)
    NN <- if (matrix.plot) length(plot.list) * length(plot.list[[1]]) else N
    multi.cex <- max(1.2 * sqrt(sqrt(NN) / NN), 0.5)  # this has absolutely no theoretical reasoning,
    # it just does a reasonably acceptable job (:
    
    
    # --- WIDTHS of various things
    # first we need to know HOW WIDE the main viewport is, and then
    # split the title text into the appropriate number of lines,
    # then calcualate the height of it.
    VPcontainer.width <- convertWidth(unit(1, "npc"), "in", TRUE)
    main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
    MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    MAIN.lnheight <- convertWidth(grobHeight(main.grob), "in", TRUE)
    if (MAIN.width > 0.9 * VPcontainer.width) {
      titles$main <- gsub(",", ",\n", titles$main)
      main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
      MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    if (MAIN.width > 0.9 * VPcontainer.width) {
      titles$main <- gsub("subset", "\nsubset", titles$main)
      main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
      MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    if (MAIN.width > 0.9 * VPcontainer.width) {
      titles$main <- gsub(" (size prop", "\n (size prop", titles$main, fixed = TRUE)
      main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
      MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    MAIN.height <- convertHeight(grobHeight(main.grob), "in", TRUE) + MAIN.lnheight
    
    # -- xaxis labels
    xlab.grob <- textGrob(titles$xlab, y = unit(0.6, "lines"),
                          gp = gpar(cex = opts$cex.lab))
    XLAB.height <- convertHeight(grobHeight(xlab.grob), "in", TRUE) * 3
    # -- yaxis labels
    if (!is.null(titles$ylab)) {
      ylab.grob <- textGrob(titles$ylab, x = unit(0.6, "lines"),
                            rot = 90, gp = gpar(cex = opts$cex.lab))
      YLAB.width <- convertWidth(grobWidth(ylab.grob), "in", TRUE) * 3
    } else {
      YLAB.width <- 0
    }
    
    ## -- xaxis marks
    XAX.height <- convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis
    
    ## -- yaxis marks
    YAX.width <- if (TYPE %in% c("dot", "hist") & !ynull & !opts$internal.labels) {
      ## need to grab the factoring variable -> might be x OR y
      yf <- if (is.factor(df$data$y)) df$data$y else df$data$x
      yl <- levels(yf)
      yWidths <- sapply(yl, function(L)
        convertWidth(grobWidth(
          textGrob(L, gp = gpar(cex = opts$cex.axis * multi.cex))
        ), "in", TRUE))
      max(yWidths)
    } else 0
    
    YAX.default.width <- convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis
    YAX.width <- YAX.width + YAX.default.width
    
    ## -- legend(s)
    barplot <- TYPE == "bar"
    leg.grob1 <- leg.grob2 <- leg.grob3 <- NULL
    cex.mult = ifelse("g1" %in% df.vs, 1,
                      ifelse("g1.level" %in% df.vs,
                             ifelse(length(levels(df$g1.level)) >= 6, 0.7, 1), 1))
    
    
    xnum <- !xfact
    yfact <- if (ynull) FALSE else yfact
    ynum <- if (ynull) FALSE else !yfact
    
    col.args <- list(missing = opts$col.missing)
    if ("colby" %in% names(varnames) &
        (TYPE %in% c("dot", "scatter") ||
         (TYPE %in% c("grid", "hex") & !is.null(opts$trend) & opts$trend.by) ||
         (TYPE == "bar" & ynull & is.factor(df$data$colby)))) {
      
      if (is.factor(df$data$colby)) {
        nby <- length(levels(as.factor(df$data$colby)))
        if (length(opts$col.pt) >= nby) {
          ptcol <- opts$col.pt[1:nby]
        } else {
          ptcol <- genCols(nby)
        }
        
        if (TYPE != "bar")
          misscol <- any(sapply(plot.list, function(x) sapply(x, function(y) y$nacol)))
        else
          misscol <- FALSE
        
        leg.grob1 <- drawLegend(f.levels <- levels(as.factor(df$data$colby)), col = ptcol,
                                pch = ifelse(barplot, 22, opts$pch),
                                title = varnames$colby, any.missing = misscol, opts = opts)
        if (misscol) {
          ptcol <- c(ptcol, opts$col.missing)
          f.levels <- c(f.levels, "missing")
        }
        col.args$f.cols <- structure(ptcol, .Names = f.levels)
      } else {
        misscol <- any(sapply(plot.list, function(x) sapply(x, function(y) y$nacol)))
        leg.grobL <- drawContLegend(df$data$colby, title = varnames$colby,
                                    height = 0.4 * PAGE.height, cex.mult = cex.mult,
                                    any.missing = misscol, opts = opts)
        leg.grob1 <- leg.grobL$fg
        col.args$n.range <- range(df$data$colby, na.rm = TRUE)
        col.args$n.cols <- leg.grobL$n.cols
      }
    } else if (xfact & yfact) {
      nby <- length(levels(as.factor(df$data$y)))
      if (length(opts$col.pt) >= nby) {
        barcol <- opts$col.pt[1:nby]
      } else {
        barcol <- genCols(nby)
      }
      
      leg.grob1 <- drawLegend(levels(as.factor(df$data$y)), col = barcol, pch = 22,
                              title = varnames$y, opts = opts)
      col.args$b.cols <- barcol
    }
    
    if (!is.null(locate.col)) col.args$locate.col <- locate.col
    
    if ("sizeby" %in% names(varnames) & TYPE %in% c("scatter")) {
      misssize <- any(sapply(plot.list, function(x) sapply(x, function(x2) x2$nasize)))
      if (misssize) {
        misstext <- paste0("missing ", varnames$sizeby)
        leg.grob2 <- drawLegend(misstext, col = "grey50", pch = 4,
                                cex.mult = cex.mult * 0.8, opts = opts)
      }
    }
    
    if (xnum & ynum) {
      df.lens <- lapply(plot.list, function(a) {
        mm <- sapply(a, function(b)
          sum(apply(cbind(b$x, b$y), 1, function(c) all(!is.na(c)))))
        A <- a[[which.max(mm)]]
        cbind(A$x, A$y)
      })
      ddd <- df.lens[[which.max(sapply(df.lens, nrow))]]
      leg.grob3 <- drawLinesLegend(ddd, opts = opts, cex.mult = cex.mult * 0.8)
    }
    
    hgts <- numeric(3)
    wdth <- 0
    
    if (!is.null(leg.grob1)) {
      hgts[1] <- convertHeight(grobHeight(leg.grob1), "in", TRUE)
      wdth <- max(wdth, convertWidth(grobWidth(leg.grob1), "in", TRUE))
    }
    if (!is.null(leg.grob2)) {
      hgts[2] <- convertHeight(grobHeight(leg.grob2), "in", TRUE)
      wdth <- max(wdth, convertWidth(grobWidth(leg.grob2), "in", TRUE))
    }
    if (!is.null(leg.grob3)) {
      hgts[3] <- convertHeight(grobHeight(leg.grob3), "in", TRUE)
      wdth <- max(wdth, convertWidth(grobWidth(leg.grob3), "in", TRUE))
    }
    
    ## --- Figure out a subtitle for the plot:
    
    if (!is.null(dots$subtitle)) {
      SUB <- textGrob(dots$subtitle, gp = gpar(cex = opts$cex.text * 0.8))
    } else {
      subtitle <- ""
      if (missing.info & length(missing) > 0) {
        POS.missing <- missing[missing != 0]
        names(POS.missing) <- unlist(varnames[match(names(POS.missing), names(varnames))])
        missinfo <-
          if (length(missing) > 1) paste0(" (", paste0(POS.missing, " in ", names(POS.missing),
                                                       collapse = ", "), ")")
        else ""
        
        if (total.missing > 0) {
          subtitle <- paste0(total.missing, " missing values", missinfo)
        }
      }
      
      if (nOutofview > 0) {
        subtitle <- ifelse(subtitle == "", "", paste0(subtitle, " | "))
        subtitle <- paste0(subtitle, nOutofview, " points out of view")
      } else if (!is.null(zoombars)) {
        subtitle <- ifelse(subtitle == "", "", paste0(subtitle, " | "))
        subtitle <- paste0(subtitle, zoombars[2], " out of ", length(levels(df$data$x)),
                           " levels of ", varnames$x, " visible")
      }
      
      if (subtitle == "")
        SUB <- NULL
      else
        SUB <- textGrob(subtitle, gp = gpar(cex = opts$cex.text * 0.8))
    }
    
    
    ## --- CREATE the main LAYOUT for the titles + main plot window
    MAIN.hgt <- unit(MAIN.height, "in")
    XAX.hgt <- unit(XAX.height, "in")
    XLAB.hgt <- unit(XLAB.height, "in")
    PLOT.hgt <- unit(1, "null")
    SUB.hgt <- if (is.null(SUB)) unit(0, "null") else convertUnit(grobHeight(SUB) * 2, "in")
    
    YLAB.wd <- unit(YLAB.width, "in")
    YAX.wd <- unit(YAX.width, "in")
    PLOT.wd <- unit(1, "null")
    LEG.wd <-
      if (wdth > 0) unit(wdth, "in") + unit(1, "char")
    else unit(0, "null")
    
    TOPlayout <- grid.layout(nrow = 6, ncol = 5,
                             heights = unit.c(MAIN.hgt, XAX.hgt, PLOT.hgt,
                                              XAX.hgt, XLAB.hgt, SUB.hgt),
                             widths = unit.c(YLAB.wd, YAX.wd, PLOT.wd, if (TYPE %in% c("scatter", "grid", "hex")) YAX.wd else unit(0.5, "in"), LEG.wd))
    
    ## Send the layout to the plot window
    pushViewport(viewport(layout = TOPlayout, name = "VP:TOPlayout"))
    
    ## Sort out XAX height:
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
    plotWidth <- convertWidth(current.viewport()$width, 'in', TRUE)
    upViewport()
    
    if (TYPE == "bar") {
      ## If the labels are too wide, we rotate them (and shrink slightly)
      x.lev <- levels(df$data$x)
      nLabs <- length(x.lev)
      maxWd <- 0.8 * plotWidth / nLabs
      rot <- any(sapply(x.lev, function(l)
        convertWidth(grobWidth(textGrob(l, gp = gpar(cex = opts$cex.axis))),
                     "in", TRUE) > maxWd))
      opts$rot <- rot
      
      if (rot) {
        ## Unable to update the viewport, so just recreate it:
        XAXht <- drawAxes(df$data$x, which = "x", main = TRUE, label = TRUE, opts,
                          heightOnly = TRUE, layout.only = layout.only)
        XAX.hgt2 <- convertWidth(XAXht, "in")
        
        ## destroy the old one
        popViewport()
        TOPlayout <- grid.layout(nrow = 6, ncol = 5,
                                 heights = unit.c(MAIN.hgt, XAX.hgt, PLOT.hgt,
                                                  XAX.hgt2, XLAB.hgt, SUB.hgt),
                                 widths = unit.c(YLAB.wd, YAX.wd, PLOT.wd, YAX.wd, LEG.wd))
        
        ## Send the layout to the plot window
        pushViewport(viewport(layout = TOPlayout, name = "VP:TOPlayout"))
      }
    }
    
    ## place the title
    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(main.grob)
    
    ## place axis labels
    if (!is.null(titles$ylab)) {
      seekViewport("VP:TOPlayout")
      pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
      grid.draw(ylab.grob)
    }
    seekViewport("VP:TOPlayout")
    pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 3))
    grid.draw(xlab.grob)
    
    ## place the legend
    if (wdth > 0) {
      seekViewport("VP:TOPlayout")
      pushViewport(viewport(layout.pos.col = 5, layout.pos.row = 3))
      leg.layout <- grid.layout(3, heights = unit(hgts, "in"))
      pushViewport(viewport(layout = leg.layout, name = "VP:LEGlayout"))
      
      if (hgts[1] > 0) {
        seekViewport("VP:LEGlayout")
        pushViewport(viewport(layout.pos.row = 1))
        grid.draw(leg.grob1)
      }
      if (hgts[2] > 0) {
        seekViewport("VP:LEGlayout")
        pushViewport(viewport(layout.pos.row = 2))
        grid.draw(leg.grob2)
      }
      if (hgts[3] > 0) {
        seekViewport("VP:LEGlayout")
        pushViewport(viewport(layout.pos.row = 3))
        grid.draw(leg.grob3)
      }
    }
    
    ## --- next, it will break the plot into subregions for g1 (unless theres only one, then it
    ## wont)
    
    ## break up plot list
    if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
    if (!matrix.plot & !is.null(g2.level)) {
      plot.list <- plot.list[g2.level]
    }
    
    plot.list <- lapply(plot.list, function(x) x[g1.level])
    
    ## and subtitle
    if (!is.null(SUB)) {
      seekViewport("VP:TOPlayout")
      pushViewport(viewport(layout.pos.row = 6, layout.pos.col = 3))
      grid.draw(SUB)
    }
    
    ## create a layout
    if (matrix.plot) {
      nr <- length(g2.level)
      nc <- length(g1.level)
    } else {
      dim1 <- floor(sqrt(N))
      dim2 <- ceiling(N / dim1)
      
      if (dev.size()[1] < dev.size()[2]) {
        nr <- dim2
        nc <- dim1
      } else {
        nr <- dim1
        nc <- dim2
      }
    }
    
    ## if the plots are DOTPLOTS or BARPLOTS, then leave a little bit of space between each
    ## we will need to add a small amount of space between the columns of the layout
    hspace <- ifelse(TYPE %in% c("scatter", "grid", "hex"), 0, 0.01)
    wds <- rep(unit.c(unit(hspace, "npc"), unit(1, "null")), nc)[-1]
    
    subt <- textGrob("dummy text", gp = gpar(cex = opts$cex.lab, fontface = "bold"))
    sub.hgt <- unit(convertHeight(grobHeight(subt), "in", TRUE) * 1.2, "in")
    vspace <- if (matrix.plot) sub.hgt else unit(0, "in")
    hgts <- rep(unit.c(vspace, unit(1, "null")), nr)
    
    
    PLOTlayout <- grid.layout(nrow = length(hgts), ncol = length(wds),
                              heights = hgts, widths = wds)
    seekViewport("VP:TOPlayout")
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
    pushViewport(viewport(layout = PLOTlayout, name = "VP:PLOTlayout"))
    
    ## --- within each of these regions, we simply plot!
    ax.gp <- gpar(cex = opts$cex.axis)
    
    ## --- START from the BOTTOM and work UP; LEFT and work RIGHT (mainly makes sense for continuous
    ## grouping variables)
    g1id <- 1  # keep track of plot levels
    g2id <- 1
    NG2 <- length(plot.list)
    NG1 <- length(plot.list[[1]])
    
    if (xfact & ynum) {
      X <- df$data$y
      Y <- df$data$x
    } else {
      X <- df$data$x
      Y <- df$data$y
    }
    
    for (r in nr:1) {
      R <- r * 2  # skip the gaps between rows
      if (matrix.plot) {
        ## add that little thingy
        seekViewport("VP:PLOTlayout")
        pushViewport(viewport(layout.pos.row = R - 1,
                              gp = gpar(cex = multi.cex, fontface = "bold")))
        grid.rect(gp = gpar(fill = "lightblue"))
        grid.text(paste(varnames$g2, "=", g2.level[g2id]), gp = gpar(cex = opts$cex.lab, fontface = "bold"))
      }
      
      for (c in 1:nc) {
        if (g2id > NG2) next()
        C <- c * 2 - 1
        
        ## This is necessary to delete the "old" viewport so we can create a new one
        ## of the same name, but retain it long enough to use it for drawing the axes
        if (TYPE %in% c("dot", "hist") & !layout.only) {
          vp2rm <- try(switch(TYPE,
                              "dot" = {
                                seekViewport("VP:dotplot-levels")
                                popViewport()
                              }, "hist" = {
                                seekViewport("VP:histplot-levels")
                                popViewport()
                              }), TRUE)
        }
        
        seekViewport("VP:PLOTlayout")
        pushViewport(viewport(layout.pos.row = R, layout.pos.col = C,
                              xscale = xlim, yscale = ylim,
                              gp = gpar(cex = multi.cex)))
        grid.rect(gp = gpar(fill = "transparent"))
        
        subt <- g1.level[g1id]
        
        ## calculate the height of the subtitle if it is specified
        p.title <- if (subt == "all") NULL else subt
        hgt <- unit.c(
          if (!is.null(p.title)) {
            subt <- textGrob(p.title, gp = gpar(cex = opts$cex.lab, fontface = "bold"))
            if (matrix.plot)
              sub.hgt
            else
              unit(convertHeight(grobHeight(subt), "in", TRUE) * 2, "in")
          } else {
            unit(0, "null")
          },
          unit(1, "null"))
        pushViewport(viewport(layout = grid.layout(2, 1, heights = hgt)))
        
        if (!is.null(p.title)) {
          pushViewport(viewport(layout.pos.row = 1))
          grid.rect(gp = gpar(fill = opts$col.sub))
          grid.draw(subt)
          upViewport()
        }
        
        ## I found "VP:locate.these.points" so far is just using here and no other
        ## depencies so I think giving the its an uniqe name would be a good idea here.
        nameVP <- if (NG1 == 1 && NG2 == 1) "VP:locate.these.points" else paste0("VP:locate.these.points", g2id, g1id)
        pushViewport(viewport(layout.pos.row = 2, xscale = xlim, yscale = ylim, clip = "on",
                              name = nameVP))
        ##pushViewport(viewport(layout.pos.row = 2, xscale = xlim, yscale = ylim, clip = "on",
        ##                      name = "VP:locate.these.points"))
        if (!layout.only) {
          plot(plot.list[[g2id]][[g1id]], gen =
                 list(opts = opts, mcex = multi.cex, col.args = col.args,
                      maxcount = maxcnt))
        }
        upViewport()
        
        
        ## add the appropriate axes:
        ## Decide which axes to plot:
        
        ## -------------
        ## For dotplots + histograms: the axis are at the bottom of every column, and on the far
        ## left
        ##
        ## For scatterplots + gridplots + hexplots: the axis alternative on both axis, left and
        ## right
        ##
        ## For barplot: the axis is on the bottom of every column, and left and right of every
        ## row - also, must rotate if too big!
        ## ------------
        
        pushViewport(viewport(layout.pos.row = 2, xscale = xlim,
                              yscale = if (TYPE == "bar") 100 * ylim else ylim))
        opts$ZOOM <- zoombars
        if (r == nr)  # bottom
          drawAxes(X, "x", TRUE, c %% 2 == 1 | !TYPE %in% c("scatter", "grid", "hex"),
                   opts, layout.only = layout.only)
        
        if (c == 1 & (!opts$internal.labels | !TYPE %in% c("dot", "hist")))  # left column
          drawAxes(if (TYPE == "bar") ylim else Y, "y", TRUE, (nr - r) %% 2 == 0, opts,
                   layout.only = layout.only)
        
        if (!TYPE %in% c("dot", "hist")) {
          if (c == nc | g1id == NG1) # right column (or last plot in top row)
            drawAxes(if (TYPE == "bar") ylim else Y, "y", FALSE, (nr - r) %% 2 == 1,
                     opts, layout.only = layout.only)
        }
        upViewport()
        
        if (TYPE %in% c("scatter", "grid", "hex")) {
          pushViewport(viewport(layout.pos.row = 1, xscale = xlim, yscale = ylim))
          if (r == 1)
            drawAxes(X, "x", FALSE, c %% 2 == 0, opts, sub = vspace,
                     layout.only = layout.only)
          upViewport()
        }
        opts$ZOOM <- NULL
        
        ## update the counters
        if (g1id < NG1) {
          g1id <- g1id + 1
        } else {
          g1id <- 1
          g2id <- g2id + 1
        }
        
        
      }
    }
    
    dev.flush()
  } else {
    ## break up plot list
    if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
    if (!matrix.plot & !is.null(g2.level)) {
      plot.list <- plot.list[g2.level]
    }
    
    plot.list <- lapply(plot.list, function(x) x[g1.level])
  }
  
  if (plot) {
    plot.list$gen <- list(opts = opts,
                          mcex = multi.cex,
                          col.args = col.args,
                          maxcount = maxcnt)
    plot.list$xlim <- xlim
    plot.list$ylim <- ylim
  }
  
  attr(plot.list, "varnames") <- varnames
  attr(plot.list, "glevels") <- g.level
  attr(plot.list, "vartypes") <- vartypes
  attr(plot.list, "missing") <- missing
  attr(plot.list, "total.missing") <- total.missing
  attr(plot.list, "total.obs") <- total.obs
  attr(plot.list, "bootstrap") <- opts$bs.inference
  attr(plot.list, "nboot") <- opts$n.boot
  attr(plot.list, "inzclass") <- xattr$class
  attr(plot.list, "nplots") <- if (exists("N")) N else NULL
  
  if (xattr$class == "inz.survey") {
    attr(plot.list, "main.design") <- design
    attr(plot.list, "design") <- df.list
  }
  
  attr(plot.list, "plottype") <- gsub("inz", "", class(plot.list[[1]][[1]]))
  if (attr(plot.list, "plottype") %in% c("dot", "hist"))
    attr(plot.list, "nbins") <- length(plot.list[[1]][[1]]$toplot[[1]]$counts)
  
  if (itsADotplot) {
    ## some recursion instructions        
    ## i.e., [original.size, new.size]
    attr(plot.list, "dotplot.redraw") <-
      round(xattr$symbol.width, 5) !=
      round(convertWidth(unit(opts$cex.dotpt, "char"),
                         "native", valueOnly = TRUE), 5)
  }
  
  class(plot.list) <- "inzplotoutput"
  return(invisible(plot.list))
}

summary.inzgrid <- function(object, vn, des, ...)
  summary.inzscatter(object, vn, des, ...)

summary.inzhex <- function(object, vn, des, ...)
  summary.inzscatter(object, vn, des, ...)





triangularMatrix <- function(factorLvls, output, statType) {
  ## takes a bunch of output (from "multiplComp") and presents it as a matrix
  
  statsMatrix <- as.matrix(output)
  colNames <- colnames(statsMatrix)
  
  condition <- FALSE
  if(!is.null(colNames)) {
    if (all(colnames(statsMatrix) == c("Estimate", "Tukey.L", "Tukey.U", "Tukey.p"))) {
      condition <- TRUE
      
      Nlev <- length(factorLvls)
      rns <- c()
      for (i in 1:(Nlev-1))
        rns <- c(rns, paste(factorLvls[i], " - ", factorLvls[(i+1):Nlev]))
      
      
      output.df <- as.data.frame(statsMatrix)
      output.df$name <- rownames(output.df)
      
      fake <- data.frame(name=rns)
      statsMatrix <- as.matrix(merge(output.df, fake, by = "name", all.y = TRUE)[, -1])
      rownames(statsMatrix) <- rns
    }
    else condition <- FALSE
  } else {
    condition <- FALSE
  }
  
  if (statType == "estimates") {
    if (condition)
      values <- statsMatrix[, 1]
    else
      values <- statsMatrix[1, ]
  } else if (statType == "p-values") {
    if (condition)
      values <- statsMatrix[, 4]
    else
      values <- statsMatrix[4, ]
  } else if (statType == "ci") {
    count <- 1
    i <- count
    
    if(condition) {
      values <- numeric(nrow(statsMatrix) * 2)
      while (count < nrow(statsMatrix) + 1) {
        values[c(i, i + 1)] <- c(statsMatrix[count, 2], statsMatrix[count, 3])
        count <- count + 1
        i <- i + 2
      }
    } else {
      values <- numeric(ncol(statsMatrix) * 2)
      while (count < ncol(statsMatrix) + 1) {
        values[c(i, i + 1)] <- c(statsMatrix[2, count], statsMatrix[3, count])
        count <- count + 1
        i = i + 2
      }
    }
  }
  
  num <- length(factorLvls)
  newMatrix <- matrix(NA, ncol <- num, nrow <- num)
  
  if (statType %in% c("estimates", "p-values")) {
    stopAt <- 0
    for(i in 1:num) {
      if (i == 1) startAt <- 1
      stopAt <- (stopAt - i) + num
      
      if (i == num) extra <- numeric(0)
      else extra <- values[startAt:stopAt]
      
      newMatrix[, i] <- format(c(rep("", num + i - num), signif(extra, 5)),
                               width = 5, justify = "r")
      startAt <- stopAt + 1
    }
    rownames(newMatrix) <- factorLvls
    colnames(newMatrix) <- factorLvls
    x <- ncol(newMatrix)
    newMatrix <- as.matrix(newMatrix[-1, -x])
    
    if (x == 2) newMatrix <- t(newMatrix)
    rownames(newMatrix) <- factorLvls[2:length(factorLvls)]
    colnames(newMatrix) <- factorLvls[1:(length(factorLvls) - 1)]
    
  } else if (statType == "ci") {
    stopAt <- 0
    doubleNum <- num * 2
    newMatrix <- matrix(NA, ncol = num, nrow = doubleNum)
    
    for (i in 1:num) {
      if (i == 1) startAt <- 1
      stopAt <- (stopAt - i * 2) + doubleNum
      
      if (i == num) extra <- numeric(0)
      else extra <- values[startAt:stopAt]
      
      newMatrix[, i] <- format(c(rep("", (doubleNum + i - doubleNum) * 2),
                                 signif(extra, 5)), width = 5, justify = "r")
      startAt <- stopAt + 1
    }
    rowNames <- rep("", doubleNum)
    temp <- 1:doubleNum
    rowNames[(temp %% 2 != 0)] <- factorLvls
    
    x <- ncol(newMatrix)
    newMatrix <- as.matrix(newMatrix[-(1:2), -x])
    
    rownames(newMatrix) <- rowNames[-c(1, 2)]
    colnames(newMatrix) <- factorLvls[-x]
  }
  newMatrix
}






##' Generate summary or inference information for an iNZight plot
##'
##' Works much the same as \code{iNZightPlot}
##' @title iNZight Plot Summary and Inference
##' @param x a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object
##' @param y a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object
##' @param g1 a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object. This variable acts as a subsetting variable.
##' @param g1.level the name (or numeric position) of the level of \code{g1} that will be
##' used instead of the entire data set
##' @param g2 a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object. This variable acts as a subsetting variable, similar to
##' \code{g1}
##' @param g2.level same as \code{g1.level}, however takes the additional value \code{"_MULTI"},
##' which produces a matrix of \code{g1} by \code{g2}
##' @param varnames a list of variable names, with the list named using the appropriate arguments
##' (i.e., \code{list(x = "height", g1 = "gender")})
##' @param colby the name of a variable (numeric or factor) to colour points by. In the
##' case of a numeric variable, a continuous colour scale is used, otherwise each level of
##' the factor is assigned a colour
##' @param sizeby the name of a (numeric) variable, which controls the size of points
##' @param data the name of a data set
##' @param design the name of a survey object, obtained from the \code{survey} package
##' @param freq the name of a frequency variable if the data are frequencies
##' @param missing.info logical, if \code{TRUE}, information regarding missingness is
##' displayed in the plot
##' @param inzpars allows specification of iNZight plotting parameters over multiple plots
##' @param summary.type one of \code{"summary"} or \code{"inference"}
##' @param ... additional arguments, see \code{inzpar}
##' @return an \code{inzight.plotsummary} object with a print method
##' @author tell029
##' @export
getPlotSummary_change <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                           g2 = NULL, g2.level = NULL, varnames = list(),
                           colby = NULL, sizeby = NULL,
                           data = NULL, design = NULL, freq = NULL,
                           missing.info = TRUE, inzpars = inzpar(),
                           summary.type = "summary", ...) {
  
  ## Grab a plot object!
  m <- match.call(expand.dots = FALSE)
  env <- parent.frame()
  
  if ("design" %in% names(m)) {
    md <- eval(m$design, env)
  } else {
    md <- eval(m$data, env)
  }
  
  varnames <- varnames
  
  ## Any varnames supplied that AREN'T needed must be removed, otherwise errors:
  nullVars <- sapply(as.list(m)[names(varnames)], is.null)
  varnames[nullVars] <- NULL
  
  ## fix up some subsetting group stuff
  if (is.null(m$g1)) {
    if (!is.null(g2)) {
      if (length(varnames) > 0) {
        varnames$g1 <- varnames$g2
        varnames$g2 <- NULL
      }
      
      getPlotSummary_change(x = x, y = y, g1 = g2, g1.level = g2.level, g2 = NULL, g2.level = NULL,
                     varnames = varnames, colby = colby, sizeby = sizeby, data = data,
                     design = design, freq = freq, missing.info = missing.info,
                     new = new, inzpars = inzpars,
                     env = env,
                     summary.type = summary.type, ...)
    }
  }
  
  ## we now want to create a data object which contains *ALL* of the necessary
  ## information, including survey design, or frequency information:
  
  ## remove these as they aren't necessary and cause problems with "n.missing"
  rmv <- which(names(m) %in% c("colby", "sizeby"))
  if (length(rmv) > 0)
    m <- m[-rmv]
  
  if (!"df" %in% ls())
    df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level, env = env)
  
  
  ### This is getting complex... so for now ignore manual use.
  
  ## ## Modify `inzpars` for the inference:
  ## dots <- list(...)
  ## inference.type <- inference.par <- NULL
  ## bs.inference <- FALSE
  ## if (summary.type[1] == "inference") {
  ##     if (!"inference.type" %in% names(dots))
  ##         inference.type <- inzpars$inference.type
  ##     else
  ##         inference.type <- dots$inference.type
  
  ##     if (is.null(inference.type))
  ##         inference.type <- "conf"
  
  
  ##     if (!"inference.par" %in% names(dots))
  ##         inference.par <- inzpars$inference.par
  ##     else
  ##         inference.par <- dots$inference.par
  
  ##     ## Set the default to "mean" - barplots automatically use proportion
  ##     if (is.null(inference.par))
  ##         inference.par <- "mean"
  
  
  ##     ## and grab bootstrap info ...
  ##     if (!"bs.inference" %in% names(dots))
  ##         bs.inference <- inzpars$bs.inference
  ##     else
  ##         bs.inference <- dots$bs.inference
  ## }
  
  
  obj <- iNZightPlot_change(x = x, y = y, g1 = g1, g1.level = g1.level,
                     g2 = g2, g2.level = g2.level, varnames = varnames,
                     colby = NULL, sizeby = NULL,
                     data = data, design = design, freq = freq,
                     missing.info = missing.info, inzpars = inzpars,
                     plot = FALSE, df = df, ...)
  
  ### Now we just loop over everything ...
  
  summary(obj, summary.type)
}


summary.inzplotoutput <- function(object, summary.type = "summary", width = 100) {
  if (length(summary.type) > 1) {
    warning("Only using the first element of `summary.type`")
    summary.type <- summary.type[1]
  }
  if (!summary.type %in% c("summary", "inference"))
    stop("`summary.type` must be either `summary` or `inference`")
  
  obj <- object  ## same typing ... but match default `summary` method arguments
  
  ## set up some variables/functions to make text processing easier ...
  
  out <- character()
  rule <- function(char, width)
    paste0(rep(char, width), collapse = "")
  Hrule <- rule("=", width)
  hrule <- rule("-", width)
  srule <- rule("*", width)
  center <- centerText
  ind <- function(x, indent = 3)
    paste0(paste0(rep(" ", indent), collapse = ""), x)
  
  add <- function(..., underline = FALSE) {
    x <- paste0(..., collapse = "")
    out <<- c(out, x)
    if (underline)
      out <<- c(out, rule("-", width = nchar(x)))
  }
  
  vnames <- attr(obj, "varnames")
  g.levels <- attr(obj, "glevels")
  vartypes <- attr(obj, "vartypes")
  missing <- attr(obj, "missing")
  total.missing <- attr(obj, "total.missing")
  total.obs <- attr(obj, "total.obs")
  bs <- attr(obj, "bootstrap")
  inzclass <- attr(obj, "inzclass")
  
  is.survey <- attr(obj, "inzclass") == "inz.survey"
  
  if (is.survey & summary.type == "inference")
    return("Inference for Survey Designs not yet implemented.")
  
  add(Hrule)
  add(center(switch(summary.type,
                    "summary" =
                      paste0("iNZight Summary",
                             ifelse(is.survey, " - Survey Design", "")),
                    "inference" =
                      paste("iNZight Inference using",
                            ifelse(bs,
                                   "the Nonparametric Bootstrap",
                                   "Normal Theory"))), width))
  add(hrule)
  
  scatter <- FALSE
  if ("y" %in% names(vnames)) {
    if (vartypes[[vnames$x]] == "numeric" & vartypes[[vnames$y]] == "numeric") {
      scatter <- TRUE
    }
  }
  
  ## A tidy header that formats the vames of the variables
  mat <- cbind(ind(ifelse(scatter, "Response/outcome variable: ", "Primary variable of interest: ")),
               paste0(ifelse(scatter, vnames$y, vnames$x),
                      " (", vartypes[[ifelse(scatter, vnames$y, vnames$x)]], ")"))
  
  if ("y" %in% names(vnames))
    mat <- rbind(mat, cbind(ind(paste0(ifelse(scatter,
                                              "Predictor/explanatory", "Secondary"),
                                       " variable: ")),
                            paste0(ifelse(scatter, vnames$x, vnames$y),
                                   " (", vartypes[[ifelse(scatter, vnames$x, vnames$y)]], ")")))
  
  wg <- c("g1", "g2") %in% names(vnames)
  
  if (is.null(g.levels$g2[1]))
    wg[2] <- FALSE
  
  if (any(wg)) {
    mat <- rbind(mat, "")
    mat <- rbind(mat, cbind(ind("Subset by: "),
                            do.call(paste, c(vnames[c("g1", "g2")[wg]], list(sep = " and ")))))
    if (is.survey)
      mat <- rbind(mat, c("NOTE: ", "survey summaries are not yet reliable for subsets."))
  }
  
  mat <- rbind(mat, "",
               cbind("Total number of observations: ", total.obs))
  if (total.missing > 0) {
    allnames <- c("x", "y", "g1", "g2")
    nn <- allnames[allnames %in% names(missing)]
    nn <- nn[sapply(missing[nn], function(m) m > 0)]
    mat <- rbind(mat,
                 cbind(ind("Number ommitted due to missingness: "),
                       paste0(total.missing,
                              if (length(missing) > 1) {
                                paste0(" (",
                                       paste(sapply(nn, function(i) {
                                         paste0(missing[[i]], " in ", vnames[[i]])
                                       }), collapse = ", "),
                                       ")")
                              })),
                 cbind(ind("Total number of observations used: "),
                       total.obs - total.missing))
  }
  mat <- cbind(format(mat[, 1], justify = "right"), mat[, 2])
  apply(mat, 1, add)
  
  if (is.survey) {
    add(hrule)
    sapply(capture.output(attr(object, "main.design")), function(o) add(ind(o)))
    design.list <- attr(object, "design")
  }
  
  add(Hrule)
  add("")
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  stype <- simpleCap(summary.type)
  
  ## Cycle through G2 first
  lapply(names(obj), function(this) {
    if (this != "all") {
      add(Hrule)
      add(ind("For the subset where ", 5), vnames$g2, " = ", this)
    }
    
    lapply(names(obj[[this]]), function(o) {
      pl <- obj[[this]][[o]]
      
      xtype <- vartypes[[vnames$x]]
      header <- switch(xtype,
                       "numeric" = {
                         if ("y" %in% names(vnames)) {
                           switch(vartypes[[vnames$y]],
                                  "numeric" = {
                                    sprintf("%s of %s versus %s",
                                            stype, vnames$y, vnames$x)
                                  },
                                  "factor" = {
                                    sprintf("%s of %s by %s",
                                            stype, vnames$x, vnames$y)
                                  })
                         } else {
                           sprintf("%s of %s", stype, vnames$x)
                         }
                       },
                       "factor" = {
                         if ("y" %in% names(vnames)) {
                           switch(vartypes[[vnames$y]],
                                  "numeric" = {
                                    sprintf("%s of the distribution of %s by %s",
                                            stype, vnames$x, vnames$y)
                                  },
                                  "factor" = {
                                    sprintf("%s of the distribution of %s (columns) by %s (rows)",
                                            stype, vnames$x, vnames$y)
                                  })
                         } else {
                           sprintf("%s of the distribution of %s", stype, vnames$x)
                         }
                       })
      
      if (o != "all") {
        add(hrule)
        header <- paste0(header, paste0(", for ", vnames$g1, " = ", o))
      }
      header <- paste0(header, ":")
      
      add(header, underline = TRUE)
      add("")
      
      pl.design <- if (is.survey) design.list[[this]][[o]] else NULL
      
      sapply(switch(summary.type,
                    "summary" = summary(pl, vn = vnames, des = pl.design),
                    "inference" = inference(pl, bs, inzclass, width = width, vn = vnames, nb = attr(obj, "nboot"))),
             add)
      
      add("")
    })
    
    add("")
  })
  
  add(Hrule)
  
  ## Notes:
  add("")
  add("")
  
  
  
  class(out) <- "inzight.plotsummary"
  out
}


##' @export
print.inzight.plotsummary <- function(x, ...) {
  cat(x, sep = "\n")
}




centerText <- function(x, width) {
  len <- nchar(x)
  pad <- floor((width - len) / 2)
  paste0(paste0(rep(" ", pad), collapse = ""), x)
}










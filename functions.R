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
                                  function(index, d, c){
                                    te = rep("observed",nrow(d))
                                    te[is.na(d[,index])] = "missing"
                                    te
                                  }, temp, columns)))
  colnames(new.dafr) = paste("missing",columns,sep=".")
  cbind(dafr, new.dafr)
}




#' add new columns to the original dataframe which replace "NA"
#' with "missing" so that the missing values could be displayed 
#' in the plot..
#' 
#' @param dafr The input data.frame.
#' @param The column names or indexes to be converted.
#' 
#' @return A data.frame with the new columns added.
#' 
#' @author Wilson Hu 
display.missing.categorical = function(dafr, columns) {
  for(i in columns) {
    temp = dafr[, i]
    if(is.factor(temp) || is.character(temp)) {
      temp = as.character(temp)
      temp[is.na(temp)] = "missing"
      temp = as.factor(temp)
      original.level = levels(temp)[levels(temp) != "missing"]
      temp = factor(temp, levels = c(original.level, "missing"))
    }
    else {
      index = is.na(temp)
      temp = rep("observed", length(temp))
      temp[index] = "missing"
      temp = factor(temp, levels = c("observed", "missing"))
    }
    temp = as.data.frame(temp)
    colnames(temp) = paste(i, "missing", sep = "_")
    dafr = data.frame(dafr, temp)
  }
  dafr
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
  colnames(temp1) = columns
  temp2 = transform.get.temp(dafr,type,columns)
  if(!is.null(temp2)){
    temp1 = data.frame(temp1,temp2)
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
  } else if (!is.null(columns)&type%in%"change to factor"){
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
      if(!(tolower(ext)%in%c("rds","rda","rdata","csv","txt", "xls", "xlsx"))){
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
          temp = read.csv(full.name[indexes[1]],comment.char="#", na.strings = c("NULL","NA","N/A","#N/A","","<NA>"), stringsAsFactors=FALSE)
        }else if(tolower(ext)%in%"txt"){
          temp = read.delim(full.name[indexes[1]],comment.char="#", na.strings = c("NULL","NA","N/A","#N/A","","<NA>"))
        }else if(tolower(ext)%in%"xls"){
          temp = read.xlsx(full.name[indexes[1]], 1)
        }else if(tolower(ext)%in%"xlsx"){
          temp = read.xlsx(full.name[indexes[1]], 1)
        }else if(tolower(ext)%in%"sas7bdat"){
          temp = read.sas7bdat(full.name[indexes[1]])
        }else if(tolower(ext)%in%"dta"){
          temp = read.dta(full.name[indexes[1]])
        }else if(tolower(ext)%in%"sav"){
          temp = read.spss(full.name[indexes[1]], use.value.labels=FALSE,to.data.frame=TRUE)
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
  #print(URL)
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

# get data from google docs urls
get.data.from.googledocs = function(URL,data.dir.import){
  ret = list()
  #URL = gsub(" ", "%20", URL)
  URL = URLencode(URL)
  #print(URL)
  #name = strsplit(URL,"/")[[1]]
  #name = strsplit(name[length(name)],"?",fixed=T)[[1]][1]
  url.index = gregexpr("output=", URL)
  url.index = unlist(url.index)
  file.type = substr(URL, url.index+7, nchar(URL))
  temp.file.name = tempfile()
  temp.file.name.index = gregexpr("file", temp.file.name)
  temp.file.name.index = unlist(temp.file.name.index)
  file.name = substr(temp.file.name, temp.file.name.index, nchar(temp.file.name))
  name = paste(file.name, file.type, sep = ".")
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
  search = function(input.list, nam=NULL){
    if(
#       "list"%in%class(input.list)||
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




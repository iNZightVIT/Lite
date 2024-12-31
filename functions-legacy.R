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
display.missing.categorical <- function(dafr, columns) {
  for (i in columns) {
    temp <- dafr[, i]
    if (is.factor(temp) || is.character(temp)) {
      temp <- as.character(temp)
      temp[is.na(temp)] <- "missing"
      temp <- as.factor(temp)
      original.level <- levels(temp)[levels(temp) != "missing"]
      temp <- factor(temp, levels = c(original.level, "missing"))
    } else {
      index <- is.na(temp)
      temp <- rep("observed", length(temp))
      temp[index] <- "missing"
      temp <- factor(temp, levels = c("observed", "missing"))
    }
    temp <- as.data.frame(temp, stringsAsFactors = TRUE)
    colnames(temp) <- paste(i, "missing", sep = "_")
    dafr <- data.frame(dafr, temp, stringsAsFactors = TRUE)
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
simplify.dafr <- function(dafr) {
  ies <- c()
  for (col in 1:ncol(dafr)) {
    if (!any(is.na(dafr[, col]))) {
      ies <- c(ies, col)
    }
  }
  dafr <- dafr[, -ies]
  if (ncol(dafr) == 0) {
    dafr <- NULL
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
convert.dafr <- function(dafr) {
  if (!is.null(dafr)) {
    temp <- do.call(cbind, lapply(1:ncol(dafr), function(i, d) {
      col <- rep("observed", nrow(d))
      col[is.na(d[, i])] <- "missing"
      col
    }, dafr))
    colnames(temp) <- colnames(dafr)
    temp
  } else {
    dafr
  }
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
order.overwrite <- function(z, na.last = TRUE, decreasing = FALSE) {
  if (any(diff(l.z <- vapply(z, length, 1L)) != 0L)) {
    stop("argument lengths differ")
  }
  ans <- vapply(z, is.na, rep.int(NA, l.z[1L]))
  ok <- if (is.matrix(ans)) {
    !apply(ans, 1, any)
  } else {
    !any(ans)
  }
  if (all(!ok)) {
    return(integer())
  }
  z[[1L]][!ok] <- NA
  ans <- do.call("order", c(z, decreasing = decreasing))
  keep <- seq_along(ok)[ok]
  ans[ans %in% keep]
}

#' Tests whether a character variable is convertable to an
#' integer value.
#'
#' @param x a character or numeric value to test for.
#'
#' @author Christoph Knapp
is.convertable.integer <- function(x) {
  temp <- is.convertable.numeric(x)
  temp[temp] <- as.numeric(x[temp]) %% 1 == 0
  temp
}

test.for.dates <- function(dafr) {
  ret <- F
  if (!is.null(dafr)) {
    ret <- unlist(lapply(
      1:ncol(dafr), function(index, d) {
        tryCatch(
          {
            is.numeric(as.numeric(as.Date(d[, index], origin = "1900-01-01")))
          },
          error = function(cond) {
            ret <- F
          },
          warning = function(cond) {
            print(cond)
          },
          finally = {}
        )
      }, dafr
    ))
  }
  ret
}

divide.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  if (is.null(dafr)) {
    return(NULL)
  } else {
    if (ncol(as.data.frame(
      stringsAsFactors = TRUE,
      dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
        is.numeric(d[, index])
      }, dafr))]
    )) == 1) {
      temp <- as.data.frame(
        stringsAsFactors = TRUE,
        dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))]
      )
      colnames(temp) <- colnames(data)[unlist(lapply(
        1:ncol(dafr),
        function(index, d) {
          is.numeric(d[, index])
        }, data
      ))]
    } else if (ncol(as.data.frame(
      stringsAsFactors = TRUE,
      dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
        is.numeric(d[, index])
      }, data))]
    )) > 1) {
      temp <- as.data.frame(
        stringsAsFactors = TRUE,
        divide(dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))])
      )
      colnames(temp) <- paste0(
        "divide.",
        paste(colnames(dafr)[unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))], collapse = ".")
      )
    } else {
      return(NULL)
    }
  }
  temp
}

divide <- function(dafr) {
  dafr <- dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
    is.numeric(d[, index])
  }, dafr))]
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  if (ncol(dafr) == 1) {
    dafr[, 1]
  } else {
    start <- dafr[, 1]
    for (col in 2:ncol(dafr)) {
      start <- start / dafr[, col]
    }
    start
  }
}

# returns the transformed columns and the original data as
# dataframe (cbind(data,<transformed columns>)).
transform.perform <- function(dafr, type, columns) {
  temp <- transform.get.temp(dafr, type, columns)
  if (!is.null(temp)) {
    temp <- cbind(dafr, temp)
  }
  temp
}

# returns the transformed columns and the original columns as
# dataframe (cbind(<original columns>,<transformed columns>)).
transform.tempTable <- function(dafr, type, columns) {
  temp1 <- as.data.frame(
    stringsAsFactors = TRUE,
    dafr[, which(colnames(dafr) %in% columns)]
  )
  colnames(temp1) <- columns
  temp2 <- transform.get.temp(dafr, type, columns)
  if (!is.null(temp2)) {
    temp1 <- data.frame(stringsAsFactors = TRUE, temp1, temp2)
  }
  temp1
}


# transorms the columns named columns in data with the selected
# type (type) of transformation.
transform.get.temp <- function(dafr, type, columns) {
  temp <- NULL
  if (!is.null(columns) && type %in% "log") {
    temp <- log.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "add") {
    temp <- add.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "subtract") {
    temp <- subtract.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "multiply") {
    temp <- multiply.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "divide") {
    temp <- divide.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "root") {
    temp <- root.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "square") {
    temp <- square.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "abs") {
    temp <- abs.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "center") {
    temp <- center.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "standardize") {
    temp <- standardize.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "median split") {
    temp <- median.split.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "reverse-coding") {
    temp <- reverse.coding.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "copy") {
    temp <- copy.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "change sign") {
    temp <- change.sign.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% "change to factor") {
    temp <- change.factor.transform(dafr[, columns], columns)
  } else if (!is.null(columns) & type %in% " ") {
    temp <- NULL
  }
  temp
}

log.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, dafr) {
      if (is.numeric(dafr[, index])) {
        log(dafr[, index])
      } else {
        NULL
      }
    }, dafr))
  )
  if (!is.null(temp) && dim(temp)[1] > 0 && dim(temp)[2] > 0) {
    colnames(temp) <- unlist(lapply(1:ncol(dafr), function(index, dafr) {
      if (is.numeric(dafr[, index])) {
        paste0("log.", colnames(dafr)[index])
      } else {
        NULL
      }
    }, dafr))
    temp
  } else {
    NULL
  }
}

root.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        sqrt(d[, index])
      } else {
        NULL
      }
    }, dafr))
  )
  ##  temp = as.data.frame(temp)
  if (dim(temp)[1] > 0 && dim(temp)[2] > 0) {
    colnames(temp) <- unlist(lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        paste0("root.", colnames(d)[index])
      } else {
        NULL
      }
    }, dafr))
    temp
  } else {
    NULL
  }
}

square.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        d[, index]^2
      } else {
        NULL
      }
    }, dafr))
  )
  ##  temp = as.data.frame(temp)
  if (dim(temp)[1] > 0 && dim(temp)[2] > 0) {
    colnames(temp) <- unlist(lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        paste0("square.", colnames(d)[index])
      } else {
        NULL
      }
    }, dafr))
    temp
  } else {
    NULL
  }
}


abs.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        abs(d[, index])
      } else {
        NULL
      }
    }, dafr))
  )
  if (dim(temp)[1] > 0 && dim(temp)[2] > 0) {
    colnames(temp) <- unlist(lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        paste0("abs.", colnames(d)[index])
      } else {
        NULL
      }
    }, dafr))
    temp
  } else {
    NULL
  }
}

delete.old.files <- function(data_dir, days) {
  if (length(list.files(paste0(data_dir, "/Imported"))) > 0) {
    unlink(list.files(paste0(data_dir, "/Imported"))[
      difftime(Sys.time(),
        file.info(list.files(
          paste0(data_dir, "/Imported"),
          full.name = T
        ))
        [, "mtime"],
        units = "days"
      ) > days
    ])
  }
}


change.file.ext <- function(name, new.ext) {
  splity <- strsplit(name, ".", fixed = T)[[1]]
  if (length(splity) > 1) {
    splity <- paste(paste(splity[1:(length(splity) - 1)], collapse = "."),
      new.ext,
      sep = "."
    )
  } else {
    splity <- paste0(splity, ".", new.ext)
  }
  splity
}

get.data.name.from.URL <- function(URL) {
  if (grepl("docs.google.com", URL)) {
    url.index <- gregexpr("output=", URL)
    url.index <- unlist(url.index)
    file.type <- substr(URL, url.index + 7, nchar(URL))
    temp.file.name <- tempfile()
    temp.file.name.index <- gregexpr("file", temp.file.name)
    temp.file.name.index <- unlist(temp.file.name.index)
    file.name <- substr(
      temp.file.name,
      temp.file.name.index, nchar(temp.file.name)
    )
    name <- paste(file.name, file.type, sep = ".")
  } else {
    name <- strsplit(URL, "/")[[1]]
    name <- strsplit(name[length(name)], "?", fixed = T)[[1]][1]
  }
  return(name)
}


# get data from google docs urls
get.data.from.googledocs <- function(URL, data.dir.import) {
  ret <- list()
  URL <- URLencode(URL)
  url.index <- gregexpr("output=", URL)
  url.index <- unlist(url.index)
  file.type <- substr(URL, url.index + 7, nchar(URL))
  temp.file.name <- tempfile()
  temp.file.name.index <- gregexpr("file", temp.file.name)
  temp.file.name.index <- unlist(temp.file.name.index)
  file.name <- substr(
    temp.file.name,
    temp.file.name.index, nchar(temp.file.name)
  )
  name <- paste(file.name, file.type, sep = ".")
  if (!file.exists(paste(data.dir.import, "/Imported", sep = "")) &&
    file.writable(data.dir.import)) {
    dir.create(paste(data.dir.import, "/Imported", sep = ""), recursive = TRUE)
  }
  tryCatch({
    if (Sys.info()["sysname"] %in% c("Windows", "Linux")) {
      download.file(
        url = URL,
        destfile = paste0(data.dir.import, "/Imported/", name), method = "auto"
      )
    } else {
      download.file(
        url = URL,
        destfile = paste0(data.dir.import, "/Imported/", name), method = "auto"
      )
    }

    temp <- load.data(data.dir.import,
      fileID = name,
      path = paste0(data.dir.import, "/Imported/", name)
    )
    if (!is.null(temp[[2]])) {
      ret$data.set <- temp[[2]]
      ret$data.name <- name
    } else {
      return(NULL)
    }
    ret
  }, error = function(e) {
    if (file.exists(paste0(data.dir.import, "/Imported/", name))) {
      unlink(paste0(data.dir.import, "Imported/", name))
    }
    print(e)
  }, warning = function(w) {
    print(w)
  }, finally = {})
}

#' fit user's own mixed effect model which include a code attribute
#'
#' @param y response variable.
#' @param x fixed effect
#' @param data Dataset
#' @param blocking random effect
#' @param name name of fitted model
#' @param data.name name of data
#'
#' @return A fitted model with 'code' attribute

fit.own <- function(y, x, data = NULL, blocking = NULL, name, data.name) {
  fit.str <- NULL
  if (!is.null(blocking)) {
    fit.str <- sprintf("%s ~ %s", y, x)
    fit <- nlme::lme(as.formula(fit.str),
      random = as.formula(blocking), data = data
    )
    attr(fit, "code") <- sprintf(
      "%s = nlme::lme(%s ~ %s, random = %s, data = %s)",
      name, y, x, blocking, data.name
    )
  } else {
    fit.str <- sprintf("%s ~ %s", y, x)
    fit <- lm(as.formula(fit.str), data = data)
    attr(fit, "code") <- sprintf(
      "%s = lm(%s ~ %s, data = %s)", name, y,
      x, data.name
    )
  }
  fit
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
change.factor.transform <- function(temp, columns) {
  temp <- as.data.frame(temp, stringsAsFactors = TRUE)
  nums <- unlist(lapply(1:ncol(temp), function(index, temp.data, columns) {
    if (is.numeric(temp.data[, index])) {
      columns[index]
    } else {
      NULL
    }
  }, temp, columns))
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(temp), function(index, temp.data) {
      if (is.numeric(temp.data[, index])) {
        as.character(temp.data[, index])
      } else {
        NULL
      }
    }, temp)), stringsAsFactors = T
  )
  if (!is.null(temp) && ncol(temp) > 0 && nrow(temp) > 0) {
    colnames(temp) <- paste("factors", nums, sep = "_")
    temp
  } else {
    NULL
  }
}

#' change the sign of numeric columns
#'
#' @param dafr a data frame with columns to transform
#' @param columns the column names of the columns in dafr
#'
#' @author Christoph Knapp
change.sign.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, dafr) {
      if (is.numeric(dafr[, index])) {
        as.matrix(dafr[, index] * (-1))
      } else {
        NULL
      }
    }, dafr))
  )
  if (!is.null(temp)) {
    colnames(temp) <- paste("change_sign",
      columns[unlist(lapply(1:ncol(dafr), function(i, d) {
        is.numeric(d[, i])
      }, dafr))],
      sep = "."
    )
  }
  temp
}

multiply.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  if (is.null(dafr)) {
    return(NULL)
  } else {
    if (ncol(as.data.frame(
      stringsAsFactors = TRUE,
      dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
        is.numeric(d[, index])
      }, dafr))]
    )) == 1) {
      temp <- as.data.frame(
        stringsAsFactors = TRUE,
        dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))]
      )
      colnames(temp) <- colnames(dafr)[unlist(lapply(
        1:ncol(dafr),
        function(index, d) {
          is.numeric(d[, index])
        }, dafr
      ))]
    } else if (ncol(as.data.frame(
      stringsAsFactors = TRUE,
      dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
        is.numeric(d[, index])
      }, dafr))]
    )) > 1) {
      temp <- as.data.frame(
        stringsAsFactors = TRUE,
        multiply(dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))])
      )
      colnames(temp) <- paste0(
        "multiply.",
        paste(colnames(dafr)[unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))], collapse = ".")
      )
    } else {
      return(NULL)
    }
  }
  temp
}

multiply <- function(dafr) {
  dafr <- dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
    is.numeric(d[, index])
  }, dafr))]
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  if (ncol(dafr) == 1) {
    dafr[, 1]
  } else {
    start <- dafr[, 1]
    for (col in 2:ncol(dafr)) {
      start <- start * dafr[, col]
    }
    start
  }
}

copy.transform <- function(dafr, columns) {
  data <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- paste("copy", columns, sep = ".")
  data
}

reverse.coding.transform <- function(dafr, columns) {
  data <- as.data.frame(dafr, stringsAsFactors = TRUE)
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        min(d[, index], na.rm = T) + max(d[, index], na.rm = T) - d[, index]
      } else {
        NULL
      }
    }, dafr))
  )
  if (!is.null(temp)) {
    colnames(temp) <- paste("reverse_coding",
      columns[unlist(lapply(
        1:ncol(dafr),
        function(i, d) {
          is.numeric(d[, i])
        }, dafr
      ))],
      sep = "."
    )
  }
  temp
}

median.split.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  nums <- unlist(lapply(1:ncol(dafr), function(index, dafr) {
    is.numeric(dafr[, index])
  }, dafr))
  dafr <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        med <- median(d[, index], na.rm = T)
        ret <- rep("high", length(d[, index]))
        ret[which(d[, index] <= med)] <- "low"
        ret
      } else {
        NULL
      }
    }, dafr)), stringsAsFactors = T
  )
  colnames(dafr) <- paste("median_split", columns[nums], sep = "_")
  dafr
}

standardize.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  dafr <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        (d[, index] - mean(d[, index], na.rm = T)) / sd(d[, index], na.rm = T)
      } else {
        (as.numeric(factor(d[, index]))
        - mean(as.numeric(factor(d[, index])), na.rm = T)) /
          sd(as.numeric(factor(d[, index])), na.rm = T)
      }
    }, dafr))
  )
  colnames(dafr) <- paste("standardize", columns, sep = ".")
  dafr
}

center.transform <- function(dafr, columns) {
  data <- as.data.frame(dafr, stringsAsFactors = TRUE)
  temp <- as.data.frame(
    stringsAsFactors = TRUE,
    do.call(cbind, lapply(1:ncol(dafr), function(index, d) {
      if (is.numeric(d[, index])) {
        d[, index] - mean(d[, index])
      } else {
        as.numeric(factor(d[, index])) - mean(as.numeric(factor(d[, index])))
      }
    }, dafr))
  )
  colnames(temp) <- paste("center", columns, sep = ".")
  temp
}

subtract.transform <- function(dafr, columns) {
  dafr <- as.data.frame(dafr, stringsAsFactors = TRUE)
  colnames(dafr) <- columns
  if (is.null(dafr)) {
    return(NULL)
  } else {
    if (ncol(as.data.frame(
      stringsAsFactors = TRUE,
      dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
        is.numeric(d[, index])
      }, dafr))]
    )) == 1) {
      temp <- as.data.frame(
        stringsAsFactors = TRUE,
        dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))]
      )
      colnames(temp) <- colnames(dafr)[unlist(lapply(
        1:ncol(dafr),
        function(index, d) {
          is.numeric(d[, index])
        }, dafr
      ))]
    } else if (ncol(as.data.frame(
      stringsAsFactors = TRUE,
      dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
        is.numeric(d[, index])
      }, dafr))]
    )) > 1) {
      temp <- as.data.frame(
        stringsAsFactors = TRUE,
        subtract(dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))])
      )
      colnames(temp) <- paste0(
        "subtract.",
        paste(colnames(dafr)[unlist(lapply(1:ncol(dafr), function(index, d) {
          is.numeric(d[, index])
        }, dafr))], collapse = ".")
      )
    } else {
      return(NULL)
    }
  }
  temp
}

subtract <- function(dafr) {
  dafr <- dafr[, unlist(lapply(1:ncol(dafr), function(index, d) {
    is.numeric(d[, index])
  }, dafr))]
  dafr <- as.data.frame(
    stringsAsFactors = TRUE,
    dafr
  )
  if (ncol(dafr) == 1) {
    dafr[, 1]
  } else {
    start <- dafr[, 1]
    for (col in 2:ncol(dafr)) {
      start <- start - dafr[, col]
    }
    start
  }
}

add.transform <- function(temp, columns) {
  temp <- as.data.frame(temp, stringsAsFactors = TRUE)
  colnames(temp) <- columns
  if (is.null(temp)) {
    return(NULL)
  } else {
    ret <- as.data.frame(
      stringsAsFactors = TRUE,
      temp[, unlist(lapply(1:ncol(temp), function(index, d) {
        is.numeric(d[, index])
      }, temp))]
    )
    if (ncol(ret) > 1) {
      ret <- as.data.frame(
        stringsAsFactors = TRUE,
        apply(ret, 1, function(row) {
          sum(row)
        })
      )
      colnames(ret) <- paste0("add_", paste(colnames(temp), collapse = "_"))
    } else {
      return(NULL)
    }
  }
  ret
}
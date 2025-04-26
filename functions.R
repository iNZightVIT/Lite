##########################################################
# To be removed when the iNZight tools package is working##
##########################################################

read_config <- function() {
  lite_config <- Sys.getenv("LITE_CONFIG")
  if (is.null(lite_config) || nchar(lite_config) <= 1) {
    return()
  }

  # read from json
  fromJSON(lite_config)
}

# Modified based off:
# https://github.com/dreamRs/shinylogs/blob/0195ac0a1f85d213c82143cfee712c9baddd1963/R/tracking.R#L134
init_lite_logs <- function(
    log_path = tempdir(),
    what = c("session", "input", "output", "error"),
    exclude_input_regex = NULL,
    exclude_input_id = NULL,
    exclude_users = NULL,
    session = getDefaultReactiveDomain()) {
  return()
  what <- match.arg(what, several.ok = TRUE)
  LITE_SESSION_ID_ <<- substr(session$token, 1, 10)

  if (!file.exists(log_path)) {
    dir.create(log_path)
  }
  addResourcePath("logs", log_path)

  print(list.files(log_path))

  app_name <- "lite"
  user <- shinylogs:::get_user_(session)
  storage_mode <- store_json(path = log_path)
  timestamp <- shinylogs:::get_timestamp(timestamp)
  log_name <- paste0("lite_logs_", LITE_SESSION_ID_, ".json")
  log_path <- file.path(log_path, log_name)
  download_path <- file.path("logs", log_name)

  timestamp <- Sys.time()
  init_log <- data.frame(
    session_id = LITE_SESSION_ID_,
    app = app_name,
    user = user,
    server_connected = timestamp,
    stringsAsFactors = FALSE
  )
  storage_mode$appname <- app_name
  storage_mode$timestamp <- format(
    bit64::as.integer64(nanotime::nanotime(timestamp)),
    scientific = FALSE
  )

  insertUI(
    selector = "body", where = "afterBegin",
    ui = singleton(tags$script(
      id = "shinylogs-tracking",
      type = "application/json",
      `data-for` = "shinylogs",
      toJSON(shinylogs:::dropNulls(list(
        what = what,
        logsonunload = FALSE,
        exclude_input_regex = exclude_input_regex,
        exclude_input_id = exclude_input_id,
        session_id = init_log$session_id,
        log_path = log_path,
        download_path = download_path
      )), auto_unbox = TRUE, json_verbatim = TRUE)
    )),
    immediate = TRUE,
    session = session
  )
  insertUI(
    selector = "body", where = "afterBegin",
    ui = htmltools::attachDependencies(
      x = tags$div(),
      value = list(
        shinylogs:::shinylogs_dependencies()
      )
    ),
    immediate = FALSE,
    session = session
  )

  if (isTRUE(storage_mode$console)) {
    observe({
      to_console(session$input$.shinylogs_browserData, init_log)
    })
    observe({
      to_console(session$input$.shinylogs_lastInput)
    })
  }

  onSessionEnded(
    fun = function() {
      init_log$server_disconnected <- shinylogs:::get_timestamp(Sys.time())
      logs <- c(
        isolate(session$input$.shinylogs_input),
        isolate(session$input$.shinylogs_error),
        isolate(session$input$.shinylogs_output)
      )
      browser_data <- isolate(session$input$.shinylogs_browserData)
      if (!is.null(browser_data)) {
        browser_data <- as.data.frame(browser_data)
        logs$session <- cbind(init_log, browser_data)
      } else {
        logs$session <- init_log
      }
      if (isTRUE(!user %in% exclude_users)) {
        # shinylogs:::write_logs(storage_mode, logs)
        jsonlite::write_json(
          x = logs,
          path = log_path,
          auto_unbox = TRUE
        )
      }
    },
    session = session
  )
}

#' Reahapes the data that all columns are merged into two
#' column with the variable names in the first column and
#' the values in the second column.
#'
#' @param dafr The data.frame to convert
#'
#' @return The converted data.frame.
#'
#' @author Christoph Knapp
get.reshape.data <- function(dafr) {
  temp <- do.call(rbind, lapply(1:ncol(dafr), function(index, d) {
    name <- colnames(d)[index]
    data.frame(groups = name, d[, index], stringsAsFactors = TRUE)
  }, dafr))
  colnames(temp)[2] <- "variables"
  temp
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
get.combinations <- function(dafr, simplify = F) {
  dafr <- data.frame(dafr, stringsAsFactors = TRUE)
  index.column <- rep(T, ncol(dafr))
  rm.na <- function(variable) {
    sum(is.na(variable)) > 0
  }
  if (simplify) {
    index.column <- sapply(dafr, rm.na)
  }
  x <- data.frame(dafr[, index.column], stringsAsFactors = TRUE)
  if (ncol(x) == 0) {
    return(NULL)
  }

  x1 <- as.numeric(apply(x, 2, function(x) length(which(is.na(x)))))
  row4col.order <- order(x1)
  x1 <- c(x1, nrow(x))
  z1 <- ifelse(is.na(x), "missing", "observed")
  tab <- table(apply(z1, 1, paste, collapse = ","))
  tab <- tab[order(names(tab), decreasing = TRUE)]
  tab <- data.frame(
    combination = names(tab),
    count = as.numeric(tab), stringsAsFactors = TRUE
  )
  tabp <- t(apply(tab, 1, function(x) {
    unlist(strsplit(x, ",", fixed = TRUE))
  }))
  tabp <- data.frame(tabp, stringsAsFactors = F)
  tabp <- tabp[, c(row4col.order, max(row4col.order) + 1)]

  #  x1[row4col.order] == numMiss
  tabp <- rbind(tabp, x1[c(row4col.order, max(row4col.order) + 1)])
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

  finaltable <- rbind(finaltable, paste0(round(percMiss * 100, 2), "%"))
  colnames(finaltable)[j] <- "Freq"
  finaltable
}

##########################################################
# To be removed when the iNZight tools package is working##
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
get.create.variables <- function(dafr, new.formula, new.name = NULL) {
  tryCatch(
    {
      colu <- eval(parse(text = new.formula), dafr)
      if (length(colu) > nrow(dafr)) {
        colu <- colu[1:nrow(dafr)]
      }
      temp <- cbind(dafr, colu)
      if (is.null(new.name) || "" %in% new.name) {
        new.name <- "new.name"
      }
      count <- 0
      while (new.name %in% colnames(dafr)) {
        count <- count + 1
        new.name <- paste(new.name, count, sep = ".")
      }
      colnames(temp)[ncol(temp)] <- new.name
      temp
    },
    error = function(cond) {
      return(NULL)
    },
    warning = function(cond) {},
    finally = {}
  )
}

##########################################################
# To be removed when the iNZight tools package is working##
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
get.collapsed.column <- function(column, to.collapse) {
  column <- as.character(column)
  new.level <- paste(to.collapse, collapse = ".")
  indices <- which(column %in% to.collapse)
  if (length(indices) > 0) {
    column[indices] <- new.level
  }
  as.factor(column)
}

#' Returns the names of all numeric columns in data
#'
#' @param dafr The input dataframe to be searched.
#'
#' @author Christoph Knapp
get.numeric.column.names <- function(dafr) {
  colnames(dafr)[which(unlist(lapply(1:ncol(dafr), function(index, d) {
    is.numeric(as.data.frame(d, stringsAsFactors = TRUE)[, index])
  }, dafr)))]
}

#' Returns the column names of the currently selected data which
#' can be converted into factors.
#'
#' @param dafr The input dataframe to be searched.
#'
#' @author Christoph Knapp
get.categorical.column.names <- function(dafr) {
  colnames(dafr)[which(unlist(lapply(1:ncol(dafr), function(index, d) {
    class(as.data.frame(d, stringsAsFactors = TRUE)[, index]) %in% "factor" ||
      class(as.data.frame(d, stringsAsFactors = TRUE)[, index]) %in% "character"
  }, dafr)))]
}


#' Returns TRUE if x can be converted to a numeric
#' value, FALSE if not.
#'
#' @param x any oblect to be tested
#'
#' @author Christoph Knapp
is.convertable.numeric <- function(x) {
  !suppressWarnings(is.na(as.numeric(x)))
}

#' Prints a summary of the currently selected data set.
#'
#' @author Christoph Knapp
data.summary <- function(dafr) {
  if (!is.null(dafr)) {
    cat("Number of rows in data: ", nrow(dafr), "\n")
    cat("Number of columns in data: ", ncol(dafr), "\n")
    cat("\n")
    for (col in 1:length(colnames(dafr))) {
      cat(colnames(dafr)[col], "\n")
      print(summary(dafr[, col]))
    }
  }
}

#' Creates a widget for moving through plots quickly.
#'
#' @param ID.forward inputID for the forward button in the player widget
#' @param ID.player inputID for the slider in the player widget
#' @param ID.backward inputID for the backward button in the player widget
#'
#' @author Christoph Knapp
get.player <- function(ID.forward, ID.player, ID.backward, maxi) {
  fixedRow(column(
    width = 8, offset = 2,
    div(
      class = "player",
      fixedRow(
        column(
          width = 1, offset = 1,
          div(
            class = "seper",
            actionButton(
              inputId = ID.backward,
              label = "", icon = icon("backward")
            )
          )
        ),
        column(
          width = 6, offset = 1,
          sliderInput(
            inputId = ID.player, label = "", min = 1, max = maxi, step = 1,
            animate = animationOptions(interval = 500, loop = T, play = T),
            width = "100%", value = 1, ticks = F
          )
        ),
        column(
          width = 1, offset = 1,
          div(
            class = "seper",
            actionButton(
              inputId = ID.forward,
              label = "", icon = icon("forward")
            )
          )
        )
      )
    )
  ))
}

###  A function for displaying help messages.
help.display <- function(title, id, file) {
  HTML(paste("<div class='modal fade' id='", id,
    "' tabindex='-1' role='dialog' aria-labelledby='basicModal'
       aria-hidden='true'>
             <div class='modal-dialog'>
             <div class='modal-content'>
             <div class='modal-header'>
             <h4 class='modal-title' id='myModalLabel'>", title, "</h4>
             </div>
             <div class='modal-body'>",
    mark_html(file = file, output = NULL, template = FALSE),
    "</div>
             <div class='modal-footer'>
             </div>
             </div>
             </div>
             </div>
             <a href='#' class='btn btn-xs btn-success' data-toggle='modal'
              data-target='#", id, "'>Help</a>",
    sep = ""
  ))
}

## reads a data set from a filename in the data directory
load.data <- function(data_dir, fileID = NULL, path = NULL) {
  temp <- NULL
  full.name <- list.files(data_dir, full.names = T, recursive = T)
  if (!is.null(fileID)) {
    if (is.null(path)) {
      indexes <- grep(paste(fileID, ".", sep = ""), full.name, fixed = T)
    } else if (!is.null(path) & file.exists(path)) {
      full.name <- path
      indexes <- 1
    } else {
      return(list(NULL, NULL))
    }
    if (length(indexes[1]) > 0) {
      ext <- strsplit(full.name[indexes[1]], ".", fixed = T)[[1]]
      ext <- ext[length(ext)]
      if (!(tolower(ext) %in% c(
        "rds", "rda", "rdata", "csv", "txt", "xls", "xlsx"
      ))) {
        ext <- strsplit(fileID, ".", fixed = T)[[1]]
        ext <- ext[length(ext)]
      }
      if (!file.exists(full.name[indexes[1]])) {
        return(list(NULL, NULL))
      }
      # catch possible problems with user data.
      tryCatch({
        if (tolower(ext) %in% "rds") {
          temp <- readRDS(file = full.name[indexes[1]])
        } else if (tolower(ext) %in% "rda" | tolower(ext) %in% "rdata") {
          name <- load(full.name[indexes[1]])
          temp <- get(name)
        } else if (tolower(ext) %in% "csv") {
          temp <- read.csv(full.name[indexes[1]],
            comment.char = "#",
            na.strings = c("NULL", "NA", "N/A", "#N/A", "", "<NA>"),
            stringsAsFactors = TRUE
          )
        } else if (tolower(ext) %in% "txt") {
          temp <- read.delim(full.name[indexes[1]],
            comment.char = "#",
            na.strings = c("NULL", "NA", "N/A", "#N/A", "", "<NA>"),
            stringsAsFactors = TRUE
          )
        } else if (tolower(ext) %in% "xls") {
          temp <- as.data.frame(
            stringsAsFactors = TRUE,
            read_excel(full.name[indexes[1]])
          )
        } else if (tolower(ext) %in% "xlsx") {
          temp <- as.data.frame(
            stringsAsFactors = TRUE,
            read_excel(full.name[indexes[1]])
          )
        } else if (tolower(ext) %in% "sas7bdat") {
          temp <- as.data.frame(
            stringsAsFactors = TRUE,
            read.sas7bdat(full.name[indexes[1]])
          )
        } else if (tolower(ext) %in% "dta") {
          temp <- as.data.frame(
            stringsAsFactors = TRUE,
            read.dta(full.name[indexes[1]])
          )
        } else if (tolower(ext) %in% "sav") {
          temp <- as.data.frame(
            stringsAsFactors = TRUE,
            read.spss(full.name[indexes[1]],
              use.value.labels = FALSE, to.data.frame = TRUE
            )
          )
        }
      }, error = function(e) {
        print(e)
      }, finally = {})
    }
  }
  if (is.null(fileID)) {
    list(NULL, temp)
  } else {
    list(data.name = basename(fileID), data.set = temp)
  }
}

## returns directories in the data directory
get.data.dirs <- function(data_dir) {
  list.files(data_dir,
    include.dirs = T,
    full.names = T
  )[file.info(paste(data_dir,
    list.files(data_dir),
    sep = "/"
  ))[, "isdir"]]
}

## returns a radioButton widget, for every filename in the dir.lable directory.
get.radio.list <- function(dir.label, idlabel) {
  files <- c()
  files <- list.files(dir.label,
    recursive = T,
    full.name = T
  )[!(file.info(list.files(dir.label,
    recursive = T,
    full.names = T
  ))[, "isdir"])]
  temp.files <- strsplit(files, "/")
  files <- unlist(lapply(
    temp.files,
    function(x, label) {
      paste(x[(which(x %in% label) + 1):length(x)], collapse = "==>")
    },
    strsplit(dir.label, "/", fixed = T)[[1]][
      length(strsplit(dir.label, "/", fixed = T)[[1]])
    ]
  ))
  ret <- NULL
  if (length(files) > 0) {
    columns <- lapply(
      1:length(files),
      function(i, ns) {
        paste(strsplit(ns[i], ".", fixed = T)[[1]][
          1:(length(strsplit(ns[i], ".", fixed = T)[[1]]) - 1)
        ], collapse = ".")
      },
      basename(files)
    )
    ret <- radioButtons(
      inputId = paste(basename(dir.label), idlabel, sep = ""),
      label = basename(dir.label), choices = columns,
      selected = columns[1]
    )
  }
  ret
}

get.vars <- function(vars.path) {
  lines <- c()
  if (is.null(vars.path)) {
    vars.path <- "VARS"
  }
  if (!file.exists(vars.path)) vars.path <- "VARS.default"
  if (file.exists(vars.path)) {
    lines <- scan(vars.path, what = "character", sep = "\n", quiet = T)
  } else {
    stop("The VARS file does not exist.")
  }
  if (length(lines) > 0) {
    ret <- NULL
    for (line in lines) {
      if (!grepl("^#", line)) {
        if (grepl("#", line)) {
          line <- strsplit(line, "#")[[1]][1]
        }
        if (grepl("=", line) && length(strsplit(line, "=")[[1]]) > 1) {
          if (is.null(ret)) {
            ret <- list()
          }
          ret[[trim(strsplit(line, "=")[[1]][1])]] <-
            trim(strsplit(line, "=")[[1]][2])
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
file.writable <- function(file, debug) {
  tryCatch({
    if (file.exists(file) &&
      "unix" %in% .Platform$OS.type &&
      "Linux" %in% Sys.info()["sysname"]) {
      grepl(
        "777",
        strsplit(system(paste("stat -c \"%a %n\" ", file, sep = ""),
          intern = T
        ), " ")[[1]][1]
      ) ||
        grepl(
          "775",
          strsplit(system(paste("stat -c \"%a %n\" ", file, sep = ""),
            intern = T
          ), " ")[[1]][1]
        ) ||
        grepl(
          "755",
          strsplit(system(paste("stat -c \"%a %n\" ", file, sep = ""),
            intern = T
          ), " ")[[1]][1]
        )
    } else {
      FALSE
    }
  }, error = function(e) {
    return(FALSE)
  }, finally = {})
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
dir.create.logical <- function(path,
                               showWarnings = TRUE,
                               recursive = FALSE,
                               mode = "0777") {
  result <- tryCatch({
    if (!file.exists(path)) {
      dir.create(path, showWarnings, recursive, mode)
    }
    TRUE
  }, warning = function(w) {
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  }, finally = {})
}

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

get.quantiles <- function(subx) {
  g1 <- rep("", length(subx))
  if (is.numeric(subx)) {
    quant <- quantile(subx, na.rm = T)
    g1[which(subx >= quant[1] & subx < quant[2])] <-
      paste(round(quant[1], 2), round(quant[2], 2), sep = "-")
    g1[which(subx >= quant[2] & subx < quant[3])] <-
      paste(round(quant[2], 2), round(quant[3], 2), sep = "-")
    g1[which(subx >= quant[3] & subx < xquant[4])] <-
      paste(round(quant[3], 2), round(quant[4], 2), sep = "-")
    g1[which(subx >= quant[4] & subx <= quant[5])] <-
      paste(round(quant[4], 2), round(quant[5], 2), sep = "-")
    g1 <- as.factor(g1)
  }
  g1
}

#' Make Syntactically Valid Names
#'
#' @param names vector to be coerced to syntactically valid names.
#'
#' @description Replace spaces with underscores and any other
#' invalid characters to dots
#'
#' @return Character vector of valid names
make_names <- function(names) {
  names <- gsub("\\s+", "_", names)
  names <- make.names(names)

  return(names)
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

parseQueryString <- function(str) {
  if (grepl("docs.google.com", str)) {
    return(list(
      url = sub(".*?url=(.*?)&land.*", "\\1", str),
      land = sub(".*?&land=(.*?)", "\\1", str)
    ))
  } else {
    shiny::parseQueryString(str)
  }
}

# # deprecated
get.data.from.URL <- function(URL, data.dir.import) {
  ret <- list()
  URL <- URLencode(URL)

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
  }, finally = {
    if (file.exists(paste0(data.dir.import, "/Imported/", name))) {
      unlink(paste0(data.dir.import, "/Imported/", name))
    }
  })
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
convert.to.percent <- function(value, back = F) {
  # percentage to numeric
  if (is.null(value)) {
    if (!back) {
      x <- 0
    } else {
      x <- 1
    }
  } else {
    if (back) {
      x <- (100 - value) * 0.01
    } else {
      x <- (1 - value) * 100
    }
  }
  x
}

get.transformation.string <- function(transform_select,
                                      transform_variable_select,
                                      arg3) {
  transformation.string <- ""
  if (transform_select %in% "log") {
    transformation.string <- paste0(
      "log(",
      transform_variable_select,
      ")"
    )
  } else if (transform_select %in% "sqrt") {
    transformation.string <- paste0(
      "sqrt(",
      transform_variable_select,
      ")"
    )
  } else if (transform_select %in% "by degree" &&
    !arg3 %in% "") {
    transformation.string <- paste0(
      "I(",
      transform_variable_select,
      "^",
      arg3,
      ")"
    )
  } else if (transform_select %in% "polynomial of degree" &&
    !arg3 %in% "") {
    transformation.string <- paste0(
      "poly(",
      transform_variable_select,
      ",", arg3, ")"
    )
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
search.name <- function(list.search, search.name = NULL) {
  list.out <- list()
  search <- function(input.list, nam = NULL) {
    if (
      #       "list"%in%class(input.list)||
      "inzplotoutput" %in% class(input.list) ||
        "inzgrid" %in% class(input.list) ||
        "inzpar.list" %in% class(input.list) ||
        "inzdot" %in% class(input.list) ||
        "inzhist" %in% class(input.list) ||
        "inzscatter" %in% class(input.list) ||
        "inzbar" %in% class(input.list)) {
      for (i in 1:length(input.list)) {
        if (!is.null(names(input.list)[i])) {
          nam <- names(input.list)[i]
        } else {
          nam <- i
        }
        search(input.list[[nam]], nam)
      }
    } else {
      if (is.null(nam)) {
        nam <- length(list.out) + 1
      }
      if (!is.null(input.list)) {
        if (nam %in% names(list.out)) {
          temp <- list.out[[nam]]
          list.out[[nam]][[length(list.out[[nam]]) + 1]] <<- input.list
        } else {
          list.out[[nam]] <<- list(input.list)
        }
      }
    }
  }
  search(list.search)
  if (is.null(search.name)) {
    list.out
  } else {
    list.out[[search.name]]
  }
}

modifyList <- function(x, val, keep.null = FALSE) {
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  vnames <- names(val)
  vnames <- vnames[vnames != ""]
  if (keep.null) {
    for (v in vnames) {
      x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) {
        list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
      } else {
        val[v]
      }
    }
  } else {
    for (v in vnames) {
      x[[v]] <- if (v %in% xnames && is.list(x[[v]]) &&
        is.list(val[[v]])) {
        modifyList(x[[v]], val[[v]], keep.null = keep.null)
      } else {
        val[[v]]
      }
    }
  }
  x
}


# #' fit the regression model using n-way anova (n = 1,2,3)
# #'
# #' @param y response variable.
# #' @param x covariates.
# #' @param data Dataset
# #' @param blocking blocking variable
# #' @param name name of fitted model
# #' @param data.name name of data
# #'
# #' @return A fitted model with 'code' attribute

# anova.fit <- function(y, x, data = NULL, blocking = NULL, name, data.name) {
#   # code.list = list()
#   fit.str <- NULL
#   if (!is.null(blocking)) {
#     fit.str <- sprintf("%s ~ %s", y, paste(x, collapse = " * "))
#     fit <- nlme::lme(as.formula(fit.str),
#       random = as.formula(sprintf("~1|%s", blocking)), data = data
#     )
#     attr(fit, "code") <- sprintf(
#       "%s = nlme::lme(%s ~ %s, random = ~1|%s, data = %s)",
#       name, y, paste(x, collapse = " * "), blocking, data.name
#     )
#   } else {
#     fit.str <- sprintf("%s ~ %s", y, paste(x, collapse = " * "))
#     fit <- lm(as.formula(fit.str), data = data)
#     attr(fit, "code") <- sprintf(
#       "%s = lm(%s ~ %s, data = %s)", name, y,
#       paste(x, collapse = " * "), data.name
#     )
#   }
#   fit
# }

#' fit ANOVA (n = 1,2,3)
#'
#' @param y response variable.
#' @param x covariates.
#' @param data Dataset
#' @param blocking blocking variable
#' @param name name of fitted model
#' @param data.name name of data
#'
#' @return ANOVA with 'code' attribute

aov.fit <- function(y, x, data = NULL, blocking = NULL, name, data.name) {
  if (!is.null(blocking)) {
    fit <- aov(as.formula(
      sprintf("%s ~ %s + Error(%s)", y, paste(x, collapse = " * "), blocking)
    ), data = data)
    attr(fit, "code") <- c(
      sprintf(
        "aov_%s = aov(%s ~ %s + Error(%s), data = %s)",
        name, y, paste(x, collapse = " * "), blocking, data.name
      ),
      sprintf("summary(%s)", paste0("aov_", name))
    )
  } else {
    fit <- aov(as.formula(sprintf("%s ~ %s", y, paste(x, collapse = " * "))),
      data = data
    )
    attr(fit, "code") <- c(sprintf(
      "aov_%s = aov(%s ~ %s, data = %s)", name, y,
      paste(x, collapse = " * "), data.name
    ), sprintf("summary(%s)", paste0("aov_", name)))
  }
  fit
}


#' ANOVA for customized which include a code attribute
#'
#' @param y response variable.
#' @param x fixed effect
#' @param data Dataset
#' @param blocking random effect
#' @param name name of fitted model
#' @param data.name name of data
#'
#' @return ANOVA with 'code' attribute

aov.own <- function(y, x, data = NULL, blocking = NULL, name, data.name) {
  fit.str <- sprintf("%s ~ %s", y, x)

  if (!is.null(blocking) && blocking != "") {
    fit <- aov(
      as.formula(sprintf("%s ~ %s + Error(%s)", y, x, blocking)),
      data = data
    )
    attr(fit, "code") <- c(
      sprintf(
        "aov_%s = aov(%s ~ %s + Error(%s), data = %s)",
        name, y, x, blocking, data.name
      ),
      sprintf("summary(%s)", paste0("aov_", name))
    )
  } else {
    fit <- aov(as.formula(sprintf("%s ~ %s", y, x)), data = data)
    attr(fit, "code") <- c(sprintf(
      "aov_%s = aov(%s ~ %s, data = %s)", name, y,
      x, data.name
    ), sprintf("summary(%s)", paste0("aov_", name)))
  }
  fit
}


construct_call <- function(settings, model, vartypes,
                           data = quote(.dataset),
                           design = quote(.design),
                           what = c("plot", "summary", "inference")) {
  if (is.null(model$dataDesign)) design <- NULL
  iNZightPlots:::construct_call(settings, vartypes, data, design, what)
}
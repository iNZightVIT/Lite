## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 3.9, fig.height = 3.5)
library(microbenchmark)
# Only print 3 significant digits
print_microbenchmark <- function (x, unit, order = NULL, ...) {
  s <- summary(x, unit = unit)
  cat("Unit: ", attr(s, "unit"), "\n", sep = "")

  timing_cols <- c("min", "lq", "median", "uq", "max")
  s[timing_cols] <- lapply(s[timing_cols], signif, digits = 3)
  s[timing_cols] <- lapply(s[timing_cols], format, big.mark = ",")

  print(s, ..., row.names = FALSE)
}
assignInNamespace("print.microbenchmark", print_microbenchmark,
  "microbenchmark")

## ------------------------------------------------------------------------
library(microbenchmark)
options(microbenchmark.unit = "us")
library(pryr)  # For object_size function
library(R6)

## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
library(scales)

# Set up ggplot2 theme
my_theme <- theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )

## ------------------------------------------------------------------------
RC <- setRefClass("RC", 
  fields = list(x = "numeric"),
  methods = list(
    initialize = function(x = 1) .self$x <- x,
    getx = function() x,
    inc = function(n = 1) x <<- x + n
  )
)

## ------------------------------------------------------------------------
RC$new()

## ------------------------------------------------------------------------
R6 <- R6Class("R6",
  public = list(
    x = NULL,
    initialize = function(x = 1) self$x <- x,
    getx = function() self$x,
    inc = function(n = 1) self$x <- x + n
  )
)

## ------------------------------------------------------------------------
R6$new()

## ------------------------------------------------------------------------
R6NoClass <- R6Class("R6NoClass",
  class = FALSE,
  public = list(
    x = NULL,
    initialize = function(x = 1) self$x <- x,
    getx = function() self$x,
    inc = function(n = 1) self$x <- self$x + n
  )
)

## ------------------------------------------------------------------------
R6NonPortable <- R6Class("R6NonPortable",
  portable = FALSE,
  public = list(
    x = NULL,
    initialize = function(value = 1) x <<- value,
    getx = function() x,
    inc = function(n = 1) x <<- x + n
  )
)

## ------------------------------------------------------------------------
R6NoClassNonPortable <- R6Class("R6NoClassNonPortable",
  portable = FALSE,
  class = FALSE,
  public = list(
    x = NULL,
    initialize = function(value = 1) x <<- value,
    getx = function() x,
    inc = function(n = 1) x <<- x + n
  )
)

## ------------------------------------------------------------------------
R6Private <- R6Class("R6Private",
  private = list(x = NULL),
  public = list(
    initialize = function(x = 1) private$x <- x,
    getx = function() private$x,
    inc = function(n = 1) private$x <- private$x + n
  )
)

## ------------------------------------------------------------------------
R6Private$new()

## ------------------------------------------------------------------------
R6PrivateNcNp <- R6Class("R6PrivateNcNp",
  portable = FALSE,
  class = FALSE,
  private = list(x = NULL),
  public = list(
    initialize = function(x = 1) private$x <- x,
    getx = function() x,
    inc = function(n = 1) x <<- x + n
  )
)

## ------------------------------------------------------------------------
ClosureEnvClass <- function(x = 1) {
  inc <- function(n = 1) x <<- x + n
  getx <- function() x
  self <- environment()
  class(self) <- "ClosureEnvClass"
  self
}

## ------------------------------------------------------------------------
ls(ClosureEnvClass())

## ------------------------------------------------------------------------
ClosureEnvNoClass <- function(x = 1) {
  inc <- function(n = 1) x <<- x + n
  getx <- function() x
  environment()
}

## ------------------------------------------------------------------------
ls(ClosureEnvNoClass())

## ----echo = FALSE--------------------------------------------------------
# Utility functions for calculating sizes
obj_size <- function(expr, .env = parent.frame()) {
  size_n <- function(n = 1) {
    objs <- lapply(1:n, function(x) eval(expr, .env))
    as.numeric(do.call(object_size, objs))
  }

  data.frame(one = size_n(1), incremental = size_n(2) - size_n(1))
}

obj_sizes <- function(..., .env = parent.frame()) {
  exprs <- as.list(match.call(expand.dots = FALSE)$...)
  names(exprs) <- lapply(1:length(exprs),
    FUN = function(n) {
      name <- names(exprs)[n]
      if (is.null(name) || name == "") paste(deparse(exprs[[n]]), collapse = " ")
      else name
    })

  sizes <- mapply(obj_size, exprs, MoreArgs = list(.env = .env), SIMPLIFY = FALSE)
  do.call(rbind, sizes)
}

## ------------------------------------------------------------------------
sizes <- obj_sizes(
  RC$new(),
  R6$new(),
  R6NoClass$new(),
  R6NonPortable$new(),
  R6NoClassNonPortable$new(),
  R6Private$new(),
  R6PrivateNcNp$new(),
  ClosureEnvClass(),
  ClosureEnvNoClass()
)
sizes

## ----echo = FALSE, results = 'hold'--------------------------------------
objnames <- c("RC", "R6", "R6NoClass", "R6NonPortable",
              "R6NoClassNonPortable", "R6Private", "R6PrivateNcNp",
              "ClosureEnvClass", "ClosureEnvNoClass")

sizes$name <- factor(objnames, levels = rev(objnames))

ggplot(sizes, aes(y = name, x = one)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, max(sizes$one[-1]) * 1.5),
                     expand = c(0, 0), oob = rescale_none) +
  scale_y_discrete(
    breaks = sizes$name,
    labels = c("RC (off chart)", "R6", "R6NoClass", "R6NonPortable",
               "R6NoClassNonPortable", "R6Private", "R6PrivateNcNp",
               "ClosureEnvClass", "ClosureEnvNoClass")) +
  my_theme +
  ggtitle("First object")

ggplot(sizes, aes(y = name, x = incremental)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  scale_x_continuous(limits = c(0, max(sizes$incremental) * 1.05),
                     expand = c(0, 0)) +
  geom_point(size = 2) +
  my_theme +
  ggtitle("Additional objects")

## ------------------------------------------------------------------------
RC2 <- setRefClass("RC2",
  fields = list(x = "numeric"),
  methods = list(
    initialize = function(x = 2) .self$x <<- x,
    inc = function(n = 2) x <<- x * n
  )
)

# Calcualte the size of a new RC2 object, over and above an RC object
as.numeric(object_size(RC$new(), RC2$new()) - object_size(RC$new()))

## ------------------------------------------------------------------------
# Function to extract the medians from microbenchmark results
mb_summary <- function(x) {
  res <- summary(x, unit="us")
  data.frame(name = res$expr, median = res$median)
}

speed <- microbenchmark(
  RC$new(),
  R6$new(),
  R6NoClass$new(),
  R6NonPortable$new(),
  R6NoClassNonPortable$new(),
  R6Private$new(),
  R6PrivateNcNp$new(),
  ClosureEnvClass(),
  ClosureEnvNoClass()
)
speed <- mb_summary(speed)
speed

## ----echo = FALSE, results = 'hold', fig.width = 8-----------------------
speed$name <- factor(speed$name, rev(levels(speed$name)))

p <- ggplot(speed, aes(y = name, x = median)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, max(speed$median) * 1.05), expand = c(0, 0)) +
  my_theme +
  ggtitle("Median time to instantiate object (\u0b5s)")

p

## ------------------------------------------------------------------------
rc           <- RC$new()
r6           <- R6$new()
r6nc         <- R6NoClass$new()
r6np         <- R6NonPortable$new()
r6nc_np      <- R6NoClassNonPortable$new()
r6priv       <- R6Private$new()
r6priv_nc_np <- R6PrivateNcNp$new()
closure      <- ClosureEnvClass()
closure_nc   <- ClosureEnvNoClass()

## ------------------------------------------------------------------------
speed <- microbenchmark(
  rc$x,
  r6$x,
  r6nc$x,
  r6np$x,
  r6nc_np$x,
  r6priv$x,
  r6priv_nc_np$x,
  closure$x,
  closure_nc$x
)
speed <- mb_summary(speed)
speed

## ----echo = FALSE, results = 'hold', fig.width = 8-----------------------
speed$name <- factor(speed$name, rev(levels(speed$name)))

p <- ggplot(speed, aes(y = name, x = median)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, max(speed$median) * 1.05), expand = c(0, 0)) +
  my_theme +
  ggtitle("Median time to access field (\u0b5s)")

p

## ------------------------------------------------------------------------
speed <- microbenchmark(
  rc$x <- 4,
  r6$x <- 4,
  r6nc$x <- 4,
  r6np$x <- 4,
  r6nc_np$x <- 4,
  # r6priv$x <- 4,         # Can't set private field directly,
  # r6priv_nc_np$x <- 4,   # so we'll skip these two
  closure$x <- 4,
  closure_nc$x <- 4
)
speed <- mb_summary(speed)
speed

## ----echo = FALSE, results = 'hold', fig.width = 8-----------------------
speed$name <- factor(speed$name, rev(levels(speed$name)))

p <- ggplot(speed, aes(y = name, x = median)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, max(speed$median) * 1.05), expand = c(0, 0)) +
  my_theme +
  ggtitle("Median time to set field (\u0b5s)")

p

## ------------------------------------------------------------------------
speed <- microbenchmark(
  rc$getx(),
  r6$getx(),
  r6nc$getx(),
  r6np$getx(),
  r6nc_np$getx(),
  r6priv$getx(),
  r6priv_nc_np$getx(),
  closure$getx(),
  closure_nc$getx()
)
speed <- mb_summary(speed)
speed

## ----echo = FALSE, results = 'hold', fig.width = 8-----------------------
speed$name <- factor(speed$name, rev(levels(speed$name)))

p <- ggplot(speed, aes(y = name, x = median)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  geom_point(size = 2) +
  my_theme +
  ggtitle("Median time to call method that accesses field (\u0b5s)")

p

## ------------------------------------------------------------------------
RCself <- setRefClass("RCself",
  fields = list(x = "numeric"),
  methods = list(
    initialize = function() .self$x <- 1,
    setx = function(n = 2) .self$x <- n
  )
)

RCnoself <- setRefClass("RCnoself",
  fields = list(x = "numeric"),
  methods = list(
    initialize = function() x <<- 1,
    setx = function(n = 2) x <<- n
  )
)

## ------------------------------------------------------------------------
R6self <- R6Class("R6self",
  portable = FALSE,
  public = list(
    x = 1,
    setx = function(n = 2) self$x <- n
  )
)

R6noself <- R6Class("R6noself",
  portable = FALSE,
  public = list(
    x = 1,
    setx = function(n = 2) x <<- n
  )
)

## ------------------------------------------------------------------------
rc_self   <- RCself$new()
rc_noself <- RCnoself$new()
r6_self   <- R6self$new()
r6_noself <- R6noself$new()

speed <- microbenchmark(
  rc_self$setx(),
  rc_noself$setx(),
  r6_self$setx(),
  r6_noself$setx()
)
speed <- mb_summary(speed)
speed

## ----echo = FALSE, results = 'hold', fig.width = 8-----------------------
speed$name <- factor(speed$name, rev(levels(speed$name)))

p <- ggplot(speed, aes(y = name, x = median)) +
  geom_segment(aes(yend = name), xend = 0, colour = "gray80") +
  geom_point(size = 2) +
  my_theme +
  ggtitle("Assignment to a field using self vs <<- (\u0b5s)")

p

## ------------------------------------------------------------------------
e1 <- new.env(hash = FALSE, parent = emptyenv())
e2 <- new.env(hash = FALSE, parent = emptyenv())
e3 <- new.env(hash = FALSE, parent = emptyenv())

e1$x <- 1
e2$x <- 1
e3$x <- 1

class(e2) <- "e2"
class(e3) <- "e3"

# Define an S3 method for class e3
`$.e3` <- function(x, name) {
  NULL
}

## ------------------------------------------------------------------------
speed <- microbenchmark(
  e1$x,
  e2$x,
  e3$x
)
speed <- mb_summary(speed)
speed

## ------------------------------------------------------------------------
lst <- list(x = 10)
env <- new.env()
env$x <- 10

mb_summary(microbenchmark(
  lst = lst$x,
  env = env$x,
  lst[['x']],
  env[['x']]
))

## ----eval=FALSE----------------------------------------------------------
#  # Utility functions for calculating sizes
#  obj_size <- function(expr, .env = parent.frame()) {
#    size_n <- function(n = 1) {
#      objs <- lapply(1:n, function(x) eval(expr, .env))
#      as.numeric(do.call(object_size, objs))
#    }
#  
#    data.frame(one = size_n(1), incremental = size_n(2) - size_n(1))
#  }
#  
#  obj_sizes <- function(..., .env = parent.frame()) {
#    exprs <- as.list(match.call(expand.dots = FALSE)$...)
#    names(exprs) <- lapply(1:length(exprs),
#      FUN = function(n) {
#        name <- names(exprs)[n]
#        if (is.null(name) || name == "") paste(deparse(exprs[[n]]), collapse = " ")
#        else name
#      })
#  
#    sizes <- mapply(obj_size, exprs, MoreArgs = list(.env = .env), SIMPLIFY = FALSE)
#    do.call(rbind, sizes)
#  }

## ------------------------------------------------------------------------
sessionInfo()


\name{NEWS}
\title{NEWS file for the iNZightPlots package}
\encoding{UTF-8}

\section{Changes in Version 2.2}{
  RELEASE DATE: 14 September 2015

  \subsection{NEW FEATURES - SURVEY DESIGN}{
    \itemize{
      \item confidence intervals for histograms and bar plots
      \item comparison intervals for histograms broken down by a factor
      \item summary information for all basic plots (histograms, bar plots, and scatter plots)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item fixes a bug where missing information on barplots and scatter plots would cause the plotting function to die
      \item fixes a bug in the printing of summary objects
      \item fixes a bug where the minimum value of a single numeric variable summary was ommited
      \item and various other small bug fixes
    }
  }
}

\section{Changes in Version 2.1}{
  RELEASE DATE: 04 August 2015

  \subsection{NEW FEATURES}{
    \itemize{
      \item allow zooming of plots with the new \code{zoom}
      argument. Works for both univariate and bivariate plots, and a
      related funcionality for 'zooming in' on bars in a barplot. 
    }
  }

  \subsection{PATCH 2.1-1: 28 August 2015}{
    \itemize{
      \item fix a bug that occurs when all survey weights are equal
    }
  }
}


\section{Changes in Version 2.0.6}{
  RELEASE DATE: 03 August 2015

  \subsection{BUG FIXES}{
    \itemize{
      \item fix an issue where requesting summary of 'dotplots' resulted
      in creating a new device, which resulted in errors on the Shiny
      server.
    }
  }
}

\section{Changes in Version 2.0.5}{
  RELEASE DATE: 27 July 2015
  
  \subsection{NEW FEATURES}{
    \itemize{
      \item additional arguments `xlim` and `ylim` allow users to
      specify the range of values shown on the plot
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item several issues for dotplots have been fixed

      \item weighting variable used when drawing a scatter plot of
      survey data

      \item \code{conf} now corresponds to Year 12 intervals in dot plot
      inferences (previously, \code{comp} corresponded to this interval)
    }
  }
}

\section{Changes in Version 2.0.3}{
  RELEASE DATE: 01 July 2015
  
  \subsection{BUG FIXES}{
    \itemize{
      \item fix up the order of bars in segmented bar plots to
      correspond to the legend
    }
  }
}

\section{Changes in Version 2.0.2}{
  RELEASE DATE: 24 June 2015
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item remove facility where the colour-by variable is ignored if
      there are 'too many' levels---this is now left up to users to
      decide if colour by a particular variable makes sense of not.
    }
  }
}

\section{Changes in Version 2.0.1}{
  RELEASE DATE: 16 June 2015
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item Dotplot locating implemented using new methodology, with the
      additional argument `label.extreme = numeric(2)`, allowing users
      to specify how many lower and upper points to identify,
      respectively.

      \item equivalently, extreme points (by using Mahalanobis'
      distance) can be labelled on scatter plots using `label.extreme =
      numeric(1)`.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item fixed a small bug that stopped inference from working in
      dotplots when `x` is a factor and `y` is the numeric variable.

      \item fix a bug that caused `nbins = 0` in some cases.
    }
  }
}

\section{Changes in Version 2.0}{
  Release Date: 26 May 2015
  
  \subsection{MAJOR CHANGES}{
    \itemize{
      \item The entire package has been rewritten to accomodate complex
      survey designs. At present, survey objects are not fully
      supported, however the functionality will be added over time.

      \item A huge reduction in computation requirements for plots to
      increase efficiency.

      \item Algorithms used to compute inference intervals have been
      modified to use iNZightMR for comparisons.

      \item lots of other changes to layout and presentation

      \item added additional arguments \code{locate}, \code{locate.id},
      \code{locate.col} (and others) for locating points by IDs.
      This is used in the improved locator functinoality in the main
      \code{iNZight} program.
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{
      \item the \code{col.by} and \code{size.by} arguments have been
      replaced by \code{colby} and \code{sizeby}

      \item documentation has been added for several of the functions (finally!)
    }
  }
}

\section{Changes in Version 1.0.3}{
  \subsection{BUG FIXES}{
    \itemize{
      \item specifying `g2.level` with numbers wasn't working, has been fixed for plots,
      summary and inference information.

      \item added more space to the y-axis on scatter plots
    }
  }
}

\section{Changes in Version 1.0.2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item An error where the response was printed instead of the
      x-variable name in summary output for quadratic curves has been
      fixed.
    }
  }
}


\section{Changes in Version 1.0.1}{
  \subsection{MINOR CHANGES}{
    \itemize{
      \item The type of plot used can be specified by setting the
      `largesample` argument. When set to `TRUE`, it uses the histogram
      or grid-density plot; when `FALSE` it uses the dotplot or scatter
      plot. If set to `NULL`, it uses the sample size to determine which
      plot to draw (default).

      \item To allow identification features and any additional features
      to be added to plots afterwards, the last viewport is the one
      surrounding the main plot (excluding the plot labels and
      legend). Note that this only works if the data haven't been broken
      down by `g1`.

      \item Display which variables cannot be plot due to too many
      levels, as well as the number of levels, when attempting to draw
      bar plot. (max levels = 101).

      \item Trend lines and smoothers added to the legend.

      \item Alternative method of shading grid-tiles on the grid-density
      plot using quantiles rather than absolute counts. This prevents
      large counts having too large of an influence.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item A bug where the grid-density plot is not using the correct scale
      has been fixed.
    }
  }
}




\section{MAJOR RELEASE VERSION 1.0.0}{
  \itemize{
    \item New major release of iNZightPlots released, completely rewritten
    using `grid`.
  }
}
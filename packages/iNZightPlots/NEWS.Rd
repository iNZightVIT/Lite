\name{NEWS}
\title{NEWS file for the iNZightPlots package}
\encoding{UTF-8}

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
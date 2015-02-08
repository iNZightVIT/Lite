\name{Change Log}
\title{Change Log for vit}
\encoding{UTF-8}

\section{Changes in version 1.2.1}{
  Release date: 18 September 2014
  
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Can now read in data files which have comments in them -
      denoted by #'s
      
    }
  }
}

\section{Changes in version 1.2.0}{
  Release date: 18 January 2014
  
  \subsection{MAJOR CHANGES}{
    \itemize{

      \item Because `iNZight` has been rewritten using `gWidgets2`, we
      needed to rewrite several functions in the `vit` package to ensure
      users could still switch between iNZight and VIT (and back again)
      without restarting their session. The current version involves
      detaching and attaching the correct gWidgets version when iNZight
      is loaded or closed.
      
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{

      \item As with other packages in the iNZightVIT bundle, the version
      numbering has been updated. The previous version was updated from
      beta to version 1.x, and the new release as version 1.2 (this is
      because there is no functional difference between the versions,
      however they are no longer compatible).
      
    }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item Users can now close the graphics device in the VIT module,
      and are able to return to the home screen (previously, they
      experienced a fatal error when trying to return to the home screen
      if they closed the graphics device).
      
    }
  }
}

\section{Changes in version 1.1.0-20130227}{
  \subsection{BUG FIXES}{
    \itemize{

      \item When doing randomisation testing with a 2 level categorical
      variable as the first variable, changing the level of interest
      could cause all data to be plotted as if it belonged to the level
      of interest.

    }
  }
}

\section{Changes in version 1.1.0-20130122}{
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Reverting 'stackPoints' to old behaviour for 'Data' and
      'Sample' panels. Modified slightly so that the "bins" are now half a
      point's width.

      \item Updating mechanism in iNZightVIT improved.

    }
  }
}

\section{Changes in version 1.1.0-20121008}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added updating mechanism to keep VIT up to date without
      grabbing the entire distribution. This can be called by running
      \code{updateVit()}. The iNZightVIT distribution does include a
      script \code{UPDATE_iNZightVIT.bat}
      (\code{UPDATE_iNZightVIT.command} for OSX) to automate this task.

      \item Added a new function \code{vitBugReport()}, which provides
      instructions for creating a bug report.

      \item Version strings are tagged with a date. This is exposed in
      the home screen.
    }
  }

  \subsection{MISC}{
    \itemize{
      \item Reworked the dependency structure of VIT. This should make
      it slightly faster to load and prints less to the console.

      \item Increasing speed of plotting regression by making slopes
      opaque. This is reverting back to the original implementation.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Fix for Mac clients which were selecting the wrong graphics
      device.
    }
  }
}

\section{Changes in version 1.1.0-20120822}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Better handling of large datasets. The data-view is now
      disabled in favour of variable-view only from 200,000 cells.

      \item Added support for showing the theoretical distributions for:
      \itemize{
        \item Differences between two proportions.
        \item Regression.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix for 2 Sample Proportions in the Bootstrapping module
      (when resampling within groups).
    }
  }
}

\section{Changes in version 1.1.0-20120725}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Minor improvements.
    }
  }
}

\section{Changes in version 1.1.0-20120417}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Major enhancements.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Various fixes.
    }
  }
}

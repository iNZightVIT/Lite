\name{Change Log}
\title{Change Log for iNZightRegression}
\encoding{UTF-8}

\section{Changes in version 1.1.4}{
  Release date: 20 July 2015

  \subsection{MINOR HANGES}{
    \itemize{
      \item added an extra argument \code{use.inzightplots} to the
      plotting functions that allows users to enabled/disable the use of
      them as desired. Currently, the default is \code{FALSE} as the
      latest version of \code{iNZightPlots} is incompatible with
      \code{iNZightRegression}.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item added missed function and method exports
    }
  }
}

\section{Changes in version 1.1.3}{
  Release date: 17 September 2014
  
  \subsection{BUG FIXES}{
    \itemize{
      \item on some graphics devices (eg. RStudio) grid.rect() is not
      transparent - now enforces these to be transparent
    }
  }
}

\section{Changes in version 1.1.2}{
  Release date: 4 April 2014
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item Continuing on from previous update, the partial residual
      plots are now modified to make use of the `iNZightPlots` library
      if it is installed.
    }
  }
}

\section{Changes in version 1.1.1}{
  Release date: 27 March 2014
  
  \subsection{NEW FEATURES}{
    \itemize{
      \item Residual summary plots from `plotlm6` can now make use of
      the `iNZightPlots` graphics rather than the defaults. It requires
      the user to have `iNZightPlots` installed to work, but reverts to
      the old plots if it is not.

      \item In the new `grid` based plots, quantile smoothers are used
      rather than loess smoothers. This greatly increases efficiency
      when large data sets are analysed.
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{
      \item Maximum sample size for drawing bootstraps implemented
      (currently at 4000), as over this they don't provide much
      information (this can be overridded by `showBootstraps = TRUE`).

      \item p-values for normality test are printed as "P < 0.001"
      rather than "P = 0".
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Shapiro Wilk test not used if sample size > 5000 (resulted in an
      error).
    }
  }
}

\section{Changes in version 1.1}{
  Release date: 18 January 2014
  
  \subsection{NEW FEATURES}{
    \itemize{
      
      \item Support for generalised lienar models (GLMs) and `svyglm`
      objects from the `survey` package.

      \item Changes to the `iNZightSummary` output include:
      \itemize{
	\item Output now hides output of counfounding
	variables through the `exclude` argument, and lists these at the
	top of the output.

	\item Displays the type of fit (e.g., Suvey / Generalised Linear
	/ Model).
      }

      \item The QQ-plot array has been replaced by a single plot with
      the parametric bootstrap data QQ-plots all on a single plot,
      overlaid by the QQ plot of the true data (this was suggested by
      Thomas Lumley).
    }
  }

  \subsection{CHANGES}{
    \itemize{
      \item The Histogram Array has been rewritten using `grid`, and
      minimises margin whitespace and draws simulated histograms in a
      different color.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item The bootstrap models functions have been re-written to
      account for the `design` option in survey GLMs, as well as the
      case when the GLM binomial response is SUCCESS / N.TRIALS. This
      caused errors in the `fit$model` that was previously being used.

      \item The bootstrap lines from `plotlm6` have been fixed so that
      they now work for `(svy)glm` objects. There is also an optional
      cut-off if the sample size becomes too small (which can be
      overridden by the `showBootstraps = TRUE|FALSE` argument.

      \item `factorMeans` and `adjustedMeans` have be fixed so they work
      for GLMs.
      
    }
  }
}

\section{Changes in version 1.0.2-20130913}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Initial support for generalised linear models (GLMs),
      including the `survey` package's `svyglm`.

    }
  }
}

\section{Changes in version 1.0.2-20130122}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item First release of new package. Contains model fitting subset
      used for the 'iNZight' package.

      \item Added 'histogramArray' and 'qqplotArray' plots to show how
      residuals from a model compare to the residuals *generated* from
      that model.

      \item New margin of error calculation functions. Initially written
      by Danny Chang. Used for comparison between levels of a
      factor. 'moecalc' has a few standard methods that can be used:
      'print', 'plot', and 'summary'. In addition, a 'multicomp' method
      has been added which is a useful tabulation of multiple comparison
      output. Note however with 'multicomp' that the p-values are
      currently *unadjusted*.

      \item New summary output, 'iNZightSummary'. Includes several changes
      compared to the R-base model summary output. These include the
      following:

      \itemize{

        \item Now showing the factor itself in the output, not just rows
        for coefficients for levels of the factor.

        \item When a factor is included in a model, the summary output
        will show the name of the factor and show the p-value for the
        factor (based on Type-III sums of squares). This p-value is not
        affected by further use of the factor (i.e. in an
        interaction). Sometimes this p-value cannot be calculated
        (i.e. when there are unobserved factor level combinations) and the
        p-value will be omitted.

        \item The baseline level of a factor is now shown, with an
        estimate of 0.

        \item All p-value output for levels of a factor is indented to the
        right by two characters to distinguish it from being a level.

        \item The output for each factor level is now just the level name
        and not the name of the variable concatenated with the level
        name. The level name is also indented by two characters, again to
        distinguish it from the variable itself.

        \item Removing F-statistic and associated p-value as it's mostly
        useless. It only shows us whether nothing is correlated with the
        response variable, i.e. whether we're completely wasting our time.

        \item Removed model call output and residual output. Replaced with
        "Model for:" (plus response name).

      }

      \item Added a new 'plot.lm' function. The main difference being that
      it includes bootstrapped smoothers in its output as well as the
      regular trend lines. Also includes plots based on the 's20x'
      package's 'normcheck' function.

      \item Added partial residual plots. Most useful for determining
      whether the inclusion of a transformation of a variable is
      necessary. For example, adding a logged or polynomial explanatory
      variable to the model.

      \item Adding bootstrapped estimates to 'iNZightSummary'. Accessed by
      calling the function with 'method="bootstrap"'.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Using 'loess' for smoothers instead of 'lowess' (newer and
      more robust).
    }
  }
}

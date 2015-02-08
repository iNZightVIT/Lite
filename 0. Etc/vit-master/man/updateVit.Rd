\name{updateVit}
\alias{updateVit}
\title{
  Updating the \sQuote{VIT} system.
}
\description{
  The purpose of this function is to avoid having to download the entire
  distribution that \sQuote{VIT} resides within. Only the components
  that require updating are updated.

  This function automatically updates all of the installed packages that
  are contributed via \sQuote{CRAN}. The latest version of \sQuote{VIT}
  package is downloaded from the \sQuote{VIT} website. Additionally,
  because \sQuote{iNZight} is also packaged with \sQuote{VIT} system we
  will update it too, if it is installed.
}
\usage{
updateVit()
}
\value{
  Nothing is returned from this function.
}
\author{
  Simon Potter
}
\examples{
  \dontrun{updateVit()}
}

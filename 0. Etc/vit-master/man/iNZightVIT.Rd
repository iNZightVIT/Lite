\name{iNZightVIT}
\alias{iNZightVIT}
\alias{vit}
\title{
Visual Inference Tools and iNZight Home
}
\description{
The home screens for VIT and iNZightVIT
}
\usage{
iNZightVIT(disposeR = FALSE)
}
\arguments{
  \item{disposeR}{
    This argument determines whether R is to be quit when the VIT GUI windows are closed.
    This argument should not be specified by a user; it should only be called by an external
    function such as iNZight.
  }
}
\details{
iNZightVIT will only be able to load iNZight if the iNZight package has been installed.
}
\author{
Simon Potter, Ben Stevenson, Garrett Grolemund
}

\examples{
\dontrun{iNZightVIT()}
}

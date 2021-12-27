% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill.R
\name{removeDup}
\alias{removeDup}
\title{Remove duplcated term}
\usage{
removeDup(x, replacement = "")
}
\arguments{
\item{x}{A vector}

\item{replacement}{A character to be replaced or NA}
}
\description{
Remove duplcated term
}
\examples{
x=rep(1:5,each=3)
removeDup(x)
}
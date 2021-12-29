% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crr_formula.R
\name{crr2stats}
\alias{crr2stats}
\title{Extract statistics from an object of class crr}
\usage{
crr2stats(x, digits = 2)
}
\arguments{
\item{x}{an object of class crr}

\item{digits}{integer indication the position of decimal place}
}
\description{
Extract statistics from an object of class crr
}
\examples{
data(melanoma,package="boot")
melanoma$status_crr=ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
x=crr_formula(time+status_crr~age+sex+thickness+ulcer,data=melanoma)
crr2stats(x)
}
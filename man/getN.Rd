% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getInteraction.R
\name{getN}
\alias{getN}
\title{Get number of data specified by 'name' and 'desc'}
\usage{
getN(name, desc, data)
}
\arguments{
\item{name}{a string with interaction term}

\item{desc}{character}

\item{data}{a data.frame}
}
\value{
A numeric vector
}
\description{
Get number of data specified by 'name' and 'desc'
}
\examples{
data(acs,package="moonBook")
df=getInteraction("TC:Dx:sex",data=acs)
getN(name=df$name,desc=df$desc,data=acs)
}

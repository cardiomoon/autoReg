% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit2list.R
\name{fit2list}
\alias{fit2list}
\title{Make a list of univariable model with multivariable regression model}
\usage{
fit2list(fit)
}
\arguments{
\item{fit}{An object of class "lm" or "glm"}
}
\value{
An object of class "fitlist" which is a list of objects of class "lm" or "glm"
}
\description{
Make a list of univariable model with multivariable regression model
}
\examples{
library(survival)
data(cancer)
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
fit2list(fit)
fit=lm(mpg~wt*hp+am,data=mtcars)
fit2list(fit)
}

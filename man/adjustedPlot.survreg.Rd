% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjustedPlot.survreg.R
\name{adjustedPlot.survreg}
\alias{adjustedPlot.survreg}
\title{Draw predicted survival curve with an object survreg}
\usage{
adjustedPlot.survreg(
  x,
  xnames = NULL,
  pred.values = list(),
  maxy.lev = 5,
  median = TRUE,
  newdata = NULL,
  addCox = FALSE,
  legend.position = "topright",
  xlim = NULL,
  ylim = NULL
)
}
\arguments{
\item{x}{An object of class survreg}

\item{xnames}{Character Names of explanatory variable to plot}

\item{pred.values}{A list A list of predictor values}

\item{maxy.lev}{Integer Maximum unique length of a numeric variable to be treated as categorical variables}

\item{median}{Logical}

\item{newdata}{A data.frame or NULL}

\item{addCox}{logical Whether or not add KM}

\item{legend.position}{Character Default value is "topright"}

\item{xlim}{numeric}

\item{ylim}{numeric}
}
\value{
No return value, called for side effects
}
\description{
Draw predicted survival curve with an object survreg
}
\examples{
library(survival)
x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
adjustedPlot(x)
adjustedPlot(x,addCox=TRUE)
\dontrun{
x=survreg(Surv(time, status) ~ sex, data=lung,dist="weibull")
adjustedPlot(x,addCox=TRUE)
x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
adjustedPlot(x)
x=survreg(Surv(time, status) ~ ph.ecog + age + sex, data=lung, dist="weibull")
adjustedPlot(x)
adjustedPlot(x,addCox=TRUE)
adjustedPlot(x,pred.values=list(age=c(20,40,60,80),sex=2,ph.ecog=3),addCox=TRUE)
newdata=data.frame(ph.ecog=0:3,sex=c(1,2,2,2),age=c(20,40,60,80))
adjustedPlot(x,newdata=newdata,addCox=TRUE)
}
}

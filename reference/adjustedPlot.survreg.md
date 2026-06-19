# Draw predicted survival curve with an object survreg

Draw predicted survival curve with an object survreg

## Usage

``` r
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
```

## Arguments

- x:

  An object of class survreg

- xnames:

  Character Names of explanatory variable to plot

- pred.values:

  A list A list of predictor values

- maxy.lev:

  Integer Maximum unique length of a numeric variable to be treated as
  categorical variables

- median:

  Logical

- newdata:

  A data.frame or NULL

- addCox:

  logical Whether or not add KM

- legend.position:

  Character Default value is "topright"

- xlim:

  numeric

- ylim:

  numeric

## Value

No return value, called for side effects

## Examples

``` r
library(survival)
x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
adjustedPlot(x)

adjustedPlot(x,addCox=TRUE)
#> Warning: Removed 42 rows containing missing values or values outside the scale range
#> (`geom_line()`).

if (FALSE) { # \dontrun{
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
} # }
```

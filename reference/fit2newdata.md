# Make a new data of mean value or most frequent value

Make a new data of mean value or most frequent value

## Usage

``` r
fit2newdata(
  fit,
  xnames = NULL,
  pred.values = list(),
  maxy.lev = 5,
  median = TRUE,
  digits = 1
)
```

## Arguments

- fit:

  An object of class "coxph"

- xnames:

  character Names of explanatory variable to plot

- pred.values:

  A list A list of predictor values

- maxy.lev:

  Integer Maximum unique length of a numeric variable to be treated as
  categorical variables

- median:

  logical If TRUE, select median value for numerical variable. Otherwise
  select most frequent value

- digits:

  integer indicating the number of decimal places

## Value

A data.frame

## Examples

``` r
require(survival)
data(cancer,package="survival")
fit=coxph(Surv(time,status)~rx+sex+age,data=colon)
fit=coxph(Surv(time,status)~rx+age+strata(sex),data=colon)
fit=survreg(Surv(time, status) ~ ph.ecog + age + sex, data=lung, dist="weibull")
fit2newdata(fit)
#>   ph.ecog age sex
#> 1       0  63   1
#> 2       1  63   1
#> 3       2  63   1
#> 4       3  63   1
fit2newdata(fit,pred.values=list(sex=0,age=58))
#>   sex age ph.ecog
#> 1   0  58       1
fit2newdata(fit,pred.values=list(age=c(20,40,60,80),sex=2,ph.ecog=3))
#>   age sex ph.ecog
#> 1  20   2       3
#> 2  40   2       3
#> 3  60   2       3
#> 4  80   2       3
```

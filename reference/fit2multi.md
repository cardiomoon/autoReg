# Make multivariable regression model by selecting univariable models with p.value below threshold

Make multivariable regression model by selecting univariable models with
p.value below threshold

## Usage

``` r
fit2multi(fit, threshold = 0.2)
```

## Arguments

- fit:

  An object of class "coxph"

- threshold:

  Numeric

## Value

An object of class "coxph"

## Examples

``` r
require(survival)
data(cancer)
fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
fit2multi(fit)
#> Call:
#> coxph(formula = Surv(time, status) ~ obstruct + perfor, data = colon)
#> 
#>             coef exp(coef) se(coef)     z       p
#> obstruct 0.23061   1.25937  0.08125 2.838 0.00453
#> perfor   0.19189   1.21154  0.18189 1.055 0.29144
#> 
#> Likelihood ratio test=9.7  on 2 df, p=0.007821
#> n= 1858, number of events= 920 
```

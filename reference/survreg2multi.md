# Make multivariable regression model by selecting univariable models with p.value below threshold

Make multivariable regression model by selecting univariable models with
p.value below threshold

## Usage

``` r
survreg2multi(fit, threshold = 0.2)
```

## Arguments

- fit:

  An object of class "survreg"

- threshold:

  Numeric

## Value

An object of class "survreg"

## Examples

``` r
require(survival)
data(cancer)
fit=survreg(Surv(time,status)~rx+age+sex+obstruct+perfor,data=colon)
survreg2multi(fit)
#> Call:
#> survreg(formula = Surv(time, status) ~ rx + obstruct + perfor, 
#>     data = colon, dist = "weibull")
#> 
#> Coefficients:
#> (Intercept)       rxLev   rxLev+5FU    obstruct      perfor 
#>  8.03151673  0.04002865  0.57533922 -0.26448179 -0.26318314 
#> 
#> Scale= 1.243185 
#> 
#> Loglik(model)= -8261.9   Loglik(intercept only)= -8285.7
#>  Chisq= 47.73 on 4 degrees of freedom, p= 1.07e-09 
#> n= 1858 
```

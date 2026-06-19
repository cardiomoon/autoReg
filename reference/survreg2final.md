# Make final model using stepwise backward elimination

Make final model using stepwise backward elimination

## Usage

``` r
survreg2final(fit, threshold = 0.2)
```

## Arguments

- fit:

  An object of class "survreg"

- threshold:

  Numeric

## Value

An object of class "survreg" which is described in
[`survreg`](https://rdrr.io/pkg/survival/man/survreg.html)

## Examples

``` r
require(survival)
data(cancer)
fit=survreg(Surv(time,status)~rx+age+sex+obstruct+perfor,data=colon)
survreg2final(fit)
#> Call:
#> survreg(formula = Surv(time, status) ~ rx + obstruct, data = data1, 
#>     dist = "weibull")
#> 
#> Coefficients:
#> (Intercept)       rxLev   rxLev+5FU    obstruct 
#>  8.02788254  0.03794983  0.57403158 -0.28020085 
#> 
#> Scale= 1.243567 
#> 
#> Loglik(model)= -8262.5   Loglik(intercept only)= -8285.7
#>  Chisq= 46.46 on 3 degrees of freedom, p= 4.53e-10 
#> n= 1858 
```

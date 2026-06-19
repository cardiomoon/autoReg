# Restore fit model data containing AsIs expressions

Restore fit model data containing AsIs expressions

## Usage

``` r
fit2model(fit)
```

## Arguments

- fit:

  An object of class "lm", "glm" or "coxph"

## Value

An object of class "data.frame"

## Examples

``` r
require(survival)
pbc$status2=ifelse(pbc$status==2,1,0)
fit=coxph(Surv(time,status2)~age+log(bili),data=pbc)
fit2model(fit)
#> Error in eval(predvars, data, env): object 'status2' not found
```

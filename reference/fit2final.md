# Make final model using stepwise backward elimination

Make final model using stepwise backward elimination

## Usage

``` r
fit2final(fit, threshold = 0.2)
```

## Arguments

- fit:

  An object of class "coxph"

- threshold:

  Numeric

## Value

An object of class "coxph" which is described in
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)

## Examples

``` r
require(survival)
data(cancer)
fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
final=fit2final(fit)
fit2summary(final)
#>                id                    stats
#> obstruct obstruct 1.27 (1.09-1.49, p=.003)
```

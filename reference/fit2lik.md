# extract likelihood information with a coxph object

extract likelihood information with a coxph object

## Usage

``` r
fit2lik(x)
```

## Arguments

- x:

  An object of class "coxph" or "survreg"

## Value

A string

## Examples

``` r
library(survival)
fit=coxph(Surv(time,status) ~rx,data=anderson)
fit2lik(fit)
#> [1] "n=42, events=30, Likelihood ratio test=16.35 on 1 df (p<.001)"
```

# Fit Simple AFT Model

Fit Simple AFT Model

## Usage

``` r
mysurvregSimple(fit, threshold = 0.2, digits = 2, mode = 1)
```

## Arguments

- fit:

  An object of class survreg

- threshold:

  numeric p-value threshold to enter multiple model

- digits:

  integer indicating the position decimal place

- mode:

  integer

## Value

An object of class "data.frame"

## Examples

``` r
require(survival)
data(cancer)
fit=survreg(Surv(time,status)~rx+age+strata(sex)+obstruct+perfor,data=colon)
mysurvregSimple(fit)
#>          id  Value Std. Error      z     p   ETR    LB    UB    HR lower upper
#> 1     rxLev  0.036      0.095  0.378  .705 1.037 0.860 1.249 0.973 0.844 1.121
#> 2 rxLev+5FU  0.604      0.106  5.693 <.001 1.829 1.485 2.251 0.632 0.539 0.740
#> 3       age  0.003      0.004  0.788  .431 1.003 0.996 1.010 0.998 0.993 1.003
#> 4  obstruct -0.307      0.101 -3.044  .002 0.735 0.603 0.896 1.267 1.088 1.475
#> 5    perfor -0.332      0.226 -1.469  .142 0.717 0.460 1.117 1.293 0.918 1.823
#>       p1                    stats      sig
#> 1 p=.705 1.04 (0.86-1.25, p=.705)       rx
#> 2 p<.001 1.83 (1.49-2.25, p<.001)       rx
#> 3 p=.431 1.00 (1.00-1.01, p=.431)     <NA>
#> 4 p=.002 0.73 (0.60-0.90, p=.002) obstruct
#> 5 p=.142 0.72 (0.46-1.12, p=.142)   perfor
```

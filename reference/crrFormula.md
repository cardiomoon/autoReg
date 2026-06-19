# Competing Risk Regression with Formula

Competing Risk Regression with Formula

## Usage

``` r
crrFormula(x, data, ...)
```

## Arguments

- x:

  formula time+status~explanatory variables

- data:

  data a data.frame

- ...:

  Further arguments to be passed to
  [`crr`](https://mskcc-epi-bio.github.io/tidycmprsk/reference/crr.html)

## Value

An object of class "tidycrr" which is described in
[`crr`](https://mskcc-epi-bio.github.io/tidycmprsk/reference/crr.html)

## Examples

``` r
data(melanoma,package="boot")
melanoma$status_crr=ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
crrFormula(time+status_crr~age+sex+thickness+ulcer,data=melanoma)
#> 
#> ── crr() ───────────────────────────────────────────────────────────────────────
#> • Call survival::Surv(time, status_crr) ~ age + sex + thickness + ulcer
#> • Failure type of interest "1"
#> 
#> Variable    Coef    SE      HR     95% CI       p-value    
#> age         0.006   0.009   1.01   0.99, 1.02   0.52       
#> sex         0.405   0.276   1.50   0.87, 2.57   0.14       
#> thickness   0.090   0.038   1.09   1.01, 1.18   0.019      
#> ulcer       1.13    0.303   3.09   1.71, 5.60   <0.001     
```

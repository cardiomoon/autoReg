# Makes table summarizing list of models

Makes table summarizing list of models

## Usage

``` r
modelsSummary(fitlist, show.lik = FALSE)
```

## Arguments

- fitlist:

  A list of objects of class "coxph"

- show.lik:

  logical Whether or not show likelihood test results

## Value

No return value, called for side effects

## Examples

``` r
library(survival)
fit1=coxph(Surv(time,status) ~rx,data=anderson)
fit2=coxph(Surv(time,status) ~rx+logWBC,data=anderson)
fit3=coxph(Surv(time,status) ~rx*logWBC,data=anderson)
fitlist=list(fit1,fit2,fit3)
modelsSummary(fitlist)
#> 
#> Dependent: Surv(time, status), data=anderson 
#> ———————————————————————————————————————————————————————————————————————
#>                coef  se(coef)       z  Pr(>|z|)      HR  lower    upper 
#> ———————————————————————————————————————————————————————————————————————
#> Model 1 
#> rx            1.572     0.412   3.812     <.001   4.817  2.147   10.809 
#> ———————————————————————————————————————————————————————————————————————
#> Model 2 
#> rx            1.386     0.425   3.263      .001   3.999  1.739    9.195 
#> logWBC        1.691     0.336   5.034     <.001   5.424  2.808   10.478 
#> ———————————————————————————————————————————————————————————————————————
#> Model 3 
#> rx            2.375     1.705   1.393      .164  10.750  0.380  304.162 
#> logWBC        1.872     0.451   4.148     <.001   6.504  2.685   15.755 
#> rx:logWBC    -0.318     0.526  -0.604      .546   0.728  0.260    2.040 
#> ———————————————————————————————————————————————————————————————————————
```

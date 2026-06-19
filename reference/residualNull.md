# Make a residual plot of NULL model

Make a residual plot of NULL model

## Usage

``` r
residualNull(x, add.log = TRUE, type = "martingale")
```

## Arguments

- x:

  An object of calss coxph

- add.log:

  logical If true, log of predictor varaibles are added

- type:

  character type of residuals

## Examples

``` r
library(survival)
data(pharmacoSmoking,package="asaur")
pharmacoSmoking$priorAttemptsT=pharmacoSmoking$priorAttempts
pharmacoSmoking$priorAttemptsT[pharmacoSmoking$priorAttemptsT>20]=20
x=coxph(Surv(ttr,relapse)~age+priorAttemptsT+longestNoSmoke,data=pharmacoSmoking)
residualNull(x)
#> Error in eval(predvars, data, env): object 'priorAttemptsT' not found
```

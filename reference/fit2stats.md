# Summarize statistics with a model

Summarize statistics with a model

## Usage

``` r
fit2stats(fit, method = "default", digits = 2, mode = 1)
```

## Arguments

- fit:

  An object of class lm or glm or coxph or survreg

- method:

  character choices are one of the c("likelihood","wald")

- digits:

  integer indicating the number of decimal places

- mode:

  integer

## Value

An object of class "data.frame"

## Examples

``` r
library(survival)
data(cancer)
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
fit2stats(fit)
#>               OR lower upper      p          id                    stats
#> (Intercept) 0.53  0.30  0.92 0.0246 (Intercept) 0.53 (0.30-0.92, p=.025)
#> rxLev       0.94  0.74  1.18 0.5757       rxLev 0.94 (0.74-1.18, p=.576)
#> rxLev+5FU   0.53  0.42  0.68 0.0000   rxLev+5FU 0.53 (0.42-0.68, p<.001)
#> sex         0.95  0.78  1.15 0.5885         sex 0.95 (0.78-1.15, p=.589)
#> age         1.00  0.99  1.01 0.5826         age 1.00 (0.99-1.01, p=.583)
#> obstruct    1.34  1.05  1.72 0.0178    obstruct 1.34 (1.05-1.72, p=.018)
#> nodes       1.21  1.17  1.25 0.0000       nodes 1.21 (1.17-1.25, p<.001)
fit=lm(mpg~wt*hp+am,data=mtcars)
fit2stats(fit)
#>                      id    Estimate        lower       upper
#> (Intercept) (Intercept) 49.45224079  38.61707633 60.28740526
#> wt                   wt -8.10055755 -11.77194963 -4.42916547
#> hp                   hp -0.11930318  -0.17377926 -0.06482709
#> am                   am  0.12510693  -2.61086742  2.86108128
#> wt:hp             wt:hp  0.02748826   0.01010407  0.04487245
#>                                       stats
#> (Intercept)  49.45 (38.62 to 60.29, p<.001)
#> wt          -8.10 (-11.77 to -4.43, p<.001)
#> hp           -0.12 (-0.17 to -0.06, p<.001)
#> am             0.13 (-2.61 to 2.86, p=.926)
#> wt:hp           0.03 (0.01 to 0.04, p=.003)
fit=survreg(Surv(time,status)~rx+sex+age+obstruct+nodes,data=colon)
fit2stats(fit)
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#>                 Value  Std. Error        z      p       ETR        LB        UB     HR  lower  upper      p1                              stats 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> (Intercept)     8.487       0.225   37.769  <.001  4851.758  3123.393  7536.535  0.001  0.001  0.001  p<.001  4851.76 (3123.39-7536.53, p<.001) 
#> rxLev           0.088       0.092    0.950   .342     1.092     0.911     1.309  0.929  0.797  1.082  p=.342           1.09 (0.91-1.31, p=.342) 
#> rxLev+5FU       0.577       0.102    5.656  <.001     1.780     1.458     2.174  0.615  0.520  0.728  p<.001           1.78 (1.46-2.17, p<.001) 
#> sex             0.081       0.080    1.011   .312     1.084     0.927     1.268  0.934  0.819  1.066  p=.312           1.08 (0.93-1.27, p=.312) 
#> age            -0.002       0.003   -0.614   .539     0.998     0.991     1.005  1.002  0.996  1.007  p=.539           1.00 (0.99-1.00, p=.539) 
#> obstruct       -0.281       0.098   -2.868   .004     0.755     0.624     0.915  1.266  1.078  1.488  p=.004           0.76 (0.62-0.92, p=.004) 
#> nodes          -0.111       0.008  -14.398  <.001     0.895     0.881     0.908  1.098  1.084  1.112  p<.001           0.90 (0.88-0.91, p<.001) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Weibull distribution 
#> Loglik(model)= -7989.4   Loglik(intercept only)= -8088 
#>  Chisq= 197.36 on 6 degrees of freedom, p= <2e-16 
#> n=1822 (36 observations deleted due to missingness)
#>  
```

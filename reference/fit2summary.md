# Summarize statistics with a model or model list

Summarize statistics with a model or model list

## Usage

``` r
fit2summary(fit, mode = 1, ...)
```

## Arguments

- fit:

  An object of class "lm" or "glm" or "fitlist" which is a result of
  [`fit2list`](https://cardiomoon.github.io/autoReg/reference/fit2list.md)

- mode:

  integer

- ...:

  Further argument to be passed to fit2stats

## Value

An object of class "data.frame"

## Examples

``` r
library(survival)
data(cancer)
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
fit2summary(fit)
#>                      id                    stats
#> (Intercept) (Intercept) 0.53 (0.30-0.92, p=.025)
#> rxLev             rxLev 0.94 (0.74-1.18, p=.576)
#> rxLev+5FU     rxLev+5FU 0.53 (0.42-0.68, p<.001)
#> sex                 sex 0.95 (0.78-1.15, p=.589)
#> age                 age 1.00 (0.99-1.01, p=.583)
#> obstruct       obstruct 1.34 (1.05-1.72, p=.018)
#> nodes             nodes 1.21 (1.17-1.25, p<.001)
fitlist=fit2list(fit)
fit2summary(fitlist)
#>                  id                    stats
#> rxLev         rxLev 0.96 (0.77-1.20, p=.709)
#> rxLev+5FU rxLev+5FU 0.55 (0.44-0.68, p<.001)
#> sex             sex 0.97 (0.81-1.17, p=.758)
#> age             age 1.00 (0.99-1.00, p=.296)
#> obstruct   obstruct 1.30 (1.03-1.63, p=.028)
#> nodes         nodes 1.21 (1.17-1.25, p<.001)
fit=survreg(Surv(time,status)~rx+sex+age+obstruct+nodes,data=colon)
fit2summary(fit)
#>            id                             stats
#> 1 (Intercept) 4851.76 (3123.39-7536.53, p<.001)
#> 2       rxLev          1.09 (0.91-1.31, p=.342)
#> 3   rxLev+5FU          1.78 (1.46-2.17, p<.001)
#> 4         sex          1.08 (0.93-1.27, p=.312)
#> 5         age          1.00 (0.99-1.00, p=.539)
#> 6    obstruct          0.76 (0.62-0.92, p=.004)
#> 7       nodes          0.90 (0.88-0.91, p<.001)
```

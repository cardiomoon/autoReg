# Perform univariable and multivariable regression and stepwise backward regression automatically

Perform univariable and multivariable regression and stepwise backward
regression automatically

## Usage

``` r
autoReg_sub(
  fit,
  threshold = 0.2,
  uni = FALSE,
  multi = TRUE,
  final = FALSE,
  imputed = FALSE,
  keepstats = FALSE,
  showstats = TRUE,
  ...
)
```

## Arguments

- fit:

  An object of class lm or glm

- threshold:

  numeric

- uni:

  logical whether or not perform univariate regression

- multi:

  logical whether or not perform multivariate regression

- final:

  logical whether or not perform stepwise backward elimination

- imputed:

  logical whether or not include imputed model

- keepstats:

  logical whether or not keep statistics

- showstats:

  logical whether or not show descriptive statistics

- ...:

  Further arguments to be passed to imputedReg()

## Value

An object of class "autoReg" which inherits from the class "data.frame"
with at least the following attributes:

- attr(\*,"yvars):

  character. name of dependent variable

- attr(\*,"model"):

  name of model. One of "lm","glm" or "coxph"

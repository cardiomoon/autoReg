# Fit Simple Proportional Hazards Regression Model

Fit Simple Proportional Hazards Regression Model

## Usage

``` r
mycphSimple(fit, threshold = 0.2, digits = 2)
```

## Arguments

- fit:

  An object of class coxph

- threshold:

  numeric p-value threshold to enter multiple model

- digits:

  integer indicating the position decimal place

## Value

An object of class "data.frame"

## Examples

``` r
require(survival)
data(cancer)
fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
mycphSimple(fit)
#>                  coef        HR     lower    upper          se          z
#> age      -0.002444104 0.9975589 0.9921094 1.003038 0.002794865 -0.8744983
#> sex      -0.033625890 0.9669332 0.8496235 1.100440 0.065988994 -0.5095682
#> obstruct  0.242055841 1.2738653 1.0881772 1.491240 0.080385064  3.0112042
#> perfor    0.264368788 1.3026085 0.9154312 1.853541 0.179966997  1.4689848
#>                    p       id                    stats
#> age      0.381846947      age 1.00 (0.99-1.00, p=.382)
#> sex      0.610354035      sex 0.97 (0.85-1.10, p=.610)
#> obstruct 0.002602138 obstruct 1.27 (1.09-1.49, p=.003)
#> perfor   0.141836904   perfor 1.30 (0.92-1.85, p=.142)
```

# Convert data.frame to printable format

Convert data.frame to printable format

## Usage

``` r
myformat(x, showid = FALSE, digits = 3)
```

## Arguments

- x:

  A data.frame

- showid:

  logical if TRUE, show id

- digits:

  Integer indicating the number of decimal places

## Value

A data.frame

## Examples

``` r
fit=lm(mpg~wt*hp,data=mtcars)
gaze(fit) %>% myformat()
#> ———————————————————————————————————————————————————————————————————————
#>                Estimate  Std. Error  t value  Pr(>|t|)    lower   upper 
#> ———————————————————————————————————————————————————————————————————————
#> (Intercept)      49.808       3.605   13.816     <.001   42.424  57.193 
#> wt               -8.217       1.270   -6.471     <.001  -10.818  -5.616 
#> hp               -0.120       0.025   -4.863     <.001   -0.171  -0.070 
#> wt:hp             0.028       0.007    3.753     <.001    0.013   0.043 
#> ———————————————————————————————————————————————————————————————————————
#> Residual SE: 2.153 on 28 DF, Multiple R^2: 0.8848, Adjusted R^2: 0.8724
#> F-statistic: 71.66 on 3 and 28 DF,  p-value: 2.981e-13 
```

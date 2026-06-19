# Summarize numeric vector to statistical summary

Summarize numeric vector to statistical summary

## Usage

``` r
num2stat(x, digits = 1, method = 1, p = NULL)
```

## Arguments

- x:

  A numeric vector

- digits:

  integer indicating the number of decimal places

- method:

  An integer indicating methods for continuous variables. Possible
  values in methods are 1 forces analysis as normal-distributed 2 forces
  analysis as continuous non-normal 3 performs a Shapiro-Wilk test or
  nortest::ad.test to decide between normal or non-normal Default value
  is 1.

- p:

  A numeric

## Value

A character vector of length 1

## Examples

``` r
library(moonBook)
num2stat(acs$age)
#> [1] "63.3 ± 11.7"
num2stat(acs$age,method=2)
#> [1] "64.0 (55.0 to 72.0)"
```

# Statistical test for continuous variables

Statistical test for continuous variables

## Usage

``` r
my.t.test2(y, x, method = 1, all = FALSE)
```

## Arguments

- y:

  a categorical vector

- x:

  a numeric vector

- method:

  method An integer indicating methods for continuous variables.
  Possible values in methods are 1 forces analysis as normal-distributed
  2 forces analysis as continuous non-normal 3 performs a Shapiro-Wilk
  test or nortest::ad.test to decide between normal or non-normal
  Default value is 1.

- all:

  A logical

## Value

A numeric vector of length 1

## Examples

``` r
library(moonBook)
y=acs$sex
x=acs$height
my.t.test2(y,x)
#> [1] 2.137925e-130
```

# Statistical test for categorical variables Statistical test for categorical variables

Statistical test for categorical variables Statistical test for
categorical variables

## Usage

``` r
my.chisq.test2(x, y, catMethod = 2, all = FALSE)
```

## Arguments

- x:

  a vector

- y:

  a vector

- catMethod:

  An integer indicating methods for categorical variables. Possible
  values in methods are

  0

  :   Perform chisq.test first. If warning present, perform fisher test

  1

  :   Perform chisq.test without continuity correction

  2

  :   Perform chisq.test with continuity correction

  3

  :   perform fisher.test

  4

  :   perform prop.trend test

  Default value is 2.

- all:

  A logical

## Value

A numeric vector of length 1

## Examples

``` r
library(moonBook)
x=acs$sex
y=acs$Dx
my.chisq.test2(x,y)
#> [1] 0.01228804
```

# Decide whether a vector can be treated as a numeric variable

Decide whether a vector can be treated as a numeric variable

## Usage

``` r
is.mynumeric(x, maxy.lev = 5)
```

## Arguments

- x:

  A vector

- maxy.lev:

  An integer indicating the maximum number of unique values of a numeric
  variable be treated as a categorical variable

## Value

A logical value

## Examples

``` r
x=1:5
is.mynumeric(x)
#> [1] FALSE
x=1:13
is.mynumeric(x)
#> [1] TRUE
```

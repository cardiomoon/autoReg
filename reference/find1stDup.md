# Find first duplicated position

Find first duplicated position

## Usage

``` r
find1stDup(x)
```

## Arguments

- x:

  a vector

## Value

A logical vector

## Examples

``` r
x=rep(1:5,each=3)
which(find1stDup(x))
#> [1]  1  4  7 10 13
```

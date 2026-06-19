# Find duplicated term

Find duplicated term

## Usage

``` r
findDup(x)
```

## Arguments

- x:

  A vector

## Value

A logical vector

## Examples

``` r
x=rep(1:5,each=3)
findDup(x)
#>  [1] FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE
#> [13] FALSE  TRUE  TRUE
x=c(6,x)
findDup(x)
#>  [1] FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
#> [13]  TRUE FALSE  TRUE  TRUE
which(!findDup(x))
#> [1]  1  2  5  8 11 14
```

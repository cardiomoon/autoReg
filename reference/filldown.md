# filldown vector with lead value

filldown vector with lead value

## Usage

``` r
filldown(x, what = c("", NA))
```

## Arguments

- x:

  a vector

- what:

  Values to be filled

## Value

A vector with the same class as x

## Examples

``` r
x=rep(1:5,each=3)
x=removeDup(x,NA)
filldown(x)
#>  [1] 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5
```

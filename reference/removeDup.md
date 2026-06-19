# Remove duplicated term

Remove duplicated term

## Usage

``` r
removeDup(x, replacement = "")
```

## Arguments

- x:

  A vector

- replacement:

  A character to be replaced or NA

## Value

A vector with the same class as x

## Examples

``` r
x=rep(1:5,each=3)
removeDup(x)
#>  [1] "1" ""  ""  "2" ""  ""  "3" ""  ""  "4" ""  ""  "5" ""  "" 
```

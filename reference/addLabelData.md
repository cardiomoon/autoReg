# Add labels to data

Add labels to data

## Usage

``` r
addLabelData(data)
```

## Arguments

- data:

  A data.frame

## Value

A data.frame

## Examples

``` r
addLabelData(data.frame(ph.ecog=0:3,sex=c(1,2,2,2),age=c(20,30,40,70)))
#>   ph.ecog sex age
#> 1       0   1  20
#> 2       1   2  30
#> 3       2   2  40
#> 4       3   2  70
```

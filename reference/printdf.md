# Print function for data.frame

Print function for data.frame

## Usage

``` r
printdf(x)
```

## Arguments

- x:

  A data.frame

## Value

No return value, called for side effects

## Examples

``` r
x=mtcars[1:5,1:5]
printdf(x)
#> ————————————————————————————
#> mpg     cyl  disp   hp  drat 
#> ————————————————————————————
#> 21        6   160  110   3.9 
#> 21        6   160  110   3.9 
#> 22.8      4   108   93  3.85 
#> 21.4      6   258  110  3.08 
#> 18.7      8   360  175  3.15 
#> ————————————————————————————
```

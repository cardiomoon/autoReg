# Count groups

Count groups

## Usage

``` r
countGroups(data, yvars)
```

## Arguments

- data:

  A data.frame

- yvars:

  variable names

## Value

An object of class "tibble"

## Examples

``` r
library(moonBook)
countGroups(acs,"sex")
#> # A tibble: 2 × 2
#>   sex    n      
#>   <chr>  <chr>  
#> 1 Female (N=287)
#> 2 Male   (N=570)
countGroups(acs,c("sex","Dx"))
#> # A tibble: 6 × 3
#>   sex    Dx              n      
#>   <chr>  <chr>           <chr>  
#> 1 Female NSTEMI          (N=50) 
#> 2 Female STEMI           (N=84) 
#> 3 Female Unstable Angina (N=153)
#> 4 Male   NSTEMI          (N=103)
#> 5 Male   STEMI           (N=220)
#> 6 Male   Unstable Angina (N=247)
```

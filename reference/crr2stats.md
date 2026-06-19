# Extract statistics from an object of class crr

Extract statistics from an object of class crr

## Usage

``` r
crr2stats(x, digits = 2)
```

## Arguments

- x:

  an object of class crr

- digits:

  integer indication the position of decimal place

## Value

An object of class "data.frame"

## Examples

``` r
data(melanoma,package="boot")
melanoma$status_crr=ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
x=crrFormula(time+status_crr~age+sex+thickness+ulcer,data=melanoma)
crr2stats(x)
#>         HR     lower    upper      p        id                    stats
#> 1 1.005945 0.9877942 1.024430 p=.520       age 1.01 (0.99-1.02, p=.520)
#> 2 1.499349 0.8736380 2.573204 p=.140       sex 1.50 (0.87-2.57, p=.140)
#> 3 1.094169 1.0149120 1.179615 p=.019 thickness 1.09 (1.01-1.18, p=.019)
#> 4 3.091416 1.7055657 5.603335 p<.001     ulcer 3.09 (1.71-5.60, p<.001)
```

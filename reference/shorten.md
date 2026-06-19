# Shorten an object of class gaze

Shorten an object of class gaze

## Usage

``` r
shorten(x, xname = NULL, ref = 1)
```

## Arguments

- x:

  an object of class gaze

- xname:

  A variable name

- ref:

  Numeric Th number to be used as reference

## Value

An object of class "gaze" which is described in
[`gaze`](https://cardiomoon.github.io/autoReg/reference/gaze.md)

## Examples

``` r
data(acs,package="moonBook")
x=gaze(sex~.,data=acs)
shorten(x)
#> ————————————————————————————————————————————————————————————————————————
#>   Dependent:sex        levels           Female          Male        p   
#>        (N)                             (N=287)        (N=570)           
#> ————————————————————————————————————————————————————————————————————————
#> age               Mean ± SD             68.7 ± 10.7   60.6 ± 11.2  <.001 
#> cardiogenicShock  Yes                     12 (4.2%)       40 (7%)   .136 
#> entry             Radial                168 (58.5%)   377 (66.1%)   .035 
#> Dx                STEMI                  84 (29.3%)   220 (38.6%)   .012 
#>                   Unstable Angina       153 (53.3%)   247 (43.3%)        
#> EF                Mean ± SD             56.3 ± 10.1    55.6 ± 9.4   .387 
#> height            Mean ± SD             153.8 ± 6.2   167.9 ± 6.1  <.001 
#> weight            Mean ± SD              57.2 ± 9.3   68.7 ± 10.3  <.001 
#> BMI               Mean ± SD              24.2 ± 3.6    24.3 ± 3.2   .611 
#> obesity           Yes                    93 (32.4%)   197 (34.6%)   .580 
#> TC                Mean ± SD            188.9 ± 51.1  183.3 ± 45.9   .124 
#> LDLC              Mean ± SD            117.8 ± 41.2  116.0 ± 41.1   .561 
#> HDLC              Mean ± SD             39.0 ± 11.5   37.8 ± 10.9   .145 
#> TG                Mean ± SD            119.9 ± 76.2  127.9 ± 97.3   .195 
#> DM                Yes                   114 (39.7%)   190 (33.3%)   .077 
#> HBP               Yes                   204 (71.1%)   297 (52.1%)  <.001 
#> smoking           Never                 209 (72.8%)   123 (21.6%)  <.001 
#>                   Smoker                 29 (10.1%)   292 (51.2%)        
#> ————————————————————————————————————————————————————————————————————————
```

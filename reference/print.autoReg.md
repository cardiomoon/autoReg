# S3 method print for an object of class autoReg

S3 method print for an object of class autoReg

## Usage

``` r
# S3 method for class 'autoReg'
print(x, ...)
```

## Arguments

- x:

  An object of class autoReg

- ...:

  Further arguments

## Value

No return value, called for side effects

## Examples

``` r
data(cancer,package="survival")
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
autoReg(fit)
#> ——————————————————————————————————————————————————————————————————————————————————
#> Dependent: status                 0 (N=938)    1 (N=920)        OR (multivariable) 
#> ——————————————————————————————————————————————————————————————————————————————————
#> rx                         Obs  285 (30.4%)  345 (37.5%)                           
#>                            Lev  287 (30.6%)  333 (36.2%)  0.94 (0.74-1.18, p=.576) 
#>                        Lev+5FU    366 (39%)  242 (26.3%)  0.53 (0.42-0.68, p<.001) 
#> sex                  Mean ± SD    0.5 ± 0.5    0.5 ± 0.5  0.95 (0.78-1.15, p=.589) 
#> age                  Mean ± SD  60.0 ± 11.5  59.5 ± 12.4  1.00 (0.99-1.01, p=.583) 
#> obstruct             Mean ± SD    0.2 ± 0.4    0.2 ± 0.4  1.34 (1.05-1.72, p=.018) 
#> nodes                Mean ± SD    2.7 ± 2.4    4.6 ± 4.2  1.21 (1.17-1.25, p<.001) 
#> ——————————————————————————————————————————————————————————————————————————————————
```

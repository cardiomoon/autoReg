# Perform univariable and multivariable regression and stepwise backward regression automatically

Perform univariable and multivariable regression and stepwise backward
regression automatically

## Usage

``` r
autoReg(x, ...)

# S3 method for class 'lm'
autoReg(x, ...)

# S3 method for class 'glm'
autoReg(x, ...)

# S3 method for class 'coxph'
autoReg(x, ...)

# S3 method for class 'survreg'
autoReg(x, ...)
```

## Arguments

- x:

  An object of class lm, glm or coxph

- ...:

  Further arguments

## Value

autoReg returns an object of class "autoReg" which inherits from the
class "data.frame" with at least the following attributes:

- attr(\*,"yvars):

  character. name of dependent variable

- attr(\*,"model"):

  name of model. One of "lm","glm" or "coxph"

## Methods (by class)

- `autoReg(lm)`: S3 method for a class lm

- `autoReg(glm)`: S3 method for a class glm

- `autoReg(coxph)`: S3 method for a class coxph

- `autoReg(survreg)`: S3 method for a class survreg

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
autoReg(fit,uni=FALSE,final=TRUE)
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: status                 0 (N=938)    1 (N=920)        OR (multivariable)                OR (final) 
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> rx                         Obs  285 (30.4%)  345 (37.5%)                                                     
#>                            Lev  287 (30.6%)  333 (36.2%)  0.94 (0.74-1.18, p=.576)  0.94 (0.74-1.18, p=.575) 
#>                        Lev+5FU    366 (39%)  242 (26.3%)  0.53 (0.42-0.68, p<.001)  0.54 (0.42-0.68, p<.001) 
#> sex                  Mean ± SD    0.5 ± 0.5    0.5 ± 0.5  0.95 (0.78-1.15, p=.589)                           
#> age                  Mean ± SD  60.0 ± 11.5  59.5 ± 12.4  1.00 (0.99-1.01, p=.583)                           
#> obstruct             Mean ± SD    0.2 ± 0.4    0.2 ± 0.4  1.34 (1.05-1.72, p=.018)  1.34 (1.05-1.71, p=.019) 
#> nodes                Mean ± SD    2.7 ± 2.4    4.6 ± 4.2  1.21 (1.17-1.25, p<.001)  1.21 (1.17-1.25, p<.001) 
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————
autoReg(fit,uni=FALSE,imputed=TRUE)
#> Warning: Number of logged events: 1
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: status                 0 (N=938)    1 (N=920)        OR (multivariable)              OR (imputed) 
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> rx                         Obs  285 (30.4%)  345 (37.5%)                                                     
#>                            Lev  287 (30.6%)  333 (36.2%)  0.94 (0.74-1.18, p=.576)  0.95 (0.76-1.20, p=.692) 
#>                        Lev+5FU    366 (39%)  242 (26.3%)  0.53 (0.42-0.68, p<.001)  0.54 (0.43-0.68, p<.001) 
#> sex                  Mean ± SD    0.5 ± 0.5    0.5 ± 0.5  0.95 (0.78-1.15, p=.589)  0.97 (0.80-1.17, p=.736) 
#> age                  Mean ± SD  60.0 ± 11.5  59.5 ± 12.4  1.00 (0.99-1.01, p=.583)  1.00 (0.99-1.01, p=.642) 
#> obstruct             Mean ± SD    0.2 ± 0.4    0.2 ± 0.4  1.34 (1.05-1.72, p=.018)  1.36 (1.06-1.73, p=.014) 
#> nodes                Mean ± SD    2.7 ± 2.4    4.6 ± 4.2  1.21 (1.17-1.25, p<.001)  1.21 (1.17-1.25, p<.001) 
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fit=lm(mpg~wt*hp+am+I(wt^2),data=mtcars)
autoReg(fit,final=TRUE)
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: mpg                         unit         value      Coefficient (multivariable)              Coefficient (final) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> wt                     [1.5,5.4]  Mean ± SD     3.2 ± 1.0  -7.67 (-14.54 to -0.81, p=.030)  -8.22 (-10.82 to -5.62, p<.001) 
#> hp                      [52,335]  Mean ± SD  146.7 ± 68.6   -0.13 (-0.25 to -0.01, p=.040)   -0.12 (-0.17 to -0.07, p<.001) 
#> am                         [0,1]  Mean ± SD     0.4 ± 0.5     0.17 (-2.68 to 3.01, p=.905)                                  
#> I(wt^2)           interpretation                             -0.11 (-1.57 to 1.36, p=.880)                                  
#> wt:hp                                                         0.03 (-0.01 to 0.07, p=.100)      0.03 (0.01 to 0.04, p<.001) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
autoReg(fit,imputed=TRUE)
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: mpg                         unit         value      Coefficient (multivariable)           Coefficients (imputed) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> wt                     [1.5,5.4]  Mean ± SD     3.2 ± 1.0  -7.67 (-14.54 to -0.81, p=.030)  -7.67 (-14.57 to -0.78, p=.031) 
#> hp                      [52,335]  Mean ± SD  146.7 ± 68.6   -0.13 (-0.25 to -0.01, p=.040)   -0.13 (-0.25 to -0.01, p=.041) 
#> am                         [0,1]  Mean ± SD     0.4 ± 0.5     0.17 (-2.68 to 3.01, p=.905)     0.17 (-2.69 to 3.02, p=.905) 
#> I(wt^2)           interpretation                             -0.11 (-1.57 to 1.36, p=.880)    -0.11 (-1.58 to 1.36, p=.880) 
#> wt:hp                                                         0.03 (-0.01 to 0.07, p=.100)     0.03 (-0.01 to 0.07, p=.101) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```

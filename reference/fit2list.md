# Make a list of univariable model with multivariable regression model

Make a list of univariable model with multivariable regression model

## Usage

``` r
fit2list(fit)
```

## Arguments

- fit:

  An object of class "lm" or "glm"

## Value

An object of class "fitlist" which is a list of objects of class "lm" or
"glm"

## Examples

``` r
library(survival)
data(cancer)
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
fit2list(fit)
#> [[1]]
#> 
#> Call:  glm(formula = as.formula(myformula), family = family, data = data)
#> 
#> Coefficients:
#> (Intercept)        rxLev    rxLev+5FU  
#>     0.19106     -0.04239     -0.60475  
#> 
#> Degrees of Freedom: 1857 Total (i.e. Null);  1855 Residual
#> Null Deviance:       2576 
#> Residual Deviance: 2541  AIC: 2547
#> 
#> [[2]]
#> 
#> Call:  glm(formula = as.formula(myformula), family = family, data = data)
#> 
#> Coefficients:
#> (Intercept)          sex  
#>   -0.004494    -0.028566  
#> 
#> Degrees of Freedom: 1857 Total (i.e. Null);  1856 Residual
#> Null Deviance:       2576 
#> Residual Deviance: 2575  AIC: 2579
#> 
#> [[3]]
#> 
#> Call:  glm(formula = as.formula(myformula), family = family, data = data)
#> 
#> Coefficients:
#> (Intercept)          age  
#>    0.223567    -0.004066  
#> 
#> Degrees of Freedom: 1857 Total (i.e. Null);  1856 Residual
#> Null Deviance:       2576 
#> Residual Deviance: 2574  AIC: 2578
#> 
#> [[4]]
#> 
#> Call:  glm(formula = as.formula(myformula), family = family, data = data)
#> 
#> Coefficients:
#> (Intercept)     obstruct  
#>    -0.06945      0.25891  
#> 
#> Degrees of Freedom: 1857 Total (i.e. Null);  1856 Residual
#> Null Deviance:       2576 
#> Residual Deviance: 2571  AIC: 2575
#> 
#> [[5]]
#> 
#> Call:  glm(formula = as.formula(myformula), family = family, data = data)
#> 
#> Coefficients:
#> (Intercept)        nodes  
#>     -0.6886       0.1891  
#> 
#> Degrees of Freedom: 1821 Total (i.e. Null);  1820 Residual
#>   (36 observations deleted due to missingness)
#> Null Deviance:       2525 
#> Residual Deviance: 2382  AIC: 2386
#> 
#> attr(,"class")
#> [1] "fitlist"
fit=lm(mpg~wt*hp+am,data=mtcars)
fit2list(fit)
#> [[1]]
#> 
#> Call:
#> lm(formula = as.formula(myformula), data = data)
#> 
#> Coefficients:
#> (Intercept)           wt  
#>      37.285       -5.344  
#> 
#> 
#> [[2]]
#> 
#> Call:
#> lm(formula = as.formula(myformula), data = data)
#> 
#> Coefficients:
#> (Intercept)           hp  
#>    30.09886     -0.06823  
#> 
#> 
#> [[3]]
#> 
#> Call:
#> lm(formula = as.formula(myformula), data = data)
#> 
#> Coefficients:
#> (Intercept)           am  
#>      17.147        7.245  
#> 
#> 
#> [[4]]
#> 
#> Call:
#> lm(formula = as.formula(myformula), data = data)
#> 
#> Coefficients:
#> (Intercept)        wt:hp  
#>    27.74564     -0.01487  
#> 
#> 
#> attr(,"class")
#> [1] "fitlist"
```

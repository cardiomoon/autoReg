# Get explanatory variables of a model with significance level below the threshold

Get explanatory variables of a model with significance level below the
threshold

## Usage

``` r
getSigVars(fit, threshold = 0.2, final = TRUE)
```

## Arguments

- fit:

  An object of class lm or glm

- threshold:

  Numeric

- final:

  logical if true, perform stepwise regression using step()

## Value

A list containing the following components:

- sigVars:

  names of explanatory variables which have significant levels below the
  threshold in univariable model

- finalVars:

  names of explanatory variables included in final model as a result of
  [`step`](https://rdrr.io/r/stats/step.html)

## Examples

``` r
library(survival)
data(cancer,package="survival")
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
getSigVars(fit)
#> $sigVars
#> [1] "rx"       "obstruct" "nodes"   
#> 
#> $finalVars
#> [1] "rx"       "obstruct" "nodes"   
#> 
fit=lm(mpg~hp*wt+am,data=mtcars)
getSigVars(fit)
#> $sigVars
#> [1] "hp"    "wt"    "am"    "hp:wt"
#> 
#> $finalVars
#> [1] "hp"    "wt"    "hp:wt"
#> 
```

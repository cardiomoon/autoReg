# Draw Cumulative Incidence Curves for Competing Risks

Draw Cumulative Incidence Curves for Competing Risks

## Usage

``` r
ggcmprsk(x, data, id = NULL, se = FALSE, strata = NULL, facet = NULL, ...)
```

## Arguments

- x:

  A formula as time+status~1

- data:

  A data.frame

- id:

  character vector label for status

- se:

  logical whether or not show confidence interval

- strata:

  character vector label for strata

- facet:

  numeric if facet is not NULL, draw plot with selected facets

- ...:

  Further arguments to be passed to tidycmprsk::cuminc

## Value

An object of class "ggplot"

## Examples

``` r
data(melanoma,package="boot")
melanoma$status1 = ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
melanoma$years=melanoma$time/365
# \donttest{
ggcmprsk(years+status1~1,data=melanoma)

ggcmprsk(years+status1~1,data=melanoma,id=c("alive","melanoma","other"),se=TRUE)
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_stepribbon()`).

ggcmprsk(years+status1~sex,data=melanoma)
#> Warning: Removed 8 rows containing missing values or values outside the scale range
#> (`geom_step()`).

ggcmprsk(years+status1~sex,data=melanoma,facet=1)
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_step()`).

ggcmprsk(years+status1~sex,data=melanoma,
id=c("alive","melanoma","other"),strata=c("female","male"))
#> Warning: Removed 8 rows containing missing values or values outside the scale range
#> (`geom_step()`).

ggcmprsk(years+status1~sex,data=melanoma,
id=c("alive","melanoma","other"),strata=c("female","male"),facet=1)
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_step()`).

# }
```

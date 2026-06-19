# Summary function for continuous variable

Summary function for continuous variable

## Usage

``` r
gazeCont(
  data,
  x,
  y = NULL,
  max.ylev = 5,
  digits = 1,
  show.total = FALSE,
  show.n = FALSE,
  show.missing = FALSE,
  show.stats = TRUE,
  show.p = TRUE,
  method = 1,
  origData,
  ...
)
```

## Arguments

- data:

  A data.frame

- x:

  A name of variable

- y:

  A name of variable, either continuous or categorical

- max.ylev:

  max.ylev An integer indicating the maximum number of levels of
  grouping variable ('y'). If a column have unique values less than
  max.ylev it is treated as a categorical variable. Default value is 5.

- digits:

  integer indicating the number of decimal places

- show.total:

  logical. Whether or not show total column

- show.n:

  logical. Whether or not show N column

- show.missing:

  logical. Whether or not show missing column

- show.stats:

  logical. Whether or not show stats column

- show.p:

  logical. Whether or not show p column

- method:

  method An integer indicating methods for continuous variables.
  Possible values in methods are 1 forces analysis as normal-distributed
  2 forces analysis as continuous non-normal 3 performs a Shapiro-Wilk
  test or nortest::ad.test to decide between normal or non-normal
  Default value is 1.

- origData:

  A data.frame containing original data

- ...:

  Further arguments

## Value

An object of class "data.frame" or "tibble"

## Examples

``` r
gazeCont(mtcars,"hp")
#>   name      desc        stats       type id
#> 1   hp Mean ± SD 146.7 ± 68.6 continuous hp
gazeCont(mtcars,"hp","mpg")
#>   name     desc      unit        value       type id
#> 1   hp [52,335] Mean ± SD 146.7 ± 68.6 continuous hp
require(moonBook)
gazeCont(acs,"log(age)")
#>       name      desc     stats       type       id
#> 1 log(age) Mean ± SD 4.1 ± 0.2 continuous log(age)
gazeCont(acs,"age",method=2)
#>   name         desc               stats       type  id
#> 1  age Median (IQR) 64.0 (55.0 to 72.0) continuous age
gazeCont(acs,"age","EF",method=2)
#>   name    desc         unit               value       type  id
#> 1  age [28,91] Median (IQR) 64.0 (55.0 to 72.0) continuous age
gazeCont(acs,"age","Dx",method=1)
#> # A tibble: 1 × 8
#>   name  desc      `NSTEMI (N=153)` `STEMI (N=304)` Unstable Angina (N=40…¹ p    
#>   <chr> <chr>     <chr>            <chr>           <chr>                   <chr>
#> 1 age   Mean ± SD 64.3 ± 12.3      62.1 ± 12.1     63.8 ± 11.0             .073 
#> # ℹ abbreviated name: ¹​`Unstable Angina (N=400)`
#> # ℹ 2 more variables: type <chr>, id <chr>
gazeCont(acs,"age","Dx",show.p=TRUE,method=3)
#> # A tibble: 1 × 8
#>   name  desc       `NSTEMI (N=153)` `STEMI (N=304)` Unstable Angina (N=4…¹ p    
#>   <chr> <chr>      <chr>            <chr>           <chr>                  <chr>
#> 1 age   Median (I… 65.0 (55.0 to 7… 62.0 (53.0 to … 65.0 (56.0 to 72.0)    .109 
#> # ℹ abbreviated name: ¹​`Unstable Angina (N=400)`
#> # ℹ 2 more variables: type <chr>, id <chr>
```

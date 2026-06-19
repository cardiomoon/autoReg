# Summary function for categorical variable

Summary function for categorical variable

## Usage

``` r
gazeCat(
  data,
  x,
  y = NULL,
  max.ylev = 5,
  digits = 1,
  show.total = FALSE,
  show.n = FALSE,
  show.missing = FALSE,
  show.stats = TRUE,
  origData = NULL,
  show.p = TRUE,
  method = 1,
  catMethod = 2,
  maxCatLevel = 20,
  ...
)
```

## Arguments

- data:

  A data frame

- x:

  Name of a categorical variable

- y:

  Name of a variable, either continuous or categorical

- max.ylev:

  max.ylev An integer indicating the maximum number of levels of
  grouping variable ('y'). If a column have unique values less than
  max.ylev it is treated as a categorical variable. Default value is 5.

- digits:

  Numeric

- show.total:

  logical. Whether or not show total column

- show.n:

  logical. Whether or not show N column

- show.missing:

  logical. Whether or not show missing column

- show.stats:

  logical. Whether or not show stats column

- origData:

  A data.frame containing original data

- show.p:

  logical. Whether or not show p column

- method:

  method An integer indicating methods for continuous variables.
  Possible values in methods are 1 forces analysis as normal-distributed
  2 forces analysis as continuous non-normal 3 performs a Shapiro-Wilk
  test or nortest::ad.test to decide between normal or non-normal
  Default value is 1.

- catMethod:

  An integer indicating methods for categorical variables. Possible
  values in methods are

  0

  :   Perform chisq.test first. If warning present, perform fisher test

  1

  :   Perform chisq.test without continuity correction

  2

  :   Perform chisq.test with continuity correction

  3

  :   perform fisher.test

  4

  :   perform prop.trend test

  Default value is 2.

- maxCatLevel:

  An integer indicating the maximum number of unique levels of
  categorical variable. If a column have unique values more than
  maxCatLevel, categorical summarization will not be performed.

- ...:

  Further arguments

## Value

An object of class "data.frame" or "tibble"

## Examples

``` r
require(moonBook)
gazeCat(acs,"Dx")
#>   name            desc       stats                id        type
#> 1   Dx          NSTEMI 153 (17.9%)          DxNSTEMI categorical
#> 2                STEMI 304 (35.5%)           DxSTEMI categorical
#> 3      Unstable Angina 400 (46.7%) DxUnstable Angina categorical
gazeCat(acs,"Dx","smoking")
#> # A tibble: 3 × 8
#>   name  desc    `Ex-smoker (N=204)` `Never (N=332)` `Smoker (N=321)` id    p    
#>   <chr> <chr>   <chr>               <chr>           <chr>            <chr> <chr>
#> 1 "Dx"  NSTEMI  42 (20.6%)          50 (15.1%)      61 (19%)         DxNS… "<.0…
#> 2 ""    STEMI   66 (32.4%)          97 (29.2%)      141 (43.9%)      DxST… ""   
#> 3 ""    Unstab… 96 (47.1%)          185 (55.7%)     119 (37.1%)      DxUn… ""   
#> # ℹ 1 more variable: type <chr>
gazeCat(acs,"sex","Dx",show.p=TRUE)
#> # A tibble: 2 × 8
#>   name  desc   `NSTEMI (N=153)` `STEMI (N=304)` `Unstable Angina (N=400)` id    
#>   <chr> <chr>  <chr>            <chr>           <chr>                     <chr> 
#> 1 "sex" Female 50 (32.7%)       84 (27.6%)      153 (38.2%)               sexFe…
#> 2 ""    Male   103 (67.3%)      220 (72.4%)     247 (61.8%)               sexMa…
#> # ℹ 2 more variables: p <chr>, type <chr>
gazeCat(acs,"Dx","sex",show.p=TRUE)
#> # A tibble: 3 × 7
#>   name  desc            `Female (N=287)` `Male (N=570)` id           p     type 
#>   <chr> <chr>           <chr>            <chr>          <chr>        <chr> <chr>
#> 1 "Dx"  NSTEMI          50 (17.4%)       103 (18.1%)    DxNSTEMI     ".01… cate…
#> 2 ""    STEMI           84 (29.3%)       220 (38.6%)    DxSTEMI      ""    cate…
#> 3 ""    Unstable Angina 153 (53.3%)      247 (43.3%)    DxUnstable … ""    cate…
gazeCat(acs,"Dx","EF")
#> # A tibble: 3 × 6
#>   name  desc                    unit      value      id                type     
#>   <chr> <chr>                   <chr>     <chr>      <chr>             <chr>    
#> 1 "Dx"  NSTEMI          (N=153) Mean ± SD 55.0 ± 9.3 DxNSTEMI          categori…
#> 2 ""    STEMI           (N=304) Mean ± SD 52.4 ± 9.5 DxSTEMI           categori…
#> 3 ""    Unstable Angina (N=400) Mean ± SD 59.2 ± 8.7 DxUnstable Angina categori…
gazeCat(acs,"sex","EF",method=2)
#> # A tibble: 2 × 6
#>   name  desc           unit         value               id        type       
#>   <chr> <chr>          <chr>        <chr>               <chr>     <chr>      
#> 1 "sex" Female (N=287) Median (IQR) 59.2 (51.4 to 63.1) sexFemale categorical
#> 2 ""    Male   (N=570) Median (IQR) 57.3 (50.0 to 61.8) sexMale   categorical
gazeCat(mtcars,"cyl","hp")
#> # A tibble: 3 × 6
#>   name  desc     unit      value        id    type       
#>   <chr> <chr>    <chr>     <chr>        <chr> <chr>      
#> 1 "cyl" 4 (N=11) Mean ± SD 82.6 ± 20.9  cyl4  categorical
#> 2 ""    6 (N=7)  Mean ± SD 122.3 ± 24.3 cyl6  categorical
#> 3 ""    8 (N=14) Mean ± SD 209.2 ± 51.0 cyl8  categorical
```

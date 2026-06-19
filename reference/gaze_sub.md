# Summary function for categorical/continuous variable

Summary function for categorical/continuous variable

## Usage

``` r
gaze_sub(data, xname, y = NULL, max.ylev = 5, autoCat = FALSE, ...)
```

## Arguments

- data:

  A data.frame

- xname:

  A name of categorical/continuous vector

- y:

  A name of vector, either continuous or categorical

- max.ylev:

  max.ylev An integer indicating the maximum number of levels of
  grouping variable ('y'). If a column have unique values less than
  max.ylev it is treated as a categorical variable. Default value is 5.

- autoCat:

  logical Whether or not use is.mynumeric() to determine whether a
  variable is numeric or not

- ...:

  Further arguments to be passed to gazeCont() or gazeCat()

## Value

An object of class "data.frame" or "tibble"

## Examples

``` r
require(moonBook)
gaze_sub(acs,"age")
#>   name      desc       stats       type  id
#> 1  age Mean ± SD 63.3 ± 11.7 continuous age
gaze_sub(acs,"log(age)")
#>       name      desc     stats       type       id
#> 1 log(age) Mean ± SD 4.1 ± 0.2 continuous log(age)
gaze_sub(acs,"I(age^2)")
#>       name      desc       stats       type       id
#> 1 I(age^2) Mean ± SD 63.3 ± 11.7 continuous I(age^2)
gaze_sub(acs,"sex")
#>   name   desc       stats        id        type
#> 1  sex Female 287 (33.5%) sexFemale categorical
#> 2        Male 570 (66.5%)   sexMale categorical
gaze_sub(acs,"age","EF")
#>   name    desc      unit       value       type  id
#> 1  age [28,91] Mean ± SD 63.3 ± 11.7 continuous age
gaze_sub(acs,"sex","EF")
#> # A tibble: 2 × 6
#>   name  desc           unit      value       id        type       
#>   <chr> <chr>          <chr>     <chr>       <chr>     <chr>      
#> 1 "sex" Female (N=287) Mean ± SD 56.3 ± 10.1 sexFemale categorical
#> 2 ""    Male   (N=570) Mean ± SD 55.6 ± 9.4  sexMale   categorical
gaze_sub(acs,"age","Dx")
#> # A tibble: 1 × 8
#>   name  desc      `NSTEMI (N=153)` `STEMI (N=304)` Unstable Angina (N=40…¹ p    
#>   <chr> <chr>     <chr>            <chr>           <chr>                   <chr>
#> 1 age   Mean ± SD 64.3 ± 12.3      62.1 ± 12.1     63.8 ± 11.0             .073 
#> # ℹ abbreviated name: ¹​`Unstable Angina (N=400)`
#> # ℹ 2 more variables: type <chr>, id <chr>
gaze_sub(acs,"sex","Dx")
#> # A tibble: 2 × 8
#>   name  desc   `NSTEMI (N=153)` `STEMI (N=304)` `Unstable Angina (N=400)` id    
#>   <chr> <chr>  <chr>            <chr>           <chr>                     <chr> 
#> 1 "sex" Female 50 (32.7%)       84 (27.6%)      153 (38.2%)               sexFe…
#> 2 ""    Male   103 (67.3%)      220 (72.4%)     247 (61.8%)               sexMa…
#> # ℹ 2 more variables: p <chr>, type <chr>
gaze_sub(iris,"Species","Sepal.Length")
#> # A tibble: 3 × 6
#>   name      desc              unit      value     id                type       
#>   <chr>     <chr>             <chr>     <chr>     <chr>             <chr>      
#> 1 "Species" setosa     (N=50) Mean ± SD 5.0 ± 0.4 Speciessetosa     categorical
#> 2 ""        versicolor (N=50) Mean ± SD 5.9 ± 0.5 Speciesversicolor categorical
#> 3 ""        virginica  (N=50) Mean ± SD 6.6 ± 0.6 Speciesvirginica  categorical
gaze_sub(mtcars,"am")
#>   name      desc     stats       type id
#> 1   am Mean ± SD 0.4 ± 0.5 continuous am
gaze_sub(mtcars,"am",autoCat=TRUE)
#>   name desc      stats  id        type
#> 1   am    0 19 (59.4%) am0 categorical
#> 2         1 13 (40.6%) am1 categorical
```

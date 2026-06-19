# Convert a numeric column in a data.frame to a factor

Convert a numeric column in a data.frame to a factor

## Usage

``` r
num2factor(data, call, name, no = 3)
```

## Arguments

- data:

  A data.frame

- call:

  a function call

- name:

  character Name of numeric column

- no:

  numeric

## Value

A data.frame

## Examples

``` r
num2factor(anderson,name="logWBC")
#>    time status sex logWBC rx
#> 1    35      0   1    Low  0
#> 2    34      0   1    Low  0
#> 3    32      0   1    Low  0
#> 4    32      0   1  Medum  0
#> 5    25      0   1    Low  0
#> 6    23      1   1  Medum  0
#> 7    22      1   1    Low  0
#> 8    20      0   1    Low  0
#> 9    19      0   0    Low  0
#> 10   17      0   0    Low  0
#> 11   16      1   1   High  0
#> 12   13      1   0  Medum  0
#> 13   11      0   0  Medum  0
#> 14   10      0   0  Medum  0
#> 15   10      1   0  Medum  0
#> 16    9      0   0  Medum  0
#> 17    7      1   0   High  0
#> 18    6      0   0  Medum  0
#> 19    6      1   0    Low  0
#> 20    6      1   1   High  0
#> 21    6      1   0   High  0
#> 22   23      1   1    Low  1
#> 23   22      1   0  Medum  1
#> 24   17      1   0  Medum  1
#> 25   15      1   0    Low  1
#> 26   12      1   0    Low  1
#> 27   12      1   0  Medum  1
#> 28   11      1   0   High  1
#> 29   11      1   0    Low  1
#> 30    8      1   0   High  1
#> 31    8      1   0  Medum  1
#> 32    8      1   0    Low  1
#> 33    8      1   1   High  1
#> 34    5      1   1   High  1
#> 35    5      1   0   High  1
#> 36    4      1   1   High  1
#> 37    4      1   1  Medum  1
#> 38    3      1   1   High  1
#> 39    2      1   1   High  1
#> 40    2      1   1   High  1
#> 41    1      1   1  Medum  1
#> 42    1      1   1   High  1
library(survival)
fit=coxph(Surv(time,status)~logWBC+rx,data=anderson)
num2factor(anderson,call=fit$call,name="logWBC",no=2)
#>    time status sex         logWBC rx
#> 1    35      0   1 logWBC <= 2.73  0
#> 2    34      0   1 logWBC <= 2.73  0
#> 3    32      0   1 logWBC <= 2.73  0
#> 4    32      0   1 logWBC <= 2.73  0
#> 5    25      0   1 logWBC <= 2.73  0
#> 6    23      1   1 logWBC <= 2.73  0
#> 7    22      1   1 logWBC <= 2.73  0
#> 8    20      0   1 logWBC <= 2.73  0
#> 9    19      0   0 logWBC <= 2.73  0
#> 10   17      0   0 logWBC <= 2.73  0
#> 11   16      1   1  logWBC > 2.73  0
#> 12   13      1   0  logWBC > 2.73  0
#> 13   11      0   0 logWBC <= 2.73  0
#> 14   10      0   0 logWBC <= 2.73  0
#> 15   10      1   0  logWBC > 2.73  0
#> 16    9      0   0  logWBC > 2.73  0
#> 17    7      1   0  logWBC > 2.73  0
#> 18    6      0   0  logWBC > 2.73  0
#> 19    6      1   0 logWBC <= 2.73  0
#> 20    6      1   1  logWBC > 2.73  0
#> 21    6      1   0  logWBC > 2.73  0
#> 22   23      1   1 logWBC <= 2.73  1
#> 23   22      1   0 logWBC <= 2.73  1
#> 24   17      1   0  logWBC > 2.73  1
#> 25   15      1   0 logWBC <= 2.73  1
#> 26   12      1   0 logWBC <= 2.73  1
#> 27   12      1   0  logWBC > 2.73  1
#> 28   11      1   0  logWBC > 2.73  1
#> 29   11      1   0 logWBC <= 2.73  1
#> 30    8      1   0  logWBC > 2.73  1
#> 31    8      1   0  logWBC > 2.73  1
#> 32    8      1   0 logWBC <= 2.73  1
#> 33    8      1   1  logWBC > 2.73  1
#> 34    5      1   1  logWBC > 2.73  1
#> 35    5      1   0  logWBC > 2.73  1
#> 36    4      1   1  logWBC > 2.73  1
#> 37    4      1   1 logWBC <= 2.73  1
#> 38    3      1   1  logWBC > 2.73  1
#> 39    2      1   1  logWBC > 2.73  1
#> 40    2      1   1  logWBC > 2.73  1
#> 41    1      1   1  logWBC > 2.73  1
#> 42    1      1   1  logWBC > 2.73  1
```

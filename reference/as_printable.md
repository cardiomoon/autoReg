# Convert data.frame to printable form

Calculate character length and apply all data

## Usage

``` r
as_printable(
  data,
  align.first = "left",
  align.chr = "right",
  align.num = "right"
)
```

## Arguments

- data:

  A data.frame

- align.first:

  character Alignment of first variable

- align.chr:

  character Alignment of character variable

- align.num:

  character Alignment of numeric variable

## Value

A data.frame

## Examples

``` r
as_printable(mtcars)
#> # A tibble: 32 × 11
#>    `mpg   ` `  cyl` `   disp` `   hp` `  drat` `     wt` `   qsec` `  vs` `  am`
#>    <chr>    <chr>   <chr>     <chr>   <chr>    <chr>     <chr>     <chr>  <chr> 
#>  1 "21    " "    6" "    160" "  110" "   3.9" "   2.62" "  16.46" "   0" "   1"
#>  2 "21    " "    6" "    160" "  110" "   3.9" "  2.875" "  17.02" "   0" "   1"
#>  3 "22.8  " "    4" "    108" "   93" "  3.85" "   2.32" "  18.61" "   1" "   1"
#>  4 "21.4  " "    6" "    258" "  110" "  3.08" "  3.215" "  19.44" "   1" "   0"
#>  5 "18.7  " "    8" "    360" "  175" "  3.15" "   3.44" "  17.02" "   0" "   0"
#>  6 "18.1  " "    6" "    225" "  105" "  2.76" "   3.46" "  20.22" "   1" "   0"
#>  7 "14.3  " "    8" "    360" "  245" "  3.21" "   3.57" "  15.84" "   0" "   0"
#>  8 "24.4  " "    4" "  146.7" "   62" "  3.69" "   3.19" "     20" "   1" "   0"
#>  9 "22.8  " "    4" "  140.8" "   95" "  3.92" "   3.15" "   22.9" "   1" "   0"
#> 10 "19.2  " "    6" "  167.6" "  123" "  3.92" "   3.44" "   18.3" "   1" "   0"
#> # ℹ 22 more rows
#> # ℹ 2 more variables: `  gear` <chr>, `  carb` <chr>
as_printable(iris)
#> Warning: invalid factor level, NA generated
#> # A tibble: 150 × 5
#>    `Sepal.Length  ` `  Sepal.Width` `  Petal.Length` `  Petal.Width`
#>    <chr>            <chr>           <chr>            <chr>          
#>  1 "5.1           " "          3.5" "           1.4" "          0.2"
#>  2 "4.9           " "            3" "           1.4" "          0.2"
#>  3 "4.7           " "          3.2" "           1.3" "          0.2"
#>  4 "4.6           " "          3.1" "           1.5" "          0.2"
#>  5 "5             " "          3.6" "           1.4" "          0.2"
#>  6 "5.4           " "          3.9" "           1.7" "          0.4"
#>  7 "4.6           " "          3.4" "           1.4" "          0.3"
#>  8 "5             " "          3.4" "           1.5" "          0.2"
#>  9 "4.4           " "          2.9" "           1.4" "          0.2"
#> 10 "4.9           " "          3.1" "           1.5" "          0.1"
#> # ℹ 140 more rows
#> # ℹ 1 more variable: `     Species` <chr>
```

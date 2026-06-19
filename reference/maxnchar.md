# Return maximum character number except NA

Return maximum character number except NA

## Usage

``` r
maxnchar(x)
```

## Arguments

- x:

  a vector

## Value

A numeric vector of length 1

## Examples

``` r
x=c(1,2,"sadf",NA)
maxnchar(x)
#> [1] 4
data(acs,package="moonBook")
lapply(acs,maxnchar)
#> $age
#> [1] 2
#> 
#> $sex
#> [1] 6
#> 
#> $cardiogenicShock
#> [1] 3
#> 
#> $entry
#> [1] 7
#> 
#> $Dx
#> [1] 15
#> 
#> $EF
#> [1] 4
#> 
#> $height
#> [1] 5
#> 
#> $weight
#> [1] 5
#> 
#> $BMI
#> [1] 11
#> 
#> $obesity
#> [1] 3
#> 
#> $TC
#> [1] 5
#> 
#> $LDLC
#> [1] 3
#> 
#> $HDLC
#> [1] 2
#> 
#> $TG
#> [1] 3
#> 
#> $DM
#> [1] 3
#> 
#> $HBP
#> [1] 3
#> 
#> $smoking
#> [1] 9
#> 
```

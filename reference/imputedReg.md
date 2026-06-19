# Make a multiple imputed model

Make a multiple imputed model

## Usage

``` r
imputedReg(fit, data = NULL, m = 20, seed = 1234, digits = 2, mode = 1, ...)
```

## Arguments

- fit:

  An object of class lm, glm, coxph or survreg

- data:

  a data.frame

- m:

  Number of multiple imputations. The default is m=20.

- seed:

  An integer that is used as argument by the set.seed() for offsetting
  the random number generator.

- digits:

  Integer indicating the number of decimal place

- mode:

  integer indicating summary mode of class survreg

- ...:

  Further argument to be passed to mice

## Value

An object of class "imputedReg" which inherits from the class
"data.frame"

## Examples

``` r
data(cancer,package="survival")
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
imputedReg(fit)
#> Warning: Number of logged events: 1
#>            id     estimate   std.error  statistic       df      p.value
#> 1 (Intercept) -0.638897928 0.281963378 -2.2658898 1833.346 2.357429e-02
#> 2       rxLev -0.046738499 0.118157562 -0.3955608 1848.303 6.924747e-01
#> 3   rxLev+5FU -0.618091905 0.120613104 -5.1245833 1845.110 3.294030e-07
#> 4         sex -0.033089561 0.098174322 -0.3370490 1846.866 7.361182e-01
#> 5         age  0.001938722 0.004171101  0.4647986 1841.764 6.421306e-01
#> 6    obstruct  0.304585914 0.123567787  2.4649297 1845.777 1.379447e-02
#> 7       nodes  0.190713187 0.018143728 10.5112457 1575.211 5.050332e-25
#>          2.5 %      97.5 %     conf.low   conf.high        OR     lower
#> 1 -1.191901079 -0.08589478 -1.191901079 -0.08589478 0.5278739 0.3036435
#> 2 -0.278474816  0.18499782 -0.278474816  0.18499782 0.9543369 0.7569373
#> 3 -0.854644419 -0.38153939 -0.854644419 -0.38153939 0.5389719 0.4254344
#> 4 -0.225633881  0.15945476 -0.225633881  0.15945476 0.9674519 0.7980102
#> 5 -0.006241862  0.01011931 -0.006241862  0.01011931 1.0019406 0.9937776
#> 6  0.062238584  0.54693324  0.062238584  0.54693324 1.3560634 1.0642162
#> 7  0.155124788  0.22630159  0.155124788  0.22630159 1.2101123 1.1678037
#>       upper                    stats
#> 1 0.9176908 0.53 (0.30-0.92, p=.024)
#> 2 1.2032158 0.95 (0.76-1.20, p=.692)
#> 3 0.6828095 0.54 (0.43-0.68, p<.001)
#> 4 1.1728712 0.97 (0.80-1.17, p=.736)
#> 5 1.0101707 1.00 (0.99-1.01, p=.642)
#> 6 1.7279457 1.36 (1.06-1.73, p=.014)
#> 7 1.2539538 1.21 (1.17-1.25, p<.001)
# \donttest{
library(survival)
fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
imputedReg(fit)
#>          id     estimate   std.error  statistic       df      p.value
#> 1     rxLev -0.046507478 0.077155431 -0.6027765 909.7921 5.468076e-01
#> 2 rxLev+5FU -0.438763591 0.084184633 -5.2119202 909.3767 2.313569e-07
#> 3       age  0.001417281 0.002781573  0.5095252 909.2875 6.105078e-01
#> 4       sex -0.068785665 0.066475870 -1.0347464 909.8595 3.010623e-01
#> 5     nodes  0.087202231 0.006279040 13.8878295 883.8862 8.235433e-40
#> 6  obstruct  0.241574969 0.081489439  2.9644942 910.5266 3.110847e-03
#> 7    perfor  0.210557594 0.181821453  1.1580459 910.9548 2.471488e-01
#>          2.5 %      97.5 %     conf.low   conf.high        HR     lower
#> 1 -0.197930789  0.10491583 -0.197930789  0.10491583 0.9545574 0.8204266
#> 2 -0.603982339 -0.27354484 -0.603982339 -0.27354484 0.6448332 0.5466304
#> 3 -0.004041767  0.00687633 -0.004041767  0.00687633 1.0014183 0.9959664
#> 4 -0.199249525  0.06167820 -0.199249525  0.06167820 0.9335267 0.8193454
#> 5  0.074878665  0.09952580  0.074878665  0.09952580 1.0911173 1.0777534
#> 6  0.081646014  0.40150392  0.081646014  0.40150392 1.2732529 1.0850716
#> 7 -0.146280017  0.56739520 -0.146280017  0.56739520 1.2343661 0.8639158
#>       upper                    stats
#> 1 1.1106171 0.95 (0.82-1.11, p=.547)
#> 2 0.7606782 0.64 (0.55-0.76, p<.001)
#> 3 1.0069000 1.00 (1.00-1.01, p=.611)
#> 4 1.0636200 0.93 (0.82-1.06, p=.301)
#> 5 1.1046470 1.09 (1.08-1.10, p<.001)
#> 6 1.4940700 1.27 (1.09-1.49, p=.003)
#> 7 1.7636671 1.23 (0.86-1.76, p=.247)
fit=survreg(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
imputedReg(fit)
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#>            id    estimate   std.error   statistic       df       p.value
#> 1 (Intercept)  8.50461911 0.222671708  38.1935325 1841.598 1.408113e-235
#> 2       rxLev  0.06988118 0.091912834   0.7602984 1843.989  4.471735e-01
#> 3   rxLev+5FU  0.55888983 0.100834060   5.5426691 1843.323  3.408684e-08
#> 4         age -0.00209593 0.003326308  -0.6301070 1842.304  5.287027e-01
#> 5         sex  0.07209732 0.079204503   0.9102680 1844.358  3.628001e-01
#> 6       nodes -0.11186792 0.007733528 -14.4653137 1771.476  6.546275e-45
#> 7    obstruct -0.28273306 0.097359527  -2.9040102 1845.818  3.727954e-03
#> 8      perfor -0.27592907 0.216732569  -1.2731315 1846.889  2.031316e-01
#> 9  Log(scale)  0.17496747 0.029384771   5.9543586 1845.528  3.118156e-09
#>          2.5 %      97.5 %     conf.low   conf.high          ETR        lower
#> 1  8.067903559  8.94133466  8.067903559  8.94133466 4937.5232060 3190.4063153
#> 2 -0.110382988  0.25014535 -0.110382988  0.25014535    1.0723808    0.8954911
#> 3  0.361128851  0.75665081  0.361128851  0.75665081    1.7487300    1.4349483
#> 4 -0.008619661  0.00442780 -0.008619661  0.00442780    0.9979063    0.9914174
#> 5 -0.083242592  0.22743724 -0.083242592  0.22743724    1.0747599    0.9201279
#> 6 -0.127035716 -0.09670012 -0.127035716 -0.09670012    0.8941624    0.8807022
#> 7 -0.473679431 -0.09178668 -0.473679431 -0.09178668    0.7537210    0.6227068
#> 8 -0.700995660  0.14913753 -0.700995660  0.14913753    0.7588668    0.4960911
#> 9  0.117336576  0.23259836  0.117336576  0.23259836    1.1912075    1.1244978
#>          upper                             stats
#> 1 7641.3889017 4937.52 (3190.41-7641.39, p<.001)
#> 2    1.2842121          1.07 (0.90-1.28, p=.447)
#> 3    2.1311267          1.75 (1.43-2.13, p<.001)
#> 4    1.0044376          1.00 (0.99-1.00, p=.529)
#> 5    1.2553786          1.07 (0.92-1.26, p=.363)
#> 6    0.9078282          0.89 (0.88-0.91, p<.001)
#> 7    0.9122997          0.75 (0.62-0.91, p=.004)
#> 8    1.1608326          0.76 (0.50-1.16, p=.203)
#> 9    1.2618746          1.19 (1.12-1.26, p<.001)
imputedReg(fit,mode=2)
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#> Warning: The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
#>            id    estimate   std.error   statistic       df       p.value
#> 1 (Intercept)  8.50461911 0.222671708  38.1935325 1841.598 1.408113e-235
#> 2       rxLev  0.06988118 0.091912834   0.7602984 1843.989  4.471735e-01
#> 3   rxLev+5FU  0.55888983 0.100834060   5.5426691 1843.323  3.408684e-08
#> 4         age -0.00209593 0.003326308  -0.6301070 1842.304  5.287027e-01
#> 5         sex  0.07209732 0.079204503   0.9102680 1844.358  3.628001e-01
#> 6       nodes -0.11186792 0.007733528 -14.4653137 1771.476  6.546275e-45
#> 7    obstruct -0.28273306 0.097359527  -2.9040102 1845.818  3.727954e-03
#> 8      perfor -0.27592907 0.216732569  -1.2731315 1846.889  2.031316e-01
#> 9  Log(scale)  0.17496747 0.029384771   5.9543586 1845.528  3.118156e-09
#>          2.5 %      97.5 %     conf.low   conf.high           HR       lower
#> 1  8.067903559  8.94133466  8.067903559  8.94133466 0.0007737485 0.001117819
#> 2 -0.110382988  0.25014535 -0.110382988  0.25014535 0.9428315172 1.097446670
#> 3  0.361128851  0.75665081  0.361128851  0.75665081 0.6244981035 0.737703146
#> 4 -0.008619661  0.00442780 -0.008619661  0.00442780 1.0017671644 1.007287600
#> 5 -0.083242592  0.22743724 -0.083242592  0.22743724 0.9410730124 1.072640419
#> 6 -0.127035716 -0.09670012 -0.127035716 -0.09670012 1.0988203229 1.112950366
#> 7 -0.473679431 -0.09178668 -0.473679431 -0.09178668 1.2689292220 1.490372401
#> 8 -0.700995660  0.14913753 -0.700995660  0.14913753 1.2616769555 1.804920377
#> 9  0.117336576  0.23259836  0.117336576  0.23259836 0.8629556087 0.905884058
#>          upper                    stats
#> 1 0.0005355848 0.00 (0.00-0.00, p<.001)
#> 2 0.8099995149 0.94 (1.10-0.81, p=.447)
#> 3 0.5286650646 0.62 (0.74-0.53, p<.001)
#> 4 0.9962769831 1.00 (1.01-1.00, p=.529)
#> 5 0.8256433367 0.94 (1.07-0.83, p=.363)
#> 6 1.0848696748 1.10 (1.11-1.08, p<.001)
#> 7 1.0803886129 1.27 (1.49-1.08, p=.004)
#> 8 0.8819384832 1.26 (1.80-0.88, p=.203)
#> 9 0.8220614725 0.86 (0.91-0.82, p<.001)
# }
```

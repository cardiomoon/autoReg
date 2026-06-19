# Produce table for descriptive statistics

Produce table for descriptive statistics by groups for several variables
easily. Depending on the nature of these variables, different
descriptive statistical methods were used(t-test, ANOVA, Kruskal-Wallis,
chi-squared, Fisher's,...)

## Usage

``` r
gaze(x, ...)

# S3 method for class 'formula'
gaze(x, ...)

# S3 method for class 'data.frame'
gaze(x, ...)

# S3 method for class 'coxph'
gaze(x, ...)

# S3 method for class 'survreg'
gaze(x, ...)

# S3 method for class 'glm'
gaze(x, ...)

# S3 method for class 'lm'
gaze(x, ...)

# S3 method for class 'tidycrr'
gaze(x, ...)
```

## Arguments

- x:

  An R object, formula or data.frame

- ...:

  arguments to be passed to gaze.data.frame or gaze.formula

## Value

An object of class "gaze" which inherits from the class "data.frame"
with at least the following attributes:

- attr(\*,"yvars):

  character. name of dependent variable

## Methods (by class)

- `gaze(formula)`: S3 method for formula

- `gaze(data.frame)`: default S3 method

- `gaze(coxph)`: default S3 method

- `gaze(survreg)`: default S3 method

- `gaze(glm)`: default S3 method

- `gaze(lm)`: default S3 method

- `gaze(tidycrr)`: default S3 method

## Examples

``` r
library(moonBook)
library(dplyr)
gaze(acs)
#> —————————————————————————————————————————————————
#>        name            levels          stats     
#> —————————————————————————————————————————————————
#> age               Mean ± SD           63.3 ± 11.7 
#> sex               Female              287 (33.5%) 
#>                   Male                570 (66.5%) 
#> cardiogenicShock  No                  805 (93.9%) 
#>                   Yes                   52 (6.1%) 
#> entry             Femoral             312 (36.4%) 
#>                   Radial              545 (63.6%) 
#> Dx                NSTEMI              153 (17.9%) 
#>                   STEMI               304 (35.5%) 
#>                   Unstable Angina     400 (46.7%) 
#> EF                Mean ± SD            55.8 ± 9.6 
#> height            Mean ± SD           163.2 ± 9.1 
#> weight            Mean ± SD           64.8 ± 11.4 
#> BMI               Mean ± SD            24.3 ± 3.3 
#> obesity           No                  567 (66.2%) 
#>                   Yes                 290 (33.8%) 
#> TC                Mean ± SD          185.2 ± 47.8 
#> LDLC              Mean ± SD          116.6 ± 41.1 
#> HDLC              Mean ± SD           38.2 ± 11.1 
#> TG                Mean ± SD          125.2 ± 90.9 
#> DM                No                  553 (64.5%) 
#>                   Yes                 304 (35.5%) 
#> HBP               No                  356 (41.5%) 
#>                   Yes                 501 (58.5%) 
#> smoking           Ex-smoker           204 (23.8%) 
#>                   Never               332 (38.7%) 
#>                   Smoker              321 (37.5%) 
#> —————————————————————————————————————————————————
gaze(~age+sex,data=acs)
#> ——————————————————————————————
#>  name   levels       stats    
#> ——————————————————————————————
#> age   Mean ± SD    63.3 ± 11.7 
#> sex   Female       287 (33.5%) 
#>       Male         570 (66.5%) 
#> ——————————————————————————————
gaze(sex~.,data=acs,digits=1,method=1,show.p=TRUE) %>% myft()


.cl-a3d9ae56{}.cl-a3d27096{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a3d270aa{font-family:'DejaVu Sans';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a3d56e40{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a3d56e54{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a3d56e5e{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a3d58fec{width:1.51in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d58fed{width:1.426in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d58ff6{width:1.548in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59000{width:1.349in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59001{width:0.711in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d5900a{width:1.51in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d5900b{width:1.426in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59014{width:1.548in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59015{width:1.349in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59028{width:0.711in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59029{width:1.51in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59032{width:1.426in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d5903c{width:1.548in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59046{width:1.349in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a3d59047{width:0.711in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


name
```

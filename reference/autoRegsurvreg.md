# perform automatic regression for a class of survreg

perform automatic regression for a class of survreg

## Usage

``` r
autoRegsurvreg(
  x,
  threshold = 0.2,
  uni = FALSE,
  multi = TRUE,
  final = FALSE,
  imputed = FALSE,
  keepstats = FALSE,
  mode = 1,
  ...
)
```

## Arguments

- x:

  An object of class survreg

- threshold:

  numeric

- uni:

  logical whether or not perform univariable regression

- multi:

  logical whether or not perform multivariable regression

- final:

  logical whether or not perform stepwise backward elimination

- imputed:

  logical whether or not perform multiple imputation

- keepstats:

  logical whether or not keep statistic

- mode:

  integer

- ...:

  Further arguments to be passed to gaze()

## Value

autoRegsurvreg returns an object of class "autoReg" which inherits from
the class "data.frame" with at least the following attributes:

- attr(\*,"yvars):

  character. name of dependent variable

- attr(\*,"model"):

  name of model. One of "lm","glm","coxph" or "survreg"

## Examples

``` r
require(survival)
require(dplyr)
data(cancer)
fit=survreg(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
autoReg(fit)
#> —————————————————————————————————————————————————————————————————————————————————
#> Dependent: Surv(time, status)                     stats       ETR (multivariable) 
#> —————————————————————————————————————————————————————————————————————————————————
#> rx                                     Obs  630 (33.9%)                           
#>                                        Lev  620 (33.4%)  1.10 (0.91-1.31, p=.323) 
#>                                    Lev+5FU  608 (32.7%)  1.78 (1.46-2.18, p<.001) 
#> age                              Mean ± SD  59.8 ± 11.9  1.00 (0.99-1.00, p=.499) 
#> sex                              Mean ± SD    0.5 ± 0.5  1.09 (0.93-1.27, p=.289) 
#> nodes                            Mean ± SD    3.7 ± 3.6  0.89 (0.88-0.91, p<.001) 
#> obstruct                         Mean ± SD    0.2 ± 0.4  0.77 (0.63-0.93, p=.007) 
#> perfor                           Mean ± SD    0.0 ± 0.2  0.74 (0.49-1.14, p=.171) 
#> —————————————————————————————————————————————————————————————————————————————————
autoReg(fit,uni=TRUE,threshold=1)
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: Surv(time, status)                     stats         ETR (univariable)       ETR (multivariable) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> rx                                     Obs  630 (33.9%)                                                     
#>                                        Lev  620 (33.4%)  1.04 (0.86-1.25, p=.683)  1.10 (0.91-1.31, p=.323) 
#>                                    Lev+5FU  608 (32.7%)  1.79 (1.46-2.20, p<.001)  1.78 (1.46-2.18, p<.001) 
#> age                              Mean ± SD  59.8 ± 11.9  1.00 (1.00-1.01, p=.394)  1.00 (0.99-1.00, p=.499) 
#> sex                              Mean ± SD    0.5 ± 0.5  1.03 (0.88-1.21, p=.701)  1.09 (0.93-1.27, p=.289) 
#> nodes                            Mean ± SD    3.7 ± 3.6  0.89 (0.88-0.91, p<.001)  0.89 (0.88-0.91, p<.001) 
#> obstruct                         Mean ± SD    0.2 ± 0.4  0.74 (0.61-0.90, p=.003)  0.77 (0.63-0.93, p=.007) 
#> perfor                           Mean ± SD    0.0 ± 0.2  0.71 (0.45-1.10, p=.124)  0.74 (0.49-1.14, p=.171) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
autoReg(fit,uni=TRUE,final=TRUE)
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: Surv(time, status)                     stats         ETR (univariable)       ETR (multivariable)               ETR (final) 
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> rx                                     Obs  630 (33.9%)                                                                               
#>                                        Lev  620 (33.4%)  1.04 (0.86-1.25, p=.683)  1.10 (0.92-1.32, p=.304)  1.10 (0.91-1.31, p=.324) 
#>                                    Lev+5FU  608 (32.7%)  1.79 (1.46-2.20, p<.001)  1.78 (1.45-2.17, p<.001)  1.77 (1.45-2.17, p<.001) 
#> age                              Mean ± SD  59.8 ± 11.9  1.00 (1.00-1.01, p=.394)                                                     
#> sex                              Mean ± SD    0.5 ± 0.5  1.03 (0.88-1.21, p=.701)                                                     
#> nodes                            Mean ± SD    3.7 ± 3.6  0.89 (0.88-0.91, p<.001)  0.90 (0.88-0.91, p<.001)  0.90 (0.88-0.91, p<.001) 
#> obstruct                         Mean ± SD    0.2 ± 0.4  0.74 (0.61-0.90, p=.003)  0.77 (0.63-0.93, p=.007)  0.76 (0.62-0.92, p=.004) 
#> perfor                           Mean ± SD    0.0 ± 0.2  0.71 (0.45-1.10, p=.124)  0.76 (0.50-1.16, p=.196)                           
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
autoReg(fit,uni=TRUE,final=TRUE) %>% myft()


.cl-3b497eec{}.cl-3b3eed1a{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-3b3eed2e{font-family:'DejaVu Sans';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-3b4223ea{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3b4223fe{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3b422408{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3b422409{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3b424992{width:2.61in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b42499c{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249a6{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249b0{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249ba{width:2.61in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249c4{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249c5{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249ce{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249d8{width:2.61in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249e2{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249e3{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249ec{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249ed{width:2.61in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249f6{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b4249f7{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b424a00{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Dependent: Surv(time, status)
```

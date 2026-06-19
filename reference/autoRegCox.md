# perform automatic regression for a class of coxph

perform automatic regression for a class of coxph

## Usage

``` r
autoRegCox(
  x,
  threshold = 0.2,
  uni = FALSE,
  multi = TRUE,
  final = FALSE,
  imputed = FALSE,
  keepstats = FALSE,
  ...
)
```

## Arguments

- x:

  An object of class coxph

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

- ...:

  Further arguments to be passed to gaze()

## Value

autoRegCox returns an object of class "autoReg" which inherits from the
class "data.frame" with at least the following attributes:

- attr(\*,"yvars):

  character. name of dependent variable

- attr(\*,"model"):

  name of model. One of "lm","glm" or "coxph"

## Examples

``` r
require(survival)
require(dplyr)
data(cancer)
fit=coxph(Surv(time,status==2)~log(bili)+age+cluster(edema),data=pbc)
autoReg(fit)
#> ——————————————————————————————————————————————————————————————————————————————————————
#> Dependent: Surv(time, status == 2)                       all        HR (multivariable) 
#> ——————————————————————————————————————————————————————————————————————————————————————
#> log(bili)                             Mean ± SD    0.6 ± 1.0  2.76 (2.59-2.94, p<.001) 
#> age                                   Mean ± SD  50.7 ± 10.4  1.04 (1.03-1.06, p<.001) 
#> ——————————————————————————————————————————————————————————————————————————————————————
#> n=418, events=161, Likelihood ratio test=186.62 on 2 df(p<.001) cluster=edema 
fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
autoReg(fit)
#> —————————————————————————————————————————————————————————————————————————————————
#> Dependent: Surv(time, status)                       all        HR (multivariable) 
#> —————————————————————————————————————————————————————————————————————————————————
#> rx                                     Obs  624 (34.2%)                           
#>                                        Lev  608 (33.4%)  0.94 (0.81-1.09, p=.409) 
#>                                    Lev+5FU  590 (32.4%)  0.63 (0.54-0.75, p<.001) 
#> age                              Mean ± SD  59.8 ± 11.9  1.00 (1.00-1.01, p=.571) 
#> sex                              Mean ± SD    0.5 ± 0.5  0.92 (0.81-1.05, p=.242) 
#> nodes                            Mean ± SD    3.7 ± 3.6  1.09 (1.08-1.10, p<.001) 
#> obstruct                         Mean ± SD    0.2 ± 0.4  1.26 (1.07-1.48, p=.006) 
#> perfor                           Mean ± SD    0.0 ± 0.2  1.26 (0.88-1.79, p=.210) 
#> —————————————————————————————————————————————————————————————————————————————————
#> n=1822, events=897, Likelihood ratio test=178.71 on 7 df(p<.001) 
autoReg(fit,uni=TRUE,threshold=1)
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Dependent: Surv(time, status)                       all          HR (univariable)        HR (multivariable) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> rx                                     Obs  624 (34.2%)                                                     
#>                                        Lev  608 (33.4%)  0.98 (0.84-1.14, p=.786)  0.94 (0.81-1.09, p=.409) 
#>                                    Lev+5FU  590 (32.4%)  0.64 (0.55-0.76, p<.001)  0.63 (0.54-0.75, p<.001) 
#> age                              Mean ± SD  59.8 ± 11.9  1.00 (0.99-1.00, p=.382)  1.00 (1.00-1.01, p=.571) 
#> sex                              Mean ± SD    0.5 ± 0.5  0.97 (0.85-1.10, p=.610)  0.92 (0.81-1.05, p=.242) 
#> nodes                            Mean ± SD    3.7 ± 3.6  1.09 (1.08-1.10, p<.001)  1.09 (1.08-1.10, p<.001) 
#> obstruct                         Mean ± SD    0.2 ± 0.4  1.27 (1.09-1.49, p=.003)  1.26 (1.07-1.48, p=.006) 
#> perfor                           Mean ± SD    0.0 ± 0.2  1.30 (0.92-1.85, p=.142)  1.26 (0.88-1.79, p=.210) 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> n=1822, events=897, Likelihood ratio test=178.71 on 7 df(p<.001) 
autoReg(fit,uni=TRUE,final=TRUE) %>% myft()


.cl-39bdbffc{}.cl-39b330be{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-39b330d2{font-family:'DejaVu Sans';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-39b6555a{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-39b6556e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-39b65582{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-39b65583{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-39b67ae4{width:0.861in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67aee{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67aef{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67af8{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67af9{width:0.861in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b02{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b03{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b0c{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b0d{width:0.861in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b16{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b20{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b21{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b2a{width:0.861in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b34{width:1.063in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b3e{width:1.144in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-39b67b3f{width:2.018in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Dependent: Surv(time, status)
```

# Getting started

## Installation

You can install autoReg package on github.

``` r

#install.packages("devtools")
devtools::install_github("cardiomoon/autoReg")
```

## Load package

To load the package, use library() function.

``` r

library(autoReg)
```

## Main features

### 1.Summarizing baseline characteristics : gaze()

You can make a table summarizing baseline characteristics easily.

``` r
library(moonBook) # For use of example data acs
gaze(sex~.,data=acs)
————————————————————————————————————————————————————————————————————————
  Dependent:sex        levels           Female          Male        p   
       (N)                             (N=287)        (N=570)           
————————————————————————————————————————————————————————————————————————
age               Mean ± SD             68.7 ± 10.7   60.6 ± 11.2  <.001 
cardiogenicShock  No                    275 (95.8%)     530 (93%)   .136 
                  Yes                     12 (4.2%)       40 (7%)        
entry             Femoral               119 (41.5%)   193 (33.9%)   .035 
                  Radial                168 (58.5%)   377 (66.1%)        
Dx                NSTEMI                 50 (17.4%)   103 (18.1%)   .012 
                  STEMI                  84 (29.3%)   220 (38.6%)        
                  Unstable Angina       153 (53.3%)   247 (43.3%)        
EF                Mean ± SD             56.3 ± 10.1    55.6 ± 9.4   .387 
height            Mean ± SD             153.8 ± 6.2   167.9 ± 6.1  <.001 
weight            Mean ± SD              57.2 ± 9.3   68.7 ± 10.3  <.001 
BMI               Mean ± SD              24.2 ± 3.6    24.3 ± 3.2   .611 
obesity           No                    194 (67.6%)   373 (65.4%)   .580 
                  Yes                    93 (32.4%)   197 (34.6%)        
TC                Mean ± SD            188.9 ± 51.1  183.3 ± 45.9   .124 
LDLC              Mean ± SD            117.8 ± 41.2  116.0 ± 41.1   .561 
HDLC              Mean ± SD             39.0 ± 11.5   37.8 ± 10.9   .145 
TG                Mean ± SD            119.9 ± 76.2  127.9 ± 97.3   .195 
DM                No                    173 (60.3%)   380 (66.7%)   .077 
                  Yes                   114 (39.7%)   190 (33.3%)        
HBP               No                     83 (28.9%)   273 (47.9%)  <.001 
                  Yes                   204 (71.1%)   297 (52.1%)        
smoking           Ex-smoker              49 (17.1%)   155 (27.2%)  <.001 
                  Never                 209 (72.8%)   123 (21.6%)        
                  Smoker                 29 (10.1%)   292 (51.2%)        
————————————————————————————————————————————————————————————————————————
```

### For easy reproducible research : myft()

You can make a publication-ready table easily using myft(). It makes a
flextable object which can use in either HTML and PDF format.

``` r
library(dplyr) # for use of `%>%`

Attaching package: 'dplyr'
The following objects are masked from 'package:stats':

    filter, lag
The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
ft=gaze(sex~.,data=acs) %>% myft()
ft
```

| name             | levels          | Female (N=287) | Male (N=570) | p      |
|------------------|-----------------|----------------|--------------|--------|
| age              | Mean ± SD       | 68.7 ± 10.7    | 60.6 ± 11.2  | \<.001 |
| cardiogenicShock | No              | 275 (95.8%)    | 530 (93%)    | .136   |
|                  | Yes             | 12 (4.2%)      | 40 (7%)      |        |
| entry            | Femoral         | 119 (41.5%)    | 193 (33.9%)  | .035   |
|                  | Radial          | 168 (58.5%)    | 377 (66.1%)  |        |
| Dx               | NSTEMI          | 50 (17.4%)     | 103 (18.1%)  | .012   |
|                  | STEMI           | 84 (29.3%)     | 220 (38.6%)  |        |
|                  | Unstable Angina | 153 (53.3%)    | 247 (43.3%)  |        |
| EF               | Mean ± SD       | 56.3 ± 10.1    | 55.6 ± 9.4   | .387   |
| height           | Mean ± SD       | 153.8 ± 6.2    | 167.9 ± 6.1  | \<.001 |
| weight           | Mean ± SD       | 57.2 ± 9.3     | 68.7 ± 10.3  | \<.001 |
| BMI              | Mean ± SD       | 24.2 ± 3.6     | 24.3 ± 3.2   | .611   |
| obesity          | No              | 194 (67.6%)    | 373 (65.4%)  | .580   |
|                  | Yes             | 93 (32.4%)     | 197 (34.6%)  |        |
| TC               | Mean ± SD       | 188.9 ± 51.1   | 183.3 ± 45.9 | .124   |
| LDLC             | Mean ± SD       | 117.8 ± 41.2   | 116.0 ± 41.1 | .561   |
| HDLC             | Mean ± SD       | 39.0 ± 11.5    | 37.8 ± 10.9  | .145   |
| TG               | Mean ± SD       | 119.9 ± 76.2   | 127.9 ± 97.3 | .195   |
| DM               | No              | 173 (60.3%)    | 380 (66.7%)  | .077   |
|                  | Yes             | 114 (39.7%)    | 190 (33.3%)  |        |
| HBP              | No              | 83 (28.9%)     | 273 (47.9%)  | \<.001 |
|                  | Yes             | 204 (71.1%)    | 297 (52.1%)  |        |
| smoking          | Ex-smoker       | 49 (17.1%)     | 155 (27.2%)  | \<.001 |
|                  | Never           | 209 (72.8%)    | 123 (21.6%)  |        |
|                  | Smoker          | 29 (10.1%)     | 292 (51.2%)  |        |

You can also make a powerpoint file using rrtable::table2pptx()
function.

``` r

library(rrtable)

table2pptx(ft)
```

    Exported table as Report.pptx

You can make a microsoft word file using rrtable::table2docx() function.

``` r

table2docx(ft)
```

    Exported table as Report.docx

### Summarizing baseline characteristics with two or more grouping variables

You can get a table summarizing baseline characteristics with two or
more grouping variables.

``` r

gaze(sex+Dx~.,data=acs) %>% myft()
```

| sex (N) |  | Female (N=287) |  |  |  | Male (N=570) |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| name | levels | NSTEMI (N=50) | STEMI (N=84) | Unstable Angina (N=153) | p | NSTEMI (N=103) | STEMI (N=220) | Unstable Angina (N=247) | p |
| age | Mean ± SD | 70.9 ± 11.4 | 69.1 ± 10.4 | 67.7 ± 10.7 | .177 | 61.1 ± 11.6 | 59.4 ± 11.7 | 61.4 ± 10.6 | .133 |
| cardiogenicShock | No | 49 (98%) | 73 (86.9%) | 153 (100%) | \<.001 | 100 (97.1%) | 183 (83.2%) | 247 (100%) | \<.001 |
|  | Yes | 1 (2%) | 11 (13.1%) | 0 (0%) |  | 3 (2.9%) | 37 (16.8%) | 0 (0%) |  |
| entry | Femoral | 22 (44%) | 45 (53.6%) | 52 (34%) | .013 | 36 (35%) | 88 (40%) | 69 (27.9%) | .022 |
|  | Radial | 28 (56%) | 39 (46.4%) | 101 (66%) |  | 67 (65%) | 132 (60%) | 178 (72.1%) |  |
| EF | Mean ± SD | 54.8 ± 9.1 | 52.3 ± 10.9 | 59.4 ± 8.8 | \<.001 | 55.1 ± 9.4 | 52.4 ± 8.9 | 59.1 ± 8.7 | \<.001 |
| height | Mean ± SD | 154.2 ± 5.1 | 155.7 ± 5.4 | 152.6 ± 6.7 | .002 | 167.5 ± 5.7 | 168.7 ± 6.0 | 167.3 ± 6.4 | .055 |
| weight | Mean ± SD | 57.2 ± 10.3 | 57.4 ± 9.0 | 57.1 ± 9.1 | .978 | 67.5 ± 8.4 | 68.8 ± 10.9 | 69.0 ± 10.6 | .479 |
| BMI | Mean ± SD | 24.1 ± 4.3 | 23.6 ± 3.2 | 24.5 ± 3.5 | .215 | 24.1 ± 2.6 | 24.1 ± 3.4 | 24.6 ± 3.4 | .205 |
| obesity | No | 35 (70%) | 60 (71.4%) | 99 (64.7%) | .528 | 71 (68.9%) | 149 (67.7%) | 153 (61.9%) | .301 |
|  | Yes | 15 (30%) | 24 (28.6%) | 54 (35.3%) |  | 32 (31.1%) | 71 (32.3%) | 94 (38.1%) |  |
| TC | Mean ± SD | 196.3 ± 52.7 | 180.7 ± 45.7 | 191.1 ± 53.1 | .192 | 192.6 ± 54.3 | 184.1 ± 42.6 | 178.7 ± 44.6 | .036 |
| LDLC | Mean ± SD | 127.7 ± 39.5 | 111.0 ± 40.0 | 118.3 ± 41.8 | .088 | 125.4 ± 47.1 | 118.9 ± 39.1 | 109.5 ± 39.2 | .002 |
| HDLC | Mean ± SD | 40.1 ± 13.8 | 39.5 ± 11.2 | 38.5 ± 10.8 | .627 | 38.4 ± 10.9 | 38.1 ± 10.9 | 37.4 ± 10.9 | .655 |
| TG | Mean ± SD | 112.5 ± 51.1 | 112.3 ± 87.2 | 126.3 ± 76.0 | .316 | 138.0 ± 100.2 | 104.3 ± 65.5 | 144.3 ± 114.2 | \<.001 |
| DM | No | 25 (50%) | 54 (64.3%) | 94 (61.4%) | .240 | 71 (68.9%) | 154 (70%) | 155 (62.8%) | .219 |
|  | Yes | 25 (50%) | 30 (35.7%) | 59 (38.6%) |  | 32 (31.1%) | 66 (30%) | 92 (37.2%) |  |
| HBP | No | 19 (38%) | 28 (33.3%) | 36 (23.5%) | .084 | 43 (41.7%) | 122 (55.5%) | 108 (43.7%) | .016 |
|  | Yes | 31 (62%) | 56 (66.7%) | 117 (76.5%) |  | 60 (58.3%) | 98 (44.5%) | 139 (56.3%) |  |
| smoking | Ex-smoker | 8 (16%) | 13 (15.5%) | 28 (18.3%) | .184 | 34 (33%) | 53 (24.1%) | 68 (27.5%) | .002 |
|  | Never | 37 (74%) | 57 (67.9%) | 115 (75.2%) |  | 13 (12.6%) | 40 (18.2%) | 70 (28.3%) |  |
|  | Smoker | 5 (10%) | 14 (16.7%) | 10 (6.5%) |  | 56 (54.4%) | 127 (57.7%) | 109 (44.1%) |  |

You can also use three or more grouping variables.The resultant table
will be too long to review, but you can try.

``` r

gaze(sex+DM+HBP~age,data=acs) %>% myft()
```

[TABLE]

### 2. For automatic selection of explanatory variables : autoReg()

You can make a table summarizing results of regression analysis. For
example, let us perform a logistic regression with the colon cancer
data.

``` r
library(survival)   # For use of data colon
data(cancer)  

fit=glm(status~rx+sex+age+obstruct+perfor+nodes,data=colon,family="binomial")
summary(fit)

Call:
glm(formula = status ~ rx + sex + age + obstruct + perfor + nodes, 
    family = "binomial", data = colon)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.645417   0.285558  -2.260   0.0238 *  
rxLev       -0.067422   0.118907  -0.567   0.5707    
rxLev+5FU   -0.627480   0.121684  -5.157 2.51e-07 ***
sex         -0.053541   0.098975  -0.541   0.5885    
age          0.002307   0.004234   0.545   0.5859    
obstruct     0.283703   0.125194   2.266   0.0234 *  
perfor       0.319281   0.292034   1.093   0.2743    
nodes        0.190563   0.018255  10.439  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2525.4  on 1821  degrees of freedom
Residual deviance: 2342.4  on 1814  degrees of freedom
  (36 observations deleted due to missingness)
AIC: 2358.4

Number of Fisher Scoring iterations: 4
```

You can make table with above result.

``` r
autoReg(fit)
——————————————————————————————————————————————————————————————————————————————————
Dependent: status                 0 (N=938)    1 (N=920)        OR (multivariable) 
——————————————————————————————————————————————————————————————————————————————————
rx                         Obs  285 (30.4%)  345 (37.5%)                           
                           Lev  287 (30.6%)  333 (36.2%)  0.93 (0.74-1.18, p=.571) 
                       Lev+5FU    366 (39%)  242 (26.3%)  0.53 (0.42-0.68, p<.001) 
sex                  Mean ± SD    0.5 ± 0.5    0.5 ± 0.5  0.95 (0.78-1.15, p=.589) 
age                  Mean ± SD  60.0 ± 11.5  59.5 ± 12.4  1.00 (0.99-1.01, p=.586) 
obstruct             Mean ± SD    0.2 ± 0.4    0.2 ± 0.4  1.33 (1.04-1.70, p=.023) 
perfor               Mean ± SD    0.0 ± 0.2    0.0 ± 0.2  1.38 (0.78-2.44, p=.274) 
nodes                Mean ± SD    2.7 ± 2.4    4.6 ± 4.2  1.21 (1.17-1.25, p<.001) 
——————————————————————————————————————————————————————————————————————————————————
```

Or you can make a publication-ready table.

``` r

autoReg(fit) %>% myft()
```

| Dependent: status |  | 0 (N=938) | 1 (N=920) | OR (multivariable) |
|----|----|----|----|----|
| rx | Obs | 285 (30.4%) | 345 (37.5%) |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.93 (0.74-1.18, p=.571) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.53 (0.42-0.68, p\<.001) |
| sex | Mean ± SD | 0.5 ± 0.5 | 0.5 ± 0.5 | 0.95 (0.78-1.15, p=.589) |
| age | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.01, p=.586) |
| obstruct | Mean ± SD | 0.2 ± 0.4 | 0.2 ± 0.4 | 1.33 (1.04-1.70, p=.023) |
| perfor | Mean ± SD | 0.0 ± 0.2 | 0.0 ± 0.2 | 1.38 (0.78-2.44, p=.274) |
| nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |

If you want make a table with more explanation, you can make categorical
variables with numeric variables. For example, the explanatory variables
obstruct(obstruction of colon by tumor) and perfor(perforation of colon)
is coded as 0 or 1, but it is “No” or “Yes” actually. Also the dependent
variable status is coded as 0 or 1, it is “Alive” or “Died”.

``` r

colon$status.factor=factor(colon$status,labels=c("Alive","Died"))
colon$obstruct.factor=factor(colon$obstruct,labels=c("No","Yes"))
colon$perfor.factor=factor(colon$perfor,labels=c("No","Yes"))
colon$sex.factor=factor(colon$sex,labels=c("Female","Male"))

fit=glm(status.factor~rx+sex.factor+age+obstruct.factor+perfor.factor+nodes,data=colon,family="binomial")
result=autoReg(fit) 
result %>% myft()
```

| Dependent: status.factor |  | Alive (N=938) | Died (N=920) | OR (multivariable) |
|----|----|----|----|----|
| rx | Obs | 285 (30.4%) | 345 (37.5%) |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.93 (0.74-1.18, p=.571) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.53 (0.42-0.68, p\<.001) |
| sex.factor | Female | 446 (47.5%) | 444 (48.3%) |  |
|  | Male | 492 (52.5%) | 476 (51.7%) | 0.95 (0.78-1.15, p=.589) |
| age | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.01, p=.586) |
| obstruct.factor | No | 775 (82.6%) | 723 (78.6%) |  |
|  | Yes | 163 (17.4%) | 197 (21.4%) | 1.33 (1.04-1.70, p=.023) |
| perfor.factor | No | 916 (97.7%) | 888 (96.5%) |  |
|  | Yes | 22 (2.3%) | 32 (3.5%) | 1.38 (0.78-2.44, p=.274) |
| nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |

You can add labels to the names of variables with setLabel() function.

``` r

colon$status.factor=setLabel(colon$status.factor,"Mortality")
colon$rx=setLabel(colon$rx,"Treatment")
colon$age=setLabel(colon$age,"Age(Years)")
colon$sex.factor=setLabel(colon$sex.factor,"Sex")
colon$obstruct.factor=setLabel(colon$obstruct.factor,"Obstruction")
colon$perfor.factor=setLabel(colon$perfor.factor,"Perforation")
colon$nodes=setLabel(colon$nodes,"Positive nodes")

fit=glm(status.factor~rx+sex.factor+age+obstruct.factor+perfor.factor+nodes,data=colon,family="binomial")
result=autoReg(fit) 
result %>% myft()
```

| Dependent: Mortality |  | Alive (N=938) | Died (N=920) | OR (multivariable) |
|----|----|----|----|----|
| Treatment | Obs | 285 (30.4%) | 345 (37.5%) |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.93 (0.74-1.18, p=.571) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.53 (0.42-0.68, p\<.001) |
| Sex | Female | 446 (47.5%) | 444 (48.3%) |  |
|  | Male | 492 (52.5%) | 476 (51.7%) | 0.95 (0.78-1.15, p=.589) |
| Age(Years) | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.01, p=.586) |
| Obstruction | No | 775 (82.6%) | 723 (78.6%) |  |
|  | Yes | 163 (17.4%) | 197 (21.4%) | 1.33 (1.04-1.70, p=.023) |
| Perforation | No | 916 (97.7%) | 888 (96.5%) |  |
|  | Yes | 22 (2.3%) | 32 (3.5%) | 1.38 (0.78-2.44, p=.274) |
| Positive nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |

If you do not want to show the reference values in table, you can
shorten the table.

``` r

shorten(result) %>% myft()
```

| Dependent: Mortality |  | Alive (N=938) | Died (N=920) | OR (multivariable) |
|----|----|----|----|----|
| Treatment | Lev | 287 (30.6%) | 333 (36.2%) | 0.93 (0.74-1.18, p=.571) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.53 (0.42-0.68, p\<.001) |
| Sex | Male | 492 (52.5%) | 476 (51.7%) | 0.95 (0.78-1.15, p=.589) |
| Age(Years) | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.01, p=.586) |
| Obstruction | Yes | 163 (17.4%) | 197 (21.4%) | 1.33 (1.04-1.70, p=.023) |
| Perforation | Yes | 22 (2.3%) | 32 (3.5%) | 1.38 (0.78-2.44, p=.274) |
| Positive nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |

### Add univariate models to table and automatic selection of explanatory variables

You can add the results of univariate analyses to the table. At this
time, the autoReg() function automatically select explanatory variables
below the threshold(default value 0.2) and perform multivariate
analysis. In this table, the p values of explanatory variables
sex.factor and age is above the default threshold(0.2), they are
excluded in multivariate model.

``` r

autoReg(fit, uni=TRUE) %>% myft()
```

| Dependent: Mortality |  | Alive (N=938) | Died (N=920) | OR (univariable) | OR (multivariable) |
|----|----|----|----|----|----|
| Treatment | Obs | 285 (30.4%) | 345 (37.5%) |  |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.96 (0.77-1.20, p=.709) | 0.93 (0.74-1.18, p=.570) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.55 (0.44-0.68, p\<.001) | 0.54 (0.42-0.68, p\<.001) |
| Sex | Female | 446 (47.5%) | 444 (48.3%) |  |  |
|  | Male | 492 (52.5%) | 476 (51.7%) | 0.97 (0.81-1.17, p=.758) |  |
| Age(Years) | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.00, p=.296) |  |
| Obstruction | No | 775 (82.6%) | 723 (78.6%) |  |  |
|  | Yes | 163 (17.4%) | 197 (21.4%) | 1.30 (1.03-1.63, p=.028) | 1.32 (1.04-1.69, p=.025) |
| Perforation | No | 916 (97.7%) | 888 (96.5%) |  |  |
|  | Yes | 22 (2.3%) | 32 (3.5%) | 1.50 (0.87-2.60, p=.149) | 1.38 (0.78-2.44, p=.273) |
| Positive nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |  |

If you want to include all explanatory variables in the multivariate
model, just set the threshold 1.

``` r

autoReg(fit, uni=TRUE,threshold=1) %>% myft()
```

| Dependent: Mortality |  | Alive (N=938) | Died (N=920) | OR (univariable) | OR (multivariable) |
|----|----|----|----|----|----|
| Treatment | Obs | 285 (30.4%) | 345 (37.5%) |  |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.96 (0.77-1.20, p=.709) | 0.93 (0.74-1.18, p=.571) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.55 (0.44-0.68, p\<.001) | 0.53 (0.42-0.68, p\<.001) |
| Sex | Female | 446 (47.5%) | 444 (48.3%) |  |  |
|  | Male | 492 (52.5%) | 476 (51.7%) | 0.97 (0.81-1.17, p=.758) | 0.95 (0.78-1.15, p=.589) |
| Age(Years) | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.00, p=.296) | 1.00 (0.99-1.01, p=.586) |
| Obstruction | No | 775 (82.6%) | 723 (78.6%) |  |  |
|  | Yes | 163 (17.4%) | 197 (21.4%) | 1.30 (1.03-1.63, p=.028) | 1.33 (1.04-1.70, p=.023) |
| Perforation | No | 916 (97.7%) | 888 (96.5%) |  |  |
|  | Yes | 22 (2.3%) | 32 (3.5%) | 1.50 (0.87-2.60, p=.149) | 1.38 (0.78-2.44, p=.274) |
| Positive nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |  |

You can perform stepwise backward elimination to select variables and
make a final model. Just set final=TRUE.

``` r

autoReg(fit, uni=TRUE,threshold=1, final=TRUE) %>% myft()
```

| Dependent: Mortality |  | Alive (N=938) | Died (N=920) | OR (univariable) | OR (multivariable) | OR (final) |
|----|----|----|----|----|----|----|
| Treatment | Obs | 285 (30.4%) | 345 (37.5%) |  |  |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.96 (0.77-1.20, p=.709) | 0.93 (0.74-1.18, p=.571) | 0.94 (0.74-1.18, p=.575) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.55 (0.44-0.68, p\<.001) | 0.53 (0.42-0.68, p\<.001) | 0.54 (0.42-0.68, p\<.001) |
| Sex | Female | 446 (47.5%) | 444 (48.3%) |  |  |  |
|  | Male | 492 (52.5%) | 476 (51.7%) | 0.97 (0.81-1.17, p=.758) | 0.95 (0.78-1.15, p=.589) |  |
| Age(Years) | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.00, p=.296) | 1.00 (0.99-1.01, p=.586) |  |
| Obstruction | No | 775 (82.6%) | 723 (78.6%) |  |  |  |
|  | Yes | 163 (17.4%) | 197 (21.4%) | 1.30 (1.03-1.63, p=.028) | 1.33 (1.04-1.70, p=.023) | 1.34 (1.05-1.71, p=.019) |
| Perforation | No | 916 (97.7%) | 888 (96.5%) |  |  |  |
|  | Yes | 22 (2.3%) | 32 (3.5%) | 1.50 (0.87-2.60, p=.149) | 1.38 (0.78-2.44, p=.274) |  |
| Positive nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) | 1.21 (1.17-1.25, p\<.001) | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |  |  |

#### Multiple imputation with mice()

When the argument imputed=TRUE, autoReg() function make a multiple
imputed model using mice::mice() function. By default, 20 imputations
performed. If you want, you can change the number of imputations with m
argument.

``` r
autoReg(fit, imputed=TRUE) %>% myft()
Warning: Number of logged events: 5
```

| Dependent: Mortality |  | Alive (N=938) | Died (N=920) | OR (multivariable) | OR (imputed) |
|----|----|----|----|----|----|
| Treatment | Obs | 285 (30.4%) | 345 (37.5%) |  |  |
|  | Lev | 287 (30.6%) | 333 (36.2%) | 0.93 (0.74-1.18, p=.571) | 0.95 (0.76-1.20, p=.688) |
|  | Lev+5FU | 366 (39%) | 242 (26.3%) | 0.53 (0.42-0.68, p\<.001) | 0.54 (0.43-0.68, p\<.001) |
| Sex | Female | 446 (47.5%) | 444 (48.3%) |  |  |
|  | Male | 492 (52.5%) | 476 (51.7%) | 0.95 (0.78-1.15, p=.589) | 0.97 (0.80-1.17, p=.736) |
| Age(Years) | Mean ± SD | 60.0 ± 11.5 | 59.5 ± 12.4 | 1.00 (0.99-1.01, p=.586) | 1.00 (0.99-1.01, p=.646) |
| Obstruction | No | 775 (82.6%) | 723 (78.6%) |  |  |
|  | Yes | 163 (17.4%) | 197 (21.4%) | 1.33 (1.04-1.70, p=.023) | 1.34 (1.05-1.71, p=.018) |
| Perforation | No | 916 (97.7%) | 888 (96.5%) |  |  |
|  | Yes | 22 (2.3%) | 32 (3.5%) | 1.38 (0.78-2.44, p=.274) | 1.37 (0.77-2.43, p=.278) |
| Positive nodes | Mean ± SD | 2.7 ± 2.4 | 4.6 ± 4.2 | 1.21 (1.17-1.25, p\<.001) | 1.21 (1.17-1.25, p\<.001) |
|  |  |  |  |  |  |

### Summarize regression model results in a plot : modelPlot()

You can draw the plot summarizing the model with modelPlot()

``` r
x=modelPlot(fit)
Warning:  [1m [22m`aes_string()` was deprecated in ggplot2 3.0.0.
 [36mℹ [39m Please use tidy evaluation idioms with `aes()`.
 [36mℹ [39m See also `vignette("ggplot2-in-packages")` for more information.
 [36mℹ [39m The deprecated feature was likely used in the  [34mautoReg [39m package.
  Please report the issue at  [3m [34m<https://github.com/cardiomoon/autoReg/issues> [39m [23m.
 [90mThis warning is displayed once per session. [39m
 [90mCall `lifecycle::last_lifecycle_warnings()` to see where this warning was [39m
 [90mgenerated. [39m
x
```

![](Getting_started_files/figure-html/unnamed-chunk-20-1.png)

You can make powerpoint file with this plot using rrtable::plot2pptx().

``` r

plot2pptx(print(x))
```

    Exported plot as Report.pptx

You can summarize models in a plot. If you want to summarize univariate
and multivariate model in a plot, just set the uni=TRUE and adjust the
threshold. You can decide whether or not show the reference by show.ref
argument.

``` r

modelPlot(fit,uni=TRUE,threshold=1,show.ref=FALSE)
```

![](Getting_started_files/figure-html/unnamed-chunk-22-1.png)

# Statistical tests in gaze

## Loading package

``` r
library(autoReg)
library(dplyr) # for use of pipe operator `%>%`

Attaching package: 'dplyr'
The following objects are masked from 'package:stats':

    filter, lag
The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
```

## Statistical tests for numeric variables

The gaze() function in this autoReg package perform statistical tests
for compare means between/among groups. The acs data included in
moonBook package is a dataset containing demographic and laboratory data
of 857 patients with acute coronary syndrome(ACS).

To make a table comparing baseline characteristics, use gaze() function.

``` r
data(acs, package="moonBook")
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

You can make a publication-ready table with myft() function which can be
used in HTML, pdf, microsoft word and powerpoint file.

``` r

gaze(sex~.,data=acs) %>% myft()
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

You can select the statistical method comparing means between/among
groups with argument method. Possible values in methods are:

- 1 forces analysis as normal-distributed
- 2 forces analysis as continuous non-normal
- 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between
  normal or non-normal

Default value is 1.

### 1. Comparison of two groups

Ejection fraction(EF) refers to how well your left ventricle (or right
ventricle) pumps blood with each heart beat. The normal values are
approximately 56-78%.

#### (1) Parametric method

``` r
gaze(sex~EF,data=acs)  # default: method=1 
——————————————————————————————————————————————————————————————
 Dependent:sex   levels        Female          Male       p   
      (N)                     (N=287)        (N=570)          
——————————————————————————————————————————————————————————————
EF             Mean ± SD       56.3 ± 10.1    55.6 ± 9.4  .387 
——————————————————————————————————————————————————————————————
```

If you want to compare EF means between males and females in acs data
with parametric method, you have to compare the variances of two
samples. If the variances of two groups are equal, the pooled variance
is used to estimate the variance. Otherwise the Welch (or Satterthwaite)
approximation to the degrees of freedom is used.

``` r
var.test(EF~sex,data=acs)  # F Test to Compare Two Variances

    F test to compare two variances

data:  EF by sex
F = 1.144, num df = 239, denom df = 482, p-value = 0.2214
alternative hypothesis: true ratio of variances is not equal to 1
95 percent confidence interval:
 0.9221264 1.4309581
sample estimates:
ratio of variances 
          1.143983 
```

The result of var.test is not significant. So we cannot reject the null
hypothesis :$`H_0 : true\  ratio\  of\  variance\ is\  equal\  to\  0`$.
With this result, we perform t-test using pooled variance.

``` r
t.test(EF~sex,data=acs,var.equal=TRUE)

    Two Sample t-test

data:  EF by sex
t = 0.86514, df = 721, p-value = 0.3872
alternative hypothesis: true difference in means between group Female and group Male is not equal to 0
95 percent confidence interval:
 -0.8346856  2.1498875
sample estimates:
mean in group Female   mean in group Male 
            56.27375             55.61615 
```

The result of t.test is not significant($`p=.387`$). The p value in the
table is the result of this test. Alternatively, if the result of
var.test() is significant, we perform t.test with the Welch
approximation to the degrees of freedom.

``` r
t.test(EF~sex,data=acs) # default value: var.equal=FALSE

    Welch Two Sample t-test

data:  EF by sex
t = 0.8458, df = 449.65, p-value = 0.3981
alternative hypothesis: true difference in means between group Female and group Male is not equal to 0
95 percent confidence interval:
 -0.8703566  2.1855585
sample estimates:
mean in group Female   mean in group Male 
            56.27375             55.61615 
```

#### (2) Non-parametric method

``` r
gaze(sex~EF,data=acs, method=2)  # method=2 forces analysis as continuous non-normal 
—————————————————————————————————————————————————————————————————————————————
 Dependent:sex     levels           Female                Male           p   
      (N)                           (N=287)              (N=570)             
—————————————————————————————————————————————————————————————————————————————
EF             Median (IQR)    59.2 (51.4 to 63.1)  57.3 (50.0 to 61.8)  .053 
—————————————————————————————————————————————————————————————————————————————
```

When you choose method=2, the Wilcoxon rank sum test(also known as
Mann-Whitney test) is performed.

``` r
wilcox.test(EF~sex,data=acs)

    Wilcoxon rank sum test with continuity correction

data:  EF by sex
W = 63078, p-value = 0.05295
alternative hypothesis: true location shift is not equal to 0
```

#### (3) Performs test for normality

``` r
gaze(sex~EF,data=acs, method=3) 
—————————————————————————————————————————————————————————————————————————————
 Dependent:sex     levels           Female                Male           p   
      (N)                           (N=287)              (N=570)             
—————————————————————————————————————————————————————————————————————————————
EF             Median (IQR)    59.2 (51.4 to 63.1)  57.3 (50.0 to 61.8)  .053 
—————————————————————————————————————————————————————————————————————————————
```

When method=3, perform the Shapiro-Wilk test or the Anderson-Daring test
for normality(nortest::ad.test) to decide between normal or non-normal.
If the number of cases are below 5000, Shapiro-Wilk test performed. If
above 5000, Anderson-Daring test for normality performed.

``` r
nrow(acs)
[1] 857
out=lm(age~sex,data=acs)
shapiro.test(resid(out))

    Shapiro-Wilk normality test

data:  resid(out)
W = 0.99343, p-value = 0.000808
```

The result of shapiro.test() is significant. So we perform Wilcoxon rank
sum test.

### 2. Comparison of three or more groups

The ‘Dx’ column of acs data is diagnosis. It has three groups : Unstable
Angina, NSTEMI and STEMI. You can make a table summarizing baseline
characteristics among three groups. The parametric method comparing
means of three or more groups is ANOVA, whereas non-parametric method is
Kruskal-Wallis rank sum test.

``` r

gaze(Dx~.,data=acs) %>% myft()
```

| name | levels | NSTEMI (N=153) | STEMI (N=304) | Unstable Angina (N=400) | p |
|----|----|----|----|----|----|
| age | Mean ± SD | 64.3 ± 12.3 | 62.1 ± 12.1 | 63.8 ± 11.0 | .073 |
| sex | Female | 50 (32.7%) | 84 (27.6%) | 153 (38.2%) | .012 |
|  | Male | 103 (67.3%) | 220 (72.4%) | 247 (61.8%) |  |
| cardiogenicShock | No | 149 (97.4%) | 256 (84.2%) | 400 (100%) | \<.001 |
|  | Yes | 4 (2.6%) | 48 (15.8%) | 0 (0%) |  |
| entry | Femoral | 58 (37.9%) | 133 (43.8%) | 121 (30.2%) | .001 |
|  | Radial | 95 (62.1%) | 171 (56.2%) | 279 (69.8%) |  |
| EF | Mean ± SD | 55.0 ± 9.3 | 52.4 ± 9.5 | 59.2 ± 8.7 | \<.001 |
| height | Mean ± SD | 163.3 ± 8.2 | 165.1 ± 8.2 | 161.7 ± 9.7 | \<.001 |
| weight | Mean ± SD | 64.3 ± 10.2 | 65.7 ± 11.6 | 64.5 ± 11.6 | .361 |
| BMI | Mean ± SD | 24.1 ± 3.2 | 24.0 ± 3.3 | 24.6 ± 3.4 | .064 |
| obesity | No | 106 (69.3%) | 209 (68.8%) | 252 (63%) | .186 |
|  | Yes | 47 (30.7%) | 95 (31.2%) | 148 (37%) |  |
| TC | Mean ± SD | 193.7 ± 53.6 | 183.2 ± 43.4 | 183.5 ± 48.3 | .057 |
| LDLC | Mean ± SD | 126.1 ± 44.7 | 116.7 ± 39.5 | 112.9 ± 40.4 | .004 |
| HDLC | Mean ± SD | 38.9 ± 11.9 | 38.5 ± 11.0 | 37.8 ± 10.9 | .501 |
| TG | Mean ± SD | 130.1 ± 88.5 | 106.5 ± 72.0 | 137.4 ± 101.6 | \<.001 |
| DM | No | 96 (62.7%) | 208 (68.4%) | 249 (62.2%) | .209 |
|  | Yes | 57 (37.3%) | 96 (31.6%) | 151 (37.8%) |  |
| HBP | No | 62 (40.5%) | 150 (49.3%) | 144 (36%) | .002 |
|  | Yes | 91 (59.5%) | 154 (50.7%) | 256 (64%) |  |
| smoking | Ex-smoker | 42 (27.5%) | 66 (21.7%) | 96 (24%) | \<.001 |
|  | Never | 50 (32.7%) | 97 (31.9%) | 185 (46.2%) |  |
|  | Smoker | 61 (39.9%) | 141 (46.4%) | 119 (29.8%) |  |

#### (1) Parametric method

Now we focus on comparing means of age among three groups.

``` r
gaze(Dx~age,data=acs)  # default : method=1
———————————————————————————————————————————————————————————————————————————————————————
 Dependent:Dx   levels        NSTEMI          STEMI          Unstable Angina       p   
     (N)                     (N=153)         (N=304)             (N=400)               
———————————————————————————————————————————————————————————————————————————————————————
age           Mean ± SD       64.3 ± 12.3    62.1 ± 12.1              63.8 ± 11.0  .073 
———————————————————————————————————————————————————————————————————————————————————————
```

We can perform ANOVA as follows

``` r
out=lm(age~Dx,data=acs)
anova(out)
Analysis of Variance Table

Response: age
           Df Sum Sq Mean Sq F value  Pr(>F)  
Dx          2    715  357.62   2.624 0.07309 .
Residuals 854 116389  136.29                  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

On analysis of variance table you can get the p value 0.073.

#### (2) Non-parametric method

``` r

gaze(Dx~age,data=acs, method=2) %>% myft()
```

| name | levels | NSTEMI (N=153) | STEMI (N=304) | Unstable Angina (N=400) | p |
|----|----|----|----|----|----|
| age | Median (IQR) | 65.0 (55.0 to 75.0) | 62.0 (53.0 to 71.0) | 65.0 (56.0 to 72.0) | .109 |

The above p value in the table is the result of Kruskal-Wallis rank sum
test.

``` r
kruskal.test(age~Dx,data=acs)

    Kruskal-Wallis rank sum test

data:  age by Dx
Kruskal-Wallis chi-squared = 4.424, df = 2, p-value = 0.1095
```

          if(sum(result)<=5000) out4=shapiro.test(resid(out3))
          else out4=nortest::ad.test(resid(out3))
          out5=kruskal.test(as.numeric(x),factor(y))
          p=c(out4$p.value,anova(out3)$Pr[1],out5$p.value)

#### (3) Performs test for normality

``` r

gaze(Dx~age,data=acs, method=3) %>% myft()
```

| name | levels | NSTEMI (N=153) | STEMI (N=304) | Unstable Angina (N=400) | p |
|----|----|----|----|----|----|
| age | Median (IQR) | 65.0 (55.0 to 75.0) | 62.0 (53.0 to 71.0) | 65.0 (56.0 to 72.0) | .109 |

When method=3, gaze() performs normality test.

``` r
out=lm(age~Dx,data=acs)
shapiro.test(resid(out))

    Shapiro-Wilk normality test

data:  resid(out)
W = 0.99102, p-value = 4.413e-05
```

Since the result for normality test is significant($`p<0.001`$), then we
perform Kruskal-Wallis test.

## Statistical tests for categorical variables

The statistical methods for categorical variables in gaze() are as
follows:

- 0 : Perform chi-squared test first. If warning present, perform
  Fisher’s exact test

- 1 : Perform chi-squared test without continuity correction

- 2 : Perform chi-squared test with continuity correction (default
  value)

- 3 : perform Fisher’s exact test

- 4 : perform test for trend in proportions

You can choose by setting catMethod argument(default value is 2).

### (1) Default method : chi-squared test with continuity correction

The default method for categorical variables is chi-squared test with
Yates’s correction for
continuity(<https://en.wikipedia.org/wiki/Yates%27s_correction_for_continuity>).

``` r
gaze(sex~Dx,data=acs) # default : catMethod=2
————————————————————————————————————————————————————————————————————
 Dependent:sex      levels           Female          Male       p   
      (N)                           (N=287)        (N=570)          
————————————————————————————————————————————————————————————————————
Dx             NSTEMI                 50 (17.4%)   103 (18.1%)  .012 
               STEMI                  84 (29.3%)   220 (38.6%)       
               Unstable Angina       153 (53.3%)   247 (43.3%)       
————————————————————————————————————————————————————————————————————
```

You can get same result with the following R code:

``` r
result=table(acs$Dx,acs$sex)
chisq.test(result)  # default: correct = TRUE

    Pearson's Chi-squared test

data:  result
X-squared = 8.7983, df = 2, p-value = 0.01229
```

### (2) Chi-squared test without continuity correction

If you want to perform chi-squared test without continuity correction,
just set catMethod=1. This is the default method in SPSS.

``` r
gaze(sex~Dx,data=acs, catMethod=1) # Perform chisq.test without continuity correction
————————————————————————————————————————————————————————————————————
 Dependent:sex      levels           Female          Male       p   
      (N)                           (N=287)        (N=570)          
————————————————————————————————————————————————————————————————————
Dx             NSTEMI                 50 (17.4%)   103 (18.1%)  .012 
               STEMI                  84 (29.3%)   220 (38.6%)       
               Unstable Angina       153 (53.3%)   247 (43.3%)       
————————————————————————————————————————————————————————————————————
```

You can get same result with the following R code:

``` r
result=table(acs$Dx,acs$sex)
chisq.test(result, correct=FALSE)  # without continuity correction

    Pearson's Chi-squared test

data:  result
X-squared = 8.7983, df = 2, p-value = 0.01229
```

### (3) Fisher’s exact test

If you want to perform Fisher’s exact test, set the catMethod=3.

``` r
gaze(sex~Dx,data=acs, catMethod=3) # Perform Fisher's exact test
————————————————————————————————————————————————————————————————————
 Dependent:sex      levels           Female          Male       p   
      (N)                           (N=287)        (N=570)          
————————————————————————————————————————————————————————————————————
Dx             NSTEMI                 50 (17.4%)   103 (18.1%)  .012 
               STEMI                  84 (29.3%)   220 (38.6%)       
               Unstable Angina       153 (53.3%)   247 (43.3%)       
————————————————————————————————————————————————————————————————————
```

You can get same result with the following R code:

``` r
result=table(acs$Dx,acs$sex)
fisher.test(result)  

    Fisher's Exact Test for Count Data

data:  result
p-value = 0.01191
alternative hypothesis: two.sided
```

### (4) Test for trend in proportions

If you want to perform test for trend in proportions, set the
catMethod=4. You can perform this test only when the grouping variable
has only two group(male and female for example).

``` r
gaze(sex~Dx,data=acs, catMethod=4) # Perform test for trend in proportions
————————————————————————————————————————————————————————————————————
 Dependent:sex      levels           Female          Male       p   
      (N)                           (N=287)        (N=570)          
————————————————————————————————————————————————————————————————————
Dx             NSTEMI                 50 (17.4%)   103 (18.1%)  .050 
               STEMI                  84 (29.3%)   220 (38.6%)       
               Unstable Angina       153 (53.3%)   247 (43.3%)       
————————————————————————————————————————————————————————————————————
```

You can get same result with the following R code:

``` r
result=table(acs$Dx,acs$sex)
result
                 
                  Female Male
  NSTEMI              50  103
  STEMI               84  220
  Unstable Angina    153  247
prop.trend.test(result[,2],rowSums(result)) 

    Chi-squared Test for Trend in Proportions

data:  result[, 2] out of rowSums(result) ,
 using scores: 1 2 3
X-squared = 3.8332, df = 1, p-value = 0.05025
```

## Make a combining table with two or more grouping variables

You can make a combining table with two or more grouping variables.

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

You can select whether or not show total column.

``` r

gaze(sex+Dx~.,data=acs,show.total=TRUE) %>% myft()
```

| sex (N) |  | Female (N=287) |  |  |  |  | Male (N=570) |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|----|----|
| name | levels | NSTEMI (N=50) | STEMI (N=84) | Unstable Angina (N=153) | total (N=287) | p | NSTEMI (N=103) | STEMI (N=220) | Unstable Angina (N=247) | total (N=570) | p |
| age | Mean ± SD | 70.9 ± 11.4 | 69.1 ± 10.4 | 67.7 ± 10.7 | 68.7 ± 10.7 | .177 | 61.1 ± 11.6 | 59.4 ± 11.7 | 61.4 ± 10.6 | 60.6 ± 11.2 | .133 |
| cardiogenicShock | No | 49 (98%) | 73 (86.9%) | 153 (100%) | 275 (95.8%) | \<.001 | 100 (97.1%) | 183 (83.2%) | 247 (100%) | 530 (93%) | \<.001 |
|  | Yes | 1 (2%) | 11 (13.1%) | 0 (0%) | 12 (4.2%) |  | 3 (2.9%) | 37 (16.8%) | 0 (0%) | 40 (7%) |  |
| entry | Femoral | 22 (44%) | 45 (53.6%) | 52 (34%) | 119 (41.5%) | .013 | 36 (35%) | 88 (40%) | 69 (27.9%) | 193 (33.9%) | .022 |
|  | Radial | 28 (56%) | 39 (46.4%) | 101 (66%) | 168 (58.5%) |  | 67 (65%) | 132 (60%) | 178 (72.1%) | 377 (66.1%) |  |
| EF | Mean ± SD | 54.8 ± 9.1 | 52.3 ± 10.9 | 59.4 ± 8.8 | 56.3 ± 10.1 | \<.001 | 55.1 ± 9.4 | 52.4 ± 8.9 | 59.1 ± 8.7 | 55.6 ± 9.4 | \<.001 |
| height | Mean ± SD | 154.2 ± 5.1 | 155.7 ± 5.4 | 152.6 ± 6.7 | 153.8 ± 6.2 | .002 | 167.5 ± 5.7 | 168.7 ± 6.0 | 167.3 ± 6.4 | 167.9 ± 6.1 | .055 |
| weight | Mean ± SD | 57.2 ± 10.3 | 57.4 ± 9.0 | 57.1 ± 9.1 | 57.2 ± 9.3 | .978 | 67.5 ± 8.4 | 68.8 ± 10.9 | 69.0 ± 10.6 | 68.7 ± 10.3 | .479 |
| BMI | Mean ± SD | 24.1 ± 4.3 | 23.6 ± 3.2 | 24.5 ± 3.5 | 24.2 ± 3.6 | .215 | 24.1 ± 2.6 | 24.1 ± 3.4 | 24.6 ± 3.4 | 24.3 ± 3.2 | .205 |
| obesity | No | 35 (70%) | 60 (71.4%) | 99 (64.7%) | 194 (67.6%) | .528 | 71 (68.9%) | 149 (67.7%) | 153 (61.9%) | 373 (65.4%) | .301 |
|  | Yes | 15 (30%) | 24 (28.6%) | 54 (35.3%) | 93 (32.4%) |  | 32 (31.1%) | 71 (32.3%) | 94 (38.1%) | 197 (34.6%) |  |
| TC | Mean ± SD | 196.3 ± 52.7 | 180.7 ± 45.7 | 191.1 ± 53.1 | 188.9 ± 51.1 | .192 | 192.6 ± 54.3 | 184.1 ± 42.6 | 178.7 ± 44.6 | 183.3 ± 45.9 | .036 |
| LDLC | Mean ± SD | 127.7 ± 39.5 | 111.0 ± 40.0 | 118.3 ± 41.8 | 117.8 ± 41.2 | .088 | 125.4 ± 47.1 | 118.9 ± 39.1 | 109.5 ± 39.2 | 116.0 ± 41.1 | .002 |
| HDLC | Mean ± SD | 40.1 ± 13.8 | 39.5 ± 11.2 | 38.5 ± 10.8 | 39.0 ± 11.5 | .627 | 38.4 ± 10.9 | 38.1 ± 10.9 | 37.4 ± 10.9 | 37.8 ± 10.9 | .655 |
| TG | Mean ± SD | 112.5 ± 51.1 | 112.3 ± 87.2 | 126.3 ± 76.0 | 119.9 ± 76.2 | .316 | 138.0 ± 100.2 | 104.3 ± 65.5 | 144.3 ± 114.2 | 127.9 ± 97.3 | \<.001 |
| DM | No | 25 (50%) | 54 (64.3%) | 94 (61.4%) | 173 (60.3%) | .240 | 71 (68.9%) | 154 (70%) | 155 (62.8%) | 380 (66.7%) | .219 |
|  | Yes | 25 (50%) | 30 (35.7%) | 59 (38.6%) | 114 (39.7%) |  | 32 (31.1%) | 66 (30%) | 92 (37.2%) | 190 (33.3%) |  |
| HBP | No | 19 (38%) | 28 (33.3%) | 36 (23.5%) | 83 (28.9%) | .084 | 43 (41.7%) | 122 (55.5%) | 108 (43.7%) | 273 (47.9%) | .016 |
|  | Yes | 31 (62%) | 56 (66.7%) | 117 (76.5%) | 204 (71.1%) |  | 60 (58.3%) | 98 (44.5%) | 139 (56.3%) | 297 (52.1%) |  |
| smoking | Ex-smoker | 8 (16%) | 13 (15.5%) | 28 (18.3%) | 49 (17.1%) | .184 | 34 (33%) | 53 (24.1%) | 68 (27.5%) | 155 (27.2%) | .002 |
|  | Never | 37 (74%) | 57 (67.9%) | 115 (75.2%) | 209 (72.8%) |  | 13 (12.6%) | 40 (18.2%) | 70 (28.3%) | 123 (21.6%) |  |
|  | Smoker | 5 (10%) | 14 (16.7%) | 10 (6.5%) | 29 (10.1%) |  | 56 (54.4%) | 127 (57.7%) | 109 (44.1%) | 292 (51.2%) |  |

## Missing data analysis

You can use gaze() for missing data analysis. Set the missing argument
TRUE.

``` r

gaze(EF~.,data=acs, missing=TRUE) %>% myft()
```

| Dependent:EF     | levels          | Not missing (N=723) | Missing (N=134) | p      |
|------------------|-----------------|---------------------|-----------------|--------|
| age              | Mean ± SD       | 63.1 ± 11.9         | 64.3 ± 10.6     | .303   |
| sex              | Female          | 240 (33.2%)         | 47 (35.1%)      | .746   |
|                  | Male            | 483 (66.8%)         | 87 (64.9%)      |        |
| cardiogenicShock | No              | 686 (94.9%)         | 119 (88.8%)     | .012   |
|                  | Yes             | 37 (5.1%)           | 15 (11.2%)      |        |
| entry            | Femoral         | 262 (36.2%)         | 50 (37.3%)      | .889   |
|                  | Radial          | 461 (63.8%)         | 84 (62.7%)      |        |
| Dx               | NSTEMI          | 139 (19.2%)         | 14 (10.4%)      | \<.001 |
|                  | STEMI           | 272 (37.6%)         | 32 (23.9%)      |        |
|                  | Unstable Angina | 312 (43.2%)         | 88 (65.7%)      |        |
| height           | Mean ± SD       | 163.2 ± 9.1         | 163.1 ± 9.3     | .908   |
| weight           | Mean ± SD       | 64.7 ± 11.4         | 66.3 ± 10.7     | .251   |
| BMI              | Mean ± SD       | 24.2 ± 3.4          | 24.9 ± 3.1      | .093   |
| obesity          | No              | 465 (64.3%)         | 102 (76.1%)     | .011   |
|                  | Yes             | 258 (35.7%)         | 32 (23.9%)      |        |
| TC               | Mean ± SD       | 186.1 ± 47.5        | 179.9 ± 49.0    | .183   |
| LDLC             | Mean ± SD       | 117.5 ± 40.5        | 111.1 ± 44.3    | .110   |
| HDLC             | Mean ± SD       | 38.5 ± 11.0         | 36.9 ± 11.6     | .135   |
| TG               | Mean ± SD       | 123.7 ± 87.2        | 134.1 ± 108.9   | .309   |
| DM               | No              | 462 (63.9%)         | 91 (67.9%)      | .428   |
|                  | Yes             | 261 (36.1%)         | 43 (32.1%)      |        |
| HBP              | No              | 303 (41.9%)         | 53 (39.6%)      | .680   |
|                  | Yes             | 420 (58.1%)         | 81 (60.4%)      |        |
| smoking          | Ex-smoker       | 172 (23.8%)         | 32 (23.9%)      | .033   |
|                  | Never           | 268 (37.1%)         | 64 (47.8%)      |        |
|                  | Smoker          | 283 (39.1%)         | 38 (28.4%)      |        |

If there is no missing data, show the table summarizing missing numbers.

``` r
gaze(sex~.,data=acs,missing=TRUE) %>% myft()
There is no missing data in column 'sex'
```

| name             | levels          | N   | stats        | n   |
|------------------|-----------------|-----|--------------|-----|
| age              | Mean ± SD       | 857 | 63.3 ± 11.7  | 857 |
| cardiogenicShock | No              | 857 | 805 (93.9%)  | 805 |
|                  | Yes             |     | 52 (6.1%)    | 52  |
| entry            | Femoral         | 857 | 312 (36.4%)  | 312 |
|                  | Radial          |     | 545 (63.6%)  | 545 |
| Dx               | NSTEMI          | 857 | 153 (17.9%)  | 153 |
|                  | STEMI           |     | 304 (35.5%)  | 304 |
|                  | Unstable Angina |     | 400 (46.7%)  | 400 |
| EF               | Mean ± SD       | 723 | 55.8 ± 9.6   | 723 |
| height           | Mean ± SD       | 764 | 163.2 ± 9.1  | 764 |
| weight           | Mean ± SD       | 766 | 64.8 ± 11.4  | 766 |
| BMI              | Mean ± SD       | 764 | 24.3 ± 3.3   | 764 |
| obesity          | No              | 857 | 567 (66.2%)  | 567 |
|                  | Yes             |     | 290 (33.8%)  | 290 |
| TC               | Mean ± SD       | 834 | 185.2 ± 47.8 | 834 |
| LDLC             | Mean ± SD       | 833 | 116.6 ± 41.1 | 833 |
| HDLC             | Mean ± SD       | 834 | 38.2 ± 11.1  | 834 |
| TG               | Mean ± SD       | 842 | 125.2 ± 90.9 | 842 |
| DM               | No              | 857 | 553 (64.5%)  | 553 |
|                  | Yes             |     | 304 (35.5%)  | 304 |
| HBP              | No              | 857 | 356 (41.5%)  | 356 |
|                  | Yes             |     | 501 (58.5%)  | 501 |
| smoking          | Ex-smoker       | 857 | 204 (23.8%)  | 204 |
|                  | Never           |     | 332 (38.7%)  | 332 |
|                  | Smoker          |     | 321 (37.5%)  | 321 |

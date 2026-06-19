# Graphical Test of Proportional Hazards

Tis is a ggplot version of plot.cox.zph. Displays a graph of the scaled
Schoenfeld residuals, along with a smooth curve.

## Usage

``` r
coxzphplot(x, resid = TRUE, se = TRUE, var = NULL, hr = FALSE, add.lm = FALSE)
```

## Arguments

- x:

  result of the cox.zph function.

- resid:

  a logical value, if TRUE the residuals are included on the plot, as
  well as the smooth fit.

- se:

  a logical value, if TRUE, confidence bands at two standard errors will
  be added.

- var:

  The set of variables for which plots are desired. It can be integer or
  variable names

- hr:

  logical If true, plot for hazard ratio, If false, plot for
  coefficients

- add.lm:

  logical If true, add linear regression line

## Value

A facetted ggplot

## Examples

``` r
library(survival)
vfit <- coxph(Surv(time,status) ~ trt + factor(celltype) + karno + age, data=veteran, x=TRUE)
x <- cox.zph(vfit)
coxzphplot(x)

coxzphplot(x,var="karno",add.lm=TRUE)
```

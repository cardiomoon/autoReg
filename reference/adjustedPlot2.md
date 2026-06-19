# Draw a survfitted plot

Draw a survfitted plot

## Usage

``` r
adjustedPlot2(fit, se = FALSE, mark.time = FALSE)
```

## Arguments

- fit:

  An object of class coxph or survfit

- se:

  logical Whether or not show se

- mark.time:

  logical Whether or not mark time

## Value

a ggplot

## Examples

``` r
library(survival)
fit=coxph(Surv(time,status)~rx+logWBC,data=anderson)
plot(survfit(fit),conf.int=TRUE)

adjustedPlot2(fit,se=TRUE)
```

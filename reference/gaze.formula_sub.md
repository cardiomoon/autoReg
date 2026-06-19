# Produce table for descriptive statistics

Produce table for descriptive statistics by groups for several variables
easily. Depending on the nature of these variables, different
descriptive statistical methods were used(t-test, ANOVA, Kruskal-Wallis,
chi-squared, Fisher's,...)

## Usage

``` r
# S3 method for class 'formula_sub'
gaze(x, data, missing = FALSE, ...)
```

## Arguments

- x:

  An object of class "formula". Left side of ~ must contain the name of
  one grouping variable or two grouping variables in an additive
  way(e.g. sex+group~), and the right side of ~ must have variables in
  an additive way.

- data:

  A data.frame

- missing:

  logical If true, missing value analysis performed

- ...:

  Further arguments to be passed to gaze()

## Value

An object of class "gaze" which inherits from the class "data.frame"
with at least the following attributes:

- attr(\*,"yvars):

  character. name of dependent variable

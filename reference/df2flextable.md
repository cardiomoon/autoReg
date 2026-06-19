# Convert data.frame to flextable

Convert data.frame to flextable

## Usage

``` r
df2flextable(
  df,
  vanilla = FALSE,
  fontname = NULL,
  fontsize = 12,
  add.rownames = FALSE,
  even_header = "transparent",
  odd_header = "#5B7778",
  even_body = "#EFEFEF",
  odd_body = "transparent",
  vlines = TRUE,
  colorheader = FALSE,
  digits = 2,
  digitp = 3,
  align_header = "center",
  align_body = "right",
  align_rownames = "left",
  NA2space = TRUE,
  pcol = NULL,
  ...
)
```

## Arguments

- df:

  A data.frame

- vanilla:

  A Logical

- fontname:

  Font name

- fontsize:

  font size

- add.rownames:

  logical. Whether or not include rownames

- even_header:

  background color of even_header

- odd_header:

  background color of even_header

- even_body:

  background color of even_body

- odd_body:

  background color of even_body

- vlines:

  Logical. Whether or not draw vertical lines

- colorheader:

  Logical. Whether or not use color in header

- digits:

  integer indicating the number of decimal places

- digitp:

  integer indicating the number of decimal places of p values

- align_header:

  alignment of header. Expected value is one of 'left', 'right',
  'center', 'justify'.

- align_body:

  alignment of body. Expected value is one of 'left', 'right', 'center',
  'justify'.

- align_rownames:

  alignment of rownames. Expected value is one of 'left', 'right',
  'center', 'justify'.

- NA2space:

  A logical. If true, convert NA value to space

- pcol:

  An integer indicating p value. If specified, convert value less than
  0.01 to "\< 0.001" in given column.

- ...:

  further arguments to be passed to
  [`flextable`](https://davidgohel.github.io/flextable/reference/flextable.html)

## Value

An object of class "flextable" which is described in
[`flextable`](https://davidgohel.github.io/flextable/reference/flextable.html)

# Get interaction data from data

Get interaction data from data

## Usage

``` r
getInteraction(name, data)
```

## Arguments

- name:

  a string with interaction term

- data:

  a data.frame

## Value

An object of class "data.frame"

## Examples

``` r
data(acs,package="moonBook")
getInteraction("TC:Dx:sex",data=acs)
#>         name                   desc                             id   n
#> 1      TC:Dx                 NSTEMI                    TC:DxNSTEMI 153
#> 2      TC:Dx                  STEMI                     TC:DxSTEMI 304
#> 3      TC:Dx        Unstable Angina           TC:DxUnstable Angina 400
#> 4     TC:sex                 Female                   TC:sexFemale 287
#> 5     TC:sex                   Male                     TC:sexMale 570
#> 6     Dx:sex          NSTEMI:Female             DxNSTEMI:sexFemale  50
#> 7     Dx:sex           STEMI:Female              DxSTEMI:sexFemale  84
#> 8     Dx:sex Unstable Angina:Female    DxUnstable Angina:sexFemale 153
#> 9     Dx:sex            NSTEMI:Male               DxNSTEMI:sexMale 103
#> 10    Dx:sex             STEMI:Male                DxSTEMI:sexMale 220
#> 11    Dx:sex   Unstable Angina:Male      DxUnstable Angina:sexMale 247
#> 12 TC:Dx:sex          NSTEMI:Female          TC:DxNSTEMI:sexFemale  50
#> 13 TC:Dx:sex           STEMI:Female           TC:DxSTEMI:sexFemale  84
#> 14 TC:Dx:sex Unstable Angina:Female TC:DxUnstable Angina:sexFemale 153
#> 15 TC:Dx:sex            NSTEMI:Male            TC:DxNSTEMI:sexMale 103
#> 16 TC:Dx:sex             STEMI:Male             TC:DxSTEMI:sexMale 220
#> 17 TC:Dx:sex   Unstable Angina:Male   TC:DxUnstable Angina:sexMale 247
```

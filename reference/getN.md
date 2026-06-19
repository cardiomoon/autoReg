# Get number of data specified by 'name' and 'desc'

Get number of data specified by 'name' and 'desc'

## Usage

``` r
getN(name, desc, data)
```

## Arguments

- name:

  a string with interaction term

- desc:

  character

- data:

  a data.frame

## Value

A numeric vector

## Examples

``` r
data(acs,package="moonBook")
df=getInteraction("TC:Dx:sex",data=acs)
getN(name=df$name,desc=df$desc,data=acs)
#>  [1] 153 304 400 287 570  50  84 153 103 220 247  50  84 153 103 220 247
```

# Parse model code

Parse model code

## Usage

``` r
nldoc_parse_modelcode(nlogocode)
```

## Arguments

- nlogocode:

  vector of netlogo code strings

## Value

tibble with structured netlogo code

## Details

The procedure searches for noxygen commands within the NetLogo code.
This information is used to structure the netlogo code strings in a
tibble. Additionally, tibbles with gui and behavior space information
are created.

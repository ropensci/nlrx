# Determine procedure calls

Determine procedure calls

## Usage

``` r
nldoc_find_procedure_calls(nlogocode)
```

## Arguments

- nlogocode:

  vector of netlogo code strings

## Value

tibble with procedure names and procedure calls

## Details

The procedure searches netlogo code for procedure definitions and calls.
The information is stored within a tibble that can be further processed.

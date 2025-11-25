# Add a distinct simdesign to a nl object

Add a distinct simdesign to a nl object

## Usage

``` r
simdesign_distinct(nl, nseeds)
```

## Arguments

- nl:

  nl object with a defined experiment

- nseeds:

  number of seeds for this simulation design

## Value

simdesign S4 class object

## Details

This function creates a simdesign S4 class which can be added to a nl
object. The distinct simdesign allows to create a parameter matrix with
distinct parameterisations.

Variables in the experiment variable list need to provide a vector of
distinct values (e.g. list(values=c(1,2,3,4)). All vectors of values
must have the same length across variables.

The distinct simdesign then creates one simulation run for all first
elements of these values vectors, one run for all second items, and so
on. With this function, multiple distinct simulations can be run at
once. Finally, the function reports a simdesign object.

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load a nl object from test data.

nl <- nl_distinct
nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 3)
#> Creating distinct simulation design

```

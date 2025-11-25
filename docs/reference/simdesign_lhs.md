# Add a latin-hypercube simdesign to a nl object

Add a latin-hypercube simdesign to a nl object

## Usage

``` r
simdesign_lhs(nl, samples, nseeds, precision)
```

## Arguments

- nl:

  nl object with a defined experiment

- samples:

  number of samples for the latin hypercube

- nseeds:

  number of seeds for this simulation design

- precision:

  number of digits for the decimal fraction of parameter values

## Value

simdesign S4 class object

## Details

This function creates a simdesign S4 class which can be added to a nl
object.

Variables in the experiment variable list need to provide a numeric
distribution with min, max and qfun (e.g. list(min=1, max=4,
qfun="qunif")).

The latin hypercube simdesign creates a parameter matrix based on these
defined distributions. Finally, the function reports a simdesign object.

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load a nl object from test data.

nl <- nl_lhs
nl@simdesign <- simdesign_lhs(nl=nl,
                               samples=100,
                               nseeds=3,
                               precision=3)
#> Creating latin hypercube simulation design
```

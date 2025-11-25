# Add an eFast simdesign to a nl object

Add an eFast simdesign to a nl object

## Usage

``` r
simdesign_eFast(nl, samples, nseeds)
```

## Arguments

- nl:

  nl object with a defined experiment

- samples:

  number of samples for the eFast sensitivity analysis

- nseeds:

  number of seeds for this simulation design

## Value

simdesign S4 class object

## Details

This function creates a simdesign S4 class which can be added to a nl
object.

Variables in the experiment variable list need to provide a numeric
distribution with min, max and qfun (e.g. list(min=1, max=4,
qfun="qunif")).

The eFast simdesign uses the sensitivity package to set up a fast99
elementary effects sensitivity analysis, including a simobject of class
fast99 and a input tibble for simulations. For details on method
specific sensitivity analysis function parameters see ?fast99 Finally,
the function reports a simdesign object.

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load a nl object from test data.

nl <- nl_eFast
nl@simdesign <- simdesign_eFast(nl=nl,
                                 samples=100,
                                 nseeds=1)
#> Creating eFast simulation design

```

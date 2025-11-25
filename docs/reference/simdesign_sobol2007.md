# Add a sobol2007 simdesign to a nl object

Add a sobol2007 simdesign to a nl object

## Usage

``` r
simdesign_sobol2007(nl, samples, sobolnboot, sobolconf, nseeds, precision)
```

## Arguments

- nl:

  nl object with a defined experiment

- samples:

  number of samples for the sobol sensitivity analysis

- sobolnboot:

  number of bootstrap replicates of the sobol sensitivity analysis

- sobolconf:

  the confidence level for bootstrap confidence intervals

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

The sobol2007 simdesign uses the sensitivity package to set up a
sobol2007 sensitivity analysis, including a simobject of class sobol and
a input tibble for simulations. For details on method specific
sensitivity analysis function parameters see ?sobol2007 Finally, the
function reports a simdesign object.

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load a nl object from test data.

nl <- nl_sobol2007
nl@simdesign <- simdesign_sobol2007(nl=nl,
samples=1000,
sobolnboot=100,
sobolconf=0.95,
nseeds=3,
precision=3)
#> Creating sobol2007 simulation design

```

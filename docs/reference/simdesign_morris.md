# Add a morris elementary effects simdesign to a nl object

Add a morris elementary effects simdesign to a nl object

## Usage

``` r
simdesign_morris(nl, morristype, morrislevels, morrisr, morrisgridjump, nseeds)
```

## Arguments

- nl:

  nl object with a defined experiment

- morristype:

  morris design type

- morrislevels:

  number of parameter levels

- morrisr:

  morris r value

- morrisgridjump:

  morris grid jump value

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

The morris simdesign uses the sensitivity package to set up a morris
elementary effects sensitivity analysis, including a simobject of class
morris and a input tibble for simulations. For details on method
specific sensitivity analysis function parameters see ?morris Finally,
the function reports a simdesign object.

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load a nl object from test data.

nl <- nl_morris
nl@simdesign <- simdesign_morris(nl=nl,
                                  morristype="oat",
                                  morrislevels=4,
                                  morrisr=20,
                                  morrisgridjump=2,
                                  nseeds=3)
#> Creating morris simulation design
#> Warning: keeping 10 repetitions out of 20

```

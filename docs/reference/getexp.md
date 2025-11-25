# Getter function to get a variable of an experiment object

Getter function to get a variable of an experiment object

## Usage

``` r
getexp(nl, var)
```

## Arguments

- nl:

  nl object

- var:

  valid experiment variable string

## Examples

``` r
# Example for Wolf Sheep Predation model from NetLogo models library:
nl <- nl(nlversion = "6.0.3",
nlpath = "/home/user/NetLogo 6.0.3/",
modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
jvmmem = 1024)

# Set experiment name
setexp(nl, "expname") <- "experimentName"

# Get experiment name
getexp(nl, "experiment")
#> NULL
```

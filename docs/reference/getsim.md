# Getter function to get a variable of a simdesign object

Getter function to get a variable of a simdesign object

## Usage

``` r
getsim(nl, var)
```

## Arguments

- nl:

  nl object

- var:

  valid simdesign variable string

## Examples

``` r
# Example for Wolf Sheep Predation model from NetLogo models library:
nl <- nl(nlversion = "6.0.3",
nlpath = "/home/user/NetLogo 6.0.3/",
modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
jvmmem = 1024)

# Set simulation seeds
setsim(nl, "simseeds") <- c(123, 456, 789)

# Set simulation seeds
getsim(nl, "simseeds")
#> [1] 123 456 789
```

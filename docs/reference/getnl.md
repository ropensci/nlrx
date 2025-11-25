# Getter function to get a variable of a nl object

Getter function to get a variable of a nl object

## Usage

``` r
getnl(nl, var)
```

## Arguments

- nl:

  nl object

- var:

  valid nl variable string

## Examples

``` r
# Example for Wolf Sheep Predation model from NetLogo models library:
nl <- nl(nlversion = "6.0.3",
nlpath = "/home/user/NetLogo 6.0.3/",
modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
jvmmem = 1024)

# get NetLogo version
getnl(nl, "nlversion")
#> [1] "6.0.3"
```

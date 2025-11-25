# Test if nlrx runs on the local system

Runs a short test if nlrx runs on the local system

## Usage

``` r
test_nlrx(nlpath, nlversion)
```

## Arguments

- nlpath:

  Provide a path to a netlogo folder

- nlversion:

  Matching version string of the provided NetLogo folder (e.g. "6.1.1")

## Details

Runs a short test if nlrx runs on the local system. Reports TRUE if
successful!

## Examples

``` r
if (FALSE) { # \dontrun{
test_nlrx(nlpath="/Users/xyz/netlogo/NetLogo 6.1.1", nlversion="6.1.1")
} # }
```

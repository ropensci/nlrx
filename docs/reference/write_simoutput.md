# Write attached NetLogo simulation output to file

Write attached NetLogo simulation output to file

## Usage

``` r
write_simoutput(nl, outpath = NA)
```

## Arguments

- nl:

  nl object

- outpath:

  optional path to directory where output is written

  Write NetLogo simulation output to a csv file in the directory outpath
  of the nl object Output has to be attached to the simdesign first with
  simoutput(nl) \<- results The outpath argument can be optionally used
  to write output to a different directory than the defined outpath of
  the nl object.

## Examples

``` r
# Load nl object including output data from testdata
nl <- nl_lhs

# Write output to outpath directory
write_simoutput(nl, outpath=tempdir())
```

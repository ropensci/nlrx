# Evaluate variables list of an experiment object for full-factorial simdesign

Evaluate variables list of an experiment object for full-factorial
simdesign

## Usage

``` r
util_eval_variables_ff(nl)
```

## Arguments

- nl:

  nl object

## Details

util_eval_variables_ff checks if the variables list of an experiment
within a nl object has enough information to create a
[simdesign_ff](https://docs.ropensci.org/nlrx/reference/simdesign_ff.md).
It reports an error message if at least one variable does not have a
defined sequence (min, max, step) or a vector of distinct values.

# Evaluate variables list of an experiment object for distinct simdesign

Evaluate variables list of an experiment object for distinct simdesign

## Usage

``` r
util_eval_variables_distinct(nl)
```

## Arguments

- nl:

  nl object

## Details

util_eval_variables_distinct checks if the variables list of an
experiment within a nl object has enough information to create a
[simdesign_distinct](https://docs.ropensci.org/nlrx/reference/simdesign_distinct.md).
It reports an error message if at least one variable does not have a
vector of distinct values or if there is a mismatch in length of these
values vectors.

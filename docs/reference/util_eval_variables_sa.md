# Evaluate variables list of an experiment object for sensitivity analysis simdesigns

Evaluate variables list of an experiment object for sensitivity analysis
simdesigns

## Usage

``` r
util_eval_variables_sa(nl)
```

## Arguments

- nl:

  nl object

## Details

util_eval_variables_sa checks if the variables list of an experiment
within a nl object has enough information to create a sensitivity
analysis simdesign. It reports an error message if at least one variable
does not have a defined distribution (min, max, qfun).

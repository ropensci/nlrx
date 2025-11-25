# Evaluate variables list of an experiment object for optimization simdesigns

Evaluate variables list of an experiment object for optimization
simdesigns

## Usage

``` r
util_eval_variables_op(nl)
```

## Arguments

- nl:

  nl object

## Details

util_eval_variables_op checks if the variables list of an experiment
within a nl object has enough information to create an optimization
simdesign. It reports an error message if at least one variable does not
have a defined range (min, max).

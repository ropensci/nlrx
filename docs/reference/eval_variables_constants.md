# Evaluate variable validity

Evaluate variables and constants defined in experiment

## Usage

``` r
eval_variables_constants(nl)
```

## Arguments

- nl:

  nl object

## Details

This function checks if the variables and constants that are defined in
the experiment are valid. It loads the model code of the NetLogo model
and checks if these variables and constants really exist. In case of
nonvalid entries, the function throws an error message, indicating which
variables and constants are not valid. Please note, that this function
might fail if the supported modelpath does not point to an existing
nlogo file. This might for example happen, if the modelpath is set up
for a remote cluster execution.

## Examples

``` r
if (FALSE) { # \dontrun{
nl <- nl_lhs
eval_variables_constants(nl)
} # }
```

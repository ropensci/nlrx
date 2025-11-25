# Report globals from a NetLogo model that is defined within a nl object

Report globals from a NetLogo model that is defined within a nl object

## Usage

``` r
report_model_parameters(nl)
```

## Arguments

- nl:

  nl object with a defined modelpath that points to a NetLogo model
  (\*.nlogo)

## Details

The function reads the NetLogo model file that is defined within the nl
object and reports all global parameters that are defined as widget
elements on the GUI of the NetLogo model. Only globals that are found by
this function are valid globals that can be entered into the variables
or constants vector of an experiment object.

## Examples

``` r
if (FALSE) { # \dontrun{
nl <- nl_lhs
report_model_parameters(nl)
} # }
```

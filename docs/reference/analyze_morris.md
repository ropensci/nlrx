# Analyze NetLogo simulation output of simdesign morris

Analyze NetLogo simulation output of simdesign morris

## Usage

``` r
analyze_morris(nl, metrics, funs)
```

## Arguments

- nl:

  nl object

- metrics:

  vector of strings defining metric columns for evaluation. Defaults to
  metrics of the experiment within the nl object

- funs:

  list with the summary metrics for the sensitivity results

## Details

The function calculates morris sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

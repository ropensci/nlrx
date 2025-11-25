# Analyze NetLogo simulation output of simdesign soboljansen

Analyze NetLogo simulation output of simdesign soboljansen

## Usage

``` r
analyze_soboljansen(nl, metrics, funs)
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

The function calculates sobol sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

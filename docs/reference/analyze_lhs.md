# Analyze NetLogo simulation output of simdesign latin-hypercube

Analyze NetLogo simulation output of simdesign latin-hypercube

## Usage

``` r
analyze_lhs(nl, metrics, funs)
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

The function calculates aggregated output metrics by dropping random
seeds and aggregating values with the provided functions.

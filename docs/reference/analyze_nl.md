# Analyze NetLogo simulation output

Analyze NetLogo simulation output

## Usage

``` r
analyze_nl(nl, metrics = getexp(nl, "metrics"), funs = list(mean = mean))
```

## Arguments

- nl:

  nl object

- metrics:

  vector of strings defining metric columns for evaluation. Defaults to
  metrics of the experiment within the nl object

- funs:

  list with the summary metrics for the sensitivity results

## Value

analysis summary tibble

## Details

The analyze_nl function runs basic analyses on NetLogo simulation
output. In order to execute this function, simulation output needs to be
attached to the simdesign first with `setsim(nl, "output") <- results`.

analyze_nl calls different post-processing analysis functions, depending
on the specified method in the simdesign object of the nl object.

**The following simdesign are currently supported:**

[simdesign_ff](https://docs.ropensci.org/nlrx/reference/simdesign_ff.md)

Calls
[analyze_ff](https://docs.ropensci.org/nlrx/reference/analyze_ff.md).
The function calculates aggregated output metrics by dropping random
seeds and aggregating values with the provided functions.

[simdesign_lhs](https://docs.ropensci.org/nlrx/reference/simdesign_lhs.md)

Calls
[analyze_lhs](https://docs.ropensci.org/nlrx/reference/analyze_lhs.md).
The function calculates aggregated output metrics by dropping random
seeds and aggregating values with the provided functions.

[simdesign_sobol](https://docs.ropensci.org/nlrx/reference/simdesign_sobol.md)

Calls
[analyze_sobol](https://docs.ropensci.org/nlrx/reference/analyze_sobol.md).
The function calculates sobol sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

[simdesign_sobol2007](https://docs.ropensci.org/nlrx/reference/simdesign_sobol2007.md)

Calls
[analyze_sobol2007](https://docs.ropensci.org/nlrx/reference/analyze_sobol2007.md).
The function calculates sobol sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

[simdesign_soboljansen](https://docs.ropensci.org/nlrx/reference/simdesign_soboljansen.md)

Calls
[analyze_soboljansen](https://docs.ropensci.org/nlrx/reference/analyze_soboljansen.md).
The function calculates sobol sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

[simdesign_morris](https://docs.ropensci.org/nlrx/reference/simdesign_morris.md)

Calls
[analyze_morris](https://docs.ropensci.org/nlrx/reference/analyze_morris.md).
The function calculates morris sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

[simdesign_eFast](https://docs.ropensci.org/nlrx/reference/simdesign_eFast.md)

Calls
[analyze_eFast](https://docs.ropensci.org/nlrx/reference/analyze_eFast.md).
The function calculates eFast sensitivity indices from the output
results using the
[sensitivity](https://rdrr.io/pkg/sensitivity/man/sensitivity-package.html)
package.

**For the following simdesign no postprocessing analysis function has
been implemented yet:**

[simdesign_simple](https://docs.ropensci.org/nlrx/reference/simdesign_simple.md),
[simdesign_distinct](https://docs.ropensci.org/nlrx/reference/simdesign_distinct.md),
[simdesign_GenSA](https://docs.ropensci.org/nlrx/reference/simdesign_GenSA.md),
[simdesign_GenAlg](https://docs.ropensci.org/nlrx/reference/simdesign_GenAlg.md)

## Examples

``` r
# Load nl object including output data from testdata
nl <- nl_sobol

# Define aggregation measurements:
myfuns <- list(mean=mean, sd=sd, min=min, max=max)

# Calculate sensitivity indices:
analyze_nl(nl, funs = myfuns)
#> [1] "No missing combinations detected!"
#> # A tibble: 18 × 8
#>    original     bias `std. error` `min. c.i.` `max. c.i.` parameter metric  seed
#>       <dbl>    <dbl>        <dbl>       <dbl>       <dbl> <chr>     <chr>  <dbl>
#>  1    0.114 0.00453        0.176       0            0.490 initial-… count…  8515
#>  2    0.554 0              0.0979      0.370        0.757 initial-… count…  8515
#>  3    0.346 0.000234       0.213       0            0.797 initial-… count…  8515
#>  4    0.652 0              0.0461      0.576        0.761 initial-… count…  8515
#>  5    0     0              0.105       0            0.172 initial-… count…  8515
#>  6    0.365 0.00481        0.122       0.124        0.625 initial-… count…  8515
#>  7    0.114 0.00388        0.171       0            0.476 initial-… count…  8515
#>  8    0.554 0.00157        0.113       0.334        0.796 initial-… count…  8515
#>  9    0.346 0              0.242       0            0.865 initial-… count…  8515
#> 10    0.652 0              0.0633      0.515        0.783 initial-… count…  8515
#> 11    0     0              0.0901      0            0.197 initial-… count…  8515
#> 12    0.365 0.0183         0.120       0.0517       0.598 initial-… count…  8515
#> 13    0.114 0.00681        0.153       0            0.425 initial-… count…  8515
#> 14    0.554 0.0101         0.110       0.300        0.776 initial-… count…  8515
#> 15    0.346 0              0.200       0            0.825 initial-… count…  8515
#> 16    0.652 0              0.0589      0.557        0.790 initial-… count…  8515
#> 17    0     0              0.0895      0            0.171 initial-… count…  8515
#> 18    0.365 0.00713        0.123       0.0685       0.626 initial-… count…  8515
```

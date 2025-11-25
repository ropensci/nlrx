# Get spatial data from metrics.turtles and metrics.patches output

Turn results from NetLogo in spatial data objects

## Usage

``` r
unnest_simoutput(nl)
```

## Arguments

- nl:

  nl object

## Value

tibble with spatial data objects

## Details

Unnests output from run_nl into long format.

## Examples

``` r
# To unnest data a nl object containing spatial output data is needed.
# For this example, we load a nl object from test data.

nl <- nl_spatial
unnest_simoutput(nl)
#> # A tibble: 30,370 × 21
#>    `[run number]` `model-version`  `initial-number-sheep` initial-number-wolve…¹
#>             <dbl> <chr>                             <dbl>                  <dbl>
#>  1              1 sheep-wolves-gr…                    100                     50
#>  2              1 sheep-wolves-gr…                    100                     50
#>  3              1 sheep-wolves-gr…                    100                     50
#>  4              1 sheep-wolves-gr…                    100                     50
#>  5              1 sheep-wolves-gr…                    100                     50
#>  6              1 sheep-wolves-gr…                    100                     50
#>  7              1 sheep-wolves-gr…                    100                     50
#>  8              1 sheep-wolves-gr…                    100                     50
#>  9              1 sheep-wolves-gr…                    100                     50
#> 10              1 sheep-wolves-gr…                    100                     50
#> # ℹ 30,360 more rows
#> # ℹ abbreviated name: ¹​`initial-number-wolves`
#> # ℹ 17 more variables: `grass-regrowth-time` <dbl>,
#> #   `sheep-gain-from-food` <dbl>, `wolf-gain-from-food` <dbl>,
#> #   `sheep-reproduce` <dbl>, `wolf-reproduce` <dbl>, `show-energy?` <lgl>,
#> #   `random-seed` <dbl>, `[step]` <dbl>, `count sheep` <dbl>,
#> #   `count wolves` <dbl>, breed <chr>, who <dbl>, pxcor <dbl>, pycor <dbl>, …

```

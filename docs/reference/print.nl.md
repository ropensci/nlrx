# Print content of nl object

Print content of nl object and embedded experiment and simdesign objects
to console

## Usage

``` r
# S3 method for class 'nl'
print(x, ...)
```

## Arguments

- x:

  nl object to print

- ...:

  further arguments passed to or from other methods

## Details

Print content of the provided nl object in a readable format.

## Examples

``` r
print(nl_lhs)
#> 
#>    NL OBJECT   
#> NetLogo version = 6.0.4
#> NetLogo path    = C:/Program Files/NetLogo 6.0.4/
#> Model path      = C:/Program Files/NetLogo 6.0.4/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo
#> JVM memory      = 1000
#> 
#>    EXPERIMENT   
#> Experiment name        = nlrx
#> Output path            = C:/out
#> NetLogo repetitions    = 1
#> Measure on each tick?  = false
#> Setup procedure(s)     = setup
#> Go procedure(s)        = go
#> Final procedure(s)     = NA
#> Run nr. widget name    = NA
#> Runtime (ticks)        = 10
#> Report output on ticks = 10
#> Stop condition         = NA
#> Metrics (output)       = count sheep, count wolves
#> 
#> Turtle metrics (output)
#>      = 
#> 
#> Patch metrics (output)
#>     NA
#> 
#> Link metrics (output)
#>      = 
#> 
#> Variable parameters (input)
#>     initial-number-sheep = list(min = 50, max = 150, step = 10, qfun = "qunif")
#>     initial-number-wolves = list(min = 50, max = 150, step = 10, qfun = "qunif")
#> 
#> Constant parameters (input)
#>     model-version = "sheep-wolves-grass"
#>     grass-regrowth-time = 30
#>     sheep-gain-from-food = 4
#>     wolf-gain-from-food = 20
#>     sheep-reproduce = 4
#>     wolf-reproduce = 5
#>     show-energy? = false
#> 
#>    SIMDESIGN   
#> Simulation method      = lhs
#> Simulation object      = 
#> Generated random seeds = -108303981
#> 
#> Parameter matrix (input)
#> # A tibble: 100 × 9
#>    `initial-number-sheep` `initial-number-wolves` `model-version`         
#>                     <dbl>                   <dbl> <chr>                   
#>  1                  117.                    109.  "\"sheep-wolves-grass\""
#>  2                   99.6                    56.1 "\"sheep-wolves-grass\""
#>  3                   91.7                    82.3 "\"sheep-wolves-grass\""
#>  4                  115.                     57.5 "\"sheep-wolves-grass\""
#>  5                   84.6                   111.  "\"sheep-wolves-grass\""
#>  6                  120.                    125.  "\"sheep-wolves-grass\""
#>  7                   54.6                   139.  "\"sheep-wolves-grass\""
#>  8                   90.5                   147.  "\"sheep-wolves-grass\""
#>  9                   50.3                    93.7 "\"sheep-wolves-grass\""
#> 10                  111.                    119.  "\"sheep-wolves-grass\""
#>    `grass-regrowth-time` `sheep-gain-from-food` `wolf-gain-from-food`
#>                    <dbl>                  <dbl>                 <dbl>
#>  1                    30                      4                    20
#>  2                    30                      4                    20
#>  3                    30                      4                    20
#>  4                    30                      4                    20
#>  5                    30                      4                    20
#>  6                    30                      4                    20
#>  7                    30                      4                    20
#>  8                    30                      4                    20
#>  9                    30                      4                    20
#> 10                    30                      4                    20
#>    `sheep-reproduce` `wolf-reproduce` `show-energy?`
#>                <dbl>            <dbl> <chr>         
#>  1                 4                5 false         
#>  2                 4                5 false         
#>  3                 4                5 false         
#>  4                 4                5 false         
#>  5                 4                5 false         
#>  6                 4                5 false         
#>  7                 4                5 false         
#>  8                 4                5 false         
#>  9                 4                5 false         
#> 10                 4                5 false         
#> # ℹ 90 more rows
#> 
#> Simulation results (output)
#> # A tibble: 100 × 15
#>    `[run number]` `initial-number-sheep` `initial-number-wolves`
#>             <dbl>                  <dbl>                   <dbl>
#>  1              1                  117.                    109. 
#>  2              1                   99.6                    56.1
#>  3              1                   91.7                    82.3
#>  4              1                  115.                     57.5
#>  5              1                   84.6                   111. 
#>  6              1                  120.                    125. 
#>  7              1                   54.6                   139. 
#>  8              1                   90.5                   147. 
#>  9              1                   50.3                    93.7
#> 10              1                  111.                    119. 
#>    `model-version`    `grass-regrowth-time` `sheep-gain-from-food`
#>    <chr>                              <dbl>                  <dbl>
#>  1 sheep-wolves-grass                    30                      4
#>  2 sheep-wolves-grass                    30                      4
#>  3 sheep-wolves-grass                    30                      4
#>  4 sheep-wolves-grass                    30                      4
#>  5 sheep-wolves-grass                    30                      4
#>  6 sheep-wolves-grass                    30                      4
#>  7 sheep-wolves-grass                    30                      4
#>  8 sheep-wolves-grass                    30                      4
#>  9 sheep-wolves-grass                    30                      4
#> 10 sheep-wolves-grass                    30                      4
#>    `wolf-gain-from-food` `sheep-reproduce` `wolf-reproduce` `show-energy?`
#>                    <dbl>             <dbl>            <dbl> <lgl>         
#>  1                    20                 4                5 FALSE         
#>  2                    20                 4                5 FALSE         
#>  3                    20                 4                5 FALSE         
#>  4                    20                 4                5 FALSE         
#>  5                    20                 4                5 FALSE         
#>  6                    20                 4                5 FALSE         
#>  7                    20                 4                5 FALSE         
#>  8                    20                 4                5 FALSE         
#>  9                    20                 4                5 FALSE         
#> 10                    20                 4                5 FALSE         
#>    `random-seed` `[step]` `count sheep` `count wolves` siminputrow
#>            <dbl>    <dbl>         <dbl>          <dbl>       <dbl>
#>  1    -108303981       10           102            115           1
#>  2    -108303981       10            80             86           2
#>  3    -108303981       10            78            100           3
#>  4    -108303981       10           101             65           4
#>  5    -108303981       10            78            118           5
#>  6    -108303981       10            93            141           6
#>  7    -108303981       10            42            145           7
#>  8    -108303981       10            64            154           8
#>  9    -108303981       10            37            100           9
#> 10    -108303981       10            76            123          10
#> # ℹ 90 more rows
#> 
#>    SUMMARY   
#> supported nlversion: ✓
#> nlpath exists on local system: ✗
#> modelpath exists on local system: ✗
#> valid jvm memory: ✓
#> valid experiment name: ✓
#> outpath exists on local system: ✗
#> setup and go defined: ✓
#> variables defined: ✓
#> constants defined: ✓
#> metrics defined: ✓
#> spatial Metrics defined: ✓
#> simdesign attached: ✓
#> siminput parameter matrix: ✓
#> number of siminputrows: 100
#> number of random seeds: 1
#> estimated number of runs: 100
#> simoutput results attached: ✓
#> number of runs calculated: 100
```

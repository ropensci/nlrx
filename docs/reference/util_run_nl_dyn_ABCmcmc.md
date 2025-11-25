# ABCmcmc call simulations function

ABCmcmc call simulations function

## Usage

``` r
util_run_nl_dyn_ABCmcmc(
  nl,
  seed,
  cleanup.csv = TRUE,
  cleanup.xml = TRUE,
  cleanup.bat = TRUE
)
```

## Arguments

- nl:

  nl object

- seed:

  current model seed

- cleanup.csv:

  TRUE/FALSE, if TRUE temporary created csv output files will be deleted
  after gathering results.

- cleanup.xml:

  TRUE/FALSE, if TRUE temporary created xml output files will be deleted
  after gathering results.

- cleanup.bat:

  TRUE/FALSE, if TRUE temporary created bat/sh output files will be
  deleted after gathering results.

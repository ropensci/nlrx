# Genetic Algorithm run simulation function

Genetic Algorithm run simulation function

Genetic Algorithm run simulation function

## Usage

``` r
util_run_nl_dyn_GenAlg_fn(
  param,
  nl,
  evalcrit,
  seed,
  cleanup.csv,
  cleanup.xml,
  cleanup.bat
)

util_run_nl_dyn_ABCmcmc_fn(param)
```

## Arguments

- param:

  vector of model parameters passed from ABC_mcmc function. If use_seeds
  = TRUE, the first element of this vector is a random seed

- nl:

  nl object

- evalcrit:

  evaluation criterion for simulated annealing

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

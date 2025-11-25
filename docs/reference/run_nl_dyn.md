# Execute NetLogo simulation without pregenerated parametersets

Execute NetLogo simulation from a nl object with a defined experiment
and simdesign but no pregenerated input parametersets

## Usage

``` r
run_nl_dyn(
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

  a random seed for the NetLogo simulation

- cleanup.csv:

  TRUE/FALSE, if TRUE temporary created csv output files will be deleted
  after gathering results.

- cleanup.xml:

  TRUE/FALSE, if TRUE temporary created xml output files will be deleted
  after gathering results.

- cleanup.bat:

  TRUE/FALSE, if TRUE temporary created bat/sh output files will be
  deleted after gathering results.

## Value

simulation output results can be tibble, list, ...

## Details

run_nl_dyn can be used for simdesigns where no predefined parametersets
exist. This is the case for dynamic designs, such as Simulated Annealing
and Genetic Algorithms, where parametersets are dynamically generated,
based on the output of previous simulations. The logical cleanup
variables can be set to FALSE to preserve temporary generated output
files (e.g. for debugging). cleanup.csv deletes/keeps the temporary
generated model output files from each run. cleanup.xml deletes/keeps
the temporary generated experiment xml files from each run. cleanup.bat
deletes/keeps the temporary generated batch/sh commandline files from
each run.

## Examples

``` r
if (FALSE) { # \dontrun{

# Load nl object form test data:
nl <- nl_lhs

# Add genalg simdesign:
nl@simdesign <- simdesign_GenAlg(nl=nl,
                                  popSize = 200,
                                  iters = 100,
                                  evalcrit = 1,
                                  nseeds = 1)

# Run simulations:
results <- run_nl_dyn(nl)

} # }
```

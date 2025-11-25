# Execute one NetLogo simulation from a nl object

Execute one NetLogo simulation from a nl object with a defined
experiment and simdesign

## Usage

``` r
run_nl_one(
  nl,
  seed,
  siminputrow,
  cleanup.csv = TRUE,
  cleanup.xml = TRUE,
  cleanup.bat = TRUE,
  writeRDS = FALSE
)
```

## Arguments

- nl:

  nl object

- seed:

  a random seed for the NetLogo simulation

- siminputrow:

  rownumber of the input tibble within the attached simdesign object
  that should be executed

- cleanup.csv:

  TRUE/FALSE, if TRUE temporary created csv output files will be deleted
  after gathering results.

- cleanup.xml:

  TRUE/FALSE, if TRUE temporary created xml output files will be deleted
  after gathering results.

- cleanup.bat:

  TRUE/FALSE, if TRUE temporary created bat/sh output files will be
  deleted after gathering results.

- writeRDS:

  TRUE/FALSE, if TRUE an rds file with the simulation results will be
  written to the defined outpath folder of the experiment within the nl
  object.

## Value

tibble with simulation output results

## Details

run_nl_one executes one simulation of the specified NetLogo model within
the provided nl object. The random seed is set within the NetLogo model
to control stochasticity. The siminputrow number defines which row of
the input data tibble within the simdesign object of the provided nl
object is executed. The logical cleanup variables can be set to FALSE to
preserve temporary generated output files (e.g. for debugging).
cleanup.csv deletes/keeps the temporary generated model output files
from each run. cleanup.xml deletes/keeps the temporary generated
experiment xml files from each run. cleanup.bat deletes/keeps the
temporary generated batch/sh commandline files from each run.

This function can be used to run single simulations of a NetLogo model.

## Examples

``` r
if (FALSE) { # \dontrun{

# Load nl object from test data:
nl <- nl_lhs

# Run one simulation:
results <- run_nl_one(nl = nl,
                      seed = getsim(nl, "simseeds")[1],
                      siminputrow = 1)

} # }
```

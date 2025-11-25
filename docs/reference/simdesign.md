# Construct a new simdesign object

Construct a new simdesign object

## Usage

``` r
simdesign(
  simmethod = character(),
  siminput = tibble::tibble(),
  simobject = list(),
  simseeds = NA_integer_,
  simoutput = tibble::tibble(),
  ...
)
```

## Arguments

- simmethod:

  character string defining the method of the simulation design

- siminput:

  tibble providing input parameterisations for the NetLogo model
  (cols=parameter, rows=runs)

- simobject:

  used for some methods to store additional information (sobol, morris,
  eFast)

- simseeds:

  a vector or model random seeds

- simoutput:

  tibble containing model results

- ...:

  ...

## Value

simdesign S4 class object

## Details

The simulation design class holds information on the input parameter
design of model simulations. It also stores information that is needed
to run method specific analysis functions. The simseeds can be used to
run all model simulations that are defined within the siminput tibble
several times with changing random-seeds. While it is possible to add
simdesign directly with this function, we suggest to use our
simdesign_helper functions. A simulation design can be attached to a nl
object by using one of these simdesign_helper functions on an already
defined [nl](https://docs.ropensci.org/nlrx/reference/nl.md) object with
a valid
[experiment](https://docs.ropensci.org/nlrx/reference/experiment.md).
All simdesign helpers use the defined constants and variables of the
experiment to create the siminput tibble. NetLogo parameters that are
not defined in constants or variables will be set with their default
value from the NetLogo interface.

Currently, following simdesign_helper functions are provided:

[simdesign_simple](https://docs.ropensci.org/nlrx/reference/simdesign_simple.md)

The simple simdesign only uses defined constants and reports a parameter
matrix with only one parameterization. To setup a simple simdesign, no
variables have to be defined.

[simdesign_distinct](https://docs.ropensci.org/nlrx/reference/simdesign_distinct.md)

The distinct simdesign can be used to run distinct parameter
combinations. To setup a distinct simdesign, vectors of values need to
be defined for each variable. These vectors must have the same number of
elements across all variables. The first simulation run consist of all
1st elements of these variable vectors; the second run uses all 2nd
values, and so on.

[simdesign_ff](https://docs.ropensci.org/nlrx/reference/simdesign_ff.md)

The full factorial simdesign creates a full-factorial parameter matrix
with all possible combinations of parameter values. To setup a
full-factorial simdesign, vectors of values need to be defined for each
variable. Alternatively, a sequence can be defined by setting min, max
and step. However, if both (values and min, max, step) are defined, the
values vector is prioritized.

[simdesign_lhs](https://docs.ropensci.org/nlrx/reference/simdesign_lhs.md)

The latin hypercube simdesign creates a Latin Hypercube sampling
parameter matrix. The method can be used to generate a near-random
sample of parameter values from the defined parameter distributions.
More Details on Latin Hypercube Sampling can be found in McKay (1979)
[doi:10.1080/00401706.1979.10489755](https://doi.org/10.1080/00401706.1979.10489755)
. nlrx uses the [lhs](https://CRAN.R-project.org/package=lhs/index.html)
package to generate the Latin Hypercube parameter matrix. To setup a
latin hypercube sampling simdesign, variable distributions need to be
defined (min, max, qfun).

Sensitivity Analyses:
[simdesign_sobol](https://docs.ropensci.org/nlrx/reference/simdesign_sobol.md),
[simdesign_sobol2007](https://docs.ropensci.org/nlrx/reference/simdesign_sobol2007.md),
[simdesign_soboljansen](https://docs.ropensci.org/nlrx/reference/simdesign_soboljansen.md),
[simdesign_morris](https://docs.ropensci.org/nlrx/reference/simdesign_morris.md),
[simdesign_eFast](https://docs.ropensci.org/nlrx/reference/simdesign_eFast.md)

Sensitivity analyses are useful to estimate the importance of model
parameters and to scan the parameter space in an efficient way. nlrx
uses the
[sensitivity](https://CRAN.R-project.org/package=sensitivity/index.html)
package to setup sensitivity analysis parameter matrices. All supported
sensitivity analysis simdesigns can be used to calculate sensitivity
indices for each parameter-output combination. These indices can be
calculated by using the
[analyze_nl](https://docs.ropensci.org/nlrx/reference/analyze_nl.md)
function after attaching the simulation results to the nl object. To
setup sensitivity analysis simdesigns, variable distributions (min, max,
qfun) need to be defined.

Optimization techniques:
[simdesign_GenSA](https://docs.ropensci.org/nlrx/reference/simdesign_GenSA.md),
[simdesign_GenAlg](https://docs.ropensci.org/nlrx/reference/simdesign_GenAlg.md)

Optimization techniques are a powerful tool to search the parameter
space for specific solutions. Both approaches try to minimize a
specified model output reporter by systematically (genetic algorithm,
utilizing the
[genalg](https://CRAN.R-project.org/package=genalg/index.html) package)
or randomly (simulated annealing, utilizing the
[genSA](https://CRAN.R-project.org/package=GenSA/index.html) package)
changing the model parameters within the allowed ranges. To setup
optimization simdesigns, variable ranges (min, max) need to be defined.
Optimization simdesigns can only be executed using the
[run_nl_dyn](https://docs.ropensci.org/nlrx/reference/run_nl_dyn.md)
function instead of
[run_nl_all](https://docs.ropensci.org/nlrx/reference/run_nl_all.md) or
[run_nl_one](https://docs.ropensci.org/nlrx/reference/run_nl_one.md).

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load nl objects from test data.

# Simdesign examples for Wolf Sheep Predation model from NetLogo models library:

nl <- nl_simple
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 3)
#> Creating simple simulation design

nl <- nl_distinct
nl@simdesign <- simdesign_distinct(nl = nl,
                                   nseeds = 3)
#> Creating distinct simulation design

nl <- nl_ff
nl@simdesign <- simdesign_ff(nl = nl,
                             nseeds = 3)
#> Creating full factorial simulation design

nl <- nl_lhs
nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=100,
                              nseeds=3,
                              precision=3)
#> Creating latin hypercube simulation design

nl <- nl_sobol
nl@simdesign <- simdesign_sobol(nl=nl,
                                samples=200,
                                sobolorder=2,
                                sobolnboot=20,
                                sobolconf=0.95,
                                nseeds=3,
                                precision=3)
#> Creating sobol simulation design

nl <- nl_sobol2007
nl@simdesign <- simdesign_sobol2007(nl=nl,
                                    samples=200,
                                    sobolnboot=20,
                                    sobolconf=0.95,
                                    nseeds=3,
                                    precision=3)
#> Creating sobol2007 simulation design

nl <- nl_soboljansen
nl@simdesign <- simdesign_soboljansen(nl=nl,
                                      samples=200,
                                      sobolnboot=20,
                                      sobolconf=0.95,
                                      nseeds=3,
                                      precision=3)
#> Creating soboljansen simulation design

nl <- nl_morris
nl@simdesign <- simdesign_morris(nl=nl,
                                 morristype="oat",
                                 morrislevels=4,
                                 morrisr=100,
                                 morrisgridjump=2,
                                 nseeds=3)
#> Creating morris simulation design
#> Warning: keeping 32 repetitions out of 100

nl <- nl_eFast
nl@simdesign <- simdesign_eFast(nl=nl,
                                samples=100,
                                nseeds=3)
#> Creating eFast simulation design

nl <- nl_lhs
nl@simdesign <- simdesign_GenAlg(nl=nl,
                                 popSize = 200,
                                 iters = 100,
                                 evalcrit = 1,
                                 elitism = NA,
                                 mutationChance = NA,
                                 nseeds = 1)
#> Creating GenAlg simulation design

nl <- nl_lhs
nl@simdesign <- simdesign_GenSA(nl=nl,
                                par=NULL,
                                evalcrit=1,
                                control=list(max.time = 600),
                                nseeds=1)
#> Creating GenSA simulation design

```

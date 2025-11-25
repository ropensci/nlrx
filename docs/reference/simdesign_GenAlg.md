# Add a Genetic Algorithm simdesign to a nl object

Add a Genetic Algorithm simdesign to a nl object

## Usage

``` r
simdesign_GenAlg(
  nl,
  popSize = 200,
  iters = 100,
  evalcrit = 1,
  elitism = NA,
  mutationChance = NA,
  nseeds = 1
)
```

## Arguments

- nl:

  nl object with a defined experiment

- popSize:

  population Size parameter for genetic algorithm

- iters:

  number of iterations for genetic algorithm function

- evalcrit:

  position of evaluation criterion within defined NetLogo metrics of nl
  experiment or a function that reports a single numeric value

- elitism:

  elitism rate of genetic algorithm function

- mutationChance:

  mutation rate of genetic algorithm function

- nseeds:

  number of seeds for this simulation design

## Value

simdesign S4 class object

## Details

This function creates a simdesign S4 class which can be added to a nl
object.

Variables in the experiment variable list need to provide a numeric
distribution with min and max (e.g. list(min=1, max=4)).

The GenAlg simdesign generates a Genetic Algorithm experiment within the
defined min and max parameter boundaries that are defined in the
variables field of the experiment object within the nl object.

The evalcrit reporter defines the evaluation criterion for the Genetic
algorithm procedure. There are two options to evaluate the fitness value
of each iteration of the algorithm:

1.  Use a reporter that is defined within the experiment metrics vector.
    You can just enter the position of that metric within the experiment
    metrics vector (e.g. 1 would use the first defined metric of the
    experiment to evaluate each iteration). The algorithm automatically
    calculates the mean value of this reporter if evalticks is defined
    to measure multiple ticks during each simulation.

2.  Use a self-defined evaluation function You can define a function
    that post-processes NetLogo output to calculate an evaluation value.
    This function must accept the nl object as input and return one
    single numeric value. The nl object that is then provided to the
    evaluation function will have results of the current iteration
    attached. The results can be accessed via the simoutput slot of the
    simdesign. You can pass this function to evalcrit. It is then
    applied to the output of each iteration.

The function uses the genalg package to set up a Genetic Algorithm
function. For details on the genalg function parameters see
?genalg::rbga Finally, the function reports a simdesign object.

Genetic Algorithm simdesigns can only be executed using the
[run_nl_dyn](https://docs.ropensci.org/nlrx/reference/run_nl_dyn.md)
function instead of
[run_nl_all](https://docs.ropensci.org/nlrx/reference/run_nl_all.md) or
[run_nl_one](https://docs.ropensci.org/nlrx/reference/run_nl_one.md).

## Examples

``` r
# To attach a simdesign, a nl object needs to be created first (see ?nl).
# For this example, we load a nl object from test data.

nl <- nl_lhs

# Example 1: Using a metric from the experiment metrics vector for evaluation:
nl@simdesign <- simdesign_GenAlg(nl=nl,
                                  evalcrit=1,
                                  nseeds=1)
#> Creating GenAlg simulation design

# Example 2: Using a self-defined evaluation function
# For demonstration we define a simple function that calculates
# the maximum value of count sheep output.
critfun <- function(nl) {
results <- nl@simdesign@simoutput
crit <- as.integer(max(results$`count sheep`))
return(crit)
}

nl@simdesign <- simdesign_GenAlg(nl=nl,
                                  evalcrit=critfun,
                                  nseeds=1)
#> Creating GenAlg simulation design

```

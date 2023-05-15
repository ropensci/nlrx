
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlrx <img src="man/figures/logo.png" align="right" width="150" />

<!-- old badges: [![Build Status](https://travis-ci.org/ropensci/nlrx.svg?branch=master)](https://travis-ci.org/ropensci/nlrx)[
![Build status](https://ci.appveyor.com/api/projects/status/swsstjxxjnkyuoh9/branch/master?svg=true)](https://ci.appveyor.com/project/marcosci/nlrx/branch/master) -->
<!-- badges: start -->

[![R build
status](https://github.com/ropensci/nlrx/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/nlrx/actions)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/nlrx/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/nlrx)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/nlrx)](https://cran.r-project.org/package=nlrx)
[![](http://cranlogs.r-pkg.org/badges/grand-total/nlrx)](https://cran.r-project.org/package=nlrx)
[![ropensci](https://badges.ropensci.org/262_status.svg)](https://github.com/ropensci/software-review/issues/262)
[![DOI:10.1111/2041-210X.13286](https://zenodo.org/badge/DOI/10.1111/2041-210X.13286.svg)](https://doi.org/10.1111/2041-210X.13286)
<!-- badges: end -->

The nlrx package provides tools to setup and execute NetLogo simulations
from R. NetLogo is a free, open-source and cross-platform modelling
environment for simulating natural and social phenomena. NetLogo focuses
on implementation of agent-based and spatially explicit simulation
models, although system dynamics models are supported as well. NetLogo
is developed and maintained at the Center for Connected Learning and
Computer-Based Modeling, Northwestern University, Evanston, IL. More
details on NetLogo itself are available online: [NetLogo online
documentation](https://ccl.northwestern.edu/netlogo/docs/)

NetLogo comes with the built-in experiment tool [Behavior
Space](https://ccl.northwestern.edu/netlogo/docs/behaviorspace.html)
that allows to setup and execute model simulations with different
settings and parameter variations and to collect model output. This
experiment tool can be executed via command line in combination with an
XML file that contains the experiment specifications, such as runtime,
variables, output measurements, stop conditions, and more. One
limitation of Behavior Space is, that it only supports full-factorial
parameter designs, which may not be appropriate for complex model
analyses. Furthermore, Behavior Space experiment specifications are
stored within the NetLogo file and are not easily accessible from R.
However, in many cases it is useful to store such specifications along
with the model output and analyses results in order to enable fully
reproducible model analyses.

The nlrx package utilizes the commandline functionality of Behavior
Space to execute NetLogo simulations directly from R. Instead of
defining experiments within NetLogo Behavior Space, experiments are
defined in R using the class objects of the nlrx package. These class
objects hold all the information that is needed to run these experiments
remotely from R, such as path to NetLogo installation folder, path to
the model file and the experiment specifications itself. nlrx provides
useful helper functions to generate parameter input matrices from
parameter range definitions that cover a wide range of parameter
exploration approaches. By storing all relevant information on
simulation experiments, including the output of the model simulations in
one class object, experiments can be easily stored and shared.

In summary, the nlrx package uses a similar structure as NetLogos
Behavior Space but offers more flexibility and additional tools for
running reproducible complex model analyses directly from R.

## Publication

Further information on the package functionality and detailed code
examples can be found in our accompanying publication: Salecker J,
Sciaini M, Meyer KM, Wiegand K. The nlrx r package: A next-generation
framework for reproducible NetLogo model analyses. Methods Ecol Evol.
2019;2041-210X. <https://doi.org/10.1111/2041-210X.13286>.

Get citation information for `nlrx` in R doing
`citation(package = 'nlrx')`.

## Prerequirements

### NetLogo

In order to use the nlrx package, [NetLogo](http://netlogoweb.org/)
(\>=5.3.1) needs to be installed on the system that is used to execute
model simulations (local/remote). For remote execution, NetLogo needs to
be installed on remote machines as well. The nlrx package provides a
utility function (`download_netlogo()`) that can be used to download and
unzip (only unix systems) a specified NetLogo version to a local folder.
For windows machines, the downloaded file needs to be executed in order
to install NetLogo on the local system. If you are running MacOS, please
use the Linux tar.gz version of NetLogo (either from the NetLogo
Homepage or by using the `download_netlogo()` function). The dmg version
from the NetLogo homepage is not compatible with nlrx.

All code snippets on this homepage should be compatible with Netlogo \<=
6.2.2. In version 6.3.0, the folder structure of NetLogo was slightly
updated, thus the modelpath in the code snippets need to be adjusted
accordingly (the `"app/"` folder needs to be removed from the
modelpath).

### Java

Because NetLogo is executed in a Java virtual machine, Java needs to be
installed on the local/remote system as well. We recommend the [Oracle
Java SE Development Kit
8](https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html)
or the [openjdk](https://github.com/ojdkbuild/ojdkbuild). While the nlrx
package might work without setting the Java system path explicitly, we
recommend to make sure that JAVA_HOME points to the correct Java
installation of the system.

## Installation

You can install the released version of nlrx from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nlrx")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/nlrx")
```

## Get started

General information that is needed to run NetLogo simulations remotely,
such as path to the NetLogo installation folder is stored within a `nl`
class object. Nested within this `nl` class are the classes `experiment`
and `simdesign`. The `experiment` class stores all experiment
specifications. After attaching a valid experiment, a `simdesign` class
object can be attached to the `nl` class object, by using one of the
simdesign helper functions. These helper functions create different
parameter input matrices from the experiment variable definitions that
can then be executed by the `run_nl_one()` and `run_nl_all()` functions.
The nested design allows to store everything related to the experiment
within one R object. Additionally, different simdesign helper functions
can be applied to the same `nl` object in order to repeat the same
experiment with different parameter exploration methods (simdesigns).

### Step by step application example

The “Wolf Sheep Predation” model from the NetLogo models library is used
to present a basic example on how to setup and run NetLogo model
simulations from R.

#### Step 1: Create a nl object:

The nl object holds all information on the NetLogo version, a path to
the NetLogo directory with the defined version, a path to the model
file, and the desired memory for the java virtual machine. Depending on
the operation system, paths to NetLogo and the model need to be
adjusted.

``` r
library(nlrx)
# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
outpath <- file.path("C:/out")
# Unix default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("/home/NetLogo 6.0.3")
modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
outpath <- file.path("/home/out")

nl <- nl(nlversion = "6.0.3",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)
```

#### Step 2: Attach an experiment

The experiment object is organized in a similar fashion as NetLogo
Behavior Space experiments. It contains all information that is needed
to generate a simulation parameter matrix and to execute the NetLogo
simulations. Details on the specific slots of the experiment class can
be found in the package documentation (`?experiment`) and the “Advanced
configuration” vignette.

``` r
nl@experiment <- experiment(expname="wolf-sheep",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup",
                            idgo="go",
                            runtime=50,
                            evalticks=seq(40,50),
                            metrics=c("count sheep", "count wolves", "count patches with [pcolor = green]"),
                            variables = list('initial-number-sheep' = list(min=50, max=150, qfun="qunif"),
                                             'initial-number-wolves' = list(min=50, max=150, qfun="qunif")),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "grass-regrowth-time" = 30,
                                             "sheep-gain-from-food" = 4,
                                             "wolf-gain-from-food" = 20,
                                             "sheep-reproduce" = 4,
                                             "wolf-reproduce" = 5,
                                             "show-energy?" = "false"))
```

#### Step 3: Attach a simulation design

While the experiment defines the variables and specifications of the
model, the simulation design creates a parameter input table based on
these model specifications and the chosen simulation design method. nlrx
provides a bunch of different simulation designs, such as
full-factorial, latin-hypercube, sobol, morris and eFast (see “Simdesign
Examples” vignette for more information on simdesigns). All simdesign
helper functions need a properly defined nl object with a valid
experiment design. Each simdesign helper also allows to define a number
of random seeds that are randomly generated and can be used to execute
repeated simulations of the same parameter matrix with different
random-seeds (see “Advanced configuration” vignette for more information
on random-seed and repetition management). A simulation design is
attached to a nl object by using one of the simdesign helper functions:

``` r
nl@simdesign <- simdesign_lhs(nl=nl,
                               samples=100,
                               nseeds=3,
                               precision=3)
```

#### Step 4: Run simulations

All information that is needed to run the simulations is now stored
within the nl object. The `run_nl_one()` function allows to run one
specific simulation from the siminput parameter table. The
`run_nl_all()` function runs a loop over all simseeds and rows of the
parameter input table siminput. The loops are constructed in a way that
allows easy parallelisation, either locally or on remote HPC machines
(see “Advanced configuration” vignette for more information on
parallelisation). Before running your simulations you might want to
check your current nl object setup. `eval_variables_constants(nl)`
evaluates if the defined variables and constants are correctly defined
and are consistent with the attached model. `print(nl)` prints a
complete summary of the provided nl object including checkmarks that
might help to indicate potential problems.

``` r
# Evaluate nl object:
eval_variables_constants(nl)
print(nl)

# Run all simulations (loop over all siminputrows and simseeds)
results <- run_nl_all(nl)
```

#### Step 5: Attach results to nl and run analysis

nlrx provides method specific analysis functions for each simulation
design. Depending on the chosen design, the function reports a tibble
with aggregated results or sensitivity indices. In order to run the
analyze_nl function, the simulation output has to be attached to the nl
object first. The simdesign class within the nl object provides a slot
for attaching output results (simoutput). An output results tibble can
be attached to this slot by using the simdesign setter function
`setsim(nl, "simoutput")`. After attaching the simulation results, these
can also be written to the defined outpath of the experiment object.

``` r
# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Write output to outpath of experiment within nl
write_simoutput(nl)

# Do further analysis:
analyze_nl(nl)
```

## Meta

- Please [report any issues or
  bugs](https://github.com/ropensci/nlrx/issues/new).
- License: GPL3
- Get citation information for `nlrx` in R doing
  `citation(package = 'nlrx')`
- We are very open to contributions - if you are interested check
  [Contributing](https://github.com/ropensci/nlrx/blob/master/CONTRIBUTING.md).
  - Please note that this project is released with a [Contributor Code
    of
    Conduct](https://github.com/ropensci/nlrx/blob/master/CODE_OF_CONDUCT.md).
    By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)

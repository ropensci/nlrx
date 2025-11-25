# Construct a new nl object

Construct a new nl object

## Usage

``` r
nl(
  nlversion = "6.0.2",
  nlpath = character(),
  modelpath = character(),
  jvmmem = 1024,
  experiment = methods::new("experiment"),
  simdesign = methods::new("simdesign"),
  ...
)
```

## Arguments

- nlversion:

  A character string defining the NetLogo version that is used

- nlpath:

  Path to the NetLogo main directory matching the defined version

- modelpath:

  Path to the NetLogo model file (\*.nlogo) that is used for simulations

- jvmmem:

  Java virtual machine memory capacity in megabytes

- experiment:

  Holds a experiment S4 class object

- simdesign:

  Holds a simdesign S4 class object

- ...:

  ...

## Value

nl S4 class object

## Details

nl objects are the main class objects used in the nlrx package. These
objects store all information that is needed to run NetLogo simulations.
nl objects are initialized with basic information on Netlogo and the
model.

After setting up the nl object, an experiment needs to be attached. The
experiment class stores all information related to the NetLogo
simulation experiment, such as runtime, variables, constants,
measurements, and more.

After attaching an experiment, different simdesign helper functions can
be used to attach a simdesign to the nl object. The simdesign helper
functions use the variable definitions from the experiment within the nl
object to generate a parameter tibble for simulations.

## Examples

``` r
# Example for Wolf Sheep Predation model from NetLogo models library:
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

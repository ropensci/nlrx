# Resubmission

nlrx version 0.4.2

## CRAN check error fixes
* An error was thrown because of a dependency on external files in the roxygen example of the nldoc function.
* This error has now been fixed.

## Changes in version 0.4.2
#### functionality
* added option to run_nl_one that allows to store results as rds files
* added eval_simoutput option to check for missing combinations of siminputrow and random-seeds

#### bugfixes
* hotfix for another dependency on external files in nldoc roxygen examples
* small bugfix in analyze_morris: A warning is now thrown if NA are present in the simulation data
* bugfix in random seed generator
* bugfix for sobol simulation design when sobolorder is higher than the available number of variables
* analyze_nl now prints a warning if missing combinations were detected in the simulation output
* updated testdata
* user rights for temporary sh scripts are now set correctly


## Test environments
* Windows 10, R 3.6.1
* Windows Server 2012 R2 x64, R 3.6.1
* ubuntu 18.10, R 3.6.1
* macOS Mojave, R 3.6.1
* win-builder (release and devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are currently no reverse dependencies.

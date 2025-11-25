# Changelog

## nlrx 0.4.6

### Functionality

- Updated readr::file_write function to avoid pkg break due to
  deprecation
- added support to download_netLogo() for up to NetLogo version 6.4.0

## nlrx 0.4.5

CRAN release: 2024-01-29

### Functionality

- No functionality updates

### Bugfixes

- documentation update to fix CRAN check notes

## nlrx 0.4.4

CRAN release: 2023-05-16

### Functionality

- added support to download_netLogo() for up to NetLogo version 6.3.0
- adjusted test_nlrx() to work with new NetLogo directory structure
  (6.3.0)
- added writeRDS parameter to run_nl_all() for storing intermediate
  results

### Bugfixes

- updating Java Version Requirement in DESCRIPTION
- refactoring download_netlogo function
- Fix readr problems with util_runnl() (deprecated in path argument in
  write_lines)
- Fix path problems
- Fix to allow for list output in agent variables (metrics.turtles,
  metrics.patches, metrics.links)

## nlrx 0.4.3

CRAN release: 2021-09-20

### Functionality

- added test_nlrx() function to check functionality of the package
- added support to download_netLogo() for NetLogo version 6.2.0

### Bugfixes

- changed timestamp for nldoc function from lubridate to base date
- fixed an error in the function parser of the nldoc procedure
- fixed bug in calculation of number of computed runs in print_nl()

## nlrx 0.4.2

CRAN release: 2020-11-13

### Functionality

- added option to run_nl_one that allows to store results as rds files
- added eval_simoutput option to check for missing combinations of
  siminputrow and random-seeds
- added support for progressr progress bars for run_nl_all function
  (details see further notes vignette) and removed the silent parameter
  of the run_nl_all function

### Bugfixes

- hotfix for another dependency on external files in nldoc roxygen
  examples
- small bugfix in analyze_morris: A warning is now thrown if NA are
  present in the simulation data
- bugfix in random seed generator
- bugfix for sobol simulation design when sobolorder is higher than the
  available number of variables
- analyze_nl now prints a warning if missing combinations were detected
  in the simulation output
- updated testdata
- user rights for temporary sh scripts are now set correctly

## nlrx 0.4.1

CRAN release: 2020-02-07

- fixed dependency on external file source in nldoc automated tests
- these files are now included in the package
- added link to documentation website in description

## nlrx 0.4.0

CRAN release: 2019-12-03

- Added new simdesigns simdesign_ABCmcmc_Marjoram,
  simdesign_ABCmcmc_Marjoram_original and simdesign_ABCmcmc_Wegmann to
  perform approximate bayesian computation
- Added print function for nl objects
- Added dependencies: crayon, EasyABC
- Added pandoc to system requirements
- Added additional pandoc_available() check for nldoc function
- Added support to download_netLogo() for NetLogo version 6.1.1
- Added new vignette showing an example for approximate bayesian
  computation with nlrx
- Updated “Sensitivity Analyses with nlrx”” vignette
- Updated “Advanced Configuration” vignette
- Updated package tests

## nlrx 0.3.0

CRAN release: 2019-09-26

- Added support for self-defined evaluation functions to optimization
  functions simdesign_GenAlg and simdesign_GenSA.
- Added support to simdesign_simple() for models without any GUI
  parameters
- Added support to download_netLogo() for NetLogo version 6.1.0
- Added new vignette showing an example for Sensitivity Analysis with
  nlrx.
- Added new vignette showing an example for Optimization with nlrx.
- Updated “Advanced Configuration” vignette.
- Updated citation information of the package.
- Hotfix for unnest_simoutput(). In the previous package version, under
  some circumstances an error occured due to NA data.
- Corrected spelling errors in some vignettes and documentation files.

## nlrx 0.2.0

CRAN release: 2019-03-28

- nl_to_raster() hotfix

## nlrx 0.1.0

CRAN release: 2019-03-18

- First release to CRAN.

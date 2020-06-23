
## nlrx 0.4.2
* hotfix for another dependency on external files in nldoc roxygen examples
* small bugfix in analyze_morris: A warning is now thrown if NA are present in the simulation data
* bugfix in random seed generator
* added option to run_nl_one that allows to store results as rds files
* bugfix for sobol simulation design when sobolorder is higher than the available number of variables
* added eval_simoutput option to check for missing combinations of siminputrow and random-seeds

## nlrx 0.4.1
* fixed dependency on external file source in nldoc automated tests
* these files are now included in the package
* added link to documentation website in description

## nlrx 0.4.0

* Added new simdesigns simdesign_ABCmcmc_Marjoram, simdesign_ABCmcmc_Marjoram_original and simdesign_ABCmcmc_Wegmann to perform approximate bayesian computation
* Added print function for nl objects
* Added dependencies: crayon, EasyABC
* Added pandoc to system requirements
* Added additional pandoc_available() check for nldoc function
* Added support to download_netLogo() for NetLogo version 6.1.1
* Added new vignette showing an example for approximate bayesian computation with nlrx
* Updated "Sensitivity Analyses with nlrx"" vignette
* Updated "Advanced Configuration" vignette
* Updated package tests


## nlrx 0.3.0

* Added support for self-defined evaluation functions to optimization functions simdesign_GenAlg and simdesign_GenSA.
* Added support to simdesign_simple() for models without any GUI parameters
* Added support to download_netLogo() for NetLogo version 6.1.0
* Added new vignette showing an example for Sensitivity Analysis with nlrx.
* Added new vignette showing an example for Optimization with nlrx.
* Updated "Advanced Configuration" vignette.
* Updated citation information of the package.
* Hotfix for unnest_simoutput(). In the previous package version, under some circumstances an error occured due to NA data.
* Corrected spelling errors in some vignettes and documentation files.

## nlrx 0.2.0

* nl_to_raster() hotfix

## nlrx 0.1.0

* First release to CRAN.

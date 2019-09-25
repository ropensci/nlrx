# Resubmission

This is a resubmission of nlrx version 0.3.0.
In the previous submission (possibly) invalid URLs were found in README.Rmd.
We have fixed these issues and updated the URLs to the correct format.

# Changes in version 0.3.0

* Added support for self-defined evaluation functions to optimization functions simdesign_GenAlg and simdesign_GenSA.
* Added support to simdesign_simple() for models without any GUI parameters
* Added support to download_netLogo() for NetLogo version 6.1.0
* Added new vignette showing an example for Sensitivity Analysis with nlrx.
* Added new vignette showing an example for Optimization with nlrx.
* Updated "Advanced Configuration" vignette.
* Updated citation information of the package.
* Hotfix for unnest_simoutput(). In the previous package version, under some circumstances an error occured due to NA data.
* Corrected spelling errors in some vignettes and documentation files.

We also fixed problems with automated tests for the nldoc() function.
These were shown on the CRAN Package check page.
Errors were thrown because of missing pandoc installations.
Thus, we added a check and only run the tests if there is a proper pandoc installation on the system.

## Test environments
* Windows 10, R 3.5.2
* ubuntu 18.10, R 3.5.2
* macOS Mojave, R 3.5.2
* win-builder (release and devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are currently no reverse dependencies.

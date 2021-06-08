# Resubmission

nlrx version 0.4.3


## CRAN check error fixes
* updated maintainer email address (package was archived because of an auto-response from the previous email address)

## Changes in version 0.4.3

#### Functionality
* added test_nlrx() function to check functionality of the package
* added support to download_netLogo() for NetLogo version 6.2.0

#### Bugfixes
* changed timestamp for nldoc function from lubridate to base date
* fixed an error in the function parser of the nldoc procedure
* fixed bug in calculation of number of computed runs in print_nl()


## Test environments
* Windows 10, R 4.0.2
* Windows Server 2012 R2 x64, R 4.0.3 (appveyor)
* ubuntu 16.04.6 LTS, R 4.0.2 (travis)
* macOS Mojave, R 4.0.2
* win-builder (release and devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are currently no reverse dependencies.

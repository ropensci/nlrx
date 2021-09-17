# Resubmission

This is a resubmission. In this version I have:

* Updated the broken URL for the lifecycle badge in the README.md 


# Original (Re-) Submission

nlrx version 0.4.3

## CRAN check error fixes
* updated maintainer and maintainer email address (package was archived because of an auto-response from the previous email address)

## Changes in version 0.4.3

#### Functionality
* added test_nlrx() function to check functionality of the package
* added support to download_netLogo() for NetLogo version 6.2.0

#### Bugfixes
* changed timestamp for nldoc function from lubridate to base date
* fixed an error in the function parser of the nldoc procedure
* fixed bug in calculation of number of computed runs in print_nl()

## Test environments
* win-builder (release and devel)
* Windows 10, R 4.1.0
* macOS 11.3 (Big Sur), R 4.1.0
* macOS 10.14 (Mojave), R 4.1.0
* macOS 10.15 (Catalina), R 4.1.0 (github actions)
* Windows Server 2019 x64, R 4.1.0 (github actions)
* Ubuntu 20.04, R 4.1.0 (github actions)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are currently no reverse dependencies.

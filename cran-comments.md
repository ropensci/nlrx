# Resubmission

This is a resubmission. In this version I have:

* Adjusted java system dependency in DESCRIPTION according to standard form
* added new functionality and bugfixes (see details below)


# Original (Re-) Submission

nlrx version 0.4.4

## CRAN check error fixes

* Adjusted java system dependency in DESCRIPTION according to standard form

## Changes in version 0.4.4

#### Functionality

* added support to download_netLogo() for up to NetLogo version 6.3.0 
* adjusted test_nlrx() to work with new NetLogo directory structure (6.3.0)
* added writeRDS parameter to run_nl_all() for storing intermediate results

#### Bugfixes

* refactoring download_netlogo function
* Fix readr problems with util_runnl() (deprecated in path argument in write_lines)
* Fix path problems
* Fix to allow for list output in agent variables (metrics.turtles, metrics.patches, metrics.links)

## Test environments
* win-builder (release and devel)
* Windows 10, R 4.3.0
* macOS 13.3.1 (Ventura), R 4.3.0
* macOS 13.3.1 (Ventura), R 4.3.0 (github actions)
* Windows Server 2022, R 4.3.0 (github actions)
* Ubuntu 20.04, R 4.3.0 (github actions)
* Ubuntu 22.04, R 4.3.0 (github actions)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are currently no reverse dependencies.

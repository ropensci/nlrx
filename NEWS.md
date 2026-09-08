
# nlrx 0.5.0

## Major changes

* Added support for NetLogo 7 and newer. The simulation backend was redesigned around the 'logolink' package, which generates the BehaviorSpace XML and executes NetLogo.
* Simulations are now executed in blocks. `run_nl_all()` gains `block_size` (number of simulations bundled into one NetLogo instance) and `threads` (NetLogo's native multithreading) arguments, replacing the previous future/`furrr`-based parallelism.
* Performance: bundling many simulations as BehaviorSpace sub-experiments within a single NetLogo instance avoids the repeated JVM/NetLogo start-up of the previous one-process-per-run approach, which can substantially speed up larger designs.
* `run_nl_one()` now accepts a vector of random seeds and executes the parameterisation once per seed. The replicated runs are executed within one NetLogo instance and can be parallelised with `threads`.
* `run_nl_dyn()` now runs one complete optimization for each random seed of the simdesign, instead of a single one for a seed given as an argument. The results are returned as a tibble with one row per seed, holding the seed and the result object of the respective optimization in a `result` list column. The objects are stored as the optimization packages return them, so their summary and plot functions still apply (`results$result[[1]]`). A failing optimization no longer discards the results of the other seeds: its error is stored in the `result` column and a warning names the affected seeds.
* `run_nl_dyn()` gains an `nreplicates` argument. Each parameterisation proposed by a dynamic simdesign (`GenSA`, `GenAlg`, `ABCmcmc_*`) is then simulated `nreplicates` times with different random seeds, which reduces the simulation noise of the evaluation criterion. The replicate seeds are derived from the `seed` argument and are identical across the evaluations of one call (common random numbers), so they do not have to be stored.

## Bugfixes

* Simulation results of dynamic simdesigns are now aggregated in two steps, first over the measured ticks of each run and then over the replicated runs. Previously a single mean was calculated over all returned rows, which weighted runs with more measured ticks higher than shorter ones.

## Breaking changes

* Support for NetLogo versions prior to 7.0.0 has been removed. NetLogo (>= 7.0.0) and 'logolink' (>= 1.0.0) are now required to run simulations.
* The `seed` argument of `run_nl_dyn()` is deprecated. The seeds of the simdesign are used instead, as in `run_nl_all()`. Passing `seed` still works for this release but emits a deprecation warning; to run a subset of the seeds, reduce them with `setsim(nl, "simseeds") <- ...`. Note that the return value of `run_nl_dyn()` changed with this (see above).
* The `repetition` slot of the experiment class has been removed, together with the `repetition` argument of `experiment()`. Repetitions were executed by NetLogo BehaviorSpace with seeds that nlrx neither controlled nor reported, so repeated runs could not be reproduced, and with more than one repetition the results could not be mapped back to their parameterisation reliably. Passing `repetition = 1` still works but emits a deprecation warning and has no effect; `repetition > 1` is an error. Use `nseeds` in the simdesign helpers instead, or `nreplicates` in `run_nl_dyn()` for dynamic designs.
* The legacy execution backend (OS-specific batch-file generation and direct NetLogo calls) has been removed; execution is delegated entirely to 'logolink'.
* The arguments `split`, `cleanup.csv`, `cleanup.xml`, `cleanup.bat` and `writeRDS` are deprecated and no longer have an effect (file handling is managed by 'logolink'). Passing them now emits a deprecation warning.
* The `n_cluster` argument of the ABC-MCMC simdesigns (`simdesign_ABCmcmc_Marjoram()`, `simdesign_ABCmcmc_Marjoram_original()`, `simdesign_ABCmcmc_Wegmann()`) has been removed. It never parallelised simulations (it was always reset to 1) and is now deprecated; passing it emits a deprecation warning.
* NetLogo 7 introduced a new model file format (`.nlogox`) and converts widget sizes. Models created in older NetLogo versions must be opened and re-saved in NetLogo 7 before they can be used with nlrx. See the NetLogo transition guide for details: <https://docs.netlogo.org/transition>.

# nlrx 0.4.6

## Functionality

* Updated readr::file_write function to avoid pkg break due to deprecation
* added support to download_netLogo() for up to NetLogo version 6.4.0 

# nlrx 0.4.5

## Functionality

* No functionality updates

## Bugfixes

* documentation update to fix CRAN check notes

# nlrx 0.4.4

## Functionality

* added support to download_netLogo() for up to NetLogo version 6.3.0 
* adjusted test_nlrx() to work with new NetLogo directory structure (6.3.0)
* added writeRDS parameter to run_nl_all() for storing intermediate results

## Bugfixes

* updating Java Version Requirement in DESCRIPTION
* refactoring download_netlogo function
* Fix readr problems with util_runnl() (deprecated in path argument in write_lines)
* Fix path problems
* Fix to allow for list output in agent variables (metrics.turtles, metrics.patches, metrics.links)



# nlrx 0.4.3

## Functionality

* added test_nlrx() function to check functionality of the package
* added support to download_netLogo() for NetLogo version 6.2.0

## Bugfixes

* changed timestamp for nldoc function from lubridate to base date
* fixed an error in the function parser of the nldoc procedure
* fixed bug in calculation of number of computed runs in print_nl()


# nlrx 0.4.2

## Functionality

* added option to run_nl_one that allows to store results as rds files
* added eval_simoutput option to check for missing combinations of siminputrow and random-seeds
* added support for progressr progress bars for run_nl_all function (details see further notes vignette) and removed the silent parameter of the run_nl_all function

## Bugfixes
* hotfix for another dependency on external files in nldoc roxygen examples
* small bugfix in analyze_morris: A warning is now thrown if NA are present in the simulation data
* bugfix in random seed generator
* bugfix for sobol simulation design when sobolorder is higher than the available number of variables
* analyze_nl now prints a warning if missing combinations were detected in the simulation output
* updated testdata
* user rights for temporary sh scripts are now set correctly

# nlrx 0.4.1
* fixed dependency on external file source in nldoc automated tests
* these files are now included in the package
* added link to documentation website in description

# nlrx 0.4.0

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


# nlrx 0.3.0

* Added support for self-defined evaluation functions to optimization functions simdesign_GenAlg and simdesign_GenSA.
* Added support to simdesign_simple() for models without any GUI parameters
* Added support to download_netLogo() for NetLogo version 6.1.0
* Added new vignette showing an example for Sensitivity Analysis with nlrx.
* Added new vignette showing an example for Optimization with nlrx.
* Updated "Advanced Configuration" vignette.
* Updated citation information of the package.
* Hotfix for unnest_simoutput(). In the previous package version, under some circumstances an error occured due to NA data.
* Corrected spelling errors in some vignettes and documentation files.

# nlrx 0.2.0

* nl_to_raster() hotfix

# nlrx 0.1.0

* First release to CRAN.

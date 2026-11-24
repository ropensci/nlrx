
# nlrx 0.5.0

## Major changes

* `run_nl_dyn()` tells upfront how many evaluations the optimization of each seed may take and shows their progress. Every evaluation is one NetLogo run, so the defaults of the dynamic simdesigns can take hours to days (for example 20,000 runs per seed for `simdesign_GenAlg()`), which looked like a hanging session: each run printed its log lines without any indication of the total. The log lines of the single runs are now held back and only shown when a run fails, a progress bar counts the evaluations, and a GenSA simdesign without any stop criterion in `control` produces a warning. The defaults are unchanged; the examples and vignettes use settings that finish within minutes.
* `simdesign_ABCmcmc_Marjoram()` and `simdesign_ABCmcmc_Wegmann()` check that the calibration keeps at least two runs (`ceiling(tolerance_quantile * n_calibration) >= 2`). With fewer, 'EasyABC' failed with an obscure "subscript out of bounds" or "incorrect number of dimensions" error after the whole calibration had run.
* nlrx no longer depends on the 'XML' package; NetLogo model files are read with 'xml2'.
* Added support for NetLogo 7 and newer (#106). The simulation backend was redesigned around the 'logolink' package, which generates the BehaviorSpace XML and executes NetLogo.
* Simulations are now executed in blocks (#60). `run_nl_all()` gains `block_size` (number of simulations bundled into one NetLogo instance) and `threads` (NetLogo's native multithreading) arguments, replacing the previous future/`furrr`-based parallelism.
* Performance: bundling many simulations as BehaviorSpace sub-experiments within a single NetLogo instance avoids the repeated JVM/NetLogo start-up of the previous one-process-per-run approach, which can substantially speed up larger designs (#94, #95, #101).
* `run_nl_one()` now accepts a vector of random seeds and executes the parameterisation once per seed. The replicated runs are executed within one NetLogo instance and can be parallelised with `threads`.
* `run_nl_dyn()` now runs one complete optimization for each random seed of the simdesign, instead of a single one for a seed given as an argument (#88). The results are returned as a tibble with one row per seed, holding the seed and the result object of the respective optimization in a `result` list column. The objects are stored as the optimization packages return them, so their summary and plot functions still apply (`results$result[[1]]`). A failing optimization no longer discards the results of the other seeds: its error is stored in the `result` column and a warning names the affected seeds.
* `run_nl_dyn()` gains an `nreplicates` argument. Each parameterisation proposed by a dynamic simdesign (`GenSA`, `GenAlg`, `ABCmcmc_*`) is then simulated `nreplicates` times with different random seeds, which reduces the simulation noise of the evaluation criterion. The replicate seeds are derived from the `seed` argument and are identical across the evaluations of one call (common random numbers), so they do not have to be stored.

* `nlpath` is optional and decides which NetLogo installation is used again. It defaults to the `NETLOGO_HOME` environment variable, so it can be omitted when that is set, and a given path now takes precedence: `NETLOGO_HOME` is set to it for the duration of a run and restored afterwards, so different nl objects can use different NetLogo installations within one session. Up to now `nlpath` was ignored by the execution path entirely — 'logolink' resolves NetLogo from `NETLOGO_HOME` and offers no argument for it — so an nl object could report one installation while another one ran. A missing installation is now reported with a clear error instead of failing inside 'logolink', and an `nlpath` that does not exist on the current system (for example in an nl object restored with `import_nl()`) warns and falls back to `NETLOGO_HOME`.
* `simdesign_morris()` gains a `scale` argument. The input factors were always scaled to [0,1] before the elementary effects were computed; passing `scale = FALSE` now yields unscaled effects.
* `experiment()` reports `evalticks` that lie beyond the `runtime` of the experiment (#111). Such ticks are never measured, so the simulation used to complete without results for them and without any indication why.
* Untested NetLogo 7 versions are no longer rejected. `nl()` and `download_netlogo()` now emit a warning and continue when the NetLogo version is not among the versions nlrx was tested with (`supported_netlogo_versions()`): simulations are executed by 'logolink', so newer releases are likely to work without an nlrx update. Versions below 7.0.0 remain an error, since their backend was removed, and `check_version = FALSE` silences the warning.

## Bugfixes

* `nldoc()` documents the GUI elements and BehaviorSpace experiments of NetLogo 7 models again. Both were still read with the parser for the NetLogo 6 file format, so the GUI section was silently left out and the BehaviorSpace tables lacked the time limit, metrics, final commands and parameters. The GUI elements are now read with the same parser as `report_model_parameters()`, and the BehaviorSpace section covers the NetLogo 7 experiment definitions, including pre- and post-experiment commands, the measurement condition and sub-experiments. The `toc` and `number_sections` options now also apply to PDF output, and rendering no longer prints the pandoc command line.
* `nldoc()` no longer fails on models without complete noxygen annotations. A model without an `@model` header failed with "missing value where TRUE/FALSE needed", and one without `@global` or `@procedure` tags with "argument is of length zero". Without a header, the name of the model file is now used as title; sections without documented globals or procedures are left out, and a message points out when the model has no annotations at all.
* `export_nl()` no longer archives more than the exported folder. A relative `path` such as `"."` made it pack the whole working directory and then the whole R session's temporary directory, and folders of earlier exports in the same session were mixed into the archive. The archive is now assembled in a staging folder of its own and written with `utils::tar()`, without listing every file in the console. The home directory and the filesystem root are refused, as is an archive stored within the exported folder, and a message reports how many files were exported. `export_nl()` returns the path of the archive invisibly.
* `import_nl()` returns the imported nl object (invisibly) instead of the status of the `tar` command, and `new_session = TRUE` opens the `.Rproj` file of the extracted folder. It looked for that file within the archive file itself, so it never found one. Archives that hold no `nlobject.rds` are rejected before anything is extracted.
* Models that use NetLogo extensions no longer fail with "Can't find extension" (#77, #98). Extensions are resolved by NetLogo itself again, instead of depending on the directories and Java options that the former backend prepared.
* Experiments accept a vector of procedures in `idsetup`, `idgo`, `idfinal` and `stopcond` again. Passing one failed with "the condition has length > 1", although the documentation describes these slots as taking a vector; the entries are joined into one block of NetLogo code, as before the backend redesign.
* Simulation results are no longer lost when the execution blocks report a metric with different types (#110, #90). A run whose metric returns a NetLogo error message made the binding of the blocks fail, which discarded the results of every completed run. Such columns are converted to character instead, with a warning naming them.
* `download_netlogo()` fetches the macOS disk image that matches the architecture of the machine. It always requested the Intel (`x86_64`) image, so users on Apple Silicon received a build for the wrong architecture. The architecture is now detected, also under Rosetta, and can be set explicitly with the new `arch` argument.
* Requested `evalticks` that carry no simulation results are now reported (#111). Simulations can end before a requested tick is reached, through `runtime` or a `stopcond`, in which case the results silently contained fewer ticks than asked for. nlrx 0.4.x intended such a message, but its index expression (`evalticks[!which(evalticks %in% steps)]`) coerced indices to logical and could never select anything.
* `simdesign_GenSA(par = )` is no longer ignored. The start values were stored in the simdesign, but `GenSA::GenSA()` was always called with `par = NULL`, so every optimization started from a random point within the parameter boundaries. The values are now passed through, and `simdesign_GenSA()` checks them against the number, order and boundaries of the experiment variables.
* String parameters no longer reach NetLogo double-quoted. 'logolink' quotes character values itself, so the escaped-quote convention of earlier nlrx versions (`"model-version" = "\"sheep-wolves-grass\""`) was written to the BehaviorSpace XML as `""sheep-wolves-grass""` and aborted every run of the experiment with "Failed to parse specified setup file". One layer of enclosing double quotes is now removed from the values of constants and variables, so experiment definitions written for nlrx <= 0.4.6 keep working. Values are expected as plain character values from now on, and the documentation was updated accordingly.
* Boolean parameters given as strings are converted to logicals for constants as well. The conversion previously applied only to the simulation input table, so a switch defined via `constants = list("show-energy?" = "false")` was passed to NetLogo as the string `"false"`. Both capitalisations (`"TRUE"`/`"true"`) are now recognised. Booleans are expected as logicals from now on.
* Simulation results of dynamic simdesigns are now aggregated in two steps, first over the measured ticks of each run and then over the replicated runs. Previously a single mean was calculated over all returned rows, which weighted runs with more measured ticks higher than shorter ones.

## Breaking changes

* Support for NetLogo versions prior to 7.0.0 has been removed. NetLogo (>= 7.0.0) and 'logolink' (>= 1.0.0) are now required to run simulations.
* The `seed` argument of `run_nl_dyn()` is deprecated. The seeds of the simdesign are used instead, as in `run_nl_all()`. Passing `seed` still works for this release but emits a deprecation warning; to run a subset of the seeds, reduce them with `setsim(nl, "simseeds") <- ...`. Note that the return value of `run_nl_dyn()` changed with this (see above).
* The `repetition` slot of the experiment class has been removed, together with the `repetition` argument of `experiment()`. Repetitions were executed by NetLogo BehaviorSpace with seeds that nlrx neither controlled nor reported, so repeated runs could not be reproduced, and with more than one repetition the results could not be mapped back to their parameterisation reliably. Passing `repetition = 1` still works but emits a deprecation warning and has no effect; `repetition > 1` is an error. Use `nseeds` in the simdesign helpers instead, or `nreplicates` in `run_nl_dyn()` for dynamic designs.
* The legacy execution backend (OS-specific batch-file generation and direct NetLogo calls) has been removed; execution is delegated entirely to 'logolink'.
* NetLogo literals can no longer be used as parameter values. Since 'logolink' quotes every character value and offers no escape hatch, a value such as `"[1 2 3]"` reaches NetLogo as a string instead of a list. `eval_variables_constants()` reports values that look like list literals; define such globals within the model instead.
* The arguments `split`, `cleanup.csv`, `cleanup.xml`, `cleanup.bat` and `writeRDS` are deprecated and no longer have an effect (file handling is managed by 'logolink'). Passing them now emits a deprecation warning.
* The `n_cluster` argument of the ABC-MCMC simdesigns (`simdesign_ABCmcmc_Marjoram()`, `simdesign_ABCmcmc_Marjoram_original()`, `simdesign_ABCmcmc_Wegmann()`) has been removed. It never parallelised simulations (it was always reset to 1) and is now deprecated; passing it emits a deprecation warning.
* NetLogo 7 introduced a new model file format (`.nlogox`) and converts widget sizes. Models created in older NetLogo versions must be opened and re-saved in NetLogo 7 before they can be used with nlrx. See the NetLogo transition guide for details: <https://docs.netlogo.org/transition>. `nl()`, `report_model_parameters()`, `nldoc()` and the run functions stop with this advice when they are given a `.nlogo` file. NetLogo 7 would convert such a file on the fly, but nlrx cannot read the old format to check parameters.

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

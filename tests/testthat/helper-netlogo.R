## Shared NetLogo discovery for the tests that execute simulations.
##
## The tests used to hardcode one installation path per operating system and to
## run only when GITHUB_ACTIONS was set. Both are gone: the installation is
## taken from NETLOGO_HOME, the variable logolink itself resolves, so the same
## tests run locally and in CI, and they skip - rather than fail - wherever no
## NetLogo installation is available.

netlogo_home <- function() {
  path <- Sys.getenv("NETLOGO_HOME")

  if (nzchar(path) && dir.exists(path)) {
    return(path)
  }

  return("")
}

wolf_sheep_model <- function(path) {
  file.path(path, "models", "Sample Models", "Biology", "Wolf Sheep Predation.nlogox")
}

## Returns the NetLogo installation to test against, or skips the test.
skip_without_netlogo <- function() {
  path <- netlogo_home()

  testthat::skip_if(
    !nzchar(path),
    "No NetLogo installation found (set NETLOGO_HOME to test the execution path)"
  )
  testthat::skip_if(
    !file.exists(wolf_sheep_model(path)),
    paste0("NETLOGO_HOME does not contain the sample models: ", path)
  )

  return(path)
}

## Gate for tests whose runtime is NetLogo start-up rather than the code they
## cover. The calibrated ABC sampler in test-exe3_dyn_nl_abc.R is the one that
## matters: it drives about a hundred sequential simulations, measured as 362s of
## the 468s R CMD check on ubuntu-latest and 3060s of a 69-minute macos-latest
## job. Its uncalibrated counterpart in the same file reaches the identical nlrx
## code in about six seconds and is not gated, so every platform keeps covering
## the ABC execution path.
##
## What the gated test adds on top is platform independent - do.call() into
## EasyABC with Marjoram's own arguments, and invoking the NetLogo callback a
## hundred times instead of twice - so running it once per CI run is enough.
##
## The CI matrix sets NLRX_SLOW_TESTS on Linux release and in the coverage job.
## Locally, run these with NLRX_SLOW_TESTS=true (or Sys.setenv()) before a
## release - they are skipped by default.
skip_if_slow_tests_disabled <- function() {
  testthat::skip_if_not(
    isTRUE(as.logical(Sys.getenv("NLRX_SLOW_TESTS", "false"))),
    "Slow execution test; set NLRX_SLOW_TESTS=true to run it"
  )
}

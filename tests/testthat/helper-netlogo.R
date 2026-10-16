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

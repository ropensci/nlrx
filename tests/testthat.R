Sys.unsetenv("R_TESTS")

library(testthat)
library(nlrx)

## The execution tests run NetLogo simulations and dominate the runtime of
## R CMD check, with macOS an order of magnitude slower than Linux. The default
## CheckReporter prints nothing between "checking tests" and the final summary,
## so the logs give no clue where that time goes. Pairing it with a
## ProgressReporter adds a per-file duration without changing how failures are
## reported: max_fail = Inf leaves the abort behaviour to CheckReporter, and
## update_interval = Inf suppresses the incremental redraws that only make sense
## on an interactive console.
test_check("nlrx", reporter = testthat::MultiReporter$new(reporters = list(
  testthat::CheckReporter$new(),
  testthat::ProgressReporter$new(max_fail = Inf, update_interval = Inf)
)))
